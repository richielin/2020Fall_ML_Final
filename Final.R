library(data.table)
library(lubridate)
library(devtools)
library(ggplot2)
library(BBmisc)

# df <- fread("https://data.cityofnewyork.us/resource/ic3t-wcy2.csv?$query=select%20*%20limit%209999999")
# fwrite(df, "C:\\Users\\Richie\\iCloudDrive\\PHD\\Classes\\DS\\Machine Learning\\Final\\Data\\DOB_12152020.csv")

# df <- fread("https://data.cityofnewyork.us/resource/nc67-uf89.csv?$query=select%20*%20limit%2099999999")
# fwrite(df, "C:\\Users\\Richie\\iCloudDrive\\PHD\\Classes\\DS\\Machine Learning\\Final\\Data\\Parking_01102021.csv")
# 


## Loading data

setwd("C:\\Users\\Richie\\iCloudDrive\\PHD\\Classes\\DS\\Machine Learning\\Final\\Data\\")
df <- fread("C:\\Users\\Richie\\iCloudDrive\\PHD\\Classes\\DS\\Machine Learning\\Final\\Data\\Parking_01102021.csv", 
            nrow = 500000, stringsAsFactors = T)

## Cleaning data
df <- df[, -c("summons_number", "summons_image")]
df <- df[df$violation_status %in% c("HEARING HELD-GUILTY",  "HEARING HELD-GUILTY REDUCTION", "HEARING HELD-NOT GUILTY")]
df <- df[!is.na(df$violation_time)]


df[is.na(df$issue_date)]

df$state_rc <- as.factor(df$state)
df[df$state != 'NY', state_rc := "Non_NY"]

df$license_type_rc <- as.factor(df$license_type)

df$issue_date <- mdy(df$issue_date)
df <- df[!is.na(df$issue_date)]

df[, issue_month_rc := month(issue_date)]
df[, issue_year_rc := year(issue_date)]

df$violation_time <- format(strptime(paste0(df$violation_time,'M'), "%I:%M%p"), "%H:%M:%S")
df <- df[!is.na(df$violation_time)]
df[,vio_period_rc := NA]
df$vio_period_rc[as.numeric(substr(df$violation_time,1,2)) < 6] <- "Late Night"
df$vio_period_rc[as.numeric(substr(df$violation_time,1,2)) >= 6 & as.numeric(substr(df$violation_time,1,2)) <12 ] <-  "Morning"
df$vio_period_rc[as.numeric(substr(df$violation_time,1,2)) >= 12 & as.numeric(substr(df$violation_time,1,2)) <19 ] <- "Afternoon"
df$vio_period_rc[as.numeric(substr(df$violation_time,1,2)) >= 19 ]<- "Night"

table(df$fine_amount)
table(df$penalty_amount)

df$interest_amount[df$interest_amount < 0 ] <- 0
df$reduction_amount[df$reduction_amount < 0 ] <- 0

df$interest_amount_rc <- df$interest_amount
df$reduction_amount_rc <- df$reduction_amount

df$interest_amount_rc <- NA
df$interest_amount_rc[df$interest_amount == 0] <- 0
df$interest_amount_rc[df$interest_amount > 0 & !is.na(df$interest_amount)] <- as.character(cut(df$interest_amount[df$interest_amount > 0 & !is.na(df$interest_amount)], 8, dig.lab = 0))

df$interest_amount_rc <-  as.factor(df$interest_amount_rc)

table(df$interest_amount_rc)


df$reduction_amount_rc <- NA

df$reduction_amount_rc[df$reduction_amount == 0] <- 0
df$reduction_amount_rc[df$reduction_amount > 0 & !is.na(df$reduction_amount)] <- as.character(cut(df$reduction_amount[df$reduction_amount > 0 & !is.na(df$reduction_amount)], 8, dig.lab = 0))

df$interest_amount_rc <- as.factor(df$interest_amount_rc)

table(df$reduction_amount_rc)

df$precinct_rc <- as.factor(df$precinct)

df$county_rc <- as.factor(df$county)

df$violation_rc <- as.factor(df$violation)


df$violation_status_rc <- df$violation_status



df[, grep("_rc$", names(df)), with = F]

df_cleaned <- as.data.frame(apply(df[, grep("_rc$", names(df)), with = F], 2, function(x) normalize(as.numeric(as.factor(x)))))

df_cleaned <- data.table(cbind(df_cleaned[,!(colnames(df_cleaned) == "violation_status_rc")], violation_status_rc = df$violation_status_rc))


 ####

## let i1 = HEARING HELD-GUILTY, i2 = HEARING HELD-GUILTY REDUCTION, i3 = "HEARING HELD-NOT GUILTY"

training_index<- sample(1:nrow(df_cleaned), floor(nrow(df_cleaned) *0.7))

df_cleaned_train <- df_cleaned[training_index,]
df_cleaned_test <- df_cleaned[-training_index,]


df_tuple <- df_cleaned_train[, setdiff(names(df_cleaned_train), "violation_status_rc"), with = F]
df_class <- df_cleaned_train[,violation_status_rc]

 
## missing feature reduction, using all features for now

R1 = t(as.matrix(df_tuple[df_class == 'HEARING HELD-GUILTY']))
R2 = t(as.matrix(df_tuple[df_class == 'HEARING HELD-GUILTY REDUCTION']))
R3 = t(as.matrix(df_tuple[df_class == 'HEARING HELD-NOT GUILTY']))
R = t(as.matrix(df_tuple))
 
get_z <-  function(R){
  R <- as.matrix(R)
  Z = ncol(R)
  return(Z)
}

get_mu <- function(R, Z = get_z(R)){
  R <- as.matrix(R)
  mu = colSums(t(R)) / Z
  return(mu)
}

get_sigma <- function(R, mu = get_mu(R), Z = get_z(R)) {
  R <- as.matrix(R)
  sigma <- cov(t(R))
  return(sigma)
}



get_sigma(R1)
get_sigma(R2)
get_sigma(R3)


get_lambda <- function(R) {
  R <- as.matrix(R)
  lambda <- eigen(get_sigma(R))$values
  return(lambda)
}

lambda1 <- get_lambda(R1)

lambda1


get_Ec <- function(R, f = .96){
  sigma <- get_sigma(R) 
  for (n in 1: ncol(sigma)){
    f_test <- sum(diag(sigma[1:n,1:n])) / sum(diag(sigma))
    if (f_test >= f) {
      return(list(vn = diag(sigma[1:n-1,1:n-1]),
                  n = n))
      break
    }
  }
}


get_Sc <- function(R, f = .96) {
  # for (i in 1:get_Ec(R, f)$n) {
  #   if (i == 1) { p <- 0 } else
  #   p <- p + eigen(get_sigma(R))$vectors[,i] %*% t(eigen(get_sigma(R))$vectors[,i])
  # }
  # return(p)
  
  eigen(get_sigma(R))$vectors[,1:get_Ec(R, f)$n]
}


get_y <- function(R, f = 0.96) {
  y <- t(get_Sc(R, 0.7)) %*% R
  return(y)
}

get_dcsq <- function(x, R, f, Sc = get_Sc(R, f), mu = get_mu(R), sigma = get_sigma(R, f)){
  # Sc <- eigen(get_sigma(R))$vectors[,1:get_Ec(R, f)$n]
  # mu <- get_mu(R)
  y <- t(Sc) %*% (x - mu)
  sigma_y <- t(Sc)%*% sigma %*% Sc
  dc_sq <- t(y) %*% t(sigma_y) %*% y

  return(dc_sq) 
}


get_training_seq <- function(R, f) {
  for (i in 1:ncol(R)) {
    if (i == 1) {
      dc_sq_train <- c()
      
      Sc_R <-  get_Sc(R, 0.7)
      mu_R <- get_mu(R)
      sigma_R <- get_sigma(R, 0.7)
    }
    
    dc_sq_train <-
      c(dc_sq_train,
        get_dcsq(
          x = R[, i],
          R = R,
          f = f,
          Sc = Sc_R,
          mu = mu_R,
          sigma = sigma_R
        ))
  }
  return(dc_sq_train[order(dc_sq_train)])
}

df_cleaned_test[1, setdiff(names(df_cleaned_train), "violation_status_rc"), with = F]

x_test = t(as.matrix(df_cleaned_test[1, setdiff(names(df_cleaned_train), "violation_status_rc"), with = F]))
x_true = df_cleaned_test[1, violation_status_rc]


get_tested <- function(x_test,  R, f, training_seq = get_training_seq(R, f)) {
  dcsq_test <- get_dcsq(x_test,R = R, f = f)
  Zc <- length(training_seq)
  if (dcsq_test  < min(training_seq)){
    pc <- 1/ Zc
  } else if (dcsq_test  > max(training_seq)) {
    pc <- 1
  } else {
    desq_test_comp<- data.frame(dcsq_test = rep(dcsq_test, length(training_seq)), training_seq)
    z <- min(as.numeric(rownames(desq_test_comp[desq_test_comp$dcsq_test <= desq_test_comp$training_seq, ])))
    pc <- (z - .5) / Zc
  }
  return(pc)
}

get_assignment <- function(x_test, training_seq, f, p0 = 0.5) {
  pc1 <- get_tested(x_test, R1, f, training_seq_1)
  pc2 <- get_tested(x_test, R2, f, training_seq_2)
  pc3 <- get_tested(x_test, R3, f, training_seq_3)
  
  lbs <- c('HEARING HELD-GUILTY',  'HEARING HELD-GUILTY REDUCTION', 'HEARING HELD-NOT GUILTY')
  pc <- c(pc1, pc2, pc3)
  result<- data.frame(pc, lbs)
  
  if (min(result$pc > p0)){
    return("RESERVED CLASS")
  } else {
    return(as.character(result[result$pc == min(result$pc),"lbs"]))
  }
}

get_assignment(x_test, f = 0.7)

for (i in 1 : 3000){
  if (i == 1) {
    result <- data.frame()
    training_seq_1 <- get_training_seq(R1, 0.7)
    training_seq_2 <- get_training_seq(R2, 0.7)
    training_seq_3 <- get_training_seq(R3, 0.7)
  }
  
  x_test = t(as.matrix(df_cleaned_test[i, setdiff(names(df_cleaned_train), "violation_status_rc"), with = F]))
  x_true = df_cleaned_test[i, violation_status_rc]
  x_asgn = get_assignment(x_test, f = 0.7)
  
  result <- rbind(result, data.frame(i, x_asgn, x_true))
  
}


nrow(result[as.character(result$x_asgn) == as.character(result$x_true),]) / nrow(result)


