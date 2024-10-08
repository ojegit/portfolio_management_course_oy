library(quantmod)
library(dplyr)
library(tidyverse)
library(e1071)

# load data
# avataan tiedot (huom. save.path ja linux vs windows polkujen erot!)
data <- read.csv("./../five_assets_and_rf.csv", row.names = 1)
data <- as.data.frame(data)

# yield to rf
#data["rf"] <-  (1 + data$DGS3MO/100)^(1/252) - 1

stock.series <- excess.ret.colnames <- c(grep("\\.Close$", names(data), value = TRUE))
stock.names <- gsub(".Close","",as.character(stock.series))
N <- length(stock.names)


# yksinkertaiset tuotot (prosentteja)
for(i in 1:N) {
  tmp <- data[paste(stock.names[i],".Close",sep="")]
  data[paste(stock.names[i],".SimpleRet",sep="")] <- 100*(tmp/(lag(tmp) + 1e-8) - 1)
}

# poista NA:t riveittäin (ainakin ensimmäinen rivi, voi olla muitakin)
data <- data %>% filter(across(everything(), ~ !is.na(.)))

# laske ylituotot yksinkertaisille tuotoille (ts vähennä riskitön)
# data <- data %>%
#   mutate(across(ends_with("SimpleRet"), ~ . - DGS3MO, .names = "{col}.Excess"))


################################################################################


# https://palomar.home.ece.ust.hk/MAFS6010R_lectures/Rsession_factor_models.html

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2023-01-01")


### Fama French 5 factors
ff5 <- read.csv("F-F_Research_Data_5_Factors_2x3_daily.CSV", skip = 3)


#https://stackoverflow.com/questions/7439977/changing-date-format-in-r
#https://stackoverflow.com/questions/48680101/converting-date-formats-in-r
ff5["X"] <- as.Date(strptime(ff5$X, "%Y%m%d"),"%Y-%m-%d") #convert date to correct format


#extract only the asset data range
I <- (ff5$X >= start_date & ff5$X <= end_date)
ff5 <- ff5[I,]
rownames(ff5) <- ff5$X #reset row number
ff5["X"] <- NULL


# yhdistä tiedot
data <- left_join(data, ff5, by = "Date")
#data <- merge(data, ff5, all = TRUE)
data <- as.data.frame(data)


# file:///C:/Users/User/Downloads/papers/Factor.pdf

