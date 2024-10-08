

# The actual presentation data

# aikajänne
start_date <- as.Date("2010-01-01")
end_date <- as.Date("2024-10-07")


#ADTRAN Holdings, Inc. 	    ADTN
#Alight, Inc. 	            ALIT
#Championx Corporation 	    CHX
#Chart Industries Inc 	    GTLS
#Denali Therapeutics, Inc. 	DNLI 


#Corsair Gaming, Inc. (CRSR)
#Kosmos Energy Ltd. (KOS)
#Magnite, Inc. (MGNI)
#3D Systems Corporation (DDD)
#Hyliion Holdings Corp. (HYLN)
#Russell 2000 index (^RUT)
#S&P500 (^GSPC)

# noudettavien osakkeiden nimet (ks yahoo finance)
assets <- c("CRSR", "KOS", "MGNI", "DDD", "HYLN", "^RUT", "^GSPC")
assets2 <- c("CRSR", "KOS", "MGNI", "DDD", "HYLN", "RUT", "GSPC")

# riskittömien nimet (ks. FRED)
risk.free <- c("DGS3MO") #3 kk t-bill, prosentteja


# hae osakkeet
getSymbols(assets, src = "yahoo", from = start_date, to = end_date)
asset_data <- do.call(merge, lapply(assets2, function(ticker) Ad(get(ticker)))) #Cl for closed, Ad for adjusted close

# hae riskittömät
getSymbols(risk.free, src = "FRED", from = start_date, to = end_date)
risk_free_data <- DGS3MO

# muunnetaan yield tuotoksi
#test_risk_free_daily <- (1 + test_risk_free_yield)^(1/252) - 1

# yhdistä tiedot
data <- merge(asset_data, risk_free_data, all = TRUE)

data$"risk.free" <- 100*((1 + data$DGS3MO/100)^(1/252) - 1)
data$DGS3MO <- NULL #get rid of
data <- as.data.frame(data)


# muunna aikasarja sarakkeeksi 
data <- cbind(rownames(data), data.frame(data, row.names=NULL))
colnames(data)[1] <- "Date"
data$Date <- as.Date(data$Date)

# datan ulottuvuudet ennen NA:n poistoa
cat("Dimensions before removing NA rows: ",dim(data))

# poista NA:t (riveittäin)
data <- data %>% 
  filter(across(everything(),
                ~ !is.na(.)))

# datan ulottuvuudet NA:n poiston jälkeen
cat("Dimensions after removing NA rows: ",dim(data))

# kirjoita tiedostoon (älä kirjoita indeksiä)
write.csv(data, file="russell_2000.csv", row.names=FALSE)



###



plot(x, y1, type = "n", xlim = c(1, 10), ylim = c(0, 10), 
     xlab = "X", ylab = "Y", main = "Multiple Lines Plot")
# Plot each line one by one
lines(x, y1, type = "l", col = "red")
lines(x, y2, type = "l", col = "blue")
lines(x, y3, type = "l", col = "green")
# Add a legend
legend("topright", legend = c("Line 1", "Line 2", "Line 3"), 
       col = c("red", "blue", "green"), lty = 1)