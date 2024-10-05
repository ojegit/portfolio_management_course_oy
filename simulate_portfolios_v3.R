################################################################################
#Portfolio Management, OY
#Tekijä: AP
#
#Tehty:
#-korjattu: pf tuottoihin liittyen, log-tuottojen muuttaminen artimeettisiksi/yksinkertaisiksi prosentuaalisiksi tuotoiksi
#-aineiston noutaminen ja tallentaminen
#-portfolioiden simulointi eri painojen rajoissa
#-eri optimien hakeminen annetuista portfolioiden tuotoista ja varianssseista jne
#-ylituotosarjan kuvailu
#-kuvioiden ja tulosten kirjoittaminen tiedostoon
#
#WIP:
#-optimipainojen palattaminen muiden "optimien" lisäksi (tällä hetkellä palautetaan vain pf:n tuotto, sharpe ratio ja varianssi)
#-kuvien tallentaminen 
#-testiainestoon benchmarkkaus (tällä hetkellä df_test:llä ei tehdä mitään!)
#-hyötyfunktiot
################################################################################
library(quantmod)
library(dplyr)
library(tidyverse)
library(e1071)
################################################################################

#Haetaanko data verkosta?
fetch.data <- FALSE

#Tallennettavan tiedoston nimi
save.path <- "./five_assets_and_rf.csv"

if (fetch.data) {
  # aikajänne
  start_date <- as.Date("2010-01-01")
  end_date <- as.Date("2023-01-01")
  
  # noudettavien osakkeiden nimet (ks yahoo finance)
  stocks <- c("AAPL", "MSFT", "GOOG", "AMZN", "NVDA")
  
  # riskittömien nimet (ks. FRED)
  risk.free <- c("DGS3MO") #3 kk t-bill, prosentteja
  
  # hae osakkeet
  getSymbols(stocks, src = "yahoo", from = start_date, to = end_date)
  stock_data <- do.call(merge, lapply(stocks, function(ticker) Cl(get(ticker))))
  
  # hae riskittömät
  getSymbols(risk.free, src = "FRED", from = start_date, to = end_date)
  risk_free_data <- DGS3MO
  
  # yhdistä tiedot
  data <- merge(stock_data, risk_free_data, all = TRUE)
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
  write.csv(data, file=save.path, row.names=FALSE)
}

################################################################################

# avataan tiedot (huom. save.path ja linux vs windows polkujen erot!)
data <- read.csv(save.path, row.names = 1)
data <- as.data.frame(data)


# logaritmiset l. jatkuvat tuotot (prosentteja)
for(i in 1:length(stocks)) { #oleta että "stocks" ts. osakkeiden nimet käsittävä muuttuja on muistissa
  tmp <- data[paste(stocks[i],".Close",sep="")]
  data[paste(stocks[i],".LogRet",sep="")] <- 100*log(tmp/(lag(tmp) + 1e-8) + 1e-8)
}

# yksinkertaiset tuotot (prosentteja)
for(i in 1:length(stocks)) {
  tmp <- data[paste(stocks[i],".Close",sep="")]
  data[paste(stocks[i],".SimpleRet",sep="")] <- 100*(tmp/(lag(tmp) + 1e-8) - 1)
}


# poista NA:t riveittäin (ainakin ensimmäinen rivi, voi olla muitakin)
data <- data %>% filter(across(everything(), ~ !is.na(.)))

# laske ylituotot yksinkertaisille tuotoille (ts vähennä riskitön)
data <- data %>%
  mutate(across(ends_with("SimpleRet"), ~ . - DGS3MO, .names = "{col}.Excess"))

# muunna aikasarake sopivaan formaattiin (ja poista rivitiedot)
data["Date"] <- as.Date(rownames(data))
rownames(data) <- NULL


# tuottojen muuttaminen aritmeettinen <-> log/ln välillä
log2simple <- function(x) { exp(x) - 1 }
simple2log <- function(x) { log(1 + x) }

################################################################################


# jaetaan sovitus ja testiaineistoihin (otetaan esim. 2500 päivää mikä on n. 10 vuotta)
df_train <- data %>% filter(row_number() <= 2500)
df_test <- data %>% filter(row_number() > 2500)

# haetaan ylituottosarakkeiden nimet
excess.ret.colnames <- c(grep("\\.Excess$", colnames(data), value = TRUE))
print(excess.ret.colnames)

# no assets
N <- length(excess.ret.colnames)

# odotetut tuotot
R <- matrix(apply(df_train[,excess.ret.colnames], 2, mean),N,1)

# assets' covariance
C <- matrix(cov(df_train[,excess.ret.colnames]),N,N)


################################################################################

# ylituottodatan kuvailu, soviteaineisto
df_train[,excess.ret.colnames] %>%
  summarise(across(everything(), list(
    min = ~ min(.),
    max = ~ max(.),
    mean = ~ mean(.),
    stdev = ~ sd(.),
    skewness = ~ skewness(.),
    kurtosis = ~ kurtosis(.),
    q2.5 = ~ quantile(., 0.025),
    q97.5 = ~ quantile(., 0.975),
    median = ~ median(.)
  ))) %>%
  pivot_longer(cols = everything(), names_to = c("asset", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = asset, values_from = value)

# ylituottodatan kuvailu, testiaineisto
df_test[,excess.ret.colnames] %>%
  summarise(across(everything(), list(
    min = ~ min(.),
    max = ~ max(.),
    mean = ~ mean(.),
    stdev = ~ sd(.),
    skewness = ~ skewness(.),
    kurtosis = ~ kurtosis(.),
    q2.5 = ~ quantile(., 0.025),
    q97.5 = ~ quantile(., 0.975),
    median = ~ median(.)
  ))) %>%
  pivot_longer(cols = everything(), names_to = c("asset", "stat"), names_sep = "_") %>%
  pivot_wider(names_from = asset, values_from = value)

################################################################################


# portfolion tuotto (w = painot, R = komponenttien odotetut tuotot (aritmeettiset), molemmat oltava matriiseja)
pf.ret <- function(w,R) { drop( t(w) %*% R ) }

# portfolion varianssi (w = painot, C = komponenttien kovarianssi, molemmat oltava matriiseja!)
pf.var <- function(w,C) {  drop( t(w) %*% C %*% w ) }

# sharpe ratio
pf.sharpe <- function(pf.ret, risk.free, pf.var) { (pf.ret - risk.free) / sqrt(pf.var) }


# tuottojen ja varianssin simulointi eri painojen arvoilla
simulate.pf <- function(R, S, w.min=0, w.max=1, mc.iters = 5000, 
                        min.pf.ret.target = -Inf, 
                        max.pf.var.target = Inf,
                        risk.free = NA,
                        save.weights = FALSE,
                        print.int = 500,
                        debug = FALSE) {
  
  # kompoenttien lkm
  N <- dim(R)[1]
  
  # tallennetaanko painot? (nämä tietenkin tarvitaan suosituksiin/raporttiin jne, mutta testaillessa ei ole tarpeen...)
  if(save.weights == TRUE) {
    w <- matrix(0,N,mc.iters)
  } else {
    w <- c() #empty
  }
  
  w.limits <- matrix(0,N,2)
  pf.moments <- data.frame(matrix(0,mc.iters,2))
  colnames(pf.moments) <- c("ret", "var")
  
  
  # yksikköjakauman rajat (vrt. painojen rajat joihin näitä verrataan)
  if (w.min > 0) {
    w.lo <- 0
  } else {
    w.lo <- w.min
  }
  w.hi <- 1
  
  iter <- 0
  
  while(TRUE) {

    # vedetään satunnainen painovektori yksikköjakaumasta, normeerataan summa [0,1] välille (huom! oletetaan että kaikki rahat allokoidaan eli painojen summa = 1)
    w.try <- matrix(w.lo + (w.hi - w.lo) * runif(N), N, 1)
    w.try <- w.try/sum(w.try) #normalize
    
    ### DEBUGGAUSTA
    if (debug) {
      print("w.try:")
      print(w.try)
      Sys.sleep(1)
    }
    ###
    
    # tarkastetaan painojen rajoitteiden toteutuminen (oletus: [0,1])
    if ( any(w.try < w.min) || any(w.try > w.max) ) {
      next
    }
    
    # lasketaan portfolion tuotto ja varianssi annetuilla painoilla (sekä tietenkin R ja S) 
    pf.ret.try <- pf.ret(w.try, R)
    pf.var.try <- pf.var(w.try, S)
    
    ### DEBUGGAUSTA
    if (debug) {
      print(paste("pf.ret.try: ",pf.ret.try, ", pf.var.try: ",pf.var.try))
      Sys.sleep(1)
    }
    ###
    
    
    # tarkastetaan portfolion tuoton ja varianssin rajoitteiden toteutuminen (oletus: ei rajoitettu)
    if (is.na(pf.ret.try) || is.na(pf.var.try) || 
      pf.ret.try < min.pf.ret.target || pf.var.try > max.pf.var.target) {
      next
    }
    
    # tulostetaan simulaation edistyminen (print.int iteraatioiden välein)
    if ( (print.int > 0) && (iter %% print.int) == 0 ) {
      ##cat("ITER NO: ",iter,"/",mc.iters, " (", round(100*iter/mc.iters,2), " %)" )
      print(paste("ITER NO: ",iter))
    }
    
    # päivitetään laskuri (huom! päivitetään vain kun rajoitteet toteutuu)
    iter <- iter + 1
    
    # tallennetaan tulokset
    pf.moments[iter,1] <- pf.ret.try
    pf.moments[iter,2] <- pf.var.try
    if (save.weights == TRUE) {
      w[,iter] <- w.try
    }
    
    # tallennetaan painojen rajat (jos näyttää siltä ettei ole vierailtu koko määritellyllä aluella, yritä uudelleen, tarkasta rajoitteet tai lisää iteraatioiden määrää!)
    w.limits[,1] <- apply(cbind(w.limits[,1],w.try),1,min)
    w.limits[,2] <- apply(cbind(w.limits[,2],w.try),1,max)
    
    
    # poistutaan silmukasta
    if (iter == mc.iters) {
      if (print.int > 0) {
        cat("Done!")
      }
      break
    }
      
  }#WHILE-silmukka
  
  # palautetut tulokset (list-muodossa)
  list(pf.moments=pf.moments, 
       w.limits=w.limits, 
       w=w)
}


# paikannetaan eri "optimit" annetusta simulaatiosta
opt.pf <- function(res, risk.free = 0) {
  #
  # lisättävä: painojen/allokoinnin palauttaminen eri pisteissä (vain kun ne on tallennettu)
  #
  
  # lasketaan sharpe ratio
  sr <- pf.sharpe(res$pf.moments$ret, risk.free, res$pf.moments$var)
  
  # optimien indeksit (Suhteessa portfolion tuottoon ja varianssiin)
  min.var.idx <- which.min(res$pf.moments$var)
  max.ret.idx <- which.max(res$pf.moments$ret)
  max.sr.idx <- which.max(sr)
  
  #minimimaalinen varianssi
  min.var.var <- res$pf.moments$var[min.var.idx]
  min.var.ret <- res$pf.moments$ret[min.var.idx]
  min.var.sr <- sr[min.var.idx]
  
  is.empty.w <- (length(res$w)==0)
  if(!is.empty.w) {
    min.var.w <- res$w[,min.var.idx]
  } else {
    min.var.w <- c()
  }
 
  #maksimaalinen tuotto
  max.ret.var <- res$pf.moments$var[max.ret.idx]
  max.ret.ret <- res$pf.moments$ret[max.ret.idx]
  max.ret.sr <- sr[max.ret.idx]
  if(!is.empty.w) {
    max.ret.w <- res$w[,max.ret.idx]
  } else {
    max.ret.w <- c()
  }
  
  #maksimaalinen sharpe ratio
  max.sr.var <- res$pf.moments$var[max.sr.idx]
  max.sr.ret <- res$pf.moments$ret[max.sr.idx]
  max.sr.sr <- sr[max.sr.idx]
  if(!is.empty.w) {
    max.sr.w <- res$w[,max.sr.idx]
  } else {
    max.sr.w <- c()
  }
  
  # tallennetaan optimit omiin listoihin....
  min.var <- list(var=min.var.var, ret=min.var.ret, sr=min.var.sr, idx=min.var.idx, w=min.var.w)
  max.ret <- list(var=max.ret.var, ret=max.ret.ret, sr=max.ret.sr, idx=max.ret.idx, w=max.ret.w)
  max.sr <- list(var=max.sr.var, ret=max.sr.ret, sr=max.sr.sr, idx=max.sr.idx, w=max.sr.w)
  
  #... ja tallennetaan nämä vielä omiin listoihinsa eri optimin mukaisesti
  list(min.var = min.var, 
       max.ret = max.ret,
       max.sr = max.sr)
}


#tulosta portfolion optimit
print.pf.opt <- function(opt, title=NA, asset.names = NA, save.path = NA) {
  
  #erottelurivin pituus
  sep.len <- 50
  
  #painojen näytetyt desimaalit
  no.dec.w <- 2
  
  #"optimien" näytetyt desimaalit
  no.dec.opt <- 3
  
  
  #avaa tiedosto kirjoitettavaksi
  if (is.character(save.path)) {
    fileConn <- file(paste(save.path,".txt",sep=""))
  } else {
    fileConn <- ""
  }
  
  #apufunktio rivin tulostamiseen (joko ruudulle tai tiedostoon)
  print.line <- function(..., sep="") {
    cat(...,'\n', file=fileConn, sep=sep, append = TRUE) #rivinvaihto lisätään tässä
  }
  
  
  ### TULOSTUS ALKAA
  
  #otsikkorivi
  print.line(rep('-',sep.len))
  if (is.character(title)) {
    print.line(title)
  } else {
    print.line("PORTFOLIO OPTIMA")
  }
  print.line(rep('-',sep.len))
  
  
  #varsinaiset tulokset
  print.line("Minimum variance:     ", round(opt$min.var$var, no.dec.opt))
  print.line("Weights: ")
  if (all(!is.na(asset.names))) {
    print.line(asset.names, sep= " ")
  }
  print.line(round(opt$min.var$w, no.dec.w), sep="  ")
  print.line(rep('-',sep.len), sep="")
  print.line("Maximum return:       ", round(opt$max.ret$ret, no.dec.opt))
  print.line("Weights: ")
  if (all(!is.na(asset.names))) {
    print.line(asset.names, sep="  ")
  }
  print.line(round(opt$max.ret$w, no.dec.w), sep="  ")
  print.line(rep('-',sep.len))
  print.line("Maximum sharpe ratio: ", round(opt$max.sr$sr, no.dec.opt))
  print.line("Weights: ")
  if (all(!is.na(asset.names))) {
    print.line(asset.names, sep="  ")
  }
  print.line(round(opt$max.sr$w, no.dec.w), sep="  ")
  print.line(rep('-',sep.len), sep="")
  
  ### TULOSTUS PÄÄTTYY
  
  
  #sulje tiedosto
  if (is.character(save.path)) {
   close(fileConn)
  }
}


#portfolioiden kuvaaja
plot.pf <- function(res, risk.free = 0, title=NA, save.path = NA, png.width = 900, png.height = 600) {
  
  # esti optimit
  opt.res <- opt.pf(res, risk.free = risk.free)
  
  #tallenna kuva png-muodossa (kuvaa ei näy jos se tallennetaan!)
  if(is.character(save.path)) {
    png(file = paste(save.path,".png",sep=""), width=png.width, height=png.height)
  }

  # näytä portfoliot
  plot(res$pf.moments$var, res$pf.moments$ret, 
       col='black', xlab="pf.var", ylab="pf.ret", main=title)
  
  # näytä eri optimit (vihreä: min.var, pun: max.tuotto, sin: max.sharpe)
  points(opt.res$min.var$var, opt.res$min.var$ret, col='green', pch=15)
  points(opt.res$max.ret$var, opt.res$max.ret$ret, col='red', pch=15) 
  points(opt.res$max.sr$var, opt.res$max.sr$ret, col='blue', pch=15)
  
  legend("bottomright",
         legend=c("pf", "min.var", "max.ret", "max.sr"),
         col=c("black", "green","red", "blue"), 
         lty=1:3, cex=0.8,
         text.font=10, bg='white')
  
  #tallenna kuva (...jatkuu)
  if(is.character(save.path)) {
    dev.off()
  }
}


################################################################################

#kuinka monta portfoliota simuloidaan?
no.pfs <- 50000


# simuloidaan portfoliot kun lyhyeksi myynti ei ole sallittu, 
# maksimiallokaatio per komponentti on 50%
res.nss <- simulate.pf(R,C,mc.iters = no.pfs, print.int=2000, w.min=0, w.max=0.5,
                       min.pf.ret.target = -Inf, 
                       save.weights = TRUE, debug = FALSE)

# simuloidaan portfoliot kun lyhyeksi myynti on sallittu, komponenttien painot 
# -50% < w < 50%
res.ss <- simulate.pf(R,C,mc.iters = no.pfs, print.int=2000, w.min=-0.5, w.max=0.5, 
                      min.pf.ret.target = -Inf,
                      save.weights = TRUE, debug = FALSE)



# tulosta simulaattorin panojen empiiriset rajat (jos nämä poikkeaa halutuista niin yritä uudelleen
# lisää iteraatiota jne.)
print(res.nss$w.limits)
print(res.ss$w.limits)

#tunnisteet (mm. kuva ja "optimit")
title.nss = paste("SS DISABLED (", no.pfs," pfs)",sep="")
title.ss = paste("SS ENABLED (", no.pfs," pfs)",sep="")

# piirrä portfoliot tuotto-riski -akselille (jos 'save.path' eli tiedostonimi annetaan kuvaa ei näytetä vaan se tallennetaan)
plot.pf(res.nss,title=title.nss, save.path = "./pf_nss_v3")
plot.pf(res.ss,title=title.ss, save.path = "./pf_ss_v3")

# hae "optimit"
opt.pf.nss <- opt.pf(res.nss)
opt.pf.ss <- opt.pf(res.ss)

# tulosta optimit näytölle ja tallenna tiedostoon (jos 'save.path' eli tiedostonimi annetaan mitään ei tulosteta vaan se tallennetaan)
print.pf.opt(opt.pf.nss, title=title.nss, asset.names = stocks) #, save.path = "./pf_nss_v3")
print.pf.opt(opt.pf.ss, title=title.ss, asset.names = stocks) #, save.path = "./pf_ss_v3")


################################################################################

#testiportfolion tuotot (w = painot, r = kompoenttien tuotot)
pf.test <- function(w,r) {
  D < dim(r)
  matrix(apply(r,1,function(r){pf.ret(W,r)}), D[1], 1)
}

#tracking error (pf.ret.real = todellinen tuotto l. testiaineisto estimoiduilla painoilla, pf.ret.est = testiaineisto )
pf.tracking.error <- function(pf.ret.real, pf.ret.est) {
  te <- pf.ret.real - pf.ret.est
  stdev <- sd(te)
  list(stdev = stdev) 
}

#vertailuportfolio no 1: 1/N testiaineisto 


#vertailuportfolio no 2: max sharpe, testiaineisto

#vertailuportfolio no 3: S&P500


