################################################################################
#Portfolio Management, OY
#Tekijä: AP
#
#Tehty:
#-mahdollisuus näyttää portfolioiden tehokkaan rintaman pisteet (approksimaatio), kaikkien sijaan
#-hyötyfunktiot (quadratic, valituilla riskitasoilla)
#-optimipainojen palattaminen muiden "optimien" lisäksi (tällä hetkellä palautetaan vain pf:n tuotto, sharpe ratio ja varianssi)
#-korjattu: pf tuottoihin liittyen, log-tuottojen muuttaminen artimeettisiksi/yksinkertaisiksi prosentuaalisiksi tuotoiksi
#-aineiston noutaminen ja tallentaminen
#-portfolioiden simulointi eri painojen rajoissa
#-eri optimien hakeminen annetuista portfolioiden tuotoista ja varianssseista jne
#-ylituotosarjan kuvailu
#-kuvioiden ja tulosten kirjoittaminen tiedostoon
#
#WIP:
#-kuvien tallentaminen 
#-testiainestoon benchmarkkaus (tällä hetkellä df_test:llä ei tehdä mitään!)

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

# portfolion hyöty (perus l. quadratic; oleta että tuotot ja varianssit on jo laskettu)
pf.utility <- function(pf.ret,pf.var,gamma=1) { pf.ret - 0.5*gamma*sqrt(pf.var) }


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
  
  exit.flag <- 0
  iter <- 0
  
  # lopetetaan etsintä jos vähintään tämän verran hylättyjä allokaatioita peräkkäin 
  #(ts. todennäköisyys löytää edes yksi rajoitteet läpäisevä allokaatiovektori on
  # liian pieni, joten etsintää ei kannata jatkaa)
  max.stuck.iters <- 1000
  iter.stuck <- 0
  debug.stop.time <- 0.1 #sekunteina
  
  while(TRUE) {

    # vedetään satunnainen painovektori yksikköjakaumasta, normeerataan summa [0,1] välille (huom! oletetaan että kaikki rahat allokoidaan eli painojen summa = 1)
    w.try <- matrix(w.lo + (w.hi - w.lo) * runif(N), N, 1)
    w.try <- w.try/sum(w.try) #normalize
    
    ### DEBUGGAUSTA
    if (debug) {
      print("w.try:")
      print(w.try)
      Sys.sleep(debug.stop.time)
    }
    ###
    
    # tarkastetaan painojen rajoitteiden toteutuminen (oletus: [0,1])
    if ( any(w.try < w.min) || any(w.try > w.max) ) {
      iter.stuck <- iter.stuck + 1
      
      ### DEBUGGAUSTA
      if (debug) {
        print(paste("iter.stuck: ", iter.stuck, " (rejected w)"))
        Sys.sleep(debug.stop.time)
      }
      ###
      
      next
    }
    
    # lasketaan portfolion tuotto ja varianssi annetuilla painoilla (sekä tietenkin R ja S) 
    pf.ret.try <- pf.ret(w.try, R)
    pf.var.try <- pf.var(w.try, S)
    
    ### DEBUGGAUSTA
    if (debug) {
      print(paste("pf.ret.try: ",pf.ret.try, ", pf.var.try: ",pf.var.try))
      Sys.sleep(debug.stop.time)
    }
    ###
    
    # tarkastetaan portfolion tuoton ja varianssin rajoitteiden toteutuminen (oletus: ei rajoitettu)
    if (is.na(pf.ret.try) || is.na(pf.var.try) || 
      pf.ret.try < min.pf.ret.target || pf.var.try > max.pf.var.target) {
      iter.stuck <- iter.stuck + 1
      
      ### DEBUGGAUSTA
      if (debug) {
        print(paste("iter.stuck: ", iter.stuck, " (rejected pf.ret or pf.var)"))
        Sys.sleep(debug.stop.time)
      }
      ###
      
      next
    }
    
    #nollataan peräkkäisten hylättyjen iteraatioiden laskuri
    iter.stuck <- 0

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
      exit.flag <- 1
      if (print.int > 0) {
        cat("Done!")
      }
      break
    } else if (iter.stuck == max.stuck.iters) {
      exit.flag <- 2
      cat("Max number of stuck iterations reached. Exiting...")
      break
    }
      
  }#WHILE-silmukka
  
  # palautetut tulokset (list-muodossa)
  list(pf.moments=pf.moments, 
       w.limits=w.limits, 
       w=w)
  
} # END simulate.pf


# paikannetaan eri "optimit" annetusta simulaatiosta
opt.pf <- function(res, risk.free = 0, gamma = c(1), asset.names = NA) {

  
  ###
  
  # lasketaan sharpe ratiot
  sr <- pf.sharpe(res$pf.moments$ret, risk.free, res$pf.moments$var)
  
  
  #komponenttien lkm 
  N <- dim(res$w)[1] #ulottuvuus 1: komponenttien lkm, ulottuvuus 2: simulaatioiden lkm
  
  # riskitasojen lkm
  ng <- length(gamma)
  
  
  # onko painomatriisi olemassa?
  is.empty.w <- (length(res$w)==0)
  ###
  
  
  # optimien indeksit (suhteessa haluttuun tavoitefuntion arvoon)
  min.var.idx <- which.min(res$pf.moments$var)
  max.ret.idx <- which.max(res$pf.moments$ret)
  max.sr.idx <- which.max(sr)
  if(ng > 0) {
    max.util.idx <- matrix(0,ng,1)
    for(i in 1:ng) {
      # lasketaan hyötyfunktion arvot eri riskitasoille
      pf.util <- pf.utility(res$pf.moments$ret, res$pf.moments$var, gamma=gamma[i])
      
      # haetaan maksimaalista hyötyä vastaava indeksi
      max.util.idx[i,1] <- which.max(pf.util)
    }

  } else {
    max.util.idx <- c()
  }
  
  #minimimaalinen varianssi
  min.var.var <- res$pf.moments$var[min.var.idx]
  min.var.ret <- res$pf.moments$ret[min.var.idx]
  min.var.sr <- sr[min.var.idx]
  if(!is.empty.w) {
    min.var.w <- matrix(res$w[,min.var.idx],N,1)
    
    #lisää komponenttien nimet painoihin
    if (all(!is.na(asset.names))) {
      rownames(min.var.w) <- asset.names
    }
    
  } else {
    min.var.w <- c()
  }
 
  #maksimaalinen tuotto
  max.ret.var <- res$pf.moments$var[max.ret.idx]
  max.ret.ret <- res$pf.moments$ret[max.ret.idx]
  max.ret.sr <- sr[max.ret.idx]
  if(!is.empty.w) {
    max.ret.w <- matrix(res$w[,max.ret.idx],N,1)
    
    #lisää komponenttien nimet painoihin
    if (all(!is.na(asset.names))) {
      rownames(max.ret.w) <- asset.names
    }
    
  } else {
    max.ret.w <- c()
  }
  
  
  #maksimaalinen sharpe ratio
  max.sr.var <- res$pf.moments$var[max.sr.idx]
  max.sr.ret <- res$pf.moments$ret[max.sr.idx]
  max.sr.sr <- sr[max.sr.idx]
  if(!is.empty.w) {
    max.sr.w <- matrix(res$w[,max.sr.idx],N,1)
    
    #lisää komponenttien nimet painoihin
    if (all(!is.na(asset.names))) {
      rownames(max.sr.w) <- asset.names
    }
    
  } else {
    max.sr.w <- c()
  }
  
  
  #maksimaalinen hyöty
  if(ng > 0) {
    max.util.var <- matrix(0,ng,1)
    max.util.ret <- matrix(0,ng,1)
    max.util.sr <- matrix(0,ng,1)
    max.util.w <- matrix(0,N,ng)
    
    #haetaan maksimihyödyt eri riskitasoille
    for(i in 1:ng) {
      max.util.var[i,1] <- res$pf.moments$var[max.util.idx[i,1]]
      max.util.ret[i,1] <- res$pf.moments$ret[max.util.idx[i,1]]
      max.util.sr[i,1] <- sr[max.util.idx[i,1]]
      if(!is.empty.w) {
        max.util.w[,i] <- res$w[,max.util.idx[i,1]]
      }
      
    }
    
    #lisää komponenttien nimet painoihin
    if(!is.empty.w && all(!is.na(asset.names))) {
      rownames(max.util.w) <- asset.names
    }
    
    #lisää lopuksi rivi/sarakenimiksi riskitasot
    rownames(max.util.idx) <- paste("gamma=",gamma,sep="")
    rownames(max.util.var) <- paste("gamma=",gamma,sep="")
    rownames(max.util.ret) <- paste("gamma=",gamma,sep="")
    rownames(max.util.sr) <- paste("gamma=",gamma,sep="")
    colnames(max.util.w) <- paste("gamma=",gamma,sep="")
      
  } else {
    max.util.var <- c()
    max.util.ret <- c()
    max.util.sr <- c()
    max.util.w <- c()
  }

  
  # tallennetaan optimit omiin listoihin
  min.var <- list(var=min.var.var, ret=min.var.ret, sr=min.var.sr, idx=min.var.idx, 
                  w=min.var.w)
  max.ret <- list(var=max.ret.var, ret=max.ret.ret, sr=max.ret.sr, idx=max.ret.idx,
                  w=max.ret.w)
  max.sr <- list(var=max.sr.var, ret=max.sr.ret, sr=max.sr.sr, idx=max.sr.idx,
                 w=max.sr.w)
  max.util <- list(var=max.util.var, ret=max.util.ret, sr=max.util.sr, idx=max.util.idx,
                   w=max.util.w)
  
  # palautetaan tulokset
  list(min.var = min.var, max.ret = max.ret, max.sr = max.sr, max.util = max.util)
  
} # END pf.opt


#tulosta portfolion optimit
print.pf.opt <- function(opt, title=NA, asset.names = NA, save.path = NA) {
  
  #erottelurivin pituus
  sep.len <- 50
  
  #painojen näytetyt desimaalit
  no.dec.w <- 2
  
  #"optimien" näytetyt desimaalit
  no.dec.opt <- 3
  
  #komponenttien nimet
  if (!is.character(asset.names)) {
    asset.names <- rownames(opt$min.var$w)
  }
  
  
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
  
  ## otsikkorivi
  print.line(rep('-',sep.len))
  if (is.character(title)) {
    print.line(title)
  } else {
    print.line("PORTFOLIO OPTIMA")
  }
  print.line(rep('-',sep.len))
  
  
  ## varsinaiset tulokset
  print.line("Minimum variance")
  print.line("Return:       ", round(opt$min.var$ret, no.dec.opt))
  print.line("Variance (*): ", round(opt$min.var$var, no.dec.opt))
  print.line("Sharpe-ratio: ", round(opt$min.var$sr, no.dec.opt))
  print.line("Weights: ")
  if (all(!is.na(asset.names))) { print.line(asset.names, sep= "  ") }
  print.line(round(opt$min.var$w, no.dec.w), sep="  ")
  print.line(rep('-',sep.len), sep="")
  
  print.line("Maximum return")
  print.line("Return (*):   ", round(opt$max.ret$ret, no.dec.opt))
  print.line("Variance:     ", round(opt$max.ret$var, no.dec.opt))
  print.line("Sharpe-ratio: ", round(opt$max.ret$sr, no.dec.opt))
  print.line("Weights: ")
  if (all(!is.na(asset.names))) { print.line(asset.names, sep="  ") }
  print.line(round(opt$max.ret$w, no.dec.w), sep="  ")
  print.line(rep('-',sep.len))
  
  print.line("Maximum sharpe ratio")
  print.line("Return:           ", round(opt$max.sr$ret, no.dec.opt))
  print.line("Variance:         ", round(opt$max.sr$var, no.dec.opt))
  print.line("Sharpe-ratio (*): ", round(opt$max.sr$sr, no.dec.opt))
  print.line("Weights: ")
  if (all(!is.na(asset.names))) { print.line(asset.names, sep="  ") }
  print.line(round(opt$max.sr$w, no.dec.w), sep="  ")
  print.line(rep('-',sep.len), sep="")
  
  ng <- length( opt$max.util$ret )
  if (ng > 0) {
    risk.names <- rownames(opt$max.util$ret)
    for(i in 1:ng) {
      print.line("Maximum utility (", risk.names[i], ")")
      print.line("Return:           ", round(opt$max.util$ret[i,1], no.dec.opt))
      print.line("Variance:         ", round(opt$max.util$var[i,1], no.dec.opt))
      print.line("Sharpe-ratio:     ", round(opt$max.util$sr[i,1], no.dec.opt))
      print.line("Weights: ")
      if (all(!is.na(asset.names))) { print.line(asset.names, sep="  ") }
      print.line(round(opt$max.util$w[,i], no.dec.w), sep="  ")
      print.line(rep('-',sep.len), sep="")
    }
  }
  
  ### TULOSTUS PÄÄTTYY
  
  
  #sulje tiedosto
  if (is.character(save.path)) {
   close(fileConn)
  }
  
} # END print.pf.opt


#portfolioiden kuvaaja (etsii optimit tämän sisällä eli niitä ei tarvitse erikseen syöttää)
plot.pf <- function(res, efficient.frontier.only = TRUE, risk.free=0, gamma = c(1), title=NA, save.path=NA, 
                    png.width=900, png.height=600) {
  
  # TBA: valitaan mitä optimeja näytetään: "min.var", "max.ret", "max.sr", "max.util"
  
  ###
  pf.marker.size <- 0.8
  pf.opt.marker.size <- 2.0
  legend.marker.size <- 0.8
  #legend.text.size <- 5
  
  pf.marker.fill.color <- 'black'
  pf.opt.marker.fill.color <- 'black'
  ###
  
  
  # etsi optimit
  opt.res <- opt.pf(res, risk.free=risk.free, gamma=gamma)
  
  #tallenna kuva png-muodossa (kuvaa ei näy jos se tallennetaan!)
  if(is.character(save.path)) {
    png(file = paste(save.path,".png",sep=""), width=png.width, height=png.height)
  }

  # näytä portfoliot
  if (efficient.frontier.only == TRUE) {
    
    #otetaan vain uloimmat pisteet huomoon, ts. ne mitkä piirtää tehokkaan rintaman
    I <- chull(cbind(res$pf.moments$var, res$pf.moments$ret))
    plot(res$pf.moments$var[I], res$pf.moments$ret[I], bg = pf.marker.fill.color,
         col='black', xlab="pf.var", ylab="pf.ret", main=title, cex=pf.marker.size, pch=1)
    
  } else {
    plot(res$pf.moments$var, res$pf.moments$ret, bg = pf.marker.fill.color,
       col='black', xlab="pf.var", ylab="pf.ret", main=title, cex=pf.marker.size, pch=1)
  }
  
  # näytä eri optimit (vih: min.var, pun: max.tuotto, sin: max.sharpe, vio: max.util)
  points(opt.res$min.var$var,
         opt.res$min.var$ret,
         col='green',
         bg=pf.opt.marker.fill.color,
         pch=2,
         cex=pf.opt.marker.size)
  points(opt.res$max.ret$var, opt.res$max.ret$ret, col='red', bg=pf.opt.marker.fill.color, pch=3, cex=pf.opt.marker.size)
  points(opt.res$max.sr$var, opt.res$max.sr$ret, col='blue', bg=pf.opt.marker.fill.color, pch=4, cex=pf.opt.marker.size)
  
  ng <- length( opt.res$max.util$ret )
  if(ng > 0) {
    risk.names <- rownames(opt.res$max.util$ret)
    for(i in 1:ng) {
      points(opt.res$max.util$var[i,1],
             opt.res$max.util$ret[i,1], 
             col='purple', 
             bg=pf.opt.marker.fill.color,
             pch=4+i, 
             cex=pf.opt.marker.size )  
    }
  } else {
    risk.names <- c()
  }
  
  # näytä selitykset
  legend("bottomright",
         legend = c( c("pf", "min.var", "max.ret", "max.sr"), risk.names ),
         col= c( c("black", "green","red", "blue"), rep('purple',ng) ), 
         lty=1:(4+ng), 
         cex=legend.marker.size,
         text.font=NULL, 
         bg='white')
  
  #tallenna kuva (...jatkuu)
  if(is.character(save.path)) {
    dev.off()
  }
  
} # END plot.pf


################################################################################

#kuinka monta portfoliota simuloidaan?
no.pfs <- 100000


# simuloidaan portfoliot kun lyhyeksi myynti ei ole sallittu, 
# maksimiallokaatio per komponentti on 50%
res.nss <- simulate.pf(R,C,mc.iters = no.pfs, print.int=10000, w.min=0, w.max=1,
                       min.pf.ret.target = -Inf, 
                       save.weights = TRUE, debug = FALSE)

# simuloidaan portfoliot kun lyhyeksi myynti on sallittu, komponenttien painot 
# -50% < w < 50%
res.ss <- simulate.pf(R,C,mc.iters = no.pfs, print.int=10000, w.min=-1, w.max=1, 
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
plot.pf(res.nss,title=title.nss, gamma = c(1,2,4))
plot.pf(res.ss,title=title.ss, gamma = c(1,2,4))
plot.pf(res.nss,title=title.nss, save.path = "./pf_nss_v3_eff", gamma = c(1,2,4))
plot.pf(res.ss,title=title.ss, save.path = "./pf_ss_v3_eff", gamma = c(1,2,4))
plot.pf(res.nss,title=title.nss, save.path = "./pf_nss_v3_all", gamma = c(1,2,4), efficient.frontier.only = FALSE)
plot.pf(res.ss,title=title.ss, save.path = "./pf_ss_v3_all", gamma = c(1,2,4), efficient.frontier.only = FALSE)



# hae "optimit"
opt.pf.nss <- opt.pf(res.nss, asset.names = stocks, gamma = c(1,2,4))
opt.pf.ss <- opt.pf(res.ss, asset.names = stocks, gamma = c(1,2,4))

# tulosta optimit näytölle ja tallenna tiedostoon (jos 'save.path' eli tiedostonimi annetaan mitään ei tulosteta vaan se tallennetaan)
print.pf.opt(opt.pf.nss, title=title.nss) #, save.path = "./pf_nss_v3")
print.pf.opt(opt.pf.ss, title=title.ss) #, save.path = "./pf_ss_v3")


################################################################################

#testiportfolion tuotot (w = painot, r = kompoenttien tuotot)
pf.test <- function(w,r) {
  D < dim(r)
  matrix(apply(r,1,function(r){pf.ret(W,r)}), D[1], 1)
}

# kumulatiiviset tuotot (r on oltava [0,1] eli desimaaleina eikä prosentteina)
cumulative.ret <- function(r) {
  cumprod(1 + r) - 1
}

#tracking error (pf.ret.real = todellinen tuotto l. testiaineisto estimoiduilla painoilla, pf.ret.est = testiaineisto )
pf.tracking.error <- function(pf.ret.real, pf.ret.est) {
  
  #tuottojen erotus
  diff.ret <- pf.ret.real - pf.ret.est
  
  #kumulatiivisten tuottojen erotus
  diff.cret <- cumulative.ret(pf.ret.real) - cumulative.ret(pf.ret.est)
  
  list(ret = sd(diff.ret), cret = sd(diff.cret))
}

#indeksi
create.index <- function(prices,weights.type = "value") {
  
  
  
  #normalisoi indeksi alkuhetkeen
  components <- apply(components, 1, function(x){   })
  
  #
  
}

#vertailuportfolio no 1: 1/N testiaineisto



#vertailuportfolio no 2: max sharpe, testiaineisto

#vertailuportfolio no 3: S&P500


