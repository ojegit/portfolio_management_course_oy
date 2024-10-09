

simulate.portfolio <- function(R, 
                               S, 
                               w.min=0,
                               w.max=1, 
                               mc.iters = 5000, 
                               min.pf.ret.target = -Inf, 
                               max.pf.var.target = Inf,
                               risk.free = NA,
                               gammas = c(1),
                               print.int = 500) {
  
  N <- dim(R)[1]
  
  w.limits <- matrix(0,N,2)
  pf.moments <- data.frame(matrix(0,mc.iters,2))
  colnames(pf.moments) <- c("ret", "var")
  
  w.lo <- w.min
  w.hi <- w.max
  
  exit.flag <- 0
  iter <- 0
  max.stuck.iters <- 1000
  iter.stuck <- 0
  
  
  ng <- length(gammas)
  max.pf.sr <- -Inf
  min.pf.var <- Inf
  max.pf.ret <- -Inf
  max.pf.util <- matrix(0,N,ng)
  
  
  while(TRUE) {
    w.try <- matrix(w.lo + (w.hi - w.lo) * runif(N), N, 1)
    w.try <- w.try/sum(w.try)

    if ( any(w.try < w.min) || any(w.try > w.max) ) {
      iter.stuck <- iter.stuck + 1
      if (iter.stuck == max.stuck.iters) {
        exit.flag <- 2
        cat("Max number of stuck iterations reached. Exiting...")
        break
      }
      
      next
    }
    
    pf.ret.try <- pf.ret(w.try, R)
    pf.var.try <- pf.var(w.try, S)

    if (is.na(pf.ret.try) || is.na(pf.var.try) || 
        pf.ret.try < min.pf.ret.target || pf.var.try > max.pf.var.target) {
      iter.stuck <- iter.stuck + 1
      if(iter.stuck == max.stuck.iters) {
        exit.flag <- 2
        cat("Max number of stuck iterations reached. Exiting...")
        break
      }
      
      next
    }

    iter.stuck <- 0
    
    if ( (print.int > 0) && (iter %% print.int) == 0 ) {
      print(paste("ITER NO: ",iter))
    }
    
    iter <- iter + 1
    
    # tallennetaan tulokset
    pf.moments[iter,1] <- pf.ret.try
    pf.moments[iter,2] <- pf.var.try
  
    
    ### save optima
    
    #maximum sharpe
    pf.sr <- (pf.ret.try - risk.free) / sqrt(pf.var.try)
    if (pf.sr > max.pf.sr) {
      max.pf.sr <- sr
      max.pf.sr.w <- w.try
      max.pf.sr.index <- iter
    }
    
    #minimum variance
    if (pf.var.try < min.pf.var) {
      min.pf.var <- pf.var.tr
      min.pf.var.w <- w.try
      min.pf.var.index <- iter
    }
    
    #maximum return
    if (pf.ret.try > max.pf.ret) {
      max.pf.ret <- pf.ret.try
      max.pf.ret.w <- w.try
      max.pf.ret.index <- iter
    }
    
    #max utility
    for(j in 1:ng) {
      pf.util <- pf.ret.try - 0.5*gammas[j]*pf.var.try
      if(util > max.pf.util[j]) {
        max.pf.util[j] <- pf.util
        max.pf.util.ret.w[,j] <- w.try
        max.pf.util.index[j] <- iter
      }
      
    }
    ###
    
    w.limits[,1] <- apply(cbind(w.limits[,1],w.try),1,min)
    w.limits[,2] <- apply(cbind(w.limits[,2],w.try),1,max)
    
    
    if (iter == mc.iters) {
      exit.flag <- 1
      if (print.int > 0) {
        cat("Done!")
      }
      break
    }
    
  }

  list(pf.moments = pf.moments, 
       w.limits = w.limits, 
       opt = list(max.sr = list(val=max.sr, w=max.sr.w, index=max.sr.index),
                min.var = list(val=min.var, w=min.var.w, index=min.var.index),
                max.ret = list(val=max.ret, w=max.ret.w, index=max.ret.index),
                max.util = list(val=max.util, w=max.util.w, index=max.util.index),
                ))
  
} # END simulate.pf