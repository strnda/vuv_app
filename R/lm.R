library(data.table)

get.corr.data <- function(x, rho) {
  
  x <- na.omit(x)
  n <- length(x)
  
  theta <- acos(rho)
  
  y <- rnorm(n, 2, .2)
  X <- cbind(x, y)
  Xctr <- scale(X, center = TRUE, scale = FALSE)
  
  Id <- diag(n)
  Q <- qr.Q(qr(Xctr[, 1, drop = FALSE]))
  P <- tcrossprod(Q)
  x2o <- (Id - P) %*% Xctr[, 2]
  Xc2 <- cbind(Xctr[, 1], x2o)
  Y <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))
  
  abs(Y[ , 2] + (1 / tan(theta)) * Y[ , 1])
}

id <- list.files('~/ownCloud/Active Docs/vuv_app/data/runoff/', pattern = '.rds')

zdroj <- 'Zelivka'

for (i in seq_along(id)) {
  
  if(zdroj == 'Zelivka') {
    
    pol <- c('Acesulfam', 'Caffein', 'Clarythromycin', 'Diclofenac', 'Gabapentin', 'Ibuprofen', 'Ibuprofen-2-hydroxy',
             'Ibuprofen-carboxy', 'Karbamazepin', 'Oxypurinol', 'Paracetamol', 'Paraxanthine', 'Saccharin', 'Sulfamethoxazol',
             'Telmisartan', 'Tramadol')
  }
  
  if(zdroj == 'Karany') {
    
    pol <- c()
  }
  
  aux <- c('DTM', 'Průtok', pol)
  
  con <- readRDS(paste0('~/ownCloud/Active Docs/vuv_app/data/conc/conc_', id[i]))
  con <- con[, which(names(con) %in% aux)]
  
  LM <- list()
  
  for (j in which(!(names(con) %in% c('DTM', 'Průtok')))) {
    
    temp <- data.table(r = con[, 'Průtok'], c = con[, j])
    temp[, thr := as.numeric(gsub('-9', '', min(c, na.rm = T)))]
    temp <- na.omit(temp)
    
    if(dim(temp[c < 0])[1] > 0)  {
      
      temp[c < 0, c := get.corr.data(r[c < 0], rho = .95)*thr]
    }

    if(dim(temp)[1] > 0) {
      
      LM[[j - 1]] <- lm(c ~ r, temp[, .(r, c)])
    } else {
      
      LM[[j - 1]] <- NA
      print('model cannot be fitted')
    }
    
   
  }
  names(LM) <- names(con)[which(!(names(con) %in% c('DTM', 'Průtok')))]
  
  saveRDS(LM, paste0('~/ownCloud/Active Docs/vuv_app/data/lm/lm_', id[i]))
}
