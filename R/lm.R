library(bilan)
library(data.table)
library(xts)
library(dygraphs)

i <- 1

id <- list.files('~/ownCloud/Active Docs/vuv_app/data/runoff/', pattern = '.rds')

# dta.r <- as.data.table(readRDS(paste0('~/ownCloud/Active Docs/vuv_app/data/runoff/', id[i])))
# dta.r[, DTM := as.Date(DTM)]
# 
# dta.c <- as.data.table(read.table(paste0('~/ownCloud/Active Docs/vuv_app/data/clim/', gsub('.rds', '.dat', id[i])), header = T))
# dta.c[, DTM := as.Date(DTM)]
# 
# dta <- merge(dta.r, dta.c, by = 'DTM')
# setnames(dta, 'V1', 'R')
# setnames(dta, 'TMP', 'T')
# 
# bil.par <- readRDS(paste0('~/ownCloud/Active Docs/vuv_app/data/bilan/bil_par_', id[i]))
# 
# bil <- bil.new('d') # , modif = 'period', period = 7)
# bil.set.values(bil, dta)
# bil.set.params(bil, bil.par)
# bil.pet(bil,'latit', latitude = 50)
# 
# rmod <- as.data.table(bil.run(bil))

pol <- c('DTM', 'Průtok', 'Acesulfam', 'Caffein', 'Clarythromycin', 'Diclofenac', 'Gabapentin', 'Ibuprofen', 'Ibuprofen-2-hydroxy',
         'Ibuprofen-carboxy', 'Karbamazepin', 'Oxypurinol', 'Paracetamol', 'Paraxanthine', 'Saccharin', 'Sulfamethoxazol',
         'Telmisartan', 'Tramadol')

con <- readRDS(paste0('~/ownCloud/Active Docs/vuv_app/data/conc/conc_', id[i]))
con <- con[, which(names(con) %in% pol)]

get.corr.data <- function(x, n, rho, threshold) {
  
  theta <- acos(rho)
  
  y <- rnorm(n, threshold, threshold/10)
  X <- cbind(x, y)
  Xctr <- scale(X, center = TRUE, scale = FALSE)
  
  Id <- diag(n)
  Q <- qr.Q(qr(Xctr[, 1, drop = FALSE]))
  P <- tcrossprod(Q)
  x2o <- (Id-P) %*% Xctr[, 2]
  Xc2 <- cbind(Xctr[, 1], x2o)
  Y <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))
  
  abs(Y[ , 2] + (1 / tan(theta)) * Y[ , 1])
}

j = 3
for (j in which(!(names(con) %in% c('DTM', 'Průtok')))) {
  
  temp <- data.table(r = con[, 'Průtok'], c = con[, j])
  temp[, thr := as.numeric(gsub('-9', '', min(c, na.rm = T)))]
  temp[(c < 0) & !is.na(r), c := get.corr.data(r, length(which(c < 0)), .85, threshold = thr)]
  
  fit <- lm(c ~ r, temp[, .(r, c)])
}

summary(fit)

plot(temp[, r], temp[, c], pch = 19, col = '#00000030')
abline(fit, col = 'red')

##############

ci <- .90

model.c <- as.data.table(predict(fit, list(RM = rgamma(100, .2)), interval = 'confidence', level = ci))
model.c[, Time := seq(from = Sys.Date() - .N, by = 'day', length.out = .N)]
model.c <- as.xts(model.c[, c(4, 1:3)])

dygraph(model.c, main = 'Koncentrace') %>%
  dyAxis('x', drawGrid = FALSE) %>%
  dySeries(c('lwr', 'fit', 'upr'), label = 'Koncentrace [ng/l]') %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, 'Set1')) %>%
  dyRangeSelector(height = 20) %>%
  dyLegend(width = 300)
