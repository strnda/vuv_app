library(bilan)
library(data.table)
library(xts)
library(dygraphs)

id <- list.files('~/ownCloud/Active Docs/vuv_app/data/runoff/', pattern = '.rds')

dta <- as.data.table(read.table(paste0('~/ownCloud/Active Docs/vuv_app/data/clim/', gsub('.rds', '.dat', id[i])), header = T))
dta[, DTM := as.Date(DTM)]
setnames(dta, 'TMP', 'T')

dta <- dta[1:100,]

bil.par <- readRDS(paste0('~/ownCloud/Active Docs/vuv_app/data/bilan/bil_par_', id[i]))

bil <- bil.new('d') # , modif = 'period', period = 7)
bil.set.values(bil, dta)
bil.set.params(bil, bil.par)
bil.pet(bil,'latit', latitude = 50)

rmod <- as.data.table(bil.run(bil))


pol <- c('DTM', 'Průtok', 'Acesulfam', 'Caffein', 'Clarythromycin', 'Diclofenac', 'Gabapentin', 'Ibuprofen', 'Ibuprofen-2-hydroxy',
         'Ibuprofen-carboxy', 'Karbamazepin', 'Oxypurinol', 'Paracetamol', 'Paraxanthine', 'Saccharin', 'Sulfamethoxazol',
         'Telmisartan', 'Tramadol')

# for (j in which(!(names(con) %in% c('DTM', 'Průtok')))) {
# 
# }

fits <- readRDS(paste0('~/ownCloud/Active Docs/vuv_app/data/lm/lm_', id[i]))
fit <- fits[[15]]

# ci <- .5

model.c <- as.data.table(predict(fit, list(r = rmod[, RM]))) # , interval = 'confidence', level = ci))
model.c[, Time := dta[, DTM]]
model.c <- as.xts(model.c[, 2:1]) # model.c <- as.xts(model.c[, c(4, 1:3)])

dygraph(model.c, main = 'Koncentrace') %>%
  dyAxis('x', drawGrid = FALSE) %>%
  dySeries('V1', label = 'Koncentrace [ng/l]') %>% # dySeries(c('lwr', 'fit', 'upr'), label = 'Koncentrace [ng/l]') %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, 'Set1')) %>%
  dyRangeSelector(height = 20) %>%
  dyLegend(width = 400)

