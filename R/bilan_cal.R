library(bilan)
library(data.table)
library(ggplot2)
library(hydroGOF)

id <- list.files('~/ownCloud/Active Docs/vuv_app/data/runoff/', pattern = '.rds')

# i = 6

for (i in seq_along(id)) {
  
  dta.r <- as.data.table(readRDS(paste0('./data/runoff/', id[i])))
  dta.r[, DTM := as.Date(DTM)]
  
  dta.c <- as.data.table(read.table(paste0('./data/clim/', gsub('.rds', '.dat', id[i])), header = T))
  dta.c[, DTM := as.Date(DTM)]
  
  # summary(dta.r)
  # summary(dta.c)
  
  dta <- merge(dta.r, dta.c, by = 'DTM')
  setnames(dta, 'V1', 'R')
  setnames(dta, 'TMP', 'T')
  
  bil <- bil.new('d') #, modif = 'period', period = 7)
  bil.set.values(bil, dta)
  bil.pet(bil,'latit', latitude = 50)
  bil.set.optim(bil, method = 'BS', crit = 'NS', max_iter = 1500)
  model <- bil.optimize(bil)
  
  # NSE(model$RM,model$R)
  
  # out <- melt(model[500:1500, c('DTM', 'R', 'RM', 'BF')], id.vars = 'DTM')
  # ggplot(data = out) +
  #   geom_line(aes(x = DTM, y = value, colour = variable)) +
  #   scale_color_manual(values = c('steelblue', 'red4', 'limegreen'),
  #                      labels = c('R', 'RM', 'BF'),
  #                      name = '') +
  #   theme_bw()
  
  saveRDS(bil.get.params(bil), paste0('./data/bilan/bil_par_', id[i]))
}

