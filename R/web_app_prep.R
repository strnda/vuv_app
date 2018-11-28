library(bilan)
library(data.table)
library(xts)
# library(dygraphs)
library(ggplot2)

id <- list.files('./data/lm/', pattern = '.rds')
id <- gsub('lm_', '', id)

for (i in seq_along(id)) {
  
  dta <- as.data.table(read.table(paste0('./data/clim/', gsub('.rds', '.dat', id[i])), header = T))
  dta[, DTM := as.Date(DTM)]
  setnames(dta, 'TMP', 'T')
  
  # dta <- dta[1:1000,]
  
  bil.par <- readRDS(paste0('./data/bilan/bil_par_', id[i]))
  
  bil <- bil.new('d') # , modif = 'period', period = 7)
  bil.set.values(bil, dta)
  bil.set.params(bil, bil.par)
  bil.pet(bil,'latit', latitude = 50)
  
  rmod <- as.data.table(bil.run(bil))
  
  fits <- readRDS(paste0('./data/lm/lm_', id[i]))
  
  model.c <- rbindlist(lapply(fits[!is.na(fits)], function(x) data.table(Time = dta[, DTM], Concentration = predict(x, list(r = rmod[, RM])))), idcol = 'Polutant')
  
  (x <- ggplot(model.c) +
    geom_line(aes(x = Time, y = Concentration), colour = 'red4') +
    facet_wrap(~Polutant, ncol = 1, scales = 'free_y') +
    theme_bw())
  
  saveRDS(model.c, paste0('./data/out/modelled_data_', id[i]))
}

