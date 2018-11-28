library(bilan)
library(data.table)
library(xts)
# library(dygraphs)
library(ggplot2)

id <- list.files('./data/lm/', pattern = '.rds')
id <- gsub('lm_', '', id)

i <- 4

delka_predpovedi <- 10

for (i in seq_along(id)) {
  
  dta <- data.table(DTM = seq.Date(from = Sys.Date(), length.out = 7*delka_predpovedi, by = 'day'))
  dta[, T := rnorm(.N, .5, 2)]
  dta[, P := rbinom(.N, 1, .2)*rgamma(.N, .75, .25)]
  
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
  
  saveRDS(model.c, paste0('./data/out/forcast/modelled_data_', id[i]))
  
}

