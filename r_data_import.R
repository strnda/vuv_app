library(rgdal)
library(rgeos)
library(raster)
library(data.table)

upov <- readOGR('~/ownCloud/Shared/BILAN_UPOV/data/geo/UPOV_poly_upraveny/UPOV_poly_opraveny_krov.shp')
pov <- readOGR('./data/gis/povodi_nova.shp')

# plot(upov)
# plot(pov, col = '#f44b4280', add = T)

upov.s <- intersect(upov, pov)

X <- list()

for (i in 1:length(unique(pov@data$DBCN))) {
  X[[i]] <- upov.s[upov.s@data$DBCN %in% unique(pov@data$DBCN)[i],]
}

# plot(upov.s)
# plot(upov.s[X[[4]],], add = T, col = 'red')

area.upov <- unique(data.table(UPOV_ID = upov.s@data$UPOV_ID, area = sapply(seq_along(upov.s@data$UPOV_ID), function(i) gArea(upov.s[upov.s@data$UPOV_ID == upov.s@data$UPOV_ID[i],]))))

dta <- lapply(X, function(i) {
  
  upov.file.names <- list.files('~/ownCloud/Shared/BILAN_UPOV/webapp/data/bilan/')
  
  temp <- lapply(paste('~/ownCloud/Shared/BILAN_UPOV/webapp/data/bilan/', i$UPOV_ID, '.rds', sep = ''), function (x) data.table(readRDS(x)))
  names(temp) <- i$UPOV_ID
  temp <- rbindlist(lapply(temp, function(x) x[, .(DTM, RM)]), idcol = 'UPOV_ID')
  temp <- merge(temp, area.upov, by = 'UPOV_ID', all.x = TRUE)
  temp[, weighted.mean(RM, area), by = DTM]
})


names(dta) <- sapply(X, function(x) unique(x@data$DBCN))

for (i in seq_along(dta)) {
  
  saveRDS(dta[[i]], file = paste0('~/ownCloud/Active Docs/vuv_app/data/runoff/', names(dta)[i], '.rds'))
}
