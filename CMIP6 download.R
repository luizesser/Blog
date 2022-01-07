cmip6_data <- function(year,gcm,ssp,resolution){
  require(raster)
  ## VALUES
  # year: c("2030", "2050", "2070", "2090")
  # gcm: c("bc", "cc", "ce","ca","gf", "ip", "mi", "mo", "mr")
  # ssp: c("126", "245", "370", "585")
  # resolution: c("10", "5", "2.5")
  all_gcm <- c("bc", "cc", "ce","ca","gf", "ip", "mi", "mo", "mr")
  gcm2 <- c("BCC-CSM2-MR", "CNRM-CM6-1", "CNRM-ESM2-1", "CanESM5", 
            "GFDL-ESM4", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0")
  gcm3 <- gcm2[match(gcm,all_gcm)]
  
  all_year <- c("2030", "2050", "2070", "2090")
  year2 <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")
  year3 <- year2[match(year,all_year)]
  
  for (g in 1:length(gcm)) {
    for (s in 1:length(ssp)) {
      for (y in 1:length(year)) {
        print(paste0(gcm[g], "_", ssp[s], "_", resolution, "_", year[y]))
        download.file(url = paste0("https://biogeo.ucdavis.edu/data/worldclim/v2.1/fut/",
                                   resolution,
                                   "m/wc2.1_",
                                   resolution,
                                   "m_bioc_",
                                   gcm3[g],
                                   "_ssp",
                                   ssp[s],
                                   "_",
                                   year3[y],
                                   ".zip"), 
                      destfile = paste0("WorldClim_data/",gcm[g], "_", ssp[s], "_", resolution, "_", year[y],".zip"), 
                      method = "auto")
        unzip(zipfile = paste0("WorldClim_data/",gcm[g], "_", ssp[s], "_", resolution, "_", year[y],".zip"),
              exdir = paste0("WorldClim_data/",gcm[g], "_", ssp[s], "_", resolution, "_", year[y]),
              overwrite = T)
        
      }
    }
  }
  l <- list.files("WorldClim_data",pattern=".tif$", recursive = T, full.names = T)
  l <- lapply(l, stack)
  return(l)
}

x <- cmip6_data("2070","mi","585","10")

x <- cmip6_data(c(2070,2050),"mi",585,10)




