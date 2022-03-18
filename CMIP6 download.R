cmip6_data <- function(variable,year,gcm,ssp,resolution){
  require(raster)
  ## VALUES
  # variable: c("tmax",""tmin,"prec","bioc")
  # year: c("2030", "2050", "2070", "2090")
  # gcm: c("ac", "bc", "ca", "cc", "ce",
  #        "cn", "ch", "cr", "ec",
  #        "ev", "fi", "gf", "gg",
  #        "gh", "hg", "in", "ic",
  #        "ip", "me", "mi", "mp",
  #        "ml", "mr", "uk")
  # ssp: c("126", "245", "370", "585")
  # resolution: c("10", "5", "2.5", "30")
  ## NOTE:
  # If your download reaches the following error:
  #     'Timeout of 60 seconds was reached'
  # You can adjust the timeout by runing:
  # options(timeout=100) 
  
  all_gcm <- c("ac", "bc", "ca", "cc", "ce","cn", "ch", "cr", "ec","ev", "fi", 
               "gf", "gg","gh", "hg", "in", "ic", "ip", "me", "mi", "mp","ml", 
               "mr", "uk")
  gcm2 <- c("ACCESS-ESM1-5","BCC-CSM2-MR","CanESM5","CanESM5-CanOE","CMCC-ESM2",
            "CNRM-CM6-1","CNRM-CM6-1-HR","CNRM-ESM2-1","EC-Earth3-Veg",
            "EC-Earth3-Veg-LR","FIO-ESM-2-0","GFDL-ESM4","GISS-E2-1-G",
            "GISS-E2-1-H","HadGEM3-GC31-LL","INM-CM4-8","INM-CM5-0",
            "IPSL-CM6A-LR","MIROC-ES2L","MIROC6","MPI-ESM1-2-HR",
            "MPI-ESM1-2-LR","MRI-ESM2-0","UKESM1-0-LL")
  gcm3 <- gcm2[match(gcm,all_gcm)]
  all_year <- c("2030", "2050", "2070", "2090")
  year2 <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")
  year3 <- year2[match(year,all_year)]
  if(resolution == 30){
    res = 's'
  } else {
    res = 'm'
  }
  for (g in 1:length(gcm)) {
    for (s in 1:length(ssp)) {
      for (y in 1:length(year)) {
        print(paste0(gcm[g], "_", ssp[s], "_", resolution, "_", year[y]))
        if(!dir.exists("WorldClim_data")){ dir.create("WorldClim_data") }
        download.file(url = paste0("https://geodata.ucdavis.edu/cmip6/",
                                   resolution,
                                   res,
                                   "/",
                                   gcm3,
                                   "/ssp",
                                   ssp,
                                   "/wc2.1_",
                                   resolution,
                                   res,
                                   "_",
                                   variable,
                                   "_",
                                   gcm3,
                                   "_ssp",
                                   ssp,
                                   "_",
                                   year3,
                                   ".tif"), 
                      destfile = paste0("WorldClim_data/",gcm[g], "_", ssp[s], "_", resolution, "_", year[y],".tif"), 
                      method = "auto")
      }
    }
  }
  l <- list.files("WorldClim_data",pattern=".tif$", recursive = T, full.names = T)
  l <- lapply(l, stack)
  return(l)
}

x <- cmip6_data("bioc","2070","mi","585","10")

x <- cmip6_data("bioc",c(2070,2050),"mi",585,10)
