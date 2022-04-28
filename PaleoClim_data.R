PaleoClim_data <- function(tp='current', res=10){ 
  require(raster)
  ## VALUES
  # tp: c("m2","mpwp","MIS19","LIG","LGM","HS1","BA",
  #       "YDS","EH","MH","LH","LH","current")
  # res: c("10", "5", "2.5", "30")
  
  ## NOTE:
  # For details on time period (tp) and resolution (res) see: http://www.paleoclim.org
  # If your download reaches the following error:
  #     'Timeout of 60 seconds was reached'
  # You can adjust the timeout by runing:
  # > options(timeout=1000) 
  
  all_tp <- c("m2","mpwp","MIS19","LIG","HS1","BA",
              "YDS","EH","MH","LH","LH","LGM","current")
  tp2 <- c("m2","mpwp","MIS19","LIG","HS1","BA","YDS","EH","MH","LH","LH", "chelsa_LGM", "chelsa_cur")
  tp3 <- c("M2","mPWP","MIS19","LIG","HS1","BA","YDS","EH","MH","LH","LH", "chelsa_LGM_v", "CHELSA_cur_V")
  tp4 <- c("M2","MP","MI","LI","HS","BA","YD","EH","MH","LH","LH", "LGM", "current")
  
  for (g in 1:length(tp)) {
    v <- which(tp[g] == all_tp)
    if (v > 11){
      for (s in 1:length(res)) {
        print(paste0(tp[g], "_", res[s]))
        if(res[s] == 30){
          r = 's'
        } else {
          r = 'm'
        }
        if(res[s] == 2.5){
          res2 = '2_5'
        } else {
          res2 = res[s]
        }
        if(!dir.exists("PaleoClim_data")){ dir.create("PaleoClim_data") }
        download.file(url = paste0("http://sdmtoolbox.org/paleoclim.org/data/",
                                   tp2[v],"/",tp3[v],"1_2B_r",res2,r,".zip"), 
                      destfile = paste0("PaleoClim_data/",tp4[v], "_", res[s],".zip"), 
                      method = "auto")
        unzip(zipfile = paste0("PaleoClim_data/",tp4[v], "_", res[s],".zip"),
              exdir = paste0("PaleoClim_data/", tp4[v], "_", res[s]),overwrite = TRUE)
      }
    } else {
      for (s in 1:length(res)) {
        if(res[s] == 30){
          print('30s resolution is not available for this time period. Skiping...')
        } else {
          print(paste0(tp[g], "_", res[s]))
          if(res[s] == 2.5){
            res2 = '2_5'
          } else {
            res2 = res[s]
          }
          if(!dir.exists("PaleoClim_data")){ dir.create("PaleoClim_data") }
          download.file(url = paste0("http://sdmtoolbox.org/paleoclim.org/data/",
                                     tp2[v],"/",tp3[v],"_v1_",res2,"m.zip"), 
                        destfile = paste0("PaleoClim_data/",tp4[v], "_", res[s],".zip"), 
                        method = "auto")
          unzip(zipfile = paste0("PaleoClim_data/",tp4[v], "_", res[s],".zip"),
               exdir = paste0("PaleoClim_data/", tp4[v], "_", res[s]),overwrite = TRUE)
        }
      }
    }
  }
}

x <- PaleoClim_data(c('current', 'YDS'), c(10,30))

