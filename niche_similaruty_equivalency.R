## ================================================================================================================== ##
##                                                                                                                    ##
## Script name:    Niche Similarity versus Niche Equivalency                                                          ##
##                                                                                                                    ##
## Purpose of script:                                                                                                 ##
##                                                                                                                    ##
## Author: MSc. Luíz Fernando Esser                                                                                   ##
##                                                                                                                    ##
## Date Created: 2021-01-26                                                                                           ##
##                                                                                                                    ##
## This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.   ##
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/4.0/                            ##
## or send a letter to Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.                                   ##
##                                                                                                                    ##
## Email: luizesser@gmail.com                                                                                         ##
##                                                                                                                    ##
## ================================================================================================================== ##
##                                                                                                                    ##
## Notes:   This script is adapted from ecospat documentation and user_script_Nsp_1a.R script written by              ##
##          Olivier Broennimann. Departement of Ecology and Evolution (DEE).                                          ##
##          16.01.2019. University of Lausanne. Department of ecology and evolution, Switzerland                      ##
##                                                                                                                    ##
## ================================================================================================================== ##
# Open Libraries
library(ecospat)
library(raster)
library(rgeos)
library(rgbif)
library(dplyr)
# Open data
s <- c("Ilex paraguariensis", "Ilex microdonta", "Myrceugenia euosma", "Euterpe edulis")
GBIF_data <- function(s){
  ids <- lapply(s, function(x) { name_suggest(q=x, rank = "species")$data$key[1]})
  ids <- unlist(ids)      
  data <- lapply(ids, function(x) { occ_data(taxonKey=x)$data[,c("species", "decimalLatitude","decimalLongitude")] } )
  data <- bind_rows(data)
  data <- data.frame(data)
  data <- na.omit(data)
  return(data)
}
spp <- GBIF_data(s)

clim <- getData('worldclim', var='bio', res=10) #environmental data
clim <- stack(list.files("wc10", pattern=".bil$", full.names = T)) # this line is just to use a subset of variables so it does not take much longer.

niche.comparison <- function(spp, clim){
  print(paste0("Build Environmental Data"))
  # Mask environmental data:
  buf <- gBuffer(SpatialPoints(spp[,c(3,2)]), widt=5) #create buffer around records
  clim2 <- mask(clim, buf)
  clim2 <- crop(clim2, buf)
  
  # Build Environment:
  clim.bkg<-na.exclude(data.frame(extract(clim2,buf)))
  clim.spp<-na.exclude(data.frame(extract(clim2,spp[,c(3,2)])))
  pca.env <-dudi.pca(clim.bkg, center = T, scale = T, scannf = F, nf = 2)
  
  # Storage matrices:
  overlap<-matrix(nrow=length(unique(spp[,1])),ncol=length(unique(spp[,1])),dimnames = list(unique(spp[,1]),unique(spp[,1])))		# store overlap values
  equivalency<-matrix(nrow=length(unique(spp[,1])),ncol=length(unique(spp[,1])),dimnames = list(unique(spp[,1]),unique(spp[,1])))	#store p-values for equivalency tests
  similarity<-matrix(nrow=length(unique(spp[,1])),ncol=length(unique(spp[,1])),dimnames = list(unique(spp[,1]),unique(spp[,1])))
  
  # Create pairs:
  sp.combn <- combn(unique(spp[,1]),2)

  print(paste0("loop of niche quantification for each combination of species"))
  # Loop of niche quantification for each combination of species:
  for(i in 1:ncol(sp.combn)) {  
    spa <- sp.combn[1,i] # name of species a
    spb <- sp.combn[2,i] # name of species b
    clim.spa <- clim.spp[spp[,1]==spa,] # env data for species a
    clim.spb <- clim.spp[spp[,1]==spb,] # env data for species b
    print("A")
    # PCA scores
    scores.bkg <- pca.env$li	                    # scores for global climate
    scores.spa <- suprow(pca.env,clim.spa)$lisup	# scores for spa
    scores.spb <- suprow(pca.env,clim.spb)$lisup	# scores for spb
    print("B")
    # calculation of occurence density
    za <- ecospat.grid.clim.dyn(scores.bkg,scores.bkg,scores.spa,1000)
    zb <- ecospat.grid.clim.dyn(scores.bkg,scores.bkg,scores.spb,1000)
    print("C")
    # test of niche equivalency and similarity
    equ  <- ecospat.niche.equivalency.test(za,zb,rep=10)
    sim  <- ecospat.niche.similarity.test(za,zb,rep=10,rand.type = 1) 
    sim2 <- ecospat.niche.similarity.test(zb,za,rep=10,rand.type = 1) 
    ov <- ecospat.niche.overlap(za,zb,cor=T)
    print("D")
    # storage of values
    overlap[sp.combn[1,i],sp.combn[2,i]]     <- ov$D      	# store overlap value
    equivalency[sp.combn[1,i],sp.combn[2,i]] <- equ$p.D			# store equivalency value
    similarity[sp.combn[1,i],sp.combn[2,i]]  <- sim$p.D			# store similarity 12 value
    similarity[sp.combn[2,i],sp.combn[1,i]]  <- sim2$p.D	  # store similarity 21 value
    
    print(paste0(i,"/",ncol(sp.combn)))
  }
  
  return(list(overlap.D=overlap,
              equivalency.D=equivalency,
              similarity.D=similarity))
  
}

set.seed(1)
x <- niche.comparison(spp, clim)
