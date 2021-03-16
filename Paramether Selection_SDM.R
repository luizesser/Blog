# tuning parameters in sdm
library(sdm)
library(dplyr)
# Open an example data:
df <- read.csv(system.file("external/pa_df.csv", package="sdm"))
d <- sdmData(sp~b15+NDVI,train=df)
d
# In getmethod function you can  find the @fitSettings slot, which can bring some of the parameters that you can improve:
info_gbm <- getmethod('gbm')
info_gbm@fitSettings
# You can also find the package where the algorithm comes from and look directly into the package what are the arguments:
info_gbm@packages
?gbm::gbm

# You can pass the same parameters from the function in the original package to the modelSettings parameter in sdm function.
# Note that you can't set multiple values for the same parameter with c(), you will need to change it in different runs.
# I find an easy way to run only one algorithm at a time.
# Note also that you may want to set a seed to make models more comparable.
gbm_g10   <- sdm(sp~b15+NDVI,data=d,methods=c('gbm'), seed=1, modelSettings=list(gbm=list(distribution = 'gaussian' , n.trees = 10 )))
gbm_g100  <- sdm(sp~b15+NDVI,data=d,methods=c('gbm'), seed=1, modelSettings=list(gbm=list(distribution = 'gaussian' , n.trees = 100)))
gbm_g1000  <- sdm(sp~b15+NDVI,data=d,methods=c('gbm'), seed=1, modelSettings=list(gbm=list(distribution = 'gaussian' , n.trees = 1000)))
gbm_l10   <- sdm(sp~b15+NDVI,data=d,methods=c('gbm'), seed=1, modelSettings=list(gbm=list(distribution = 'laplace'  , n.trees = 10 )))
gbm_l100  <- sdm(sp~b15+NDVI,data=d,methods=c('gbm'), seed=1, modelSettings=list(gbm=list(distribution = 'laplace'  , n.trees = 100)))
gbm_l1000  <- sdm(sp~b15+NDVI,data=d,methods=c('gbm'), seed=1, modelSettings=list(gbm=list(distribution = 'laplace'  , n.trees = 1000)))
gbm_b10   <- sdm(sp~b15+NDVI,data=d,methods=c('gbm'), seed=1, modelSettings=list(gbm=list(distribution = 'bernoulli', n.trees = 10 )))
gbm_b100  <- sdm(sp~b15+NDVI,data=d,methods=c('gbm'), seed=1, modelSettings=list(gbm=list(distribution = 'bernoulli', n.trees = 100)))
gbm_b1000  <- sdm(sp~b15+NDVI,data=d,methods=c('gbm'), seed=1, modelSettings=list(gbm=list(distribution = 'bernoulli', n.trees = 1000)))

# Now let's see what was our best combination:
l <- ls(pattern = 'gbm_')
auc_gbm <- apply(as.matrix(l), 1, function(x){ m <- get(x)
                                               m <- getEvaluation(m)
                                               paste0(m$AUC)
                                             }
                 )
result <- data.frame(model=l,AUC=auc_gbm)

# If you run:
m3_gbm@models$sp$brt$`1`@object
gbm_g10@models$sp$brt$`1`@object

# You will see at the end a description from the model generated:
# > A gradient boosted model with bernoulli loss function.
# > 10000 iterations were performed.
# > There were 2 predictors of which 2 had non-zero influence.

# Remember to take a look at sdmSetting function for some other useful information:
?sdmSetting

# UPDATE:
# A more straigthforward approach, that may be more useful is to build a expand.grid with tuning parameters:
# Open an example data:
df <- read.csv(system.file("external/pa_df.csv", package="sdm"))
d <- sdmData(sp~b15+NDVI,train=df)
d
# Create tune grid:
tuneGrid_gbm <- expand.grid(distribution = c('gaussian', 'laplace', 'bernoulli'),
                        n.trees = c(10, 100, 1000),
                        stringsAsFactors = F)
# We then apply it in this function, which runs all combination of models
# d = sdmData object
# tuneGrid = a data.frame with the combination of parameters to be tunned.
# algo = algorithm to be tested
# stat = evaluation statistics to be considered when tuning (see ?getEvaluation for all stats available)
tune_sdm <- function(d, algo, tuneGrid, stat){ 
  tuneList <- alply(tuneGrid, 1, as.list)
  result <- data.frame(tuneGrid, AUC=NA)
  for(i in 1:length(tuneList)){
    l <- list(tuneList[[i]])
    names(l) <- algo
    s <- sdm(data=d,methods=paste0(algo), seed=1, modelSettings=l)
    result$AUC[i] <- getEvaluation(s, stat = paste0(stat))[2]
  }
  return(result)
}

tuning_result <- tune_sdm(d, algo='gbm', tuneGrid = tuneGrid_gbm, 'AUC')






