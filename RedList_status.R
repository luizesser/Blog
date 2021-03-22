# IUCN RedList Status #
# Open Library:
library(taxize)

# Select Species:
s <- c("Terminalia laxiflora", "Combretum glutinosum", "Fake name", "terminalia.laxiflora")

# Set Tolken:
k <- c("YOUR TOLKEN HERE")

# Set Function:
RedList_status <- function(s, k){
  info  <- iucn_summary(s, key=k)
  status <- iucn_status(info, key=k)
  status <- as.data.frame(status)
  return(status)
}

# Run Function:
resultado <- RedList_status(s,k)
resultado
nrow(resultado)