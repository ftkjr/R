# DistMod(observation)
# Fred Kaesmann Jr
# 2019 - 03 - 07
# Checking for which mod of the data is most normal

DistMod <- function(observation){
  # Making sure we're dealing with numbers
  observation <- as.numeric(observation)
  
  # Original data
  obsiqr <- IQR(observation) # Inter Quartile Range
  obssd <- sd(observation) # Standard Deviation
  obscalc <- obsiqr/obssd # IQR/SD
  obsrow <- c("Original:", obscalc) # labeled and stored
  hist(observation) # Histogram
  
  # Square root of the observations
  squirt <- sqrt(observation) # Take Square Root
  sqiqr <- IQR(squirt)  # Inter Quartile Range
  sqsd <- sd(squirt)  # Standard Deviation
  sqcalc <- sqiqr/sqsd # IQR/SD
  sqrow <- c("Square root:",sqcalc) # labeled and stored
  hist(squirt) # Histogram
  
  # Log of the observations
  lagdat <- log(observation) # Take the log
  lagiqr <- IQR(lagdat) # Inter Quartile Range
  lagsd <- sd(lagdat) # Standard Deviation
  lagcalc <- lagiqr/lagsd # IQR/SD
  lagrow <-  c("Log:", lagcalc) # labeled and stored
  hist(lagdat) # Histogram
  
  # Determining which version is most normal
  if (abs(1.34-obscalc)<abs(1.34-sqcalc) & abs(1.34-obscalc)<abs(1.34-lagcalc)){
    print("Original data is most normal")
  } else if (abs(1.34-sqcalc)<abs(1.34-obscalc) & abs(1.34-sqcalc)<abs(1.34-lagcalc)) {
    print("Square rooted data is most normal")
  } else {
    print("Log of data is most normal")
  }
  
  # Group everything as a dataframe
  thegroup <- as.data.frame(rbind(obsrow,sqrow,lagrow))
  
  # Print all nice like
  print(thegroup, row.names = F)
}