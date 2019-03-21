# DistMod(observation)
# Fred Kaesmann Jr  2019 - 03 - 07

DistMod <- function(observation, treatments){
  # Checking for which modification of the data is most normal
  #
  # Args:
  #      observation: vector of numeric values
  #      treatments: factors by which the observations are broken down
  #
  # Returns:
  #         For single level or nonexistant treatment the IQR/SD is
  #           reported for the original data, the square root of the
  #           data, and the log of the data, along with an arrow pointing
  #           to the IQR/SD closest to 1.34
  #         For levels of treatment 2 or greater it provides the IQR/SD for
  #           the original, sqrt of, and log of the observations, broken 
  #           down by the treatment factors
  
  
  # Making sure we're dealing with numbers
  observation <- as.numeric(observation)
  
  
  if (missing(treatments) || nlevels(as.factor(treatments)) < 2){
  

  # Original data
  obsiqr <- IQR(observation) # Inter Quartile Range
  obssd <- sd(observation) # Standard Deviation
  obscalc <- obsiqr/obssd # IQR/SD
  obsrow <- c("Original:", obscalc) # labeled and stored
  

  # Square root of the observations
  squirt <- sqrt(observation) # Take Square Root
  sqiqr <- IQR(squirt)  # Inter Quartile Range
  sqsd <- sd(squirt)  # Standard Deviation
  sqcalc <- sqiqr/sqsd # IQR/SD
  sqrow <- c("Square root:",sqcalc) # labeled and stored
  
  # Log of the observations
  lagdat <- log(observation) # Take the log
  lagiqr <- IQR(lagdat) # Inter Quartile Range
  lagsd <- sd(lagdat) # Standard Deviation
  lagcalc <- lagiqr/lagsd # IQR/SD
  lagrow <-  c("Log:", lagcalc) # labeled and stored
  
  # Histogram
  par(ask=TRUE) # Wait for feedback before producing next graph
  hist(observation, main = "Original Data") # original data
  hist(squirt, main = "Square Root of Data") # sqrt(data)
  hist(lagdat, main = "Log of Data") # log(data)
  
  # Determining which version is most normal
  if (abs(1.34-obscalc)<abs(1.34-sqcalc) & abs(1.34-obscalc)<abs(1.34-lagcalc)){
    print("Original data is most normal")
    obsrow <- c(obsrow, "<-")
    sqrow <- c(sqrow,"  ")
    lagrow <- c(lagrow,"  ")
  } else if (abs(1.34-sqcalc)<abs(1.34-obscalc) & abs(1.34-sqcalc)<abs(1.34-lagcalc)) {
    print("Square rooted data is most normal")
    obsrow <- c(obsrow, "  ")
    sqrow <- c(sqrow,"<-")
    lagrow <- c(lagrow,"  ")
  } else {
    print("Log of data is most normal")
    obsrow <- c(obsrow, "  ")
    sqrow <- c(sqrow,"  ")
    lagrow <- c(lagrow,"<-")
  }
  
  } else {
    treatments <- as.factor(treatments)
    
    # Original data
    obsiqr <- tapply(observation, treatments, IQR) # Inter Quartile Range
    obssd <- tapply(observation, treatments, sd) # Standard Deviation
    obscalc <- obsiqr/obssd # IQR/SD
    obsrow <- c("Original:", obscalc) # labeled and stored
    
    
    # Square root of the observations
    sqiqr <- tapply(sqrt(observation), treatments, IQR)  # Inter Quartile Range
    sqsd <- tapply(sqrt(observation), treatments, sd)  # Standard Deviation
    sqcalc <- sqiqr/sqsd # IQR/SD
    sqrow <- c("Square root:",sqcalc) # labeled and stored
    
    # Log of the observations
    lagiqr <- tapply(log(observation), treatments, IQR) # Inter Quartile Range
    lagsd <- tapply(log(observation), treatments, sd) # Standard Deviation
    lagcalc <- lagiqr/lagsd # IQR/SD
    lagrow <-  c("Log:", lagcalc) # labeled and stored
  }
  
  # Group everything as a dataframe
  thegroup <- as.data.frame(rbind(obsrow,sqrow,lagrow))

  # Print all nice like
  print(thegroup, row.names = F)

}