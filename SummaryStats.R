# SummaryStats(Observations, treatments)
# Fred Kaesmann Jr
# 2019-03-04

SummaryStats = function(observations, treatments){
  # Add requisite libraries
  library(lawstat)
  library(MASS)
  
  # Make sure observations is numeric
  observations <- as.numeric(observations)
  
  # Make sure treatments is characters
  treatments <- as.character(treatments)
  
  # 5 Number Summary
  numsum <- tapply(observations, treatments, summary) 
 
  # Inter Quartile Range
  iqr <- tapply(observations, treatments, IQR) 
  
  # Variance
  variance <- tapply(observations, treatments, var) 
  
  # Standard Deviation
  standdev <- tapply(observations, treatments, sd) 
  
  # Checking for Homogeniety of Variance with Levene Test
  levy <- levene.test(observations, treatments) 
  
  # Checking to see if the means are the same
  anov <- aov(observations~treatments)
  AnovaTable <- summary(anov)
  
  # TukeyHSD
  tuke <- TukeyHSD(anov)

  
   print(numsum)
   print("IQR:")
   print(iqr)
   print("Variance:")
   print(variance)
   print("Standard Deviation:")
   print(standdev)
   print("IQR/SD:")
   print(iqr/standdev)
   print("Levene Test:")
   print(levy)
   print("ANOVA")
   print(AnovaTable)
   print("TukeyHSD:")
   print(tuke)
   plot(tuke)

}
