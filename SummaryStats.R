# SummaryStats(Observations, treatments)
# Fred Kaesmann Jr
# 2019-03-04

SummaryStats = function(observations, treatments){
  
  numsum <- tapply(observations, treatments, summary) 
  # 5 Number Summary
  
  iqr <- tapply(observations, treatments, IQR) 
  # Inter Quartile Range
  
  variance <- tapply(observations, treatments, var) 
  # Variance
  
  standdev <- tapply(observations, treatments, sd) 
  # Standard Deviation
  
  levy <- levene.test(observations, treatments) 
  # Checking for Homogeniety of Variance
  
   anov <- summary(aov(observations~treatments)) 
  # Checking to see if the means are the same
  
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
   print(anov)

}