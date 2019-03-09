# SummaryStats(Observations, treatments)
# Fred Kaesmann Jr Date: 2019-03-04

SummaryStats <- function(observations, treatments, confidenceInterval){
  # Computes some basic summary statistics, runs a Levene test to determine
  # if ANOVA assumptions are met, an ANOVA test, and a TukeyHSD test if 
  # ANOVA p value is less than the confidence interval.
  #
  # Args:
  #   observations: .
  #   treatments: Different indexes of observations.
  #   confidenceInterval: Measure of accuracy of statistics tests, 
  #                          if no arg present, assumes .05 (95%).
  #
  # Returns:
  #   A series of summary statistics. (Designed for MAT 342 Project 2)
  
  
  
  # Add requisite libraries
  library(lawstat)
  library(MASS)
  
  # Condfidence interval check
  if (missing(confidenceInterval)){
    # If no confidence interval specified, assume .05 (95%)
    ConfidenceInterval = .05 
  } else if (confidenceInterval > 1 & confidenceInterval < 100) {
    # (if they input something like 95 instead of .05)
    confidenceInterval <- 1 - (confidenceInterval/100) 
  } else if (confidenceInterval >= 100){ # Erroneous Confidence Interval
    stop("Confidence Interval Error")
  }
  
  # Make sure observations is numeric
  observations <- as.numeric(observations)
  
  # Make sure treatments is characters
  treatments <- as.factor(treatments)
  
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
  anov <- aov(observations ~ treatments)
  AnovaTable <- anova(anov)
  
  # Boxcox
  # boxcox(lm(observations~treatments))
  # Why wont this work goddammit!!
  
  # TukeyHSD
  tuke <- TukeyHSD(anov)
  
  

   print("5 Number Summary")
   print(numsum)
   print("-------------------------------------------------------------------")
   print("Inter Quartile Range:")
   print(iqr)
   print("-------------------------------------------------------------------")
   print("Variance:")
   print(variance)
   print("-------------------------------------------------------------------")
   print("Standard Deviation:")
   print(standdev)
   print("-------------------------------------------------------------------")
   print("IQR/SD:")
   print(iqr/standdev)
   print("-------------------------------------------------------------------")
   
   
   boxplot(observations ~ treatments, main="For Internal Use Only")
   
   # Levene Test
   print("Levene Test:")
   print(levy)
   print("-------------------------------------------------------------------")
   
   # If Levene test p value is less than confidence interval,
   # then assumptions from anova not met and therefor inapplicaple
   if (levy$p.value < confidenceInterval){
    print("Variances significantly different across treatments")  
    print("therefore assumptions for ANOVA not met")
   } else {
   print("ANOVA")
   print(AnovaTable)
   print("-------------------------------------------------------------------")
   
   # If ANOVA p value is less than confidence interval,
   # run Tukey table to see which means are different
   if (AnovaTable$`Pr(>F)`[1] < confidenceInterval){
    print("There is at least one set of sample means.")
    print("TukeyHSD:")
    print(tuke)
    plot(tuke)
   } else {
      print("Means are not significantly different")
    }
  }
}