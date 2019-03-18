# MAT342Project1(observations)
# Fred Kaesmann Jr

MAT342Project1 <- function(observations, confidenceInterval){
  # 
  # Purpose: Does our MAT 342 project 1
  # 
  #Input: observation: Some observation variable
  #        confidenceInterval: input for confidence interval
  #
  # Output: A series of summary stats:
  #                 5 Number Summary
  #                 Inter Quartile Range
  #                 Standard Deviation
  #                 Normality Check
  #                 Histogram
  #                 Boxplot
  #                 Single Variable t-test
  
  # Condfidence interval check
  if (missing(confidenceInterval)){
    confidenceInterval <- .05 
    cat(sprintf("Alpha = %s\n", confidenceInterval))
    cat("\n", "-------------------------------------------------------------------", "\n")
  } else if (confidenceInterval > 1 & confidenceInterval < 100){
    # (if they input something like 95 instead of .05)
    confidenceInterval <- 1 - (confidenceInterval/100)
    cat(sprintf("Alpha = %s\n", confidenceInterval))
    cat("\n", "-------------------------------------------------------------------", "\n")
  } else if (confidenceInterval >= 100){ # Erroneous Confidence Interval
    stop("Confidence Interval Error")
  } else if (confidenceInterval <= 0) {
    stop("Confidence Interval Error")
  }

  # Make sure observations is numeric
  observations <- as.numeric(observations)

  # 5 number summary
  numsum <- summary(observations)
  print(numsum)
  cat("\n", "-------------------------------------------------------------------", "\n")
  
  cat("\n", "Inter Quartile Range:")
  iqr <- IQR(observations)
  print(iqr)
  cat("\n", "-------------------------------------------------------------------", "\n")
  
  cat("\n", "Variance:")
  variance <- var(observations)
  print(variance)
  cat("\n", "-------------------------------------------------------------------", "\n")
  
  cat("\n", "Standard Deviation:")
  standdev <- sd(observations)
  print(standdev)
  cat("\n", "-------------------------------------------------------------------", "\n")
  
  cat("\n", "IQR/SD:")
  print(iqr/standdev)
  cat("\n", "-------------------------------------------------------------------", "\n")
  
  # Histogram and Boxplot
  hist(observations, main = "For Internal Use Only")
  par(ask=TRUE)
  boxplot(observations, main = "For Internal Use Only", xlab = observations, horizontal = TRUE)
  
  # Single Variable t.test
  ttest <- t.test(observations)
  pvalTtest <- ttest$p.value
  print(ttest)
  cat("\n", "-------------------------------------------------------------------", "\n")
  
  # Null Hypothesis test
  if (pvalTtest < confidenceInterval) {
    cat("\n", sprintf("t-test pval (%f) < (%s) confidence interval", pvalTtest, confidenceInterval))
    cat("\n", "REJECT Null Hypothesis")
  } else if (pvalTtest > confidenceInterval) {
    cat("\n", sprintf("t-test pval (%f) > (%s) confidence interval",pvalTtest, confidenceInterval))
    cat("\n", "CANNOT REJECT Null Hypothesis")
  }
  
  
  
}