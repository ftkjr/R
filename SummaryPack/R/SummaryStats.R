# SummaryStats(Observations, treatments, confidenceInterval)
# Fred Kaesmann Jr Date: 2019-03-04

SummaryStats <- function(observations, treatments, confidenceInterval){
  # Computes some basic summary statistics, runs a Levene test to determine
  # if ANOVA assumptions are met, if levene p > confidenceInterval then it runs an ANOVA test,
  # and a TukeyHSD test if ANOVA p value is less than the confidence interval.
  #
  # Args:
  #   observations: observation variable (numeric)
  #   treatments: Different indexes of observations.
  #   confidenceInterval: Measure of accuracy of statistics tests,
  #                          if no arg present, assumes .05 (95%).
  #                             (optional input)
  #
  # Returns:
  #   A series of summary statistics. (Designed for MAT 342 Project 2)

  # Add requisite libraries
  library(lawstat)
  library(MASS)

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

  # For a single set of observations
  if (missing(treatments) || nlevels(as.factor(treatments)) == 1){
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

    # Single Variable t.test
    ttest <- t.test(observations)
    pvalTtest <- ttest$p.value
    print(ttest)

    # Null Hypothesis test
    if (pvalTtest < confidenceInterval) {
      cat("\n", sprintf("t-test pval (%f) < (%s) confidence interval", pvalTtest, confidenceInterval))
      cat("\n", "REJECT Null Hypothesis")
    } else if (pvalTtest > confidenceInterval) {
      cat("\n", sprintf("t-test pval (%f) > (%s) confidence interval",pvalTtest, confidenceInterval))
      cat("\n", "CANNOT REJECT Null Hypothesis")
    }

    # Plot your dataset hist -> wait -> boxplot
    hist(observations, main = "For Internal Use Only")
    par(ask=TRUE)
    boxplot(observations, main = "For Internal Use Only", horizontal = TRUE)

  } else {

  # Make sure treatments is characters
  treatments <- as.factor(treatments)

  # 5 Number Summary
  numsum <- tapply(observations, treatments, summary)
  print("5 Number Summary")
  print(numsum)
  cat("\n", "-------------------------------------------------------------------", "\n")

  # Inter Quartile Range
  iqr <- tapply(observations, treatments, IQR)
  print("Inter Quartile Range:")
  print(iqr)
  cat("\n", "-------------------------------------------------------------------", "\n")

  # Variance
  variance <- tapply(observations, treatments, var)
  print("Variance:")
  print(variance)
  cat("\n", "-------------------------------------------------------------------", "\n")

  # Standard Deviation
  standdev <- tapply(observations, treatments, sd)
  print("Standard Deviation:")
  print(standdev)
  cat("\n", "-------------------------------------------------------------------", "\n")

  # Check normal
  print("IQR/SD:")
  print(iqr/standdev)
  cat("\n", "-------------------------------------------------------------------", "\n")

  # Boxplot to see everything
  boxplot(observations ~ treatments, main = "For Internal Use Only")

  # If there's only two treatments
  if (nlevels(treatments)== 2){
    welch <- t.test(observations~treatments)
    pvalWelch <- welch$p.value
    print(welch)

    # Null Hypothesis check
    if (pvalWelch > confidenceInterval){
      cat(sprintf("p-value (%s) > (%s) confidence interval",
                  pvalWelch, confidenceInterval))
      cat("\n", "CANNOT REJECT null hypothesis")
    } else if (pval < confidenceInterval){
      cat(sprintf("p-value (%s) < (%s) confidence interval",
                  pvalWelch, confidenceInterval))
      cat("\n","REJECT null hypothesis")
    }

  } else {

  # Checking for Homogeniety of Variance with Levene Test
  levy <- levene.test(observations, treatments)
  pvalLevy <- levy$p.value
  print("Levene Test:")
  print(levy)

  # If Levene test p value is less than confidence interval,
  # then assumptions from anova not met and therefor inapplicaple
  if (levy$p.value < confidenceInterval){
    cat(sprintf("Levene Test p-value (%f) < (%s) confidence interval",
                pvalLevy, confidenceInterval))
    cat("\n", "REJECT null hypothesis of homoscedasticity of variance")
    cat("\n", "Variances significantly different across treatments")
    cat("\n", "therefore assumptions for ANOVA not met")
    cat("\n", "-------------------------------------------------------------------", "\n")

  } else { # Assuming levene test doesnt disqualify our data, we proceed to ANOVA
    cat(sprintf("Levene Test p-value (%f) > (%s) confidence interval",
                pvalLevy, confidenceInterval))
    cat("\n", "CANNOT REJECT null hypothesis of homoscedasticity of variance")
    cat("\n", "-------------------------------------------------------------------", "\n")

    # Checking to see if means across treatments are the same
    anov <- aov(observations ~ treatments)
    AnovaTable <- anova(anov)
    pvalAnova <- AnovaTable$`Pr(>F)`[1]
    print(AnovaTable)

    # If ANOVA p value is less than confidence interval,
    if (pvalAnova < confidenceInterval){
      cat(sprintf("ANOVA p-value (%f) < (%s) confidence interval",
                  pvalAnova, confidenceInterval))
      cat("\n", "REJECT null hypothesis")
      cat("\n", "There is at least one set of different sample means")
      cat("\n", "-------------------------------------------------------------------", "\n")

      # run Tukey table to see which means are different
      tuke <- TukeyHSD(anov)
      print(tuke)
      par(ask=TRUE)
      plot(tuke)
    } else {
      cat(sprintf("ANOVA p-value (%f) > (%s) confidence interval",
                  pvalAnova, confidenceInterval))
      print("CANNOT REJECT null hypothesis")
      print("Means are not significantly different across treatments")
    }
   }
  }
 }
}
