# RBD(Observations, treatment, block, confidenceInterval)
# Frederick T. Kaesmann Jr.  3/20/19

RBD <- function(observations, treatments, block, confidenceInterval){
  # Performs a Randomized Block Design
  # Args: observations
  #       treatment
  #       block: "Something you can't control and 
  #                  something you usually aren't interested in."
  #                                             - Prof Ray Mugno
  #       confidenceInterval: self explanatory, assumed to be .05 (95%) if unentered
  #
  # Returns:
  #         5 number summary for the set of observations
  #         5 number summary for observations by treatments
  #         Means by treatment
  #         Means by block
  #         Levene test for observations by treatments
  #         Two-Way ANOVA
  #         TukeyHSD if appropriate
  #         Boxplot of observations~treatments
  #         ggboxplot(dataset, x = "block", y = "observations", color = "treatments")
  #
  
  #if(require("ggpubr")) install.packages("ggpubr")
  
  # Add requisite libraries
  library(ggpubr)
  library(lawstat)
  library(MASS)
  
  #Checking inputs
  dataset <- as.data.frame(cbind(treatments,observations,block))
  dataset$observations <- as.numeric(observations)
  dataset$treatments <- as.factor(treatments)
  dataset$block <- as.factor(block)
  
  
  # Condfidence interval check
  if (missing(confidenceInterval)){
    confidenceInterval <- .05
    cat(sprintf("Alpha = %s (%s pct) \n", confidenceInterval, (1-confidenceInterval)*100))
    cat("\n", "-------------------------------------------------------------------", 
        "\n")
  } else if (confidenceInterval > 1 & confidenceInterval < 100){
    # (if they input something like 95 instead of .05)
    confidenceInterval <- 1 - (confidenceInterval/100)
    cat(sprintf("Alpha = %s\n", confidenceInterval))
    cat("\n", "-------------------------------------------------------------------", 
        "\n")
  } else if (confidenceInterval >= 100){ # Erroneous Confidence Interval
    stop("Confidence Interval Error")
  } else if (confidenceInterval <= 0) {
    stop("Confidence Interval Error")
  }
  
  # Basic Number Summary
  cat("Summary for all observations")
  print(summary(observations))
  cat( "-------------------------------------------------------------------", "\n")
  print(tapply(observations, treatments, summary))
  
  # View means
  treatmentsMean <- tapply(observations, treatments, mean)
  blockMean <- tapply(observations, block, mean)
  cat("Means by Treatments", "\n")
  print(treatmentsMean)
  cat("\n", "-------------------------------------------------------------------")
  cat("\n", "Means by Block", "\n")
  print(blockMean)
  cat("-------------------------------------------------------------------","\n")
  
  # Levene test for homoscedasticity of variance
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
    cat("\n", "-------------------------------------------------------------------", 
        "\n")
    
  } else { # Assuming levene test doesnt disqualify our data, we proceed to ANOVA
    cat(sprintf("Levene Test p-value (%f) > (%s) confidence interval",
                pvalLevy, confidenceInterval))
    cat("\n", "CANNOT REJECT null hypothesis of homoscedasticity of variance")
    cat("\n", "-------------------------------------------------------------------", 
        "\n")
    
  # ANOVA Table  
  anov <-lm(observations ~ treatments + block)
  anovaTable <- anova(anov)
  pvalTreatments <- anovaTable$`Pr(>F)`[1]
  pvalBlock <- anovaTable$`Pr(>F)`[2]
  print(anovaTable)
  cat("\n", "-------------------------------------------------------------------", 
      "\n")
  
  # Tukey table if applicable
  if ((pvalTreatments < confidenceInterval) || (pvalBlock < confidenceInterval) ){
    cat("P value less than confidence interval", "\n", 
        "Tukey table provided to evaluate which means are different")
    cat("\n", "-------------------------------------------------------------------", 
        "\n")
    stuff <- aov(observations~treatments+block)
    tukeTable <- TukeyHSD(stuff)
  print(tukeTable)
  }
  
  # Plot it
  par(ask=TRUE)
  boxplot(observations~treatments, xlab = "observations~treatments", main = "For Internal Use Only")
  gbp <- ggboxplot(dataset, x = "block", y = "observations", color = "treatments", palette = "npc",
                   title = "For Internal Use only") 
  print(gbp)
 }
}
# Fin