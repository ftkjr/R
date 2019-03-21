# RBD(Observations, treatment, block, confidenceInterval)
# Frederick T. Kaesmann Jr.  3/20/19

RBD <- function(observations, treatments, block, confidenceInterval, LeveneBlock=FALSE){
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
  library(SummaryPack)

  #Checking inputs
  dataset <- as.data.frame(cbind(treatments,observations,block))
  dataset$observations <- as.numeric(observations)
  dataset$treatments <- as.factor(treatments)
  dataset$block <- as.factor(block)


  # Condfidence interval check
  if (missing(confidenceInterval) || !missing(confidenceInterval)){
    confidenceInterval <- checkConfidence(confidenceInterval)
  }  # That may not have been the prettiest way to do that
  cat("Your confidence interval is:", "\n",
      sprintf("%s, (%s percent)", confidenceInterval, confidenceInterval*100))
  cat("\n",
      "-------------------------------------------------------------------", "\n")


  # Basic Number Summary
  cat("\n", "Summary for all observations", "\n")
  sumObs <- summary(observations)
  print(sumObs)
  cat( "-------------------------------------------------------------------", "\n")
  sumObsbyTreat <- tapply(observations, treatments, summary)
  print(sumObsbyTreat)
  cat("\n", "-------------------------------------------------------------------",
      "\n")

  # Levene test for homoscedasticity of variance OF TREATMENTS
  levyTreat <- levene.test(observations, treatments)
  pvalLevyTreat <- levyTreat$p.value
  print("Levene Test by Treatment:")
  print(levyTreat)
  cat("\n", "-------------------------------------------------------------------",
      "\n")


  # If LeveneBlock==TRUE run levene test on observations by block
  if (LeveneBlock == TRUE){
    levyBlock <- levene.test(observations, block)
    pvalLevyBlock <- levyBlock$p.value
    cat("Levene Test by block because you asked so nicely")
    print(levyBlock)
    if (pvalLevyBlock < confidenceInterval){
      cat("Reject Null Hypothesis in favor of alternative", "\n")
      cat("observations by block heteroscedastic")
    }
  }

  # If Levene test p value is less than confidence interval,
  # then assumptions from anova not met and therefor inapplicaple
  if (levyTreat$p.value < confidenceInterval){
    cat(sprintf("Levene Test p-value (%f) < (%s) confidence interval",
                pvalLevyTreat, confidenceInterval))
    cat("\n", "REJECT null hypothesis of homoscedasticity of variance")
    cat("\n", "Variances significantly different across treatments")
    cat("\n", "therefore assumptions for ANOVA not met")
    cat("\n", "-------------------------------------------------------------------",
        "\n")

  } else { # Assuming levene test doesnt disqualify our data, we proceed to ANOVA
    cat(sprintf("Levene Test p-value (%f) > (%s) confidence interval",
                pvalLevyTreat, confidenceInterval))
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
  plot(tukeTable)
  }

  # Plot it
  par(ask=TRUE)
  boxplot(observations~treatments, xlab = "observations~treatments", main = "For Internal Use Only")
  gbp <- ggboxplot(dataset, x = "block", y = "observations", color = "treatments", palette = "npc",
                   title = "For Internal Use only")
  print(gbp) # Print multicolored boxplot

  }#closeANOVA
}#close function
# Fin
