# CheckNormality(observation, treatment)
# Fred Kaesmann Jr
# 2019 - 03 - 04

CheckNormality <- function(observation, treatment ){

  # Let's make sure it's the right kind of variable
  observation <- as.numeric(observation)
  
  # If there's no treatment variable then run this:
  if (missing(treatment)){
  
  
  # If treatment not entered, run single dataset
  print("Checking for Normality of single dataset:")
    
  # Obligatory histogram
  hist(observation, main = "For Internal Use Only")
  
  # Boxplot
  boxplot(observation, horizontal = TRUE, main = "For Internal Use Only")
    
  # The meat and potatoes
  
  iqr <- c("IQR:", IQR(observation))
  standdev <- c("Stand Dev:", sd(observation))
  
  # iqr/sd should be approx equal to 1.34 for normal data
  normalish <- c("IQR/Sigma:", IQR(observation)/sd(observation))
  
  # Combine them into a data frame 
  numberset <- as.data.frame(rbind(iqr,standdev,normalish))
  
  #numberset <- rownames(numberset, do.NULL = TRUE)
  
  print(numberset, row.names = F)
  
  } else {  # If there is a treatment variable run this:
    
    # Treatments need to be factors
    treatment <- as.factor(treatment)
    
    # tapply to calculate for each treatment
    iqr <- c("IQR:", tapply(observation, treatment, IQR))
    standdev <- c("Stand Dev:", tapply(observation, treatment, sd))
    
    # They should be floating around 1.34 if the data is normal
    normalish <- c("IQR/Sigma:", tapply(observation, treatment, IQR)/tapply(observation, treatment, sd))
    
    # Comine them into one data frame
    numberset <- as.data.frame(rbind(iqr, standdev, normalish))
    
    # Print that ish
    print(numberset, row.names = F)
    
    # Plot them all together in a boxplot 
    boxplot(observation~treatment, main = "For Internal Use Only")
  }
}
# Fin