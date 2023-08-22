

##create a function to add effect size to the table1

# Load necessary libraries
library(effectsize)
library(vcd)

# Create a function to add effect size to table1 package results
effect_size_table1 <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  
  if (is.numeric(y)) {
    # For numeric variables, check if variances differ between the groups
    levene_test <- car::leveneTest(y, g)
    p_val_levene <- levene_test[["Pr(>F)"]][[1]]
    var_equal <- p_val_levene > 0.05
    
    if (var_equal) {
      # Use Cohen's d if variances do not differ between the groups
      es <- cohens_d(y ~ g)$Cohens_d
      test_name <- "Cohen's d"
      interp <- ifelse(abs(es) < 0.2, "Neglig.", 
                       ifelse(abs(es) < 0.5, "Small", 
                              ifelse(abs(es) < 0.8, "Medium", "Large")))
    } else {
      # Use Glass's delta if variances differ between the groups
      es <- glass_delta(y ~ g)$Glass_delta
      test_name <- "Glass' Delta"
      interp <- ifelse(abs(es) < 0.2, "Neglig.", 
                       ifelse(abs(es) < 0.5, "Small", 
                              ifelse(abs(es) < 0.8, "Medium", "Large")))
    }
  } else {
    # For categorical variables, use Cramer's V
    es <- cramers_v(table(y, g))$Cramers_v
    dF <- min(dim(table(y, g))[1]-1, dim(table(y, g))[2]-1)
    test_name <- paste0("Cramer's V - dF (", dF, ")") 
    interp <- ifelse(abs(es) < 0.1, "Neglig.", 
                     ifelse(abs(es) < 0.3, "Small", 
                            ifelse(abs(es) < 0.5, "Medium", "Large")))
  }
  

  # Format the effect size and include the name of the test applied
  result <- paste0(format(round(es, 3), nsmall = 3), " (", test_name, ") ", interp)
  # The initial empty string places the output on the line below the variable label.
  c("", result)
}


#how to use it : 
#my_table1 <- table1::table1(~  predictor1 + predictor2 + predictor3 | Outcome, data = data, overall=F, extra.col=list(`P-value`=pvalue, `Effect size`=effect_size_table1))

