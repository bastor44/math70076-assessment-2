##### Function to compare user to desired school

require(tidyverse)
require(ggplot2)

## plot SAT/ACT scores and add line for user 
plot_test_score <- function(data=institutions, school, 
                            test=c("SAT_Math", "SAT_Reading",  "SAT_Writing", 
                                   "SAT_Average", "ACT_Composite", "ACT_Math", 
                                   "ACT_English", "ACT_Writing"), 
                            user_score=NULL) {
  
  # filter data to only look @ school of interest 
  school_data <- data[, data$INSTNM==school]
  
  # filter data to only look @ test of interest 
  if (test=="SAT_Math") {
    school_data <- school_data |>
      select(colnames(school_data), "SATMT")
    x <- seq(200, 800, 1)
  }
  if (test=="SAT_Reading") {
    school_data <- school_data |>
      select(colnames(school_data), "SATVR")
    x <- seq(200, 800, 1)
  }
  if (test=="SAT_Writing") {
    school_data <- school_data |>
      select(colnames(school_data), "SATWR")
    x <- seq(200, 800, 1)
  } 
  if (test=="ACT_Composite") {
    school_data <- school_data |>
      select(colnames(school_data), "ACTCM")
    x <- seq(1, 36, 1)
  }
  if (test=="ACT_MATH") {
    school_data <- school_data |>
      select(colnames(school_data), "ACTMT")
    x <- seq(1, 36, 1)
  }
  if (test=="ACT_English") {
    school_data <- school_data |>
      select(colnames(school_data), "ACTEN")
    x <- seq(1, 36, 1)
  }
  if (test=="ACT_Writing") {
    school_data <- school_data |>
      select(colnames(school_data), "ACTWR")
    x <- seq(2, 12, 1)
  }
  
  if (any(is.na(school_data))){
    return("Requested test information is unavailable.")
  }
  
  # extract mean, median, and quartiles 
  avg <- school_data[ , str_detect(colnames(school_data), "MID")]
  q50 <- school_data[ , str_detect(colnames(school_data), "50")]
  q25 <- school_data[ , str_detect(colnames(school_data), "25")]
  q75 <- school_data[ , str_detect(colnames(school_data), "75")]
  
  # make distribution - assume Normal distributions for all
  #   not necesarily true 
  sd <- (q75 - q25)/2
  y <- dnorm(x, avg, sd)
  dist_data <- data.frame(x=x, y=y)
  
  # plot distribution
  scores_plot <- ggplot(dist_data, aes(x=x, y=y)) +
    geom_line(color="cornflowerblue") +
    geom_area(fill="cornflowerblue", alpha=0.5)+
    theme_minimal() + 
    labs(x="Test score", y="Density",
         title=paste(test, "score distribution at \n", school)) +
    geom_vline(xintercept=avg, color="grey70", linetype="dashed", linewidth=1) + 
    geom_vline(xintercept=c(q25, q75), color="grey90", linetype="dashed", linewidth=1)
  
  if (!is.null(user_score)) {
    scores_plot <- scores_plot +
      geom_vline(xintercept=user_score, color="firebrick", linewidth=1)
  }
  
  # user quantile
  user_quantile <- qnorm(user_score, avg, sd)
  
  return(list(plot=scores_plot, 
              quantile=user_quantile))
}


## 