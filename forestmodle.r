setwd("C:\\Users\\guoha\\Desktop\\Rdemo")
#install.packages("forestmodel")
library(forestmodel)
library("survival")
library(dplyr,warn.conflicts = FALSE)
data <- read.csv("dataset-42025.csv",sep = ",",header = T)
str(data)
mycolon <- data %>% 
  transmute (time,
                             status,
                             Age = age,
                             Sex = factor(sex,levels = c(0,1),
                                          labels = c("Female","Male")),
                             Obstruct = factor(data$obstruct),
                             Differ = factor(data$differ),
                             Extent = factor(data$extent)
                             )
head(mycolon)
str(mycolon)
coxphmodel <- coxph(Surv(time,status)~.,mycolon)
coxphmodel1 <- coxph(Surv(time,status) ~ Age+Sex+Obstruct, mycolon) 

panels <- list(
  list(width = 0.03),
  list(width = 0.1, display = ~variable, fontface = "bold", heading = "Variable"),
  list(width = 0.1, display = ~level),
  list(width = 0.05, display = ~n, hjust = 1, heading = "N"),
  list(width = 0.05, display = ~n_events, width = 0.05, hjust = 1, heading = "Events"),
  list(
    width = 0.05,
    display = ~ replace(sprintf("%0.1f", person_time / 365.25), is.na(person_time), ""),
    heading = "Person-\nYears", hjust = 1
  ),
  list(width = 0.03, item = "vline", hjust = 0.5),
  list(
    width = 0.55, item = "forest", hjust = 0.5, heading = "Hazard ratio", linetype = "dashed",
    line_x = 0
  ),
  list(width = 0.03, item = "vline", hjust = 0.5),
  list(width = 0.12, display = ~ ifelse(reference, "Reference", sprintf(
    "%0.2f (%0.2f, %0.2f)",
    trans(estimate), trans(conf.low), trans(conf.high)
  )), display_na = NA),
  list(
    width = 0.05,
    display = ~ ifelse(reference, "", format.pval(p.value, digits = 1, eps = 0.001)),
    display_na = NA, hjust = 1, heading = "p"
  ),
  list(width = 0.03)
) 

forest_model(coxphmodel1,panels = panels)
forest_model(coxphmodel)
forest_model(coxphmodel,factor_separate_line = T,panels = panels)
forest_model(coxphmodel1,factor_separate_line = T)
