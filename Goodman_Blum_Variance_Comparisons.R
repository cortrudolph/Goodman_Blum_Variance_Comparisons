library(tidyverse)

## Example Data
data<-tibble::tribble(
  ~Variable, ~Attrition_Category,
  5L,          "Complete",
  5L,        "Incomplete",
  5L,          "Complete",
  6L,        "Incomplete",
  7L,          "Complete",
  5L,        "Incomplete",
  1L,          "Complete",
  4L,        "Incomplete",
  1L,          "Complete",
  7L,        "Incomplete",
  3L,          "Complete",
  1L,        "Incomplete",
  7L,          "Complete",
  3L,        "Incomplete",
  7L,          "Complete",
  4L,        "Incomplete",
  6L,          "Complete",
  7L,        "Incomplete",
  4L,          "Complete",
  7L,        "Incomplete",
  7L,          "Complete",
  3L,        "Incomplete"
)


## This is the variance test "code"

# This first block just extracts the variances for "complete" vs. "incompletes" into a table called "TEMP"
TEMP<-data %>%
  group_by(Attrition_Category) %>%
  select(Variable) %>%
  summarize(variance = var(Variable))
t(TEMP) # Table of variances

TEMP$variance[1] #"Pull" variance for Complete Cases (just to inspect)
TEMP$variance[2] #"Pull" variance for Incomplete Cases (just to inspect)

TEMP2<-data$Attrition_Category %>% table() # Determine N in complete category

n <- TEMP2[1] # N in attrition group
v <- n - 1
chi2 <- (v*TEMP$variance[2])/TEMP$variance[1] # Chi-Square of difference between variances
chi2

z <- (chi2-v)/sqrt(2*v) # Convert Chi2 to z-score for NHST testing
z 

2*(1-pnorm(z)) # What is the area under the curve associated with "z"? 

# Critical value for Z with "K" pair-wise comparisons
qnorm(1-.05/2) # Single Test - i.e., p < .05 no adjustments for familywise error
# 1.48 < 1.95 so "fail to reject the null" (i.e., variances are not sig. dif. @ p < .05)

# Multiple Comparisons Z criticials 
#qnorm(1-0.007/2) # e.g., For seven tests - i.e., p < .007 adjusted for familywise error (e.g., Bonferroni) 

