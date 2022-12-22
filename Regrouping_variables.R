# C) Regrouping certan variables before we perform Regression analysis----

library(dplyr)

(PWIDHIVtestingdata$lastHIVtest <-factor(PWIDHIVtestingdata$lastHIVtest, levels = c(1,2,3,4), 
                                         labels = c("Never tested", "Within last month", "Within 1-2 years ago", "More than two years ago")))

df <- data.frame(PWIDHIVtestingdata)

df <- df %>% 
  mutate(lastHIVtest2 = case_when(
    lastHIVtest == "Never tested" ~ 0,
    lastHIVtest == "Within last month" ~ 1,
    lastHIVtest == "Within 1-2 years ago" ~ 1,
    lastHIVtest == "More than two years ago" ~ 1
  ))

PWIDHIVtestingdata$lastHIVtest2 = df$lastHIVtest2


# Making groups for age:
library(dplyr)


df <- df %>% mutate(agegroup = case_when(
  age >= 45 ~ '4',
  age >= 35  & age <= 44 ~ '3',
  age >= 25  & age <= 34 ~ '2',
  age >= 18 & age <= 24 ~ '1')) 

PWIDHIVtestingdata$agegroup = df$agegroup
