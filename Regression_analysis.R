# Making models 
# by age groups

model1<-glm(lastHIVtest2~agegroup,data=PWIDHIVtestingdata, family="binomial")
summary(model1)
exp(cbind(OR = coef(model1), confint(model1)))


# city
(PWIDHIVtestingdata$city <-factor(PWIDHIVtestingdata$city, levels = c(1,2,3,4,5,6,7,8), 
                                  labels = c("Kabul", "Herat","Mazar_I_Sharif", "Jalalabad", "Kunduz", "Faizabad", "Kandahar", "Zaranj")))
df <- data.frame(PWIDHIVtestingdata)

df <- df %>% mutate(city_new = case_when(
  city == "Kabul" ~ 1,
  city == "Herat" ~ 2,
  city == "Mazar-I-Sharif" ~ 3,
  city == "Jalalabad" ~ 4,
  city == "Kunduz" ~ 5,
  city == "Faizabad" ~ 6,
  city == "Kandahar" ~ 7,
  city == "Zaranj" ~ 8)) 

PWIDHIVtestingdata$city_new = df$city_new


# D) Making models - logistic regression----
model_city <- glm(lastHIVtest2~city, data = PWIDHIVtestingdata, family = "binomial")
summary(model_city)
exp(cbind(OR = coef(model_city), confint(model_city)))

# models by opium

model2 <- glm(lastHIVtest2~opium, data = PWIDHIVtestingdata, family = "binomial")
summary(model2)
exp(cbind(OR = coef(model2), confint(model2)))


# multiple logistic

model_adj <- glm(lastHIVtest2~agegroup+city+opium, data = PWIDHIVtestingdata, family = "binomial")
summary(model_adj)
exp(cbind(OR = coef(model_adj), confint(model_adj)))
