
# A) Loading dataset----
PWIDHIVtestingdata <- read_excel("Desktop/Fall-Sem/BIOSTAT200/Final Exam Materials-20221125/PWIDHIVtestingdata.xlsx")

# Making description table1

# a) for cities----
summary(PWIDHIVtestingdata)

(PWIDHIVtestingdata$city <-factor(PWIDHIVtestingdata$city, levels = c(1,2,3,4,5,6,7,8), 
                                   labels = c("Kabul", "Herat","Mazar_I_Sharif", "Jalalabad", "Kunduz", "Faizabad", "Kandahar", "Zaranj")))

library(epiDisplay)
tab1(PWIDHIVtestingdata$city, sort.group = "increasing", cum.percent = TRUE)

# b) for languages----
(PWIDHIVtestingdata$language <-factor(PWIDHIVtestingdata$language, levels = c(1,2,3), 
                                  labels = c("Pashtoo", "Dari","Uzbek")))

tab1(PWIDHIVtestingdata$language, sort.group = "increasing", cum.percent = TRUE)


#c) heroine use----
(PWIDHIVtestingdata$heroin <-factor(PWIDHIVtestingdata$heroin, levels = c(0,1), 
                                      labels = c("No", "Yes")))

tab1(PWIDHIVtestingdata$heroin, sort.group = "increasing", cum.percent = TRUE)

#d) cocaine use----
(PWIDHIVtestingdata$cocaine <-factor(PWIDHIVtestingdata$cocaine, levels = c(0,1), 
                                    labels = c("No", "Yes")))

tab1(PWIDHIVtestingdata$cocaine, sort.group = "increasing", cum.percent = TRUE)

#e) Opium use----

(PWIDHIVtestingdata$opium <-factor(PWIDHIVtestingdata$opium, levels = c(0,1), 
                                     labels = c("No", "Yes")))

tab1(PWIDHIVtestingdata$opium, sort.group = "increasing", cum.percent = TRUE)


#f) Amphetamine use----

(PWIDHIVtestingdata$amphetamine <-factor(PWIDHIVtestingdata$amphetamine, levels = c(0,1), 
                                   labels = c("No", "Yes")))

tab1(PWIDHIVtestingdata$amphetamine, sort.group = "increasing", cum.percent = TRUE)


#g) Last HIV test----

(PWIDHIVtestingdata$lastHIVtest <-factor(PWIDHIVtestingdata$lastHIVtest, levels = c(1,2,3,4), 
                                         labels = c("Never tested", "Within last month", "Within 1-2 years ago", "More than two years ago")))

tab1(PWIDHIVtestingdata$lastHIVtest, sort.group = "increasing", cum.percent = TRUE)

#h) Current marital status----

(PWIDHIVtestingdata$curntmarriage <-factor(PWIDHIVtestingdata$curntmarriage, levels = c(1,2,3,4,5,6), 
                                         labels = c("Single", "Married and living with partner", "Married but not living with partner", 
                                                    "Not married but living with partner", "separated/ divorced", "widowed")))

tab1(PWIDHIVtestingdata$curntmarriage, sort.group = "increasing", cum.percent = TRUE)


#i) ever had anal/ oral sex with men ----
(PWIDHIVtestingdata$evermsm <-factor(PWIDHIVtestingdata$evermsm, levels = c(1,2), 
                                           labels = c("No", "Yes")))

tab1(PWIDHIVtestingdata$evermsm, sort.group = "increasing", cum.percent = TRUE)


# Grouping HIV tested and not tested in two groups----

library(dplyr)


(PWIDHIVtestingdata$lastHIVtest <-factor(PWIDHIVtestingdata$lastHIVtest, levels = c(1,2,3,4), 
                                         labels = c("Never tested", "Within last month", "Within 1-2 years ago", "More than two years ago")))



# Stratifying HIV tested and not tested into two groups instead of 4----
df <- data.frame(PWIDHIVtestingdata)

df <- df %>% 
  mutate(lastHIVtest2 = case_when(
    lastHIVtest == "Never tested" ~ 0,
    lastHIVtest == "Within last month" ~ 1,
    lastHIVtest == "Within 1-2 years ago" ~ 1,
    lastHIVtest == "More than two years ago" ~ 1
  ))

PWIDHIVtestingdata$lastHIVtest2 = df$lastHIVtest2

# Citywise stratification of opium = yes----
opium_1 <- subset(PWIDHIVtestingdata, opium == 1 & city == 1, opium)
opium_1

opium_2 <- subset(PWIDHIVtestingdata, opium == 1 & city == 2, opium)
opium_2

opium_3 <- subset(PWIDHIVtestingdata, opium == 1 & city == 3, opium)
opium_3

opium_4 <- subset(PWIDHIVtestingdata, opium == 1 & city == 4, opium)
opium_4

opium_5 <- subset(PWIDHIVtestingdata, opium == 1 & city == 5, opium)
opium_5

opium_6 <- subset(PWIDHIVtestingdata, opium == 1 & city == 6, opium)
opium_6

opium_7 <- subset(PWIDHIVtestingdata, opium == 1 & city == 7, opium)
opium_7

opium_8 <- subset(PWIDHIVtestingdata, opium == 1 & city == 8, opium)
opium_8

# Citywise stratification of opium = NO----
opium_1_no <- subset(PWIDHIVtestingdata, opium == 0 & city == 1, opium)
opium_1_no

opium_2_no <- subset(PWIDHIVtestingdata, opium == 0 & city == 2, opium)
opium_2_no

opium_3_no <- subset(PWIDHIVtestingdata, opium == 0 & city == 3, opium)
opium_3_no

opium_4_no <- subset(PWIDHIVtestingdata, opium == 0 & city == 4, opium)
opium_4_no

opium_5_no <- subset(PWIDHIVtestingdata, opium == 0 & city == 5, opium)
opium_5_no

opium_6_no <- subset(PWIDHIVtestingdata, opium == 0 & city == 6, opium)
opium_6_no

opium_7_no <- subset(PWIDHIVtestingdata, opium == 0 & city == 7, opium)
opium_7_no

opium_8_no <- subset(PWIDHIVtestingdata, opium == 0 & city == 8, opium)
opium_8_no


# City wise distribution for heroin, NO----

heroin_1_no <- subset(PWIDHIVtestingdata, heroin == 0 & city == 1, heroin)
heroin_1_no

heroin_2_no <- subset(PWIDHIVtestingdata, heroin == 0 & city == 2, heroin)
heroin_2_no

heroin_3_no <- subset(PWIDHIVtestingdata, heroin == 0 & city == 3, heroin)
heroin_3_no

heroin_4_no <- subset(PWIDHIVtestingdata, heroin == 0 & city == 4, heroin)
heroin_4_no

heroin_5_no <- subset(PWIDHIVtestingdata, heroin == 0 & city == 5, heroin)
heroin_5_no

heroin_6_no <- subset(PWIDHIVtestingdata, heroin == 0 & city == 6, heroin)
heroin_6_no

heroin_7_no <- subset(PWIDHIVtestingdata, heroin == 0 & city == 7, heroin)
heroin_7_no

heroin_8_no <- subset(PWIDHIVtestingdata, heroin == 0 & city == 8, heroin)
heroin_8_no


# City wise distribution for heroine, YES----
heroin_1_yes <- subset(PWIDHIVtestingdata, heroin == 1 & city == 1, heroin)
heroin_1_yes

heroin_2_yes <- subset(PWIDHIVtestingdata, heroin == 1 & city == 2, heroin)
heroin_2_yes

heroin_3_yes <- subset(PWIDHIVtestingdata, heroin == 1 & city == 3, heroin)
heroin_3_yes

heroin_4_yes <- subset(PWIDHIVtestingdata, heroin == 1 & city == 4, heroin)
heroin_4_yes

heroin_5_yes <- subset(PWIDHIVtestingdata, heroin == 1 & city == 5, heroin)
heroin_5_yes

heroin_6_yes <- subset(PWIDHIVtestingdata, heroin == 1 & city == 6, heroin)
heroin_6_yes

heroin_7_yes <- subset(PWIDHIVtestingdata, heroin == 1 & city == 7, heroin)
heroin_7_yes

heroin_8_yes <- subset(PWIDHIVtestingdata, heroin == 1 & city == 8, heroin)
heroin_8_yes


# City wise distribution for Cocaine, No----


cocaine_1_no <- subset(PWIDHIVtestingdata, cocaine == 0 & city == 1, cocaine)
cocaine_1_no

cocaine_2_no <- subset(PWIDHIVtestingdata, cocaine == 0 & city == 2, cocaine)
cocaine_2_no

cocaine_3_no <- subset(PWIDHIVtestingdata, cocaine == 0 & city == 3, cocaine)
cocaine_3_no

cocaine_4_no <- subset(PWIDHIVtestingdata, cocaine == 0 & city == 4, cocaine)
cocaine_4_no

cocaine_5_no <- subset(PWIDHIVtestingdata, cocaine == 0 & city == 5, cocaine)
cocaine_5_no

cocaine_6_no <- subset(PWIDHIVtestingdata, cocaine == 0 & city == 6, cocaine)
cocaine_6_no

cocaine_7_no <- subset(PWIDHIVtestingdata, cocaine == 0 & city == 7, cocaine)
cocaine_7_no

cocaine_8_no <- subset(PWIDHIVtestingdata, cocaine == 0 & city == 8, cocaine)
cocaine_8_no

## City wise distribution for Cocaine, Yes----
cocaine_1_yes <- subset(PWIDHIVtestingdata, cocaine == 1 & city == 1, cocaine)
cocaine_1_yes

cocaine_2_yes <- subset(PWIDHIVtestingdata, cocaine == 1 & city == 2, cocaine)
cocaine_2_yes

cocaine_3_yes <- subset(PWIDHIVtestingdata, cocaine == 1 & city == 3, cocaine)
cocaine_3_yes

cocaine_4_yes <- subset(PWIDHIVtestingdata, cocaine == 1 & city == 4, cocaine)
cocaine_4_yes

cocaine_5_yes <- subset(PWIDHIVtestingdata, cocaine == 1 & city == 5, cocaine)
cocaine_5_yes

cocaine_6_yes <- subset(PWIDHIVtestingdata, cocaine == 1 & city == 6, cocaine)
cocaine_6_yes

cocaine_7_yes <- subset(PWIDHIVtestingdata, cocaine == 1 & city == 7, cocaine)
cocaine_7_yes

cocaine_8_yes <- subset(PWIDHIVtestingdata, cocaine == 1 & city == 8, cocaine)
cocaine_8_yes


# City wise distribution for Amphetamine, No----

amphetamine_1_no <- subset(PWIDHIVtestingdata, amphetamine == 0 & city == 1, amphetamine)
amphetamine_1_no

amphetamine_2_no <- subset(PWIDHIVtestingdata, amphetamine == 0 & city == 2, amphetamine)
amphetamine_2_no

amphetamine_3_no <- subset(PWIDHIVtestingdata, amphetamine == 0 & city == 3, amphetamine)
amphetamine_3_no

amphetamine_4_no <- subset(PWIDHIVtestingdata, amphetamine == 0 & city == 4, amphetamine)
amphetamine_4_no

amphetamine_5_no <- subset(PWIDHIVtestingdata, amphetamine == 0 & city == 5, amphetamine)
amphetamine_5_no

amphetamine_6_no <- subset(PWIDHIVtestingdata, amphetamine == 0 & city == 6, amphetamine)
amphetamine_6_no

amphetamine_7_no <- subset(PWIDHIVtestingdata, amphetamine == 0 & city == 7, amphetamine)
amphetamine_7_no

amphetamine_8_no <- subset(PWIDHIVtestingdata, amphetamine == 0 & city == 8, amphetamine)
amphetamine_8_no

# City wise distribution for Amphetamine, Yes----

amphetamine_1_yes <- subset(PWIDHIVtestingdata, amphetamine == 1 & city == 1, amphetamine)
amphetamine_1_yes

amphetamine_2_yes <- subset(PWIDHIVtestingdata, amphetamine == 1 & city == 2, amphetamine)
amphetamine_2_yes

amphetamine_3_yes <- subset(PWIDHIVtestingdata, amphetamine == 1 & city == 3, amphetamine)
amphetamine_3_yes

amphetamine_4_yes <- subset(PWIDHIVtestingdata, amphetamine == 1 & city == 4, amphetamine)
amphetamine_4_yes

amphetamine_5_yes <- subset(PWIDHIVtestingdata, amphetamine == 1 & city == 5, amphetamine)
amphetamine_5_yes

amphetamine_6_yes <- subset(PWIDHIVtestingdata, amphetamine == 1 & city == 6, amphetamine)
amphetamine_6_yes

amphetamine_7_yes <- subset(PWIDHIVtestingdata, amphetamine == 1 & city == 7, amphetamine)
amphetamine_7_yes

amphetamine_8_yes <- subset(PWIDHIVtestingdata, amphetamine == 1 & city == 8, amphetamine)
amphetamine_8_yes

# # City wise distribution for Ever had anal or oral sex with men, No----

evermsm_1_no <- subset(PWIDHIVtestingdata, evermsm == 1 & city == 1, evermsm)
evermsm_1_no

evermsm_2_no <- subset(PWIDHIVtestingdata, evermsm == 1 & city == 2, evermsm)
evermsm_2_no

evermsm_3_no <- subset(PWIDHIVtestingdata, evermsm == 1 & city == 3, evermsm)
evermsm_3_no

evermsm_4_no <- subset(PWIDHIVtestingdata, evermsm == 1 & city == 4, evermsm)
evermsm_4_no

evermsm_5_no <- subset(PWIDHIVtestingdata, evermsm == 1 & city == 5, evermsm)
evermsm_5_no

evermsm_6_no <- subset(PWIDHIVtestingdata, evermsm == 1 & city == 6, evermsm)
evermsm_6_no

evermsm_7_no <- subset(PWIDHIVtestingdata, evermsm == 1 & city == 7, evermsm)
evermsm_7_no

evermsm_8_no <- subset(PWIDHIVtestingdata, evermsm == 1 & city == 8, evermsm)
evermsm_8_no





# # City wise distribution for Ever had anal or oral sex with men, YES----

evermsm_1_yes <- subset(PWIDHIVtestingdata, evermsm == 2 & city == 1, evermsm)
evermsm_1_yes

evermsm_2_yes <- subset(PWIDHIVtestingdata, evermsm == 2 & city == 2, evermsm)
evermsm_2_yes

evermsm_3_yes <- subset(PWIDHIVtestingdata, evermsm == 2 & city == 3, evermsm)
evermsm_3_yes

evermsm_4_yes <- subset(PWIDHIVtestingdata, evermsm == 2 & city == 4, evermsm)
evermsm_4_yes

evermsm_5_yes <- subset(PWIDHIVtestingdata, evermsm == 2 & city == 5, evermsm)
evermsm_5_yes

evermsm_6_yes <- subset(PWIDHIVtestingdata, evermsm == 2 & city == 6, evermsm)
evermsm_6_yes

evermsm_7_yes <- subset(PWIDHIVtestingdata, evermsm == 2 & city == 7, evermsm)
evermsm_7_yes

evermsm_8_yes <- subset(PWIDHIVtestingdata, evermsm == 2 & city == 8, evermsm)
evermsm_8_yes




# City wise distribution for language, PASHTOO, 
# city 1----
lang_pashtoo_1 <- subset(PWIDHIVtestingdata, language == 1 & city == 1, language)
lang_pashtoo_1

# # City wise distribution for language , Dari----
lang_dari_1 <- subset(PWIDHIVtestingdata, language == 2 & city == 1, language)
lang_dari_1

# # City wise distribution for language , Uzbek----
lang_uzbek_1 <- subset(PWIDHIVtestingdata, language == 3 & city == 1, language)
lang_uzbek_1


# city 2----
lang_pashtoo_2 <- subset(PWIDHIVtestingdata, language == 1 & city == 2, language)
lang_pashtoo_2

# # City wise distribution for language , Dari----
lang_dari_2 <- subset(PWIDHIVtestingdata, language == 2 & city == 2, language)
lang_dari_2

# # City wise distribution for language , Uzbek----
lang_uzbek_2 <- subset(PWIDHIVtestingdata, language == 3 & city == 2, language)
lang_uzbek_2


# city 3----
lang_pashtoo_3 <- subset(PWIDHIVtestingdata, language == 1 & city == 3, language)
lang_pashtoo_3

# # City wise distribution for language , Dari----
lang_dari_3 <- subset(PWIDHIVtestingdata, language == 2 & city == 3, language)
lang_dari_3

# # City wise distribution for language , Uzbek----
lang_uzbek_3 <- subset(PWIDHIVtestingdata, language == 3 & city == 3, language)
lang_uzbek_3


# city 4----
lang_pashtoo_4 <- subset(PWIDHIVtestingdata, language == 1 & city == 4, language)
lang_pashtoo_4

# # City wise distribution for language , Dari----
lang_dari_4 <- subset(PWIDHIVtestingdata, language == 2 & city == 4, language)
lang_dari_4

# # City wise distribution for language , Uzbek----
lang_uzbek_4 <- subset(PWIDHIVtestingdata, language == 3 & city == 4, language)
lang_uzbek_4



# city 5----
lang_pashtoo_5 <- subset(PWIDHIVtestingdata, language == 1 & city == 5, language)
lang_pashtoo_5

# # City wise distribution for language , Dari----
lang_dari_5 <- subset(PWIDHIVtestingdata, language == 2 & city == 5, language)
lang_dari_5

# # City wise distribution for language , Uzbek----
lang_uzbek_5 <- subset(PWIDHIVtestingdata, language == 3 & city == 5, language)
lang_uzbek_5


# city 6----
lang_pashtoo_6 <- subset(PWIDHIVtestingdata, language == 1 & city == 6, language)
lang_pashtoo_6

# # City wise distribution for language , Dari----
lang_dari_6 <- subset(PWIDHIVtestingdata, language == 2 & city == 6, language)
lang_dari_6

# # City wise distribution for language , Uzbek----
lang_uzbek_6 <- subset(PWIDHIVtestingdata, language == 3 & city == 6, language)
lang_uzbek_6


# city 7----
lang_pashtoo_7 <- subset(PWIDHIVtestingdata, language == 1 & city == 7, language)
lang_pashtoo_7

# # City wise distribution for language , Dari----
lang_dari_7 <- subset(PWIDHIVtestingdata, language == 2 & city == 7, language)
lang_dari_7

# # City wise distribution for language , Uzbek----
lang_uzbek_7 <- subset(PWIDHIVtestingdata, language == 3 & city == 7, language)
lang_uzbek_7


# city 8----
lang_pashtoo_8 <- subset(PWIDHIVtestingdata, language == 1 & city == 8, language)
lang_pashtoo_8

# # City wise distribution for language , Dari----
lang_dari_8 <- subset(PWIDHIVtestingdata, language == 2 & city == 8, language)
lang_dari_8

# # City wise distribution for language , Uzbek----
lang_uzbek_8 <- subset(PWIDHIVtestingdata, language == 3 & city == 8, language)
lang_uzbek_8


#City wise distribution for last HIV test, tested----


hiv_not_tested_1 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 1 & city == 1, lastHIVtest2)
hiv_not_tested_1

hiv_not_tested_2 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 1 & city == 2, lastHIVtest2)
hiv_not_tested_2

hiv_not_tested_3 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 1 & city == 3, lastHIVtest2)
hiv_not_tested_3

hiv_not_tested_4 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 1 & city == 4, lastHIVtest2)
hiv_not_tested_4

hiv_not_tested_5 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 1 & city == 5, lastHIVtest2)
hiv_not_tested_5

hiv_not_tested_6 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 1 & city == 6, lastHIVtest2)
hiv_not_tested_6

hiv_not_tested_7 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 1 & city == 7, lastHIVtest2)
hiv_not_tested_7

hiv_not_tested_8 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 1 & city == 8, lastHIVtest2)
hiv_not_tested_8





# City wise- not tested HIV----

hiv_tested_1 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 0 & city == 1, lastHIVtest2)
hiv_tested_1

hiv_tested_2 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 0 & city == 2, lastHIVtest2)
hiv_tested_2

hiv_tested_3 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 0 & city == 3, lastHIVtest2)
hiv_tested_3

hiv_tested_4 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 0 & city == 4, lastHIVtest2)
hiv_tested_4

hiv_tested_5 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 0 & city == 5, lastHIVtest2)
hiv_tested_5

hiv_tested_6 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 0 & city == 6, lastHIVtest2)
hiv_tested_6

hiv_tested_7 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 0 & city == 7, lastHIVtest2)
hiv_tested_7

hiv_tested_8 <- subset(PWIDHIVtestingdata, lastHIVtest2 == 0 & city == 8, lastHIVtest2)
hiv_tested_8

# City wise marital status----

# City- Single

single_1 <- subset(PWIDHIVtestingdata, curntmarriage == 1 & city == 1, curntmarriage)
single_1

single_2 <- subset(PWIDHIVtestingdata, curntmarriage == 1 & city == 2, curntmarriage)
single_2

single_3 <- subset(PWIDHIVtestingdata, curntmarriage == 1 & city == 3, curntmarriage)
single_3

single_4 <- subset(PWIDHIVtestingdata, curntmarriage == 1 & city == 4, curntmarriage)
single_4

single_5 <- subset(PWIDHIVtestingdata, curntmarriage == 1 & city == 5, curntmarriage)
single_5

single_6 <- subset(PWIDHIVtestingdata, curntmarriage == 1 & city == 6, curntmarriage)
single_6

single_7 <- subset(PWIDHIVtestingdata, curntmarriage == 1 & city == 7, curntmarriage)
single_7

single_8 <- subset(PWIDHIVtestingdata, curntmarriage == 1 & city == 8, curntmarriage)
single_8


# City- Married+ living with partner

married_n_partner_1 <- subset(PWIDHIVtestingdata, curntmarriage == 2 & city == 1, curntmarriage)
married_n_partner_1

married_n_partner_2 <- subset(PWIDHIVtestingdata, curntmarriage == 2 & city == 2, curntmarriage)
married_n_partner_2

married_n_partner_3 <- subset(PWIDHIVtestingdata, curntmarriage == 2 & city == 3, curntmarriage)
married_n_partner_3

married_n_partner_4 <- subset(PWIDHIVtestingdata, curntmarriage == 2 & city == 4, curntmarriage)
married_n_partner_4

married_n_partner_5 <- subset(PWIDHIVtestingdata, curntmarriage == 2 & city == 5, curntmarriage)
married_n_partner_5

married_n_partner_6 <- subset(PWIDHIVtestingdata, curntmarriage == 2 & city == 6, curntmarriage)
married_n_partner_6

married_n_partner_7 <- subset(PWIDHIVtestingdata, curntmarriage == 2 & city == 7, curntmarriage)
married_n_partner_7

married_n_partner_8 <- subset(PWIDHIVtestingdata, curntmarriage == 2 & city == 8, curntmarriage)
married_n_partner_8

# Married but not living with partner

married_but_not_living_w_partner_1 <- subset(PWIDHIVtestingdata, curntmarriage == 3 & city == 1, curntmarriage)
married_but_not_living_w_partner_1

married_but_not_living_w_partner_2 <- subset(PWIDHIVtestingdata, curntmarriage == 3 & city == 2, curntmarriage)
married_but_not_living_w_partner_2

married_but_not_living_w_partner_3 <- subset(PWIDHIVtestingdata, curntmarriage == 3 & city == 3, curntmarriage)
married_but_not_living_w_partner_3

married_but_not_living_w_partner_4 <- subset(PWIDHIVtestingdata, curntmarriage == 3 & city == 4, curntmarriage)
married_but_not_living_w_partner_4

married_but_not_living_w_partner_5 <- subset(PWIDHIVtestingdata, curntmarriage == 3 & city == 5, curntmarriage)
married_but_not_living_w_partner_5

married_but_not_living_w_partner_6 <- subset(PWIDHIVtestingdata, curntmarriage == 3 & city == 6, curntmarriage)
married_but_not_living_w_partner_6

married_but_not_living_w_partner_7 <- subset(PWIDHIVtestingdata, curntmarriage == 3 & city == 7, curntmarriage)
married_but_not_living_w_partner_7

married_but_not_living_w_partner_8 <- subset(PWIDHIVtestingdata, curntmarriage == 3 & city == 8, curntmarriage)
married_but_not_living_w_partner_8


# Not married but living with partner----

not_married_w_partner_1 <- subset(PWIDHIVtestingdata, curntmarriage == 4 & city == 1, curntmarriage)
not_married_w_partner_1

not_married_w_partner_2 <- subset(PWIDHIVtestingdata, curntmarriage == 4 & city == 2, curntmarriage)
not_married_w_partner_2

not_married_w_partner_3 <- subset(PWIDHIVtestingdata, curntmarriage == 4 & city == 3, curntmarriage)
not_married_w_partner_3

not_married_w_partner_4 <- subset(PWIDHIVtestingdata, curntmarriage == 4 & city == 4, curntmarriage)
not_married_w_partner_4

not_married_w_partner_5 <- subset(PWIDHIVtestingdata, curntmarriage == 4 & city == 5, curntmarriage)
not_married_w_partner_5

not_married_w_partner_6 <- subset(PWIDHIVtestingdata, curntmarriage == 4 & city == 6, curntmarriage)
not_married_w_partner_6

not_married_w_partner_7 <- subset(PWIDHIVtestingdata, curntmarriage == 4 & city == 7, curntmarriage)
not_married_w_partner_7

not_married_w_partner_8 <- subset(PWIDHIVtestingdata, curntmarriage == 4 & city == 8, curntmarriage)
not_married_w_partner_8

#separated/ divorced----


separated_divorced_1 <-  subset(PWIDHIVtestingdata, curntmarriage == 5 & city == 1, curntmarriage)
separated_divorced_1

separated_divorced_2 <-  subset(PWIDHIVtestingdata, curntmarriage == 5 & city == 2, curntmarriage)
separated_divorced_2

separated_divorced_3 <-  subset(PWIDHIVtestingdata, curntmarriage == 5 & city == 3, curntmarriage)
separated_divorced_3

separated_divorced_4 <-  subset(PWIDHIVtestingdata, curntmarriage == 5 & city == 4, curntmarriage)
separated_divorced_4

separated_divorced_5 <-  subset(PWIDHIVtestingdata, curntmarriage == 5 & city == 5, curntmarriage)
separated_divorced_5

separated_divorced_6 <-  subset(PWIDHIVtestingdata, curntmarriage == 5 & city == 6, curntmarriage)
separated_divorced_6

separated_divorced_7 <-  subset(PWIDHIVtestingdata, curntmarriage == 5 & city == 7, curntmarriage)
separated_divorced_7

separated_divorced_8 <-  subset(PWIDHIVtestingdata, curntmarriage == 5 & city == 8, curntmarriage)
separated_divorced_8


# Widowed 
widowed_1 <-  subset(PWIDHIVtestingdata, curntmarriage == 6 & city == 1, curntmarriage)
widowed_1

widowed_2 <-  subset(PWIDHIVtestingdata, curntmarriage == 6 & city == 2, curntmarriage)
widowed_2

widowed_3 <-  subset(PWIDHIVtestingdata, curntmarriage == 6 & city == 3, curntmarriage)
widowed_3

widowed_4 <-  subset(PWIDHIVtestingdata, curntmarriage == 6 & city == 4, curntmarriage)
widowed_4

widowed_5 <-  subset(PWIDHIVtestingdata, curntmarriage == 6 & city == 5, curntmarriage)
widowed_5

widowed_6 <-  subset(PWIDHIVtestingdata, curntmarriage == 6 & city == 6, curntmarriage)
widowed_6

widowed_7 <-  subset(PWIDHIVtestingdata, curntmarriage == 6 & city == 7, curntmarriage)
widowed_7

widowed_8 <-  subset(PWIDHIVtestingdata, curntmarriage == 6 & city == 8, curntmarriage)
widowed_8

