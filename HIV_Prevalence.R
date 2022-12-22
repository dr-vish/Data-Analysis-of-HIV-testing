# B) Making description table for HIV testing prevalence ----

#Point prevalence and 95% CI
# City 1: Kabul

N1 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==1])
N1
n1 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==1 & 
                                         PWIDHIVtestingdata$lastHIVtest2])
n1
prev_age1 <- n1/N1
prev_age1

library(DescTools)
BinomCI(300,400, method = "wald")


# City: Herat
N2 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==2])
N2
n2 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==2 & 
                                       PWIDHIVtestingdata$lastHIVtest2])
n2
prev_age2 <- n2/N2
prev_age2
BinomCI(174, 200, method = "wald")

# mazar-i-sharif
N3 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==3])
N3
n3 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==3 & 
                                       PWIDHIVtestingdata$lastHIVtest2])
n3
prev_age3 <- n3/N3
prev_age3

BinomCI(149, 149, method = "wald")

#jalalabad

N4 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==4])
N4
n4 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==4 & 
                                       PWIDHIVtestingdata$lastHIVtest2])
n4
prev_age4 <- n4/N4
prev_age4

BinomCI(149, 150, method = "wald")

#kunduz

N5 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==5])
N5
n5 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==5 & 
                                       PWIDHIVtestingdata$lastHIVtest2])
n5
prev_age5 <- n5/N5
prev_age5

BinomCI(135, 151, method = "wald")

#faizabad

N6 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==6])
N6
n2 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==6 & 
                                       PWIDHIVtestingdata$lastHIVtest2])
n6
prev_age2 <- n6/N6
prev_age6

BinomCI(34, 35, method = "wald")

#kandahar

N7 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==7])
N7
n7 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==7 & 
                                       PWIDHIVtestingdata$lastHIVtest2])
n7
prev_age7 <- n7/N7
prev_age7

BinomCI(84, 150, method = "wald")

#zaranj

N8 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==8])
N8
n8 <- length(PWIDHIVtestingdata$city[PWIDHIVtestingdata$city==8 & 
                                       PWIDHIVtestingdata$lastHIVtest2])
n8
prev_age8 <- n8/N8
prev_age8

BinomCI(110, 150, method = "wald")
