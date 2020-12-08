##Scrtipt devoir méthodes avancées de modélisation 

library(readr)
devoir_sample <- read_csv("devoir_sample.csv")
View(devoir_sample)

data <-devoir_sample
require(tidyverse)

#prévalence de la maladie M sur l'échantillon : 

mean(data$statut) 
#prévalence sur l'échantillon de 0.08 soit 8% 

# retrait des valeurs NA pour les poids d'échantillon 
data_1 <- data %>% drop_na(poids_sond)


#Question 1 : 
#Calcul des prévalences pour chaque strate en faisant l'hypothèse que les poids de sondage décrivent 
#directement le nombre de sujets représentés dans la population cible

#On peut estimer la prévalence de la maladie M dans la population grâce à l'estimateur de 
#Horvitz-Thompson

#strate A 
strate_A <- filter(data_1, cohorte =="A") 
prevalence_A <- sum(strate_A$poids_sond*strate_A$statut)/sum(strate_A$poids_sond)

prevalence_A

#IC 95% strate A 
varA <- prevalence_A*(1-prevalence_A)
IC95_A <- prevalence_A + qnorm(c(.025, .975)) * sqrt(varA / nrow(strate_A))

#strate B 
strate_B <- filter(data_1, cohorte =="B") 
prevalence_B <- sum(strate_B$poids_sond*strate_B$statut)/sum(strate_B$poids_sond)

prevalence_B

#IC 95% strate B 
varB <- prevalence_B*(1-prevalence_B)
IC95_B <- prevalence_B + qnorm(c(.025, .975)) * sqrt(varB / nrow(strate_B))


#strate C 
strate_C <- filter(data_1, cohorte =="C") 
prevalence_C <- sum(strate_C$poids_sond*strate_C$statut)/sum(strate_C$poids_sond)

prevalence_C

#IC 95% strate C
varC <- prevalence_C*(1-prevalence_C)
IC95_C <- prevalence_C + qnorm(c(.025, .975)) * sqrt(varC / nrow(strate_C))

#strate D 
strate_D <- filter(data_1, cohorte =="D") 
prevalence_D <- sum(strate_D$poids_sond*strate_D$statut)/sum(strate_D$poids_sond)

prevalence_D

#IC 95% strate D 
varD <- prevalence_D*(1-prevalence_D)
IC95_D <- prevalence_D + qnorm(c(.025, .975)) * sqrt(varD / nrow(strate_D))

#strate E 
strate_E <- filter(data_1, cohorte =="E") 
prevalence_E <- sum(strate_E$poids_sond*strate_E$statut)/sum(strate_E$poids_sond)

prevalence_E

#IC 95% strate E 
varE <- prevalence_E*(1-prevalence_E)
IC95_E <- prevalence_E + qnorm(c(.025, .975)) * sqrt(varE / nrow(strate_E))

?qbinom
#Affichage des prévalences et de leurs écarts types à 95% pour chaque strate 

prevalence <- c(prevalence_A, prevalence_B, prevalence_C, prevalence_D, prevalence_E)*100
IC95_2.5 <- c(IC95_A[1], IC95_B[1], IC95_C[1], IC95_D[1], IC95_E[1])*100
IC95_97.5 <- c(IC95_A[2], IC95_B[2], IC95_C[2], IC95_D[2], IC95_E[2])*100

data.frame(prevalence, IC95_2.5, IC95_97.5, row.names = c("strate A", "strate B", "strate C", 
                                                          "strate D", "strate E"))
#         prevalence   IC95_2.5 IC95_97.5
# strate A  7.9588571  6.9218957  8.995818
# strate B 10.6067872  9.4925895 11.720985
# strate C  2.0484843 -0.5748944  4.671863
# strate D  0.8432632 -0.7996601  2.486187
# strate E  8.3881804  5.9510457 10.825315

#On a donc dans ce tableau les estimateurs des prévalences de la maladie M selon les strates 
# avec leurs intervalles de confiance à 95% ; les extrémités inférieures des IC95% des strates
# C et D ne devraient pas être négatifs ; Ceci est lié au fait de l'utilisation de la loi 
# normale pour l'approximation ; cependant en raison des poids différents des sujets 
# dans les populations cibles, il ne peut pas être utilisé une loi binomiale et pas de solution
# trouvée pour régler ce problème. 

#Question 2 : 

#En considérant que chaque poids de sondage décrit le nomnbre de sujet représentés dans 
#la population cible, on connait le poids de chaque sujet dans sa cohorte respective 
# exemple : le poids du sujet A1 est de 10.05 dans la cohorte A
# cependant on ne connait pas la proportion de chaque strate dans la population générale 
#d'Ile de France.

# De plus, il existe des chevauchement entre les différentes strates puisque la strate E 
# correspond aux hommes et femmes de 25 à 70 ans, et sont donc compris dans la strate A qui
# correspond aux hommes et femmes de 20 à 90 ans. 

# Pour estimer la prévalence sur l'ensemble de l'échantillon, il faudrait connaïtre la taille de 
# la population d'Ile de France, la proportion que chaque strate représente dans cette population 
# et connaître la méthode de sondage pour les différentes strates. 
