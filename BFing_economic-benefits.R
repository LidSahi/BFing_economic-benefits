#*****************************************************************************# 
#         Beneficios económicos - inteligencia atribuida a lactancia          
                                                                             
                                                                            
# Authors:
# Lidia Sarahi Peña-Ruiz (1)                                                  
# Mishel Unar-Munguía, PhD (1)                                                 
# Fernando Alarid-Escudero, PhD (2)                                           
 
# 1 Instituto Nacional de Salud Pública, Cuernavaca, Morelos                  
# 2 Division of Public Administration, Center for Research and Teaching in 
#   Economics (CIDE), Aguascalientes, Mexico                                  
                                                                                                                        #
# Ultima corrección: 18 mayo 2022                                             
#*****************************************************************************# 

rm(list = ls()) # to clean the workspace // memory
ptm <- proc.time() #determines how much real and CPU time (in seconds) the 
#currently running R process has already taken.#

setwd ("/Users/lidiasarahipenaruiz/Desktop/Paper2/bases") 

# *****************************************************************************
#### 01. Load packages ####
# *****************************************************************************

library('Hmisc')
library('gtools')
library('survival')
library('stargazer')
library('graphics')
library ('triangle')
library ('Runuran')
library('data.table')  
library('compiler')
library('reshape2')

## Load additional functions
#source("R/Functions.R")  #This line only works when you "Function.R" file is in 
#the same folder

# *****************************************************************************
#### 02 Input Model Parameters ####
# *****************************************************************************

#set.seed(1)              # set the seed 
set.seed(21032022) #seed of random number generator, which is useful for 
#creating simulations or random objects that can be reproduced.

# 02.1 Population characteristics
# Initial Age 
n_i           <- 100000             # number of individuals
v_age0        <- 0
# v_age0        <- sample  (x=0,   
#                           prob      =1, 
#                           size      =n_i, 
#                           replace   =TRUE) 
n_age_max     <- 65                 # maximum age of follow up

p_female      <- 0.501              # proportion population who are female-2002

# https://www.inegi.org.mx/sistemas/olap/consulta/general_ver4/MDXQueryDatos.asp?#Regreso&c=23699

v_sex         <- sample (x= c("Female", "Male"),
                         prob     = c(p_female, 1-p_female),
                         size     = n_i,
                         replace  = TRUE)   
                                  # randomly sample sex

# Sample from age distribution an initial age for every individual
df_sim_pop <- data.frame(Ind  = 1:n_i,
                         Age0 = v_age0,
                         Sex  = v_sex)

esp_vida      <- 74.9    # Esperanza de vida en México 2002
edad_jub      <- 65      # Edad de jubilacion (¿dejarla?)
PIB           <- 9593    # PIB PER CAPITA (dólares de 2002)
r             <- 0.03    # discount rate
r_5           <- 0.05    # discount rate 5%


## Escenario basal lactancia de acuerdo a ENNVIH 2002 
prob_lacta    <- 0.914   # Probabilidad de lactar

# Nlacta=c()
# 
# for (ind in 1:n_i)
#   
# {
#   lacta=0
#   
#   if (n_i[ind]==1) {
#     
#     lacta=rbinom(1,1,prob_lacta)
#     
#   } else  {
#     lacta<-0
#   }
#   
#   Nlacta=c(Nlacta, lacta) 
#   #Nlacta=Nlacta
# }

Nlacta <- rbinom(n = n_i, size = 1, prob_lacta)

# probabilidad de LM cada mes - LM predominante - AMBOS SEXOS*
prob_lacta0       <- 0.085   # Probabilidad de lactar 0 meses
prob_lacta0.5     <- 0.314   # Probabilidad de lactar medio mes
prob_lacta1       <- 0.040   # Probabilidad de lactar 1 mes
prob_lacta2       <- 0.091   # Probabilidad de lactar 2 meses
prob_lacta3       <- 0.158   # Probabilidad de lactar 3 meses
prob_lacta4       <- 0.125   # Probabilidad de lactar 4 meses
prob_lacta5       <- 0.035   # Probabilidad de lactar 5 meses
prob_lacta6       <- 0.088   # Probabilidad de lactar 6 meses
prob_lacta_mas7   <- 0.059   # Probabilidad de lactar mas de 7 meses

# probabilidad de LM cada mes - LM predominante - FEMENINO
# prob_lacta0_fem       <- 0.086   # Probabilidad de lactar 0 meses
# prob_lacta0.5_fem     <- 0.332   # Probabilidad de lactar medio mes
# prob_lacta1_fem       <- 0.035   # Probabilidad de lactar 1 mes
# prob_lacta2_fem       <- 0.082   # Probabilidad de lactar 2 meses
# prob_lacta3_fem       <- 0.163   # Probabilidad de lactar 3 meses
# prob_lacta4_fem       <- 0.122   # Probabilidad de lactar 4 meses
# prob_lacta5_fem       <- 0.038   # Probabilidad de lactar 5 meses
# prob_lacta6_fem       <- 0.082   # Probabilidad de lactar 6 meses
# prob_lacta_mas7_fem   <- 0.056   # Probabilidad de lactar mas de 7 meses

# probabilidad de LM cada mes - LM predominante - MASCULINO
# prob_lacta0_ma       <- 0.084   # Probabilidad de lactar 0 meses
# prob_lacta0.5_ma     <- 0.297   # Probabilidad de lactar medio mes
# prob_lacta1_ma       <- 0.046   # Probabilidad de lactar 1 mes
# prob_lacta2_ma       <- 0.100   # Probabilidad de lactar 2 meses
# prob_lacta3_ma       <- 0.153   # Probabilidad de lactar 3 meses
# prob_lacta4_ma       <- 0.128   # Probabilidad de lactar 4 meses
# prob_lacta5_ma       <- 0.033   # Probabilidad de lactar 5 meses
# prob_lacta6_ma       <- 0.093   # Probabilidad de lactar 6 meses
# prob_lacta_mas7_ma   <- 0.062   # Probabilidad de lactar mas de 7 meses

## Distribución inteligencia paper ENNVIH 
## med_intel     <- 0.20   # Media del score z de la prueba de Raven 
## de_intel      <- 0.95   #DE del score z de la prueba de Raven 
## v_intel       <- rnorm  (n_i,
##                          mean=med_intel, 
##                          de_intel)   
## head(v_intel)           

##Inteligencia - score z  Raven's coloured progressive matrices test (CPM)
med_intel_0mes     <- 0.11   # Media de score z CPM zero mes
de_intel_0mes      <- 0.86    #DE scorez z CPM zero mes

med_intel_0.5mes   <- -0.11   # Media de score z CPM medio mes
de_intel_0.5mes    <- 0.95    #DE score z CPM medio mes

med_intel_1mes     <- -0.02  # Media de score z CPM 1 mes
de_intel_1mes      <- 1.02   #DE score z CPM 1 mes

med_intel_2mes     <- 0.15   # Media de score z CPM 2 mes
de_intel_2mes      <- 0.89   #DE score z CPM 2 mes

med_intel_3mes     <- 0.29  # Media de score z CPM 3 mes
de_intel_3mes      <- 0.96   #DE score z CPM 3 mes

med_intel_4mes     <- 0.31   # Media de score z CPM 4 mes
de_intel_4mes      <- 0.91   #DE score z CPM 4 mes

med_intel_5mes     <- 0.14   # Media de score z CPM 5 mes
de_intel_5mes      <- 0.97   #DE score z CPM 5 mes

med_intel_6mes     <- 0.25   # Media de score z CPM 6 mes
de_intel_6mes      <- 0.90   #DE score z CPM 6 mes

med_intel_7mes     <- 0.04   # Media de score z CPM 7 o más mes
de_intel_7mes      <- 0.99   #DE score z CPM 7 o más mes

