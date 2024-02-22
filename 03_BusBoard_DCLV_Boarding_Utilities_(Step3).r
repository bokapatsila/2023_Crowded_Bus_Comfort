### This code was developed to perform utility calculations for the paper
### "Impact of the COVID-19 pandemic on the comfort of riding a crowded bus in Metro Vancouver, Canada" 
### The paper is available at https://doi.org/10.1016/j.tranpol.2023.07.018

### The code was developed by Bogdan Kapatsila in Spring-Summer 2022


# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())
setwd("D:/Research/2020_TransLink_Overcrowding/Data/Mode_Choice")

### Load libraries
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="OL_LC_Crowded_Bus_All_binary_3Class_exp",
  modelDescr ="OL_LC_Crowded_Bus_All_binary_3Class_exp",
  #mixing          = TRUE, 
  indivID         = "UNID",
  nCores          = 4, 
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("translink_crowded_bus_classes.csv",header=TRUE)
database$CRCHOICEB <- ifelse(database$CRCHOICE>=1, 1, 0)

#time <- read.csv('sample_UNID_time.csv', header=T)
#database <- merge(database, time, by='UNID')
#database <- filter(database, TOTAL_TIME>=269.8) #25% mean
#database <- filter(database, TOTAL_TIME>=539.6) #50% mean
#database <- filter(database, TOTAL_TIME>=755.44)#70% mean
#database <- filter(database, TOTAL_TIME>=1079.202) #100% mean

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(const_crowd_1234 = 0,
              const_crowd_56 = 0,
              
              b_crlevel_1234 = 0,
              b_crlevel_5 = 0,
              b_crlevel_6 = 0,

              b_crcovid = 0,
              b_wave = 0,

              b_crlevel_X_covid_1234 = 0,
              b_crlevel_X_covid_56 = 0,

              b_kids = 0,
              b_edbach = 0,
              b_car = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

### Read in starting values for at least some parameters from existing model output file
#apollo_beta=apollo_readBeta(apollo_beta,
#                            apollo_fixed,
#                           "Translink_MOL_LC_Crowded_Bus_OLD2",
#                            overwriteFixed=FALSE)


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
#apollo_draws = list(
#  interDrawsType = "sobol",
#  interNDraws    = 250,
#  interNormDraws = c("draws_crlevel") #,"draws_crcovid", "draws_wave", "draws_crlevel_X_covid")
#)

### Create random parameters
#apollo_randCoeff = function(apollo_beta, apollo_inputs){
#  randcoeff = list()
  
  #randcoeff[["b_crlevel_1"]] = b_crlevel_mu_1 + b_crlevel_sig_1 * draws_crlevel
  #randcoeff[["b_crlevel_2"]] = b_crlevel_mu_2 + b_crlevel_sig_2 * draws_crlevel
  #randcoeff[["b_crlevel_3"]] = b_crlevel_mu_3 + b_crlevel_sig_3 * draws_crlevel
  #randcoeff[["b_crlevel_4"]] = b_crlevel_mu_4 + b_crlevel_sig_4 * draws_crlevel
  #randcoeff[["b_crlevel_5"]] = b_crlevel_mu_5 + b_crlevel_sig_5 * draws_crlevel
  #randcoeff[["b_crlevel_6"]] = b_crlevel_mu_6 + b_crlevel_sig_6 * draws_crlevel
  
  #randcoeff[["b_crcovid_1"]] = b_crcovid_mu_1 + b_crcovid_sig_1 * draws_crcovid
  #randcoeff[["b_crcovid_2"]] = b_crcovid_mu_2 + b_crcovid_sig_2 * draws_crcovid
  #randcoeff[["b_crcovid_3"]] = b_crcovid_mu_3 + b_crcovid_sig_3 * draws_crcovid
  #randcoeff[["b_crcovid_4"]] = b_crcovid_mu_4 + b_crcovid_sig_4 * draws_crcovid
  #randcoeff[["b_crcovid_5"]] = b_crcovid_mu_5 + b_crcovid_sig_5 * draws_crcovid
  #randcoeff[["b_crcovid_6"]] = b_crcovid_mu_6 + b_crcovid_sig_6 * draws_crcovid
  
  #randcoeff[["b_crlevel_X_covid_1"]] = b_crlevel_X_covid_mu_1 + b_crlevel_X_covid_sig_1 * draws_crlevel_X_covid
  #randcoeff[["b_crlevel_X_covid_2"]] = b_crlevel_X_covid_mu_2 + b_crlevel_X_covid_sig_2 * draws_crlevel_X_covid
  #randcoeff[["b_crlevel_X_covid_3"]] = b_crlevel_X_covid_mu_3 + b_crlevel_X_covid_sig_3 * draws_crlevel_X_covid
  #randcoeff[["b_crlevel_X_covid_4"]] = b_crlevel_X_covid_mu_4 + b_crlevel_X_covid_sig_4 * draws_crlevel_X_covid
  #randcoeff[["b_crlevel_X_covid_5"]] = b_crlevel_X_covid_mu_5 + b_crlevel_X_covid_sig_5 * draws_crlevel_X_covid
  #randcoeff[["b_crlevel_X_covid_6"]] = b_crlevel_X_covid_mu_6 + b_crlevel_X_covid_sig_6 * draws_crlevel_X_covid
  
  #randcoeff[["b_wave_1"]] = b_wave_mu_1 + b_wave_sig_1 * draws_wave 
  #randcoeff[["b_wave_2"]] = b_wave_mu_2 + b_wave_sig_2 * draws_wave 
  #randcoeff[["b_wave_3"]] = b_wave_mu_3 + b_wave_sig_3 * draws_wave 
  #randcoeff[["b_wave_4"]] = b_wave_mu_4 + b_wave_sig_4 * draws_wave 
  #randcoeff[["b_wave_5"]] = b_wave_mu_5 + b_wave_sig_5 * draws_wave 
  #randcoeff[["b_wave_6"]] = b_wave_mu_6 + b_wave_sig_6 * draws_wave 
  
#  return(randcoeff)
#}

# ################################################################# #
#### READ IN LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
 lcpars = list()
  
  ### Define lists of parameters for each class (No need if classes defined manually)
  ###                      classA   classB  ...
  #lcpars[["const_crowd"]] = list(const_crowd_1234, const_crowd_56)
  #lcpars[["b_crlevel"]] = list(b_crlevel_1234, b_crlevel_5, b_crlevel_6)
  #lcpars[["b_crcovid"]] = list(b_crcovid_1234, b_crcovid_56)
  #lcpars[["b_crlevel_X_covid"]] = list(b_crlevel_X_covid_1234, b_crlevel_X_covid_56)
  #lcpars[["b_wave"]] = list(b_wave_12, b_wave_34, b_wave_56)
  #lcpars[["b_age_2024"]] = list(b_age_2024_1, b_age_2024_2, b_age_2024_3, b_age_2024_4, b_age_2024_5, b_age_2024_6)
  #lcpars[["b_income_low"]] = list(b_income_low_1, b_income_low_2, b_income_low_3, b_income_low_4, b_income_low_5, b_income_low_6)
  #lcpars[["b_edbach"]] = list(b_edbach_1, b_edbach_2, b_edbach_3, b_edbach_4, b_edbach_5, b_edbach_6)
  #lcpars[["b_car"]] = list(b_car_1, b_car_2, b_car_3, b_car_4, b_car_5, b_car_6)
  
# Probabilitie from the previous estimation
  
  piA = piA1piA2.mean
  piB = piA1piB2.mean
  piC = piB1piA2.mean
  piD = piB1piB2.mean
  piE = piC1piA2.mean
  piF = piC1piB2.mean
  
  lcpars[["pi_values"]] = list(piA, piB, piC, piD, piE, piF)
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component 1
  V1=list()
  V1[["alt1"]]  = 0
  V1[["alt2"]]  = const_crowd_1234 + b_crcovid * CRCOVID + b_crlevel_1234 * CRLEVEL + 
    b_crlevel_X_covid_1234 * (CRLEVEL*CRCOVID) + b_wave * WAVE2 + b_kids * HOUSEHOLD_CHILD_CLEAN_N +
    b_edbach * EDUCATION_BACH + 
    b_car * CAR_B
  
  mnl_settings1 = list(
  alternatives = c(alt1=0, alt2=1),
 #avail  = list(alt1=1, alt2=1),
  choiceVar = CRCHOICEB,
  utilities = V1,
  componentName = 'Class1'
  )

  ### Define settings for MNL model component 2
  V2=list()
  V2[["alt1"]]  = 0
  V2[["alt2"]]  = const_crowd_1234 + b_crcovid * CRCOVID + b_crlevel_1234 * CRLEVEL + 
    b_crlevel_X_covid_1234 * (CRLEVEL*CRCOVID) + b_wave * WAVE2 + b_kids * HOUSEHOLD_CHILD_CLEAN_N +
    b_edbach * EDUCATION_BACH + 
    b_car * CAR_B
  
  mnl_settings2 = list(
    alternatives = c(alt1=0, alt2=1),
    #avail  = list(alt1=1, alt2=1),
    choiceVar = CRCHOICEB,
    utilities = V2,
    componentName = 'Class2'
  )
  
  ### Define settings for MNL model component 3
  V3=list()
  V3[["alt1"]]  = 0
  V3[["alt2"]]  = const_crowd_1234 + b_crcovid * CRCOVID + b_crlevel_1234 * CRLEVEL + 
    b_crlevel_X_covid_1234 * (CRLEVEL*CRCOVID) + b_wave * WAVE2 + b_kids * HOUSEHOLD_CHILD_CLEAN_N +
    b_edbach * EDUCATION_BACH + 
    b_car * CAR_B
  
  mnl_settings3 = list(
    alternatives = c(alt1=0, alt2=1),
    #avail  = list(alt1=1, alt2=1),
    choiceVar = CRCHOICEB,
    utilities = V3,
    componentName = 'Class3'
  )
  
  ### Define settings for MNL model component 4
  V4=list()
  V4[["alt1"]]  = 0
  V4[["alt2"]]  = const_crowd_1234 + b_crcovid * CRCOVID + b_crlevel_1234 * CRLEVEL + 
    b_crlevel_X_covid_1234 * (CRLEVEL*CRCOVID) + b_wave * WAVE2 + b_kids * HOUSEHOLD_CHILD_CLEAN_N +
    b_edbach * EDUCATION_BACH + 
    b_car * CAR_B
  
  mnl_settings4 = list(
    alternatives = c(alt1=0, alt2=1),
    avail  = list(alt1=1, alt2=1),
    choiceVar = CRCHOICEB,
    utilities = V4,
    componentName = 'Class4'
  )
  
  ### Define settings for MNL model component 5
  V5=list()
  V5[["alt1"]]  = 0
  V5[["alt2"]]  = const_crowd_56 + b_crcovid * CRCOVID + b_crlevel_5 * CRLEVEL + 
    b_crlevel_X_covid_56 * (CRLEVEL*CRCOVID) + b_wave * WAVE2 + b_kids * HOUSEHOLD_CHILD_CLEAN_N +
    b_edbach * EDUCATION_BACH + 
    b_car * CAR_B
  
  mnl_settings5 = list(
    alternatives = c(alt1=0, alt2=1),
    #avail  = list(alt1=1, alt2=1),
    choiceVar = CRCHOICEB,
    utilities = V5,
    componentName = 'Class5'
  )
  
  ### Define settings for MNL model component 6
  V6=list()
  V6[["alt1"]]  = 0
  V6[["alt2"]]  = const_crowd_56 + b_crcovid * CRCOVID + b_crlevel_6 * CRLEVEL + 
    b_crlevel_X_covid_56 * (CRLEVEL*CRCOVID) + b_wave * WAVE2 + b_kids * HOUSEHOLD_CHILD_CLEAN_N +
    b_edbach * EDUCATION_BACH + 
    b_car * CAR_B
  
  mnl_settings6 = list(
    alternatives = c(alt1=0, alt2=1),
    #avail  = list(alt1=1, alt2=1),
    choiceVar = CRCHOICEB,
    utilities = V6,
    componentName = 'Class6'
  )
    
    ### Compute within-class choice probabilities using MNL model
    P[["Class1"]] = apollo_mnl(mnl_settings1, functionality)
    P[["Class2"]] = apollo_mnl(mnl_settings2, functionality)
    P[["Class3"]] = apollo_mnl(mnl_settings3, functionality)
    P[["Class4"]] = apollo_mnl(mnl_settings4, functionality)
    P[["Class5"]] = apollo_mnl(mnl_settings5, functionality)
    P[["Class6"]] = apollo_mnl(mnl_settings6, functionality)
    
    ### Take product across observation for same individual
    P[["Class1"]] = apollo_panelProd(P[["Class1"]], apollo_inputs ,functionality)
    P[["Class2"]] = apollo_panelProd(P[["Class2"]], apollo_inputs ,functionality)
    P[["Class3"]] = apollo_panelProd(P[["Class3"]], apollo_inputs ,functionality)
    P[["Class4"]] = apollo_panelProd(P[["Class4"]], apollo_inputs ,functionality)
    P[["Class5"]] = apollo_panelProd(P[["Class5"]], apollo_inputs ,functionality)
    P[["Class6"]] = apollo_panelProd(P[["Class6"]], apollo_inputs ,functionality)

    
  ### Mix the probabilities from each class
  lc_settings   = list(inClassProb=P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Likelihood of the WHOLE MODEL
  #P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

