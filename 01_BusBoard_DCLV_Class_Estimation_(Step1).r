### This code was developed to perform classification using DCLV method for the paper 
### "Impact of the COVID-19 pandemic on the comfort of riding a crowded bus in Metro Vancouver, Canada" 
### The paper is available at https://doi.org/10.1016/j.tranpol.2023.07.018

### The code was developed by Bogdan Kapatsila in Spring-Summer 2022

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load libraries
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="DCLV with 2 LV",
  modelDescr ="DCLV with 2 LV",
  indivID    ="UNID",
  panelData = FALSE,
  mixing     = TRUE,
#workInLogs = TRUE,
  nCores     = 3)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

setwd("D:/Research/2020_TransLink_Overcrowding/Data/DCLV/Combined")

database = read.csv("translink_latent_class.csv",header=TRUE)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c( #std1 = 1,
               #std2 = 1,
  
              std = 1,
               
              #LV_1
  
              zeta_aconC = 1, 
              #zeta_sconC = 1, 
              zeta_bothC = 1,
              zeta_seatC = 1,
              zeta_offpeakC = 1, 
              #zeta_altC = 1, 
              tau_acon_1C =-2, 
              tau_acon_2C =-1, 
              tau_acon_3C = 1, 
              tau_acon_4C = 2,
              #tau_scon_1C =-2, 
              #tau_scon_2C =-1, 
              #tau_scon_3C = 1, 
              #tau_scon_4C = 2,
              tau_both_1C =-2, 
              tau_both_2C =-1, 
              tau_both_3C = 1, 
              tau_both_4C = 2,
              tau_seat_1C =-3, 
              tau_seat_2C =-1, 
              tau_seat_3C = 1, 
              tau_seat_4C = 2,
              tau_offpeak_1C =-2, 
              tau_offpeak_2C =-1,
              tau_offpeak_3C = 1, 
              tau_offpeak_4C = 2,
              #tau_alt_1C =-2, 
              #tau_alt_2C =-1,
              #tau_alt_3C = 1, 
              #tau_alt_4C = 2,

              #zeta_sconX_A = 1,
              #zeta_sconX_B = -1,
              const_sconX_A = -0.1,
              const_sconX_BC = 0.1,

              tau_scon_1X = 0, 
              tau_scon_2X = 1,
              tau_scon_3X = 2, 
              tau_scon_4X = 3,
              
              #zeta_altX_A = 1,
              #zeta_altX_B = -1,
              const_altX_AB = -0.1,
              const_altX_C = 0.1,

              tau_alt_1X = 0, 
              tau_alt_2X = 1,
              tau_alt_3X = 2, 
              tau_alt_4X = 3,
              

              gamma_LV1_female  = 1, 
              gamma_LV1_age2544  = 1,
              gamma_LV1_kids = 1,
              gamma_LV1_ampeak = 1,

              lambdaX1 = 1,
              b1 = 1,
              m1 = 0,
              thre11 = -0.5728,
              thre12 = 2.098,
              
              #LV_2
              
              #zeta_flextwC = 1, 
              zeta_flexfwC = 0.8830, 
              #tau_flextw_1C =-2, 
              #tau_flextw_2C =-1, 
              #tau_flextw_3C = 1, 
              #tau_flextw_4C = 2,
              tau_flexfw_1C =-1.4323, 
              tau_flexfw_2C =-0.4719, 
              tau_flexfw_3C = 0.7648, 
              tau_flexfw_4C = 1.9808,
              
              #zeta_flextwX_A = 1,
              #zeta_flextwX_B = -1,
              
              const_flextwX_A = -7.0369,
              const_flextwX_B = 7.4990,
              
              tau_flextw_1X = 0, 
              tau_flextw_2X = 6.7629, 
              tau_flextw_3X = 7.9624, 
              tau_flextw_4X = 8.9161,
              
              gamma_LV2_female  = -0.4386, 
              gamma_LV2_incomelow  = 0.3154,
              gamma_LV2_age65O  =  -0.7157,
              gamma_LV2_edbach = 0.3938,
              #gamma_LV2_kids = 0.1168,

              lambdaX2 = 1,
              b2 = 1,
              m2 = 0,
              thre2 = -0.4421)
              

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("b1", "m1", "lambdaX1", "tau_scon_1X","tau_alt_1X","lambdaX2", "b2", "m2", "tau_flextw_1X")

### Read in starting values for at least some parameters from existing model output file
#apollo_beta=apollo_readBeta(apollo_beta,
#                            apollo_fixed,
#                           "Translink_LatentClass_LV1_N1.4_1.6_LV2_N1.2_DCLV_3-2Classes(Coded)_alt_er_OLD2",
#                            overwriteFixed=FALSE)


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="sobol", 
  interNDraws=1000,          
  interUnifDraws=c("eta1", "eta2"),      
  interNormDraws=c("eta"), 
  
  intraDrawsType='',
  intraNDraws=0,          
  intraUnifDraws=c(),     
  intraNormDraws=c()      
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()

  randcoeff[["LVC1"]] = gamma_LV1_female * FEMALE + gamma_LV1_age2544 * AGE_2544 + 
    gamma_LV1_kids * HOUSEHOLD_CHILD_CLEAN_N + gamma_LV1_ampeak * TIME_BC_AM + 
    2*b1*atanh(2*eta1-1)+m1 + eta*std #Error term follows a logistic distribution (to match the logit in allocation function) + et
  
  randcoeff[["LVC2"]] = gamma_LV2_female * FEMALE + gamma_LV2_incomelow * INCOME_LOW + 
    gamma_LV2_age65O * AGE_65O + gamma_LV2_edbach * EDUCATION_BACH +  
    #gamma_LV2_kids * HOUSEHOLD_CHILD_CLEAN_N + 
    2*b2*atanh(2*eta2-1)+m2 + eta*std
  
  
  return(randcoeff)
}

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
 # LV_1

  LVX1 = gamma_LV1_female * FEMALE + gamma_LV1_age2544 * AGE_2544 + 
    gamma_LV1_kids * HOUSEHOLD_CHILD_CLEAN_N + gamma_LV1_ampeak * TIME_BC_AM + 
    eta*std # 
  
  VA1 = lambdaX1*(LVX1-thre11)
  VB1 = lambdaX1*(LVX1-thre12)
  
  piA1 = 1/(1 + exp(VA1))
  piB1 = 1/(1 + exp(VB1)) - piA1
  piC1 = 1-piA1-piB1
  
  lcpars[["pi_values1"]] = list(piA1, piB1, piC1)
  
  # LVX_2
  
  lcpars[["const_flextwX"]]  = list(const_flextwX_A, const_flextwX_B)
  
  LVX2 = gamma_LV2_female * FEMALE + gamma_LV2_incomelow * INCOME_LOW + 
    gamma_LV2_age65O * AGE_65O + gamma_LV2_edbach * EDUCATION_BACH +  
   #gamma_LV2_kids * HOUSEHOLD_CHILD_CLEAN_N + 
    eta*std 
  
  VA2  = lambdaX2*(LVX2-thre2)
  piA2 = 1/(1 + exp(VA2))
  piB2 = 1 - piA2
  
  lcpars[["pi_values2"]] = list(piA2, piB2)
  
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
  PClass4 = list()
  PClass6 = list()
  PClass7 = list()
  
  ### Likelihood of indicators for LV_1
  ol_settings1C = list(outcomeOrdered=AGREE_BOTH_CROWD_BC_R, 
                      V=zeta_bothC*LVC1, 
                      tau=list(tau_both_1C, tau_both_2C, tau_both_3C, tau_both_4C))
  ol_settings2C = list(outcomeOrdered=AGREE_CONCERN_BC_R, 
                      V=zeta_aconC*LVC1, 
                      tau=list(tau_acon_1C, tau_acon_2C, tau_acon_3C, tau_acon_4C))
  ol_settings3C = list(outcomeOrdered=AGREE_SEAT_BC_R, 
                      V=zeta_seatC*LVC1, 
                      tau=list(tau_seat_1C, tau_seat_2C, tau_seat_3C, tau_seat_4C))
  #ol_settings4C = list(outcomeOrdered=STATE_CONCERNED_R, 
  #                    V=zeta_sconC*LVC1, 
  #                    tau=list(tau_scon_1C, tau_scon_2C, tau_scon_3C, tau_scon_4C))
  ol_settings5C = list(outcomeOrdered=AGREE_OFFPEAK_BC_R, 
                      V=zeta_offpeakC*LVC1, 
                      tau=list(tau_offpeak_1C, tau_offpeak_2C, tau_offpeak_3C, tau_offpeak_4C))
  #ol_settings6C = list(outcomeOrdered=AGREE_ALT_BC_R, 
  #                    V=zeta_altC*LVC1, 
  #                    tau=list(tau_alt_1C, tau_alt_2C, tau_alt_3C, tau_alt_4C))
  
  P[["indic_bothC"]]     = apollo_ol(ol_settings1C, functionality)
  P[["indic_aconC"]]     = apollo_ol(ol_settings2C, functionality)
  P[["indic_seatC"]]      = apollo_ol(ol_settings3C, functionality)
  #P[["indic_sconC"]]      = apollo_ol(ol_settings4C, functionality)
  P[["indic_offpeakC"]]     = apollo_ol(ol_settings5C, functionality)
  #P[["indic_altC"]]      = apollo_ol(ol_settings6C, functionality)

  ### Categorical LV_1 - Indicator 4
  ol_settings41X = list(outcomeOrdered = STATE_CONCERNED_R, 
                        V = const_sconX_A,
                        tau = list(tau_scon_1X, tau_scon_2X, tau_scon_3X, tau_scon_4X),
                        coding = c(1,2,3,4,5))
  
  ol_settings42X = list(outcomeOrdered = STATE_CONCERNED_R, 
                        V = const_sconX_BC,
                        tau = list(tau_scon_1X, tau_scon_2X, tau_scon_3X, tau_scon_4X),
                        coding = c(1,2,3,4,5))
  
  ol_settings43X = list(outcomeOrdered = STATE_CONCERNED_R, 
                        V = const_sconX_BC,
                        tau = list(tau_scon_1X, tau_scon_2X, tau_scon_3X, tau_scon_4X),
                        coding = c(1,2,3,4,5))
  
  PClass4[["sconClass1"]] = apollo_ol(ol_settings41X, functionality)
  PClass4[["sconClass2"]] = apollo_ol(ol_settings42X, functionality)
  PClass4[["sconClass3"]] = apollo_ol(ol_settings43X, functionality)
  
  lc_settings4   = list(inClassProb=PClass4, classProb=pi_values1)
  P[["indic_sconX"]] = apollo_lc(lc_settings4, apollo_inputs, functionality) 
  
  ### Categorical LV_1 - Indicator 6
  ol_settings61X = list(outcomeOrdered = AGREE_ALT_BC_R, 
                        V = const_altX_AB,
                        tau = list(tau_alt_1X, tau_alt_2X, tau_alt_3X, tau_alt_4X),
                        coding = c(1,2,3,4,5))
  
  ol_settings62X = list(outcomeOrdered = AGREE_ALT_BC_R, 
                        V = const_altX_AB,
                        tau = list(tau_alt_1X, tau_alt_2X, tau_alt_3X, tau_alt_4X),
                        coding = c(1,2,3,4,5))
  
  ol_settings63X = list(outcomeOrdered = AGREE_ALT_BC_R, 
                        V = const_altX_C,
                        tau = list(tau_alt_1X, tau_alt_2X, tau_alt_3X, tau_alt_4X),
                        coding = c(1,2,3,4,5))
  
  PClass6[["altClass1"]] = apollo_ol(ol_settings61X, functionality)
  PClass6[["altClass2"]] = apollo_ol(ol_settings62X, functionality)
  PClass6[["altClass3"]] = apollo_ol(ol_settings63X, functionality)

  lc_settings6   = list(inClassProb=PClass6, classProb=pi_values1)
  P[["indic_altX"]] = apollo_lc(lc_settings6, apollo_inputs, functionality) 
  
  ### Likelihood of indicators LV_2
  
  #ol_settings7C = list(outcomeOrdered=FLEX_PT_TW_R_N, 
  #                    V=zeta_flextwC*LVC2, 
  #                    tau=list(tau_flextw_1C, tau_flextw_2C, tau_flextw_3C, tau_flextw_4C))
  ol_settings8C = list(outcomeOrdered=FLEX_PT_FW_R_N, 
                       V=zeta_flexfwC*LVC2, 
                       tau=list(tau_flexfw_1C, tau_flexfw_2C, tau_flexfw_3C, tau_flexfw_4C))
  
  #P[["indic_flextwC"]]     = apollo_ol(ol_settings7C, functionality)
  P[["indic_flexfwC"]]      = apollo_ol(ol_settings8C, functionality)
  
  ### Categorical LV_2 - Indicator 7  
  
  ### Define settings for OL model component that are generic across classes
  ol_settings7X = list(
    outcomeOrdered = FLEX_PT_TW_R_N, 
    tau            = list(tau_flextw_1X, tau_flextw_2X, tau_flextw_3X, tau_flextw_4X),
    coding         = c(1,2,3,4,5))
  ### Loop over classes
  S = 2 # number of classes
  for(s in 1:S){
    ### Class-specific utilities
    ol_settings7X$V = const_flextwX[[s]] #constX[[s]] + zeta_flextwX[[s]]*LVC2
    
    ### Within-class choice probabilities using OL model
    label = paste0("flextwClass",s)
    PClass7[[label]] = apollo_ol(ol_settings7X, functionality)
    
    ### Take product across observation for same individual
    #PClass[[label]] = apollo_panelProd(PClass[[label]], apollo_inputs, functionality)
  }
  ### Mix the probabilities from each class
  lc_settings7   = list(inClassProb=PClass7, classProb=pi_values2)
  P[["indic_flextwX"]] = apollo_lc(lc_settings7, apollo_inputs, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)

  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### ESTIMATE SETTINGS                                           ####
# ################################################################# #

estimate_settings = list(maxIterations = 250, scaling = TRUE)

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

summary(as.data.frame(unconditionals[["pi_values"]]))


# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)
