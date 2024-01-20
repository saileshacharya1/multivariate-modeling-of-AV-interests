### Prepare the data for analysis ##############################################
################################################################################
# import the data
df <- readRDS("Data/Cleaned data/cleaned_data.rds")

# dummy coding
library(fastDummies)
df <- dummy_cols(df, select_columns = c(
      'age_grp',              'region',           'future_decision_role',
      'autonomous_aware',     'housing',          'income',
      'employment',           'student',          'education',            
      'drive_freq',           'job_type',         'work_mode',        
      'school_mode'), 
      remove_selected_columns = TRUE)

# convert all categorical variables to factors
df1 <- subset(df, select = -c(num_hh_vehicles,        household_members_1,    
                             household_members_2,    household_members_3,    
                             household_members_4,    autonomous_att_1,
                             autonomous_att_2,       autonomous_att_3,
                             autonomous_att_4,       autonomous_att_5,
                             autonomous_att_7,       autonomous_att_8,
                             transit_freq,           tnc_freq,               
                             work_distance,          work_days,              
                             school_distance))
df2 <- subset(df, select = c(num_hh_vehicles,        household_members_1,    
                             household_members_2,    household_members_3,    
                             household_members_4,    autonomous_att_1,
                             autonomous_att_2,       autonomous_att_3,
                             autonomous_att_4,       autonomous_att_5,
                             autonomous_att_7,       autonomous_att_8,
                             transit_freq,           tnc_freq,               
                             work_distance,          work_days,              
                             school_distance))
df1[] <- lapply(df1[],as.factor)
df <- cbind(df1, df2); rm(df1); rm(df2)
summary(df)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Exploratory factor analysis (EFA) ##########################################
################################################################################
# packages
library(parameters)
library(psych)
library(GPArotation)

# columns of interest
df1 <- subset(df,select = c(autonomous_att_1, autonomous_att_2, autonomous_att_3,
       autonomous_att_4, autonomous_att_5, autonomous_att_7, autonomous_att_8))

# factor analysis suitability (Sphericity and KMO test)
check_factorstructure(df1)

# no. of factors 
fa.parallel(df1, cor = "poly")

# fit an EFA
cor <- polychoric(df1); cor
efa <- fa(df1, nfactors = 2, rotate = "oblimin", cor = "poly", n.iter = 10)
print(efa$loadings, cutoff = 0.3); rm(efa); rm(cor); rm(df1)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Structural Equation Modeling ###############################################
################################################################################
# packages
library(lavaan)

# fit a measurement equation model
model <- 
  '
  factor1  =~    autonomous_att_1 + autonomous_att_3 + autonomous_att_5 + 
                 autonomous_att_7 + autonomous_att_8
  factor2  =~    autonomous_att_2 + autonomous_att_4
  '
fit  <-    cfa(model, data = df, estimator = "WLSM", ordered = TRUE); rm(model)
sink("Outputs/1.measurement.txt")
print(summary(fit, rsquare = TRUE, fit.measures=TRUE, standardized=TRUE))

# fit a structural equation model
model <- '
  factor1  =~    autonomous_att_1 + autonomous_att_3 + autonomous_att_5 + 
                 autonomous_att_7 + autonomous_att_8
  factor2  =~    autonomous_att_2 + autonomous_att_4
  factor1  ~     age_grp_2              + age_grp_3           + 
                 region_3               + num_hh_vehicles     + 
                 household_members_2    + household_members_3 + 
                 modes_used_2           + modes_used_6        +
                 modes_used_7           + modes_used_9        + 
                 modes_used_12          + autonomous_aware_2  + 
                 autonomous_aware_3     + income_4            + 
                 income_5               + gender              +
                 work_mode_5            + race_2                             
  factor2 ~      age_grp_2              + age_grp_3           + 
                 region_5               + num_hh_vehicles     + 
                 modes_used_2           + modes_used_4        +
                 modes_used_6           + modes_used_7        +
                 modes_used_12          + autonomous_aware_2  + 
                 autonomous_aware_3     + income_4            + 
                 gender                 + employment_4        + 
                 education_2            + race_2              +
                 race_7'

fit1  <-    cfa(model, data = df, estimator = "WLSM", 
            ordered = c('autonomous_att_1', 'autonomous_att_2', 
                        'autonomous_att_3', 'autonomous_att_4', 
                        'autonomous_att_5', 'autonomous_att_7', 
                        'autonomous_att_8')); rm(model)
options(max.print = 100000000)
sink("Outputs/2.structural.txt")
print(summary(fit1, rsquare = TRUE, fit.measures=TRUE, standardized=TRUE))
rm(fit1)


# Join predicted CFA outputs to main df
df1 <- as.data.frame(lavPredict(fit)); rm(fit)
df <- cbind(df, df1); rm(df1)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### multivariate ordered probit model ##########################################
################################################################################
# packages
library(mvord)

# make the outcomes variables ordered
df$autonomous_hhveh     <- as.ordered((df$autonomous_hhveh))
df$autonomous_rideshare <- as.ordered((df$autonomous_rideshare))
df$autonomous_pooled    <- as.ordered((df$autonomous_pooled))
df$autonomous_pref      <- as.ordered((df$autonomous_pref))

# fit null model
res0 <- mvord(formula = MMO2(autonomous_hhveh, autonomous_rideshare,
        autonomous_pooled, autonomous_pref) ~ 0,
        threshold.constraints = c(1, 2, 3, 4),
        coef.constraints =      c(1, 2, 3, 4),
        link = mvprobit(), data = df)
summary(res0); rm(res0)

# fit final model with exogenous and latent variables 
res <- mvord(formula = MMO2(autonomous_hhveh, autonomous_rideshare,
                            autonomous_pooled, autonomous_pref) ~ 0 +
       age_grp_2              + age_grp_3           + region_3               +
       region_5               + region_7            + future_decision_role_2 +
       future_decision_role_3 + num_hh_vehicles     + household_members_1    +
       modes_used_2           + modes_used_3        + modes_used_4           + 
       modes_used_6           + modes_used_7        + modes_used_8           + 
         
       modes_used_12          + autonomous_aware_2  + autonomous_aware_3     +
       housing_6              + income_3            + income_4               + 
       gender                 + employment_3        + employment_4           +
       education_2            + education_3         + drive_freq_3           +
       drive_freq_4           + transit_freq        + job_type_3             +
       
       work_mode_2            + work_mode_3         + work_days              +
       ethnicity              + race_2              + race_3                 +
       race_7                 + factor1             + factor2,
             
       threshold.constraints = c(1, 2, 3, 4), coef.constraints = cbind(
       c(1, NA, NA, 4),         c(1, NA, 3, 4),        c(NA, NA, 3, NA),               
       c(NA, NA, 3, NA),        c(NA, NA, NA, 4),      c(NA, 2, NA, NA),
       c(NA, 2, NA, NA),        c(NA, 2, NA, NA),      c(NA, NA, NA, 4),
       c(NA, 2, 3, NA),         c(NA, NA, 3, NA),      c(NA, NA, NA, 4),               
       c(NA, NA, 3, NA),        c(1, 2, 3, 4),         c(NA, NA, 3, 4), 
       
       c(NA, 2, 3, NA),         c(1, 2, NA, 4),        c(1, NA, NA, 4), 
       c(NA, 2, NA, NA),        c(1, NA, 3, NA),       c(1, NA, 3, NA),
       c(NA, 2, 3, NA),         c(NA, NA, 3, NA),      c(1, NA, NA, NA),              
       c(NA, 2, 3, NA),         c(NA, 2, NA, NA),      c(1, NA, NA, NA),         
       c(NA, 2, NA, NA),        c(1, NA, NA, NA),      c(1, NA, NA, NA),  
       
       c(NA, NA, 3, NA),        c(NA, NA, 3, 4),       c(1, NA, NA, NA),
       c(1, NA, NA, NA),        c(NA, 2, NA, NA),      c(NA, 2, NA, NA),
       c(NA, 2, NA, NA),        c(1, 2, 3, 4),         c(1, 2, 3, 4)),
       link = mvprobit(), data = df)
options(max.print = 100000000)
sink("Outputs/3.mv_probit.txt")
print(summary(res))
closeAllConnections()
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Partial pseudo elasticity calculation ######################################
################################################################################

# all individuals are 65+ years old
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 - 0.505 * ifelse(df1$age_grp_1 == 1, 1, 0) -
                       (0.505-0.325) * ifelse(df1$age_grp_2 == 1, 1, 0) 
df1$factor2         <- df1$factor2 + 0.135 * ifelse(df1$age_grp_1 == 1, 1, 0) -
                       (0.135-0.086) * ifelse(df1$age_grp_2 == 1, 1, 0) 
df1$age_grp_1[df1$age_grp_1 == 1] <- 0
df1$age_grp_2[df1$age_grp_2 == 1] <- 0
df1$age_grp_3[df1$age_grp_3 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

# all individuals are sole decision maker for vehicle purchase
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$future_decision_role_1[df1$future_decision_role_1 == 0] <- 1
df1$future_decision_role_2[df1$future_decision_role_2 == 1] <- 0
df1$future_decision_role_3[df1$future_decision_role_3 == 1] <- 0
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

# no. of household vehicles increase by 20%
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 - 0.049 * 0.2 
df1$factor2         <- df1$factor2 + 0.037 * 0.2
df1$num_hh_vehicles <- 1.2 * df1$num_hh_vehicles
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

# no. of <5 years old household increase by 20%
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                                                      newdata = df))
df1                 <- df
df1$household_members_1 <- 1.2 * df1$household_members_1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                                                      newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)


# no. of 5-11 years old household increase by 20%
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 + 0.092 * 0.2 
df1$household_members_2 <- 1.2 * df1$household_members_2
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

# no. of 12-16 years old household increase by 20%
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                      newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 + 0.177 * 0.2 
df1$household_members_3 <- 1.2 * df1$household_members_3
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

# all individuals are experienced with public bus 
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 + 0.167 * ifelse(df1$modes_used_2 == 0, 1, 0) 
df1$factor2         <- df1$factor2 - 0.114 * ifelse(df1$modes_used_2 == 0, 1, 0)
df1$modes_used_2[df1$modes_used_2 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

# all individuals are experienced with light rail/tram/subway
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$modes_used_3[df1$modes_used_3 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

# all individuals are experienced with commuter train 
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor2         <- df1$factor2 - 0.069 * ifelse(df1$modes_used_4 == 0, 1, 0)
df1$modes_used_4[df1$modes_used_4 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals are experienced with rental car
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 - 0.074 * ifelse(df1$modes_used_6 == 0, 1, 0) 
df1$factor2         <- df1$factor2 + 0.079 * ifelse(df1$modes_used_6 == 0, 1, 0)
df1$modes_used_6[df1$modes_used_6 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals are experienced with ride-hailing
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 + 0.272 * ifelse(df1$modes_used_7 == 0, 1, 0) 
df1$factor2         <- df1$factor2 - 0.160 * ifelse(df1$modes_used_7 == 0, 1, 0)
df1$modes_used_7[df1$modes_used_7 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals are experienced with shared ride-hailing 
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$modes_used_8[df1$modes_used_8 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals are experienced with carsharing 
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 + 0.340 * ifelse(df1$modes_used_9 == 0, 1, 0) 
df1$modes_used_9[df1$modes_used_9 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals are experienced with peer-to-peer car rental
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 + 0.264 * ifelse(df1$modes_used_12 == 0, 1, 0) 
df1$factor2         <- df1$factor2 + 0.162 * ifelse(df1$modes_used_12 == 0, 1, 0)
df1$modes_used_12[df1$modes_used_12 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals are very familiar with AV
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 + 
                       0.454 * ifelse(df1$autonomous_aware_1 == 1, 1, 0) -
                       (0.454-0.165) * ifelse(df1$autonomous_aware_2 == 1, 1, 0) 
df1$factor2         <- df1$factor2 - 
                        0.338 * ifelse(df1$autonomous_aware_1 == 1, 1, 0) -
                       (0.338-0.146) * ifelse(df1$autonomous_aware_2 == 1, 1, 0) 
df1$autonomous_aware_1[df1$autonomous_aware_1 == 1] <- 0
df1$autonomous_aware_2[df1$autonomous_aware_2 == 1] <- 0
df1$autonomous_aware_3[df1$autonomous_aware_3 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals housing is building with >= 20 apartments
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$housing_1[df1$housing_1 == 1] <- 0
df1$housing_2[df1$housing_2 == 1] <- 0
df1$housing_3[df1$housing_3 == 1] <- 0
df1$housing_4[df1$housing_4 == 1] <- 0
df1$housing_5[df1$housing_5 == 1] <- 0
df1$housing_6[df1$housing_6 == 0] <- 1
df1$housing_7[df1$housing_7 == 1] <- 0
df1$housing_8[df1$housing_8 == 1] <- 0
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals have income >= $150,000
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 + 0.133 * ifelse(df1$income_1 == 1, 1, 0) +
                       0.133 * ifelse(df1$income_2 == 1, 1, 0) +
                       0.133 * ifelse(df1$income_3 == 1, 1, 0) +
                       (0.133+0.115) * ifelse(df1$income_5 == 1, 1, 0) 
df1$factor2         <- df1$factor2 - 0.184 * ifelse(df1$income_1 == 1, 1, 0) -
                       0.184 * ifelse(df1$income_2 == 1, 1, 0) -
                       0.184 * ifelse(df1$income_3 == 1, 1, 0)
df1$income_1[df1$income_1 == 1] <- 0
df1$income_2[df1$income_2 == 1] <- 0
df1$income_3[df1$income_3 == 1] <- 0
df1$income_4[df1$income_4 == 0] <- 1
df1$income_5[df1$income_5 == 1] <- 0
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals are female
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 - 0.146 * ifelse(df1$gender == 0, 1, 0) 
df1$factor2         <- df1$factor2 + 0.069 * ifelse(df1$gender == 0, 1, 0)
df1$gender[df1$gender == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals don't work for pay
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor2         <- df1$factor2 + 0.075 * ifelse(df1$employment_4 == 0, 1, 0)
df1$employment_1[df1$employment_1 == 1] <- 0
df1$employment_2[df1$employment_2 == 1] <- 0
df1$employment_3[df1$employment_3 == 1] <- 0
df1$employment_4[df1$employment_4 == 0] <- 1
df1$employment_5[df1$employment_5 == 1] <- 0
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals have higher than graduate degree
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor2         <- df1$factor2 - 0.149 * ifelse(df1$education_2 == 1, 1, 0)
df1$education_1[df1$education_1 == 1] <- 0
df1$education_2[df1$education_2 == 1] <- 0
df1$education_3[df1$education_3 == 1] <- 0
df1$education_4[df1$education_4 == 0] <- 1
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals have driving license and drive everyday
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$license[df1$license == 0] <- 1
df1$drive_freq_1[df1$drive_freq_1 == 0] <- 1
df1$drive_freq_2[df1$drive_freq_2 == 1] <- 0
df1$drive_freq_3[df1$drive_freq_3 == 1] <- 0
df1$drive_freq_4[df1$drive_freq_4 == 1] <- 0
df1$drive_freq_5[df1$drive_freq_5 == 1] <- 0
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals (who work) job type is work from home
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$job_type_1[df1$job_type_1 == 1] <- 0
df1$job_type_2[df1$job_type_2 == 1] <- 0
df1$job_type_3[df1$job_type_3 == 0] <- 1
df1$job_type_4[df1$job_type_4 == 1] <- 0
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals transit trips increase by 20%
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$transit_freq <- 1.2 * df1$transit_freq
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals drive alone for commute
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 + 0.282 * ifelse(df1$work_mode_5 == 1, 1, 0)
df1$work_mode_1[df1$work_mode_1 == 0] <- 1
df1$work_mode_2[df1$work_mode_2 == 1] <- 0
df1$work_mode_3[df1$work_mode_3 == 1] <- 0
df1$work_mode_4[df1$work_mode_4 == 1] <- 0
df1$work_mode_5[df1$work_mode_5 == 1] <- 0
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals commute days per week increase by 20%
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                                                      newdata = df))
df1                 <- df
df1$work_days <- 1.2 * df1$work_days
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                                                      newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals are hispanic,latino, or spanish origin
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$ethnicity[df1$ethnicity == 1] <- 0
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

#all individuals are white
pr1                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df))
df1                 <- df
df1$factor1         <- df1$factor1 - 0.227 * ifelse(df1$race_2 == 1, 1, 0)
df1$factor2         <- df1$factor2 + 0.152 * ifelse(df1$race_2 == 1, 1, 0) + 
                       0.108 * ifelse(df1$race_7 == 1, 1, 0)
df1$race_1[df1$race_1 == 1] <- 0
df1$race_2[df1$race_2 == 1] <- 0
df1$race_3[df1$race_3 == 1] <- 0
df1$race_4[df1$race_4 == 1] <- 0
df1$race_5[df1$race_5 == 0] <- 1
df1$race_6[df1$race_6 == 1] <- 0
df1$race_7[df1$race_7 == 1] <- 0
pr2                 <- as.data.frame(marginal_predict(res, type = "all.prob", 
                       newdata = df1))
pseudo_elasticity_individual <- pr2 - pr1
pseudo_elasticity_average_per    <- 100 * colMeans(pseudo_elasticity_individual)
round(pseudo_elasticity_average_per,digits = 2)

rm(res); rm(pr2); rm(df1)
rm(pseudo_elasticity_individual); rm(pseudo_elasticity_average_per)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Policy implications based on latent variables###############################
################################################################################
# packages
library(dplyr)

# calculation 
df2             <- df[,c("factor1", "factor2")]
df2$factor1     <- factor(ntile(df2$factor1, 3), levels = 1:3, 
                   labels = c("low", "medium", "high"))
df2$factor2     <- factor(ntile(df2$factor2, 3), levels = 1:3, 
                   labels = c("low", "medium", "high"))
df3             <- cbind(df2, pr1); rm(df2); rm(pr1)

df.factor1      <- aggregate(. ~ factor1, data = subset(df3, 
                   select = -c(factor2)), mean)
df.factor2      <- aggregate(. ~ factor2, data = subset(df3, 
                   select = -c(factor1)), mean)
rm(df3)

df.factor1; rm(df.factor1)
df.factor2; rm(df.factor2)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Descriptive statistics######################################################
################################################################################
summary(df); rm(df)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

