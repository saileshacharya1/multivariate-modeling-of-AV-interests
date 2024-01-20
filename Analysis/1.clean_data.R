### Import, join, and keep the required columns#################################
################################################################################
# import the data
df_main <- read.csv("Data/Raw data/survey_res_main.csv")
df_pers <- read.csv("Data/Raw data/survey_res_person.csv")

# join two data frames for the respondents' personal details
df_pers <- df_pers[df_pers$relationship == 8,]
df <- merge(df_main, df_pers, by.x = "sampno"); rm(df_main); rm(df_pers)

# keep the required columns only
df <- subset(df, select = c(age_grp,                region, 
                            future_decision_role,   num_hh_vehicles,
                            household_members_1,    household_members_2, 
                            household_members_3,    household_members_4,
                            modes_used_1,           modes_used_2, 
                            modes_used_3,           modes_used_4, 
                            modes_used_5,           modes_used_6, 
                            modes_used_7,           modes_used_8, 
                            modes_used_9,           modes_used_10,       
                            modes_used_11,          modes_used_12,        
                            autonomous_aware,       autonomous_att_1, 
                            autonomous_att_2,       autonomous_att_3, 
                            autonomous_att_4,       autonomous_att_5, 
                            autonomous_att_7,       autonomous_att_8, 
                            autonomous_hhveh,       autonomous_rideshare,        
                            autonomous_pooled,      autonomous_pref, 
                            housing,                income,        
                            gender,                 employment, 
                            student,                education, 
                            license,                drive_freq,       
                            transit_freq,           tnc_freq, 
                            job_type,               work_mode, 
                            work_distance,          work_days,       
                            school_mode,            school_distance, 
                            ethnicity,              race_1, 
                            race_2,                 race_3,    
                            race_4,                race_5, 
                            race_6,                race_7))

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Manipulate individual columns###############################################
################################################################################
# age_grp
df$age_grp <- factor(df$age_grp, levels = c(2, 3, 4), labels = c(1, 2, 3))

# modes_used
df$modes_used_1 <-  factor(df$modes_used_1, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_2 <-  factor(df$modes_used_2, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_3 <-  factor(df$modes_used_3, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_4 <-  factor(df$modes_used_4, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_5 <-  factor(df$modes_used_5, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_6 <-  factor(df$modes_used_6, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_7 <-  factor(df$modes_used_7, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_8 <-  factor(df$modes_used_8, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_9 <-  factor(df$modes_used_9, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_10 <- factor(df$modes_used_10, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_11 <- factor(df$modes_used_11, levels = c(1, 2, 3, 4), 
                    labels = c(0, 0, 0, 1))
df$modes_used_12 <- factor(df$modes_used_12, levels = c(1, 2, 3, 4),
                    labels = c(0, 0, 0, 1))

# autonomous_aware
df$autonomous_aware <- factor(df$autonomous_aware, levels = c(1, 2, 3, 4), 
                       labels = c(1, 1, 2, 3))

# autonomous_hhveh
df <- df[!(is.na(df$autonomous_hhveh)),] #delete missing observations
df$autonomous_hhveh <- 4 - df$autonomous_hhveh #reverse coding
summary(df$autonomous_hhveh)

# autonomous_rideshare
df <- df[!(is.na(df$autonomous_rideshare)),] #delete missing observations

# autonomous_pooled
df <- df[!(is.na(df$autonomous_pooled)),] #delete missing observations
df$autonomous_pooled <- 5 - df$autonomous_pooled #reverse coding

# autonomous_pref
df <- df[!(is.na(df$autonomous_pref)),] #delete missing observations
df$autonomous_pref <- 5 - df$autonomous_pref #reverse coding

# income
df$income <- factor(df$income, levels = (1:11), 
             labels = c(1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 5))

# gender
df$gender <- factor(df$gender, levels = c(1, 2, 3, 4), labels = c(0, 1, 1, 1))

# education
df$education <- factor(df$education, levels = c(1:8), 
                labels = c(1, 2, 2, 3, 3, 3, 4, 4))

# license
df$license <- factor(df$license, levels = c(1, 2), labels = c(0, 1))

# drive_freq
df$drive_freq <- ifelse(is.na(df$drive_freq), 5, df$drive_freq)

# job_type
df$job_type <- ifelse(is.na(df$job_type), 5, df$job_type)

# work_mode
df$work_mode <- ifelse(is.na(df$work_mode), 19, df$work_mode)
df$work_mode <- factor(df$work_mode, levels = c(1:19), 
                labels =c(1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 5, 6))

# work_distance 
df$work_distance <- ifelse(is.na(df$work_distance), 0, df$work_distance)

# work_days
df$work_days     <- ifelse(is.na(df$work_days), 0, df$work_days)

# school_mode
df$school_mode <- ifelse(is.na(df$school_mode), 18, df$school_mode)
df$school_mode <- factor(df$school_mode, levels = c(1:18), 
                  labels =c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 2, 2, 2, 2, 5, 6))

# school distance
df$school_distance <- ifelse(is.na(df$school_distance), 0, df$school_distance)

# ethnicity
df$ethnicity <- factor(df$ethnicity, levels = c(1, 2, 3), labels = c(0, 1, 1))
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#


### Export cleaned data#########################################################
################################################################################
write.csv(df, "Data/Cleaned data/cleaned_data.csv")
saveRDS(df, "Data/Cleaned data/cleaned_data.rds")
rm(df)
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

