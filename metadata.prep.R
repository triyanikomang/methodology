# Version: 2.0
#
# Date: October 29, 2025

# Authors: Paraskevi Massara (email: massaraevi@yahoo.com), and edited by Isabella Oberto Monasterios
#
# OBJECTIVES: Preprocess MiGrowD data to prepare them for data cleaning, 
# and generate complete metadata file with family demographics to calculate descriptive statistics

# NOTE: This script is dependent on the ASA24 cleaning repository and the 1a.dietdata_preparation.R script. 
# For simplicity an intermediate file is provided. The data is not publicly available due to privacy concerns.

# RESOURCES:

#TOC> ==========================================================================
#TOC>
#TOC>   Pipeline Section
#TOC> --------------------------------------------------------------------------
#TOC>   1        Packages and Dependencies
#TOC>   2        Data
#TOC>   3        Pre-processing and Metadata
#TOC>   4        Descriptive Statistics
#TOC> ==========================================================================


# ====   1   Packages and Dependencies   =======================================

source("~/GitHub/MiGrowD/Code/HEI2015.R")

if (!exists("diet") && !exists("ASA24_linkage")) {
  source("~/GitHub/MiGrowD/Code/1a.dietdata_preparation.R")
}

# Packages needed
packages <- c("ggplot2", "dplyr", 'sas7bdat', "lubridate", "arsenal",'vtable', "janitor",
              "gtsummary", "readxl", 'tidyr')

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


## ====   2   Data   ===========================================================

# Participant data for metadata preparation
subject <-subj_2022_05_26 #read.sas7bdat("G:/ASA24/subj_2022_05_26.sas7bdat")
original_migrowd <-MiGrowD_DATA_2022_06_16_1431 #read.csv("G:/ASA24/MiGrowD_DATA_2022-06-16_1431.csv") # original file (REDCap)
recruitment <-merged_participants_25_Apr_2025_5_ #read.csv("G:/MiGrowD/merged_participants_25-Apr-2025.csv") # merged recruitment file Isabella and Triyani worked on
unimputed_crossectional_parents <-unimputed_crossectional_parents #readRDS("G:/MiGrowD/Parents_data/unimputed_crossectional_parents.rds") #demographics
tk_clusters <-mg_df #read.csv("G:/ASA24/mg_df.csv")
cagef_subjects <-MiGrowD_SampleAssignment_Comelli_TK_CAGEF #read_xlsx("~/GitHub/MiGrowD/Useful/MiGrowD_SampleAssignment_Comelli_TK_CAGEF.xlsx") %>% 
names(cagef_subjects)
cagef_subjects <- cagef_subjects %>%
  rename(sample_id = Comelli_Sample_ID, 
         subject = TK_Sample_ID, 
         CAGEF_id = `CAGEF_Sample_#`) # standardizing column names, 111 obs
diet <-totals_FB_only #readRDS("G:/MiGrowD/totals_FB_only.rds") # 251 obs, 135 variables, file created in dietdata script
stool_samples <-stool_samples #read.csv("G:/ASA24/stool_samples.csv") #subjects that collected stool samples (104)
instance <-instance_2022_05_26 #read.sas7bdat("G:/ASA24/instance_2022_05_26.sas7bdat")
tk_data <-df2 #readRDS("G:/MiGrowD/Parents_data/df2.rds") # this file contains all historical TK data available for these children
data_labels <-MiGrowD_DATA_LABELS_2022_06_16_1431 #read.csv("~/GitHub/MiGrowD/Data/MiGrowD_DATA_LABELS_2022-06-16_1431.csv") # legend for varibles

### ====   3   Pre-processing and Metadata   ===================================

# ---- 3.1 Subject Data (DOB) ----
# Step 1: Concatenate and reformat age in the original TK dataset to match with the ASA24 output

# Standardizing dates and creating a new column for the full date of birth in subject database 
subject_DOB <- subject %>% 
  mutate(
    CHILDDOBDT1_MM = sprintf("%02d",                      # add a leading 0, having 2 digits 
                             as.numeric(CHILDDOBDT1_MM)), # convert to a numeric object to be able to concatenate
    date_birth = paste0("15", CHILDDOBDT1_YYYY),          # adding 15 as the day (since we don't know the exact birth date)
    date_birth = paste0(CHILDDOBDT1_MM, date_birth),      # concatenating the birth month 
    date_birth = mdy(date_birth, truncated = 0),
    date_birth = format(date_birth, "%Y-%m-%d"))          # ensuring the format is YYYY-MM-DD


# ---- 3.2 Date of Enrollment ----
# Step 2: Include date of enrollment from MiGrowD REDCap data export

# Standardizing the date of enrollment
migrowd_date <- original_migrowd %>% 
  rename(enrollment_date = date_of_enrollment) %>% 
  mutate(enrollment_date = ymd(enrollment_date, truncated = 0), # Turning date into a date object 
         enrollment_date = format(enrollment_date, "%Y-%m-%d")) # Formatting the date into YYYY-MM-DD

# ---- 3.3 Recruitment Data ----
# Step 3: Merging original migrowd file with new recruitment file 

# Standardizing the recruitment files
recruitment <- recruitment[,-1] %>% # removing the row numbers column
summary(recruitment)

migrowd_date <- migrowd_date %>% 
summary(migrowd_date)

# Concatenating the value inside column ...57 into comments for row 62
recruitment$Comments[62] <- paste(
  recruitment$Comments[62], recruitment[62, "...58"])

# Deleting column ...57 (empty for all subjects) and duplicate columns 
common_cols <- intersect(names(recruitment), names(migrowd_date))
common_cols <- c("paq_boys_complete", 
                 "paq_girls_complete",  
                 "date_anthropometry", 
                 "exclusion_antibiotics", 
                 "exclusion_laxatives",  
                 "exclusion_probiotics", 
                 "exclusion_gidisorders", 
                 "...58", 
                 "date_of_enrollment")

recruitment_final <- recruitment %>% 
  select(-all_of(common_cols)) %>% 
  rename(asa_issues = asa_isues) # 253 obs, 56 variables


# Merging old migrowd file and new merged recruitment log 
merged_migrowd <- left_join(recruitment_final, migrowd_date,
                            by = "subject") # 253 subjects 191 variables 

# Removing repeated or empty columns in merged file
colnames(merged_migrowd)

# ZIP CODE: 2 variables zip_code and postal_code 
postal_zip <- merged_migrowd$zip_code == merged_migrowd$postal_code # only 1 FALSE 
print(postal_zip)

# Keeping only one postal_code column, checking no values get overwritten or deleted 
merged_migrowd %>% filter(postal_zip == FALSE) %>% 
  pull(subject) # 26-1361441 has two different zip codes values 

# Extracting the values for subject 26-1361441
merged_migrowd %>% filter(subject == "26-1361441") %>% 
  select("zip_code", "postal_code") # postal_code is empty 

# Merging the two postal code columns in one (now 190 variables)
merged_migrowd <- merged_migrowd %>% 
  mutate(post_code = ifelse(is.na(zip_code),      # if zip_code is empty
                            postal_code,          # use the value in postal_code
                            zip_code),            # if not, use the value in zip_code
         post_code = ifelse(post_code == "",      # if the postal code is empty
                            NA,                   # replace with NA 
                            post_code)) %>%       # if not, keep the value already in post_code
  select(-c(zip_code, postal_code))               # removing the repeated zip code columns, keeping only post_code 

# STOOL COLLECTION DATE: 2 variables stool_collected and stool_collection_date 
merged_migrowd <- merged_migrowd %>% 
exists("merged_migrowd")
colnames(merged_migrowd)
View(merged_migrowd)
  mutate(stool_collected_date = ifelse(is.na(stool_collected_date) | stool_collected_date == "",
                                       stool_collected,        # if stool collection is empty or NA, use value in stool_collected (from new recruitment file)
                                       stool_collection_date), # if it has a value, keep stool_collection_date
         stool_collected_date = ymd(stool_collection_date)) %>% # turning into a Date object
# removing repeated columns from original files (189 variables)


# One observation had to be manually reviewed due to discrepancies in dates
merged_migrowd %>% filter(stool_collected_date == "2021-09-01") %>% 
  pull(subject) # 26-2490228 dates did not match 

# ----- #(! MISSING)
# # ENROLMENT DATE: 2 variables enrol_date and enrollment_date (pending)
# diff_enroldates <- merged_migrowd %>% 
#   filter(enrol_date != enrollment_date) %>% 
#   pull(subject)
# 
# unique(diff_enroldates) # 39 subjects have different dates 

merged_migrowd2 <- merged_migrowd %>%
  mutate(enrol_date = coalesce(enrol_date, enrollment_date))

# Keep the enrollment dates from recruitment file only
merged_migrowd <- merged_migrowd %>% select(-enrollment_date)

------
  
  # Reorganizing column order 
  subject_info <- c("subject", "subject_status", 
                    "active_participant", 
                    "study_status_complete")

enrollment_info <- c("enrol_date", "site",
                     "recruit_type", "recruit_code", "hospital", 
                     "ra_approached_date", "recruit_issues",
                     "method_enrollment",  
                     "redcap_data_access_group",                   
                     "participant_id_complete",                    
                     "clinical_site",                              
                     "date_initial_screening",
                     "enrollment_complete")

stool_info <- c("stool_collection_status", 
                "stool_collected_date",
                "stool_received", "sample_issues",
                "stool_given_date", "stool_1st_reminder",                         
                "stool_2nd_reminder",                         
                "stool_3rd_reminder",                      
                "stool_rec_ms",                               
                "stool_status___1",                           
                "stool_status___2",                           
                "stool_status___3",                           
                "stool_status___4",                           
                "notes_flup_stool_samples",                   
                "stool_samples_tracking_complete","kit_mailed_given___1",                       
                "kit_mailed_given___2",                      
                "kit_mailed_given___3")

ant_info <- c("ant", "ant_date", "date_anthropometry", 
              "child_weight_kg", "child_weight",                           
              "child_height_cm", "child_height",                        
              "child_ab_cm", "child_waist_circumference",                               
              "par_weight_kg", "mother_weight",                             
              "par_height_cm", "mother_height",                             
              "par_ab_cm", "mother_waist_circumference",
              "Ant")

asa_info <- c("asa1_sent",                                  
              "asa1_completed",                             
              "asa1_remind_ct",                             
              "asa1_remind_dates",                          
              "asa1_status",                                
              "asa2_sent",                                  
              "asa2_completed",                             
              "asa2_remind_ct",                             
              "asa2_remind_dates",                          
              "asa2_status",
              "asa_issues")

contact_info <- c("kit_sent",                                   
                  "initial_contact_date", "initial_contact_time",                       
                  "preferred_time", "email_or_call",  
                  "preconsent", "preconsent_complete",
                  "consented",  "rescreened_or_reassented",                   
                  "assent_discussion", "assent_returned",                            
                  "remote_assent", "remote_assent_time",
                  "followup_issues",
                  "contact_attempts", 
                  "followup_dates",                             
                  "withdrew",                                   
                  "withdrawal_reason")

health_info <- c("health_conditions",                          
                 "prebiotics", "prebiotics_duration",                        
                 "prebiotics_type", "supplements",                                
                 "type_supplements___1", "type_supplements___2",                       
                 "type_supplements___3", "type_supplements___4",                       
                 "type_supplements___5", "type_supplements___6",                       
                 "type_supplements___7", "other_supplements",                          
                 "special_diet", "type_of_restriction_diet",                   
                 "duration_diet_restriction",                  
                 "autism", "adhd",                                       
                 "prematurity", "physician_pub_develop",                      
                 "age_mother_frperiod", 
                 "exclusion_antibiotics", "exclusion_laxatives",                        
                 "exclusion_probiotics", "exclusion_gidisorders",                      
                 "past_temporal_exclusion", 
                 "temporalily_excluded", 
                 "temp_exc", "date_initial_exclusion",                     
                 "reasons_temp_exclusion___1", "reasons_temp_exclusion___2",                 
                 "reasons_temp_exclusion___3",                 
                 "reasons_temp_exclusion___4", "screening_complete")

paq_info <- c("pub_assess",
              "paq_b_tsp_devstage", "paq_b_pbchr_stage",                          
              "paq_boys_complete", "paq_g_period",                               
              "paq_g_brst_devstage", "paq_g_pbchr_stage",                          
              "paq_girls_complete")

status_info <- c("dropout", "date_drop_or_completed",                     
                 "reasons_dropout", "withdrew",                                   
                 "withdrawal_reason",                          
                 "Comments", "Other")

homevisit_info <- c("home_visit", "date_home_visit", 
                    "participants_want_hv", "date_past_visit_clinic", 
                    "taks_home_visit___1", "taks_home_visit___2",                        
                    "taks_home_visit___3", "taks_home_visit___4",                        
                    "taks_home_visit___5", "taks_home_visit___6",                        
                    "tasks_missing___1", "tasks_missing___2",                          
                    "tasks_missing___3", "tasks_missing___4",                          
                    "tasks_missing___5", "tasks_missing___6",                          
                    "tasks_missing___7", "tasks_missing___8",                          
                    "home_visit_complete")

personal_info <- c("first_name", "last_name", "sex",                                        
                   "parent_name", "email", "phone_number",                               
                   "home_address", "city", "post_code",                                  
                   "participant_and_parent_information_complete",
                   "inclusion_age")

other <- c("number_screenings", "gift_card",                                  
           "certificate", "gift_cards",                                 
           "gift_cards_date", "protocol_deviation",                         
           "type_protocol_deviation___1",                
           "type_protocol_deviation___2",                
           "type_protocol_deviation___3",                
           "type_protocol_deviation___4",                
           "type_protocol_deviation___5",                
           "type_protocol_deviation___6",                
           "type_protocol_deviation___7",                
           "type_protocol_deviation___8",                
           "type_protocol_deviation___9",                
           "other_protocol_deviation",                   
           "deviation_date_1", "deviation_description_1",                    
           "deviation_date_2", "deviation_description_2",                    
           "deviation_date_3", "deviation_description_3",                    
           "protocol_deviations_complete", "items_missing___1",                          
           "items_missing___2", "items_missing___3",                          
           "items_missing___4", "items_missing___5",                          
           "items_missing___6", "items_missing___7", 
           "study_email")

all_variables <- c(subject_info, enrollment_info, stool_info, 
                   ant_info, asa_info, contact_info,
                   health_info, paq_info, status_info, 
                   homevisit_info, personal_info, other)

# Reorganizing column order 
migrowd_final <- merged_migrowd %>% 
  select(all_of(all_variables)) 

# Verifying no columns were skipped 
not_in_data <- setdiff(colnames(merged_migrowd), all_variables)
print(not_in_data) # all variables are included in the final file 

# Saving this file as a CSV (no demographic data)
migrowd_finalz <- migrowd_final
View(migrowd_finalz)
write.csv(migrowd_finalz, "migrowd_finalz.csv", row.names = FALSE)
  

# ---- 3.4 Pubertal and Anthropometric Data ----
# Step 4: Calculate pubertal development score

# As in Lorena's paper "children were grouped into three categories corresponding to Tanner
# Stages 1-2, Stage 3, and Stages 4-5 excluding pubic hair questions."

# Creating a new column for pubertal score, based on the 3 categories above 
migrowd_final2 <- migrowd_finalz %>% 
  mutate(pubertal_score = case_when(                                        # creating a new variable pubertal_score 
    paq_b_tsp_devstage %in% c(1, 2) | paq_g_brst_devstage %in% c(1, 2) ~ 1, # if dev stage is 1 or 2, assign pubertal score 1
    paq_b_tsp_devstage == 3 | paq_g_brst_devstage == 3 ~ 2,                 # if dev stage is 3, assign pubertal score 2
    paq_b_tsp_devstage %in% c(4, 5) | paq_g_brst_devstage %in% c(4, 5) | 
      paq_g_period == 1 ~ 3)) %>%                                           # all other stages (4 or 5) or having a period, assign pubertal score 3
  select(subject:paq_girls_complete, pubertal_score,                        # reorganizing column order, to have pubertal_score with pubertal data 
         dropout:study_email)                                               # 253 obs, 190 variables

# Merging migrowd data (now with pubertal score) with tk clusters and subject data (with date of birth calculated )
migrowd_final2 <- migrowd_final2 %>% 
  left_join(tk_clusters, by = "subject") %>% # 2 extra columns - "class" and "label"
  left_join(subject_DOB, by = "subject") # 253 subjects, 234 columns 

# Calculate age at enrollment
migrowd_final2 <- migrowd_final2 %>% 
  mutate(date_birth = ymd(date_birth),      # convert to Date object (YYYY-MM-DD)
         enrol_date = ymd(enrol_date),      # convert to Date object (YYYY-MM-DD)
         age_enrollment = round(as.numeric( # calculating age at enrollment in months 
           difftime(enrol_date, date_birth, units = "days")) / (365.25 / 12))) # 235 columns now 


# ---- 3.5 Demographic Data ----
# Step 5: Adding parental demographic data using unimputed cross-sectional data 

# Add the most recent NHQ data, concatenating a 26- in front of the subject ID (missing)
unimputed_crossectional_parents$subject <- paste0("26-", unimputed_crossectional_parents$subject) #6023 obs and 93 variables

# Adding the columns of interest that are missing in migrowd_final
keep4 <- c(
  "subject", 
  "EDUCATIONLEVEL", 
  "MOTHER_LEVEL_EDUCATION", 
  "FAMILY_INCOM", 
  "TIMEBREASTFED", 
  "NUTRITIONSCORE", 
  "HOURSSLEEPING_145_216", 
  "HOURSSLEEPING_61_144")

# Subsetting parent data 
parents <- unimputed_crossectional_parents %>% 
  select(all_of(keep4))

# Creating a merged dataframe with migrowd information and parental demographics 
migrowd_final2 <- left_join(migrowd_final2, parents, by = "subject") # 253 subjects, 242 variables      


# ---- 3.6 TK Data ----
# Step 6: Adding TK historical data 

# Cleaning tk_data file (originally 24906 obs, 99 variables)
tk_data$subject <- paste0("26-", tk_data$subject) # Adding a 26- before each subject ID (missing) using the column name instead of column index 
print(n_distinct(tk_data$subject)) # 6023 unique subject IDs

# Subsetting tk_data to include only the subject IDs present in the migrowd dataset 
tk_migrowd <- tk_data[which(tk_data$subject %in% migrowd_final2$subject), ] # 1055 obs, 99 variables 
print(n_distinct(tk_migrowd$subject)) # 177 (unique) subjects

# Standardizing data in tk_migrowd
tk_migrowd <- tk_migrowd %>% 
  mutate(instancedate = as.Date(instancedate)) %>% # Ensure the instancedate column is of Date type
  filter(!is.na(instancedate)) # Remove rows with NA instancedate, 1055 obs, 99 variables

# Select the most recent instance for each subject
most_recent_instances <- tk_migrowd %>%
  group_by(subject) %>%
  filter(instancedate == max(instancedate, na.rm = TRUE)) %>% # keeping "max" instance date
  ungroup() %>%
  as.data.frame() # Ensure the result is a data frame, 177 obs, 99 variables

# Subsetting instance data set to keep only the columns we will merge with migrowd file 
keep6 <- c("subject","instancedate","ZBMI","PARENTBMI")
most_recent_instances <- most_recent_instances[keep6]

# Final metadata file for all enrolled participants (n = 253)
migrowd_final3 <- left_join(migrowd_final2, most_recent_instances, by = "subject") # 253 obs, 245 variables
write.csv(migrowd_final3, "migrowd_final3.csv", row.names = FALSE)

# ---- 3.7 Diet Data ----
# Step 7: Adding basic diet data to migrowd file 

# NOTE: Diet file was created in the 1a.dietata_preparation.R script, the one currently used 
# is the previously existing one (there is a new file "totals_new.rds" not yet incorporated)
diet <- #readRDS("G:/MiGrowD/totals_new.rds")

# Subsetting diet file to contain only nutrients of interest
nutrients <- diet %>% 
  select(c("subject":"RecallNo", "KCAL":"A_DRINKS")) # 229 obs, 105 columns

# Turning all columns into numeric factors (to be able to run HEI function below)
nutrients <- nutrients %>% 
  mutate(across(4:ncol(nutrients), as.numeric))

# Calculate HEI per child and per recall and basic diet data  
HEI_ASA <- HEI2015(nutrients) # 229 obs, 16 variables

# ! date of enrollment variable needs to be updated once this is finalized 

# Merging final_diet file with HEI dataset 
final_diet <- left_join(diet, HEI_ASA, by = c("subject","RecallNo")) # 149 columns
# saveRDS(final_diet,"G:/MiGrowD/final_diet.rds")

# ---- 3.8 Growth Cluster Data  ----


# Merging obese and overweight clusters (269 variables now)
migrowd_final3 <- migrowd_final3 %>% 
  mutate(merged_class = ifelse(is.na(class.x), NA, # only for values that are NOT NA 
                               ifelse(class.x == 1 | class.x == 2, 
                                      2,   # if class is 1 or 2, group into 2
                                      1))) # anything else (except NA values), class should be 1 

class(migrowd_final3$class)
str(migrowd_final3$class)
View(migrowd_final3)

saveRDS(migrowd_final3, "migrowd_final.rds")

saveRDS(migrowd_final3,"G:/MiGrowD/migrowd_final.rds")---
  # ! MISSING - go through Date_collection and stool_collection_date columns (repeated)
  colnames(migrowd_clusters)
stool_collections <- migrowd_clusters$Date_collection == migrowd_clusters$stool_collected_date
print(stool_collections) # all dates are different, will have to be manually inspected 
---
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Subsetting anthropometric and pubertal data 
  ant_paq_variables <- c(
    "subject",
    "inclusion_age",
    "exclusion_antibiotics",
    "exclusion_laxatives",
    "exclusion_probiotics",
    "exclusion_gidisorders",
    "enrollment_date", # correct name after merging enrollment dates 
    "paq_b_tsp_devstage",
    "paq_b_pbchr_stage",
    "paq_boys_complete",
    "paq_g_period",
    "paq_g_brst_devstage",
    "paq_g_pbchr_stage",
    "paq_girls_complete",
    "date_anthropometry",
    "child_weight",
    "child_height",
    "child_waist_circumference",
    "mother_weight",
    "mother_height",
    "mother_waist_circumference")

# Creating a subset only for anthropometry and pubertal data 
ant_paq <- migrowd_finalz %>% 
  select(all_of(ant_paq_variables)) %>%         # keeping only the columns of interest in keep vector 

  rename(paq_brs_dev = paq_g_brst_devstage,     # renaming columns
         paq_period = paq_g_period,
         paq_tsp = paq_b_tsp_devstage,
         child_weight_enrollment = child_weight, 
         child_height_enrollment = child_height) %>%
  mutate(subject = as.character(subject))        # turning subject ID into a character, 253 subjects, 21 variables




# ================ 3. Inspect migrowd subjects  ================================

# Checking all children that collected stool sample are within age range
out_age_range <- migrowd_stool[which(migrowd$inclusion_age==2), ] # the children with stool sample are within range

migrowd %>%
  filter(!is.na(stool_collected_date)) %>%  # Only children with stool samples
  summarise(
    min_age = min(age_at_stool_collection, na.rm = TRUE),
    max_age = max(age_at_stool_collection, na.rm = TRUE),
    mean_age = mean(age_at_stool_collection, na.rm = TRUE),
    n_children = n()
  )
# Create a summary table for descriptive statistics
tbl_summary(migrowd_stool)
?tbl_summary(migrowd_stool %>% select(-Comments))
tbl_summary(migrowd %>% select(-date_of_enrollment))


print(length(migrowd_kids)) # migrowd_kids missing?

unimputed_longitudinal_parents <- readRDS("/Volumes/My Passport/MiGrowD Feasibility study/MiGrowD/Parents_data/unimputed_longitudinal_parents.rds")
print(n_distinct(unimputed_longitudinal_parents$subject)) # 6023 unique subjects
print(n_distinct(unimputed_crossectional_parents$subject)) # 6023 unique subjects
imputed_crossectional_parents <- readRDS("/Volumes/My Passport/MiGrowD Feasibility study/MiGrowD/Parents_data/imputed_crosssectional_parents.rds")
print(n_distinct(imputed_crosssectional_parents$subject)) # 6023 unique
growth_longitudinal_parents <- readRDS("/Volumes/My Passport/MiGrowD Feasibility study/MiGrowD/Parents_data/growth_longitudinal_parents.rds")
print(n_distinct(growth_longitudinal_parents$subject)) # 6022 unique subjects


# ================ 2. Subset for all migrowd subjects  =========================

growth_longitudinal_parents_migrowd <- growth_longitudinal_parents[which(growth_longitudinal_parents$subject %in% migrowd_kids_num), ]
print(n_distinct(growth_longitudinal_parents_migrowd$subject)) # 164 subjects

subject_migrowd <- subject[which(subject$subject %in% migrowd_kids), ]
n_distinct(migrowd$subject) # 233 subjects
fullterm <- migrowd_finalz[which(migrowd$GESTATION_AGE == ">=37 weeks"), ]
n_distinct(fullterm$subject) # 179 subjects
normal_birthweight <- subject_migrowd[which(subject_migrowd$birthweight_kilo >= 1.5), ]
n_distinct(normal_birthweight$subject) # 212 subjects
length(intersect(normal_birthweight$subject, fullterm$subject)) # subject 174

length(intersect(metadata3$subject, fullterm$subject)) # 86 subjects
length(intersect(metadata3$subject, normal_birthweight$subject)) # 97 subjects
not_adopted <- subject_migrowd[-which(subject_migrowd$ADOPTED_YN == "Yes"), ]
n_distinct(not_adopted$subject) # 232 subjects


#### ====   4   Descriptive Statistics    ======================================

# ---- 4a. Subject -----
subject_migrowd <- mutate_if(subject_migrowd, is.character, as.factor)
summary(subject_migrowd)
n_distinct(subject_migrowd$fam_id)
nrow(subject_migrowd[-which(subject_migrowd$fam_id == ""), ])
table(subject_migrowd$NHQ_GESTATION_AGE)
table(subject_migrowd$FULLTERM_YN)
table(subject_migrowd$GESTATION_AGE)
sd(subject_migrowd$mom_birth_age, na.rm = T)
sd(subject_migrowd$dad_birth_age, na.rm = T)
sd(subject_migrowd$birthweight_kilo, na.rm = T)
table(subject_migrowd$dadethnicity)
table(subject_migrowd$momethnicity)

# ==================== 4b. TK =================================================
summary(tk_migrowd)
n_distinct(tk_migrowd$subject) # 177 distinct subject ids
mean(as.data.frame(tk_migrowd %>% group_by(subject) %>% tally())$n)
sd(as.data.frame(tk_migrowd %>% group_by(subject) %>% tally())$n)

# ============= 5 Stool samples ================================================

clusters <- left_join(subjects, tk_clusters)

# merge obese and overweight clusters

clusters$merged_class <- NA

for (i in 1:nrow(subjects)) {
  if (clusters$class[i] == 1 || clusters$class[i] == 2)
  {
    clusters$merged_class[i] <- 2
  }
  else {
    clusters$merged_class[i] <- 1
  }
}