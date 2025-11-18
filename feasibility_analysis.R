# Date: September 26, 2025

# Authors: Triyani (email: triyanikomang.kartinawati@mail.utoronto.ca),
#
# OBJECTIVES: Preparing for Feasibility Study
# SOURCE: migrowd_demographics
setwd("C:/Users/ameth/Desktop/TARGet Kids!/FEASIBILITY STUDY/Feasibility")

## ----------- Check on Complete Sample-------------------------------------------
## Sample size = 300 samples. With initial screening at TK!:

View(migrowd_demographics)
summary(migrowd_demographics) #length = 253 (sample size = 253)

## Creating data frame for analysis ----------------------------------------------
demographics <- migrowd_demographics
demographics

### Calculating numbers of lost to follow up from column followup_issues
table(demographics$followup_issues) #lost to follow-up are all contains value 7

#### All double values, like 5, 7; 6, 7; and 6.7 will be counted as 7 = lost to followup
demographics$followup_issues <- as.character(demographics$followup_issues)
migrowd_demographics$followup_issues[migrowd_demographics$followup_issues %in% c("5, 7", "6, 7", "6.7")] <- "7"
demographics$followup_issues <- as.numeric(demographics$followup_issues)
table(migrowd_demographics$followup_issues) #there are 67 lost to follow up

### Calculating numbers of withdrawal
table(migrowd_demographics$withdrew)

### View rows with "lost to followup"
which(migrowd_demographics$followup_issues == 7)

### View rows with "withdrew"      
which(migrowd_demographics$withdrew == TRUE)

### Remove rows with lost to followup and withdrawal to find the denominator of new total participants
demographics_cut <- migrowd_demographics[-c(81:88, 90:93, 95:102, 104, 106:130, 132, 135:149, 151, 209:217, 219, 233:243), ]
summary(demographics_cut)

### ----------Calculating completed stool collection from new dataframe-----------
table(demographics_cut$stool_collection_status, useNA = "ifany") 
prop.table(table(demographics_cut$stool_collection_status, useNA = "ifany")) *100
round((prop.table(table(demographics_cut$stool_collection_status, useNA = "ifany")) *100), 2)


### Calculating withdrawal reason
table(demographics$withdrawal_reason)

####Legend for withdrawal reason: 
##### 0 = no reason provided
##### 1 = child ineligible - taking probiotics temporarily or parent is planning on keeping them on probiotics
##### 2 = child ineligible - taking laxatives 
##### 3 = child dealing with a health condition or disease
##### 4 = too much going on with the family 
##### 5 = did not remember signing the consent form



## ----------- Calculating Completion Rate --------------------------------------
### Stool collection
table(migrowd_demographics$stool_collection_status)#104 complete and 56 incomplete\
prop.table(table(demographics$stool_collection_status)) *100
table(recruitinstance_cut$stool_collection_status) #104 complete and 56 incomplete
table(migrowd_demographics$stool_collection_status, migrowd_demographics$asa1_status)

table(demographics$paq_boys_complete)
table(demographics$paq_girls_complete)
table(demographics$asa1_status)
table(demographics$label)
table(merged_demograph_clinical$EDUCATIONLEVEL.x)
round((prop.table(table(merged_demograph_clinical$EDUCATIONLEVEL.x)) *100))


### Count how many NA values in the column
sum(is.na(demographics_cut$stool_collection_status)) #10 = NA

### Assume that NA is incomplete
demographics_cut$stool_collection_status[is.na(demographics_cut$stool_collection_status)] <- 0
table(demographics_cut$stool_collection_status)

### Count the percentage of stool collection status
prop.table(table(demographics_cut$stool_collection_status)) *100
stool_cut <- prop.table(table(demographics_cut$stool_collection_status)) *100
round(stool_cut, digits = 2)


## ----------- Distribution of recruitment site --------------------------------------
### Recruitment type
table(demographics_cut$recruit_type) #108 home-based apporach
sum(is.na(demographics_cut$recruit_type)) #10 NA
round(prop.table(table(demographics_cut$recruit_type)) *100, 2)
table(demographics_cut$recruit_type, demographics_cut$stool_collection_status)

### Recruitment type
table(complete_recruitinstance$site)
prop.table(table(merged_recruitclinical$site)) *100
round(prop.table(table(merged_recruitclinical$site)) * 100, 2)

imputed_crosssectional_parents <- readRDS("C:/Users/ameth/Desktop/TARGet Kids!/FEASIBILITY STUDY/Feasibility/imputed_crosssectional_parents.rds")
View(imputed_crosssectional_parents)


## -------------------Interval days of stool completed --------------------------------
demographics_cut$enrollment_date <- as.Date(demographics_cut$enrollment_date, format = "%Y-%m-%d")
demographics_cut$stool_received <- as.Date(demographics_cut$stool_received, format = "%Y-%m-%d")

#calculate days between enrollment and stool received  
difftime(demographics_cut$stool_received, demographics_cut$enrollment_date, units = "days")

#create data frame for day differences
stool_day <- difftime(demographics_cut$stool_received, demographics_cut$enrollment_date, units = "days")
stool_day
  complete.cases(stool_day) <- #view data frame with NA values
  which(!complete.cases(stool_day)) <- #view row with NA values
  na_vec <- which(!complete.cases(stool_day))
na_vec
stool_no_na <- stool_day[-na_vec] <- #prepare new data frame without NA values
  stool_no_na <- #new df to calculate average days and IQR
  
####calculate mean and IQR of days difference
  mean(stool_no_na) #33 days
  IQR(stool_no_na) #31,5 days
  sd(stool_no_na) #87 days
  
  View(demographics_cut)

### -------------------Interval days of stool completed and ASA 24 --------------------------------
 demographics_cut$stool_collected_date <- as.Date(demographics_cut$stool_collected_date, format = "%Y-%m-%d")
  demographics_cut$asa1_completed <- as.Date(demographics_cut$asa1_completed, format = "%Y-%m-%d")
  difftime(demographics_cut$stool_collected_date, demographics_cut$asa1_completed, units = "days")
  
  View(merged_recruitclinical)
  table(merged_recruitclinical$interval_asa_stool)
  
  
### -------------------Description of Stool Sample Issues --------------------------------
table(demographics_cut$sample_issues) 
  demographics_cut$sample_issues <- as.character(demographics_cut$sample_issues)
  demographics_cut$sample_issues[demographics_cut$sample_issues %in% c("9", "10")] <- "8"
  demographics_cut$sample_issues <- as.numeric(demographics_cut$sample_issues)
  table(demographics_cut$sample_issues) #there are 54 missing samples, 10 NA
  
  ### -------------------SOCIODEMOGRAPHICS DISTRIBUTION --------------------------------
  ##Objectives:
  #### 1. AGE
  table(merged_demograph_clinical$age)
  prop.table(table(merged_recruitclinical$site)) *100
  round(prop.table(table(merged_recruitclinical$site)) * 100, 2)
  ((15*8)+(9*44)+(10*35)+(11*20)+(12*27))/141
  merged_demograph_clinical$age <- as.character(merged_demograph_clinical$age)
  merged_demograph_clinical$age[merged_demograph_clinical$age %in% c("2019", "2020")] <- "8"
  merged_demograph_clinical$age <- as.numeric(merged_demograph_clinical$age)
  table(merged_demograph_clinical$age)
  mean(merged_recruitclinical$age)
  sd(merged_recruitclinical$age)
  
  #### 2. SEX
  ###### Legend = 0 (Female), 1 (Male)
  table(demographics_cut$sex)
  table(merged_recruitclinical$CHILDGENDER1)
  round(prop.table(table(merged_recruitclinical$CHILDGENDER1)) * 100, 2)
  
  #### 3. GROWTH PATTERN (BMI)
  table(demographics_cut$label)
  round(prop.table(table(demographics_cut$label)) * 100, 2)
  
  
  #### 4. EDUCATION LEVEL
  table(merged_recruitclinical$EDUCATIONLEVEL)
  round(prop.table(table(merged_recruitclinical$EDUCATIONLEVEL)) * 100, 2)
  table(merged_demograph_clinical$EDUCATIONLEVEL.x, merged_demograph_clinical$stool_collection_status.x)
  education_stool <- table(merged_demograph_clinical$EDUCATIONLEVEL.x, merged_demograph_clinical$stool_collection_status.x)
  education_stool
  prop.table(table(education_stool) * 100)
  #### 5. MATERNAL EMPLOYMENT
  table(demographics_cut$MOM_EMPLOYED_YN)
  round(prop.table(table(demographics_cut$MOM_EMPLOYED_YN)) * 100, 2)
  
  #### 6. FAMILY INCOME
  table(merged_recruitclinical$FAMILY_INCOM)
  
  #### reclassify the categorical for Less than $100, 000
  merged_recruitclinical$FAMILY_INCOM[
    merged_recruitclinical$FAMILY_INCOM %in% c(
      "Less than $10, 000", "10, 000 to $19, 999",
      "$20, 000 to $29, 999", "$30, 000 to $39, 999",
      "$40, 000 to $49, 999", "50, 000 to $59, 999",
      "60, 000 to $79, 999", "$80, 000 to $99, 999"
    )
  ] <- "Less than $100, 000"
  
  table(merged_recruitclinical$FAMILY_INCOM)
  round(prop.table(table(merged_recruitclinical$FAMILY_INCOM)) * 100, 2)
    
#### 7.  MOTHER'S ETHNICITY
  table(demographics_cut$COUNTRYBIOMOMBORN)
  demographics_cut$COUNTRYBIOMOMBORN <- with(
    demographics_cut, 
    ifelse(COUNTRYBIOMOMBORN %in% c("CANADA"), "Canada",
           ifelse(COUNTRYBIOMOMBORN %in% c("USA", "MEXICO", "BRAZIL", "COLOMBIA", "TRINIDAD", "TRINIDAD & TOBAGO"), "America",
                  ifelse(COUNTRYBIOMOMBORN %in% c("ALBANIA", "ENGLAND", "FRANCE", "IRELAND", "ITALY", "PORTUGAL", "ROMANIA", "SERBIA"), "Europe",
                         ifelse(COUNTRYBIOMOMBORN  %in% c("BANGLADESH", "BURUNDI", "CAMBODIA", "CHINA", "INDIA", "PHILIPPINES", "PHILLIPINES", 
                                                 "SOUTH KOREA", "SRILANKA", "TAIWAN", "THAILAND", "VIETNAM", "TURKEY"), "Asia",
                                ifelse(COUNTRYBIOMOMBORN  %in% c("AUSTRALIA"), "Australia",
                                       ifelse(COUNTRYBIOMOMBORN  %in% c("NIGERIA", "SOMALIA"), "Africa", NA
                                       ))))))
  )
  
    
    table(demographics_cut$COUNTRYBIOMOMBORN)
    round(prop.table(table(demographics_cut$COUNTRYBIOMOMBORN)) * 100, 2)
    
### 8.  FATHER'S ETHNICITY
    table(demographics_cut$COUNTRYBIOFATHERBORN)
    demographics_cut$COUNTRYBIOFATHERBORN <- with(
      demographics_cut, 
      ifelse(COUNTRYBIOFATHERBORN %in% c("CANADA"), "Canada",
             ifelse(COUNTRYBIOFATHERBORN %in% c("USA", "MEXICO", "BRAZIL", "COLOMBIA", "TRINIDAD", "TRINIDAD & TOBAGO"), "America",
                    ifelse(COUNTRYBIOFATHERBORN %in% c("ALBANIA", "ENGLAND", "FRANCE", "IRELAND", "ITALY", "PORTUGAL", "ROMANIA", "SERBIA"), "Europe",
                           ifelse(COUNTRYBIOFATHERBORN  %in% c("BANGLADESH", "BURUNDI", "CAMBODIA", "CHINA", "INDIA", "PHILIPPINES", "PHILLIPINES", 
                                                            "SOUTH KOREA", "SRILANKA", "TAIWAN", "THAILAND", "VIETNAM", "TURKEY"), "Asia",
                                  ifelse(COUNTRYBIOFATHERBORN  %in% c("AUSTRALIA"), "Australia",
                                         ifelse(COUNTRYBIOFATHERBORN  %in% c("NIGERIA", "SOMALIA"), "Africa", NA
                                         ))))))
    )
    
    
    table(demographics_cut$COUNTRYBIOFATHERBORN)
    round(prop.table(table(demographics_cut$COUNTRYBIOFATHERBORN)) * 100, 2)

    
##### -------------- PREDICTORS FOR STOOL COMPLETION ------------------------------------------------------
## Preprocess dataframe for prediction
    library(dplyr)
    
    # left join
    merged_demograph_clinical <- left_join(merged_recruitclinical, demographics_cut, by = "subject")
    merged_demograph_clinical
    write.csv(merged_demograph_clinical, "merged_demograph_clinical.csv", row.names = FALSE)
    
    # selecting variables (predictors)
    demograph_clinical.df <- merged_demograph_clinical %>%
      dplyr::select(
        stool_collection_status.x,
        days_received,
        months_received,
        year_enrollment,
        site.x,
        recruit_type.x,
        age,
        EDUCATIONLEVEL.x,
        FAMILY_INCOM.x,
        asa1_remind_ct.x,
        pub_assess.x,
        asa1_status.x,
        Siblings,
        GESTATION_AGE.x,
        CHILDGENDER1.x,
        dadethnicity,
        momethnicity,
        FATHER_EMPLOYED_YN,
        MOM_EMPLOYED_YN,
        label
      )
    
    demograph_clinical.df
    
    # save to CSV
    write.csv(demograph_clinical.df, "demograph_clinical_df.csv", row.names = FALSE)
    getwd()
    
## Logistic Regression
    
    # 0) Packages: install if needed, then load
    pkgs <- c("dplyr", "broom", "car", "pROC", "ResourceSelection", "caret", "MASS", "mice")
    inst <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
    if(length(inst)) install.packages(inst)
    lapply(pkgs, library, character.only = TRUE)
    
    # 1) Choose dataframe (change name if necessary)
    regression <- merged_demograph_clinical   # <- replace with your df name
    
    # 2) Inspect outcome and predictors quickly
    str(regression)
    
    # 3) Create a binary 0/1 outcome 'stool_comp_bin' handling common formats
    regression <- regression %>%
      mutate(
        stool_collection_status.x = as.character(stool_collection_status.x),
        stool_collection_status.x = dplyr::case_when(
          # numeric 0/1
          is.numeric(stool_collection_status.x) & stool_collection_status.x %in% c(0,1) ~ as.integer(stool_collection_status.x),
          # common yes/no variants -> 1
          tolower(stool_collection_status.x) %in% c("yes","y","completed","complete","1","true") ~ 1L,
          # common no variants -> 0
          tolower(stool_collection_status.x) %in% c("no","n","incomplete","not completed","0","false") ~ 0L,
          TRUE ~ NA_integer_
        )
      )
    
    table(regression$stool_collection_status.x, useNA = "ifany")   # check mapping
    
    # 4) Clean predictors: numeric conversion, trim whitespace, collapse spaces, factorize
    regression <- regression %>%
      mutate(
        age = as.numeric(as.character(age)),
        Siblings = as.numeric(as.character(Siblings)),   
        days_received      = ifelse(is.na(days_received), NA, trimws(as.character(days_received))),
        months_received    = ifelse(is.na(months_received), NA, trimws(as.character(months_received))),
        site.x             = ifelse(is.na(site.x), NA, trimws(as.character(site.x))),
        recruit_type.x     = ifelse(is.na(recruit_type.x), NA, trimws(as.character(recruit_type.x))),
        EDUCATIONLEVEL.x   = ifelse(is.na(EDUCATIONLEVEL.x), NA, trimws(as.character(EDUCATIONLEVEL.x))),
        FAMILY_INCOM.x     = ifelse(is.na(FAMILY_INCOM.x), NA, trimws(as.character(FAMILY_INCOM.x))),
        CHILDGENDER1.x     = ifelse(is.na(CHILDGENDER1.x), NA, trimws(as.character(CHILDGENDER1.x))),
        MOM_EMPLOYED_YN    = ifelse(is.na(MOM_EMPLOYED_YN), NA, trimws(as.character(MOM_EMPLOYED_YN)))
      ) %>%
      mutate(
        days_received      = ifelse(is.na(days_received), NA, gsub("\\s+", " ", days_received)),
        months_received    = ifelse(is.na(months_received), NA, gsub("\\s+", " ", months_received)), 
        site.x             = ifelse(is.na(site.x), NA, gsub("\\s+", " ", site.x)), 
        recruit_type.x     = ifelse(is.na(recruit_type.x), NA, gsub("\\s+", " ", recruit_type.x)),
        EDUCATIONLEVEL.x   = ifelse(is.na(EDUCATIONLEVEL.x), NA, gsub("\\s+", " ", EDUCATIONLEVEL.x)), 
        FAMILY_INCOM.x     = ifelse(is.na(FAMILY_INCOM.x), NA, gsub("\\s+", " ", FAMILY_INCOM.x)), 
        CHILDGENDER1.x     = ifelse(is.na(CHILDGENDER1.x), NA, gsub("\\s+", " ", CHILDGENDER1.x)), 
        MOM_EMPLOYED_YN    = ifelse(is.na(MOM_EMPLOYED_YN), NA, gsub("\\s+", " ", MOM_EMPLOYED_YN))
      ) %>%
      mutate(
        days_received      = factor(days_received),
        months_received    = factor(months_received),
        site.x             = factor(site.x),
        recruit_type.x     = factor(recruit_type.x),
        EDUCATIONLEVEL.x   = factor(EDUCATIONLEVEL.x),
        FAMILY_INCOM.x     = factor(FAMILY_INCOM.x),
        CHILDGENDER1.x     = factor(CHILDGENDER1.x),
        MOM_EMPLOYED_YN    = factor(MOM_EMPLOYED_YN)
      )
    
    
    # OPTIONAL: view levels to pick sensible reference levels
    levels(regression$stool_collection_status.x)
    levels(regression$age)
    levels(regression$Siblings)
    levels(regression$days_received)
    levels(regression$months_received)
    levels(regression$site.x)
    levels(regression$recruit_type.x)
    levels(regression$EDUCATIONLEVEL.x)
    levels(regression$FAMILY_INCOM.x)
    levels(regression$CHILDGENDER1.x)
    levels(regression$MOM_EMPLOYED_YN)
    
    
    # e.g. set reference (adjust the ref level name to one that exists in your data)
    # df$education <- relevel(df$education, ref = "High school")   # example
    
    # 5) Create modeling dataset (select variables) and handle missingness:
    model_data <- regression %>%
      dplyr::select(
        stool_collection_status.x,
        days_received,
        months_received,
        site.x,
        recruit_type.x,
        age,
        EDUCATIONLEVEL.x,
        FAMILY_INCOM.x,
        Siblings,
        MOM_EMPLOYED_YN
      )
    
    # Option A: listwise deletion (simple)
    model_data_complete <- na.omit(model_data)
    nrow(model_data); nrow(model_data_complete)  # see how many rows dropped
    
    # Option B: multiple imputation (mice) — uncomment if you prefer imputation
    # imp <- mice::mice(model_data, m = 5, seed = 123)
    # pooled <- with(imp, glm(stool_comp_bin ~ age + education + income, family = binomial))
    # pool_res <- mice::pool(pooled)
    # summary(pool_res)
    
    # For the rest of the steps we'll use the complete-case dataset:
    md <- model_data_complete
    
    # 6) Univariable logistic regressions (quick screening)
    vars <- c("age", "site.x", "recruit_type.x", "EDUCATIONLEVEL.x", "FAMILY_INCOM.x", "Siblings", "MOM_EMPLOYED_YN")
    library(broom)
    univar_list <- lapply(vars, function(v) {
      f <- as.formula(paste("stool_collection_status.x ~", v))
      m <- glm(f, data = md, family = binomial)
      tidy(m) %>%
        filter(term != "(Intercept)") %>%
        mutate(variable = v,
               OR = exp(estimate),
               lower = exp(estimate - 1.96 * std.error),
               upper = exp(estimate + 1.96 * std.error))
    })
    univar_table <- do.call(rbind, univar_list)
    univar_table[, c("variable","term","OR","lower","upper","p.value")]
    
    # 7) Multivariable logistic regression (all predictors)
    # Load required package
    library(broom)
    
    # Fit the logistic regression model
    full_model <- glm(
      stool_collection_status.x ~ age + site.x + recruit_type.x + EDUCATIONLEVEL.x +
        FAMILY_INCOM.x + Siblings + MOM_EMPLOYED_YN,
      data = md,
      family = binomial
    )
    
    # Summarize the model
    summary(full_model)
    
    
    # 8) Check multicollinearity (VIF)
    car::vif(full_model)  # VIF > 5-10 may indicate multicollinearity
    
    # 9) Exponentiate coefficients and get Wald CIs for ORs
    coef_table <- data.frame(
      term = names(coef(full_model)),
      estimate = coef(full_model),
      stringsAsFactors = FALSE
    )
    ci <- stats::confint.default(full_model)   # faster Wald CI (on log-odds scale)
    or_table <- data.frame(
      term = rownames(ci),
      OR = exp(coef(full_model)),
      lower = exp(ci[,1]),
      upper = exp(ci[,2]),
      row.names = NULL
    )
    or_table
    
    # 10) Model goodness-of-fit: Hosmer-Lemeshow
    # (Requires sufficient variability and a reasonable sample size)
    
    # 10) Model goodness-of-fit: Hosmer-Lemeshow test
    
    if (nrow(md) >= 50) {
      
      # Make sure response is binary numeric (0/1)
      observed <- md$stool_collection_status.x
      
      # Convert factor/character to binary numeric if needed
      if (is.factor(observed) || is.character(observed)) {
        observed <- as.numeric(factor(observed)) - 1
      }
      
      # Check binary status
      if (!all(observed %in% c(0, 1))) {
        stop("Response variable must be binary (0/1) after conversion.")
      }
      
      # Get predicted probabilities from glm model
      yhat <- fitted(full_model)
      
      # Check unique predictions
      if (length(unique(yhat)) < 10) {
        warning("Too few unique predicted probabilities — reducing number of groups.")
        g_val <- max(5, floor(length(unique(yhat)) / 2))
      } else {
        g_val <- 10
      }
      
      # Run Hosmer-Lemeshow test
      hl_test <- ResourceSelection::hoslem.test(observed, yhat, g = g_val)
      print(hl_test)
      
    } else {
      message("Hosmer-Lemeshow test may be unreliable for small sample size (<50).")
    }
    
    
    
    # 11) Discrimination: ROC and AUC
    roc_obj <- pROC::roc(md$stool_comp_bin, predict(full_model, type = "response"))
    print(roc_obj)
    pROC::auc(roc_obj)
    plot(roc_obj, main = "ROC curve for stool completion model")
    
    # 12) Classification & confusion matrix (default threshold 0.5)
    md$pred_prob <- predict(full_model, type = "response")
    md$pred_class <- ifelse(md$pred_prob >= 0.5, 1, 0)
    caret::confusionMatrix(factor(md$pred_class), factor(md$stool_comp_bin), positive = "1")
    
    # 13) Variable selection (optional): stepwise (AIC)
    step_model <- MASS::stepAIC(full_model, direction = "both", trace = FALSE)
    summary(step_model)
    
    # 14) Nicely-formatted OR table for reporting
    library(dplyr)
    report_table <- broom::tidy(full_model) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        OR = exp(estimate),
        lower = exp(estimate - 1.96*std.error),
        upper = exp(estimate + 1.96*std.error),
        p.value = p.value
      ) %>%
      select(term, OR, lower, upper, p.value)
    report_table
    
    # 15) Save model and results to files
    saveRDS(full_model, file = "stool_logistic_model.rds")
    write.csv(report_table, file = "stool_model_ORs.csv", row.names = FALSE)
    write.csv(md, file = "stool_model_data_used.csv", row.names = FALSE)
    
    
    