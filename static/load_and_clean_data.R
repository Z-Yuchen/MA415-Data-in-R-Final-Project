library(tidyverse)
library(here)

# read and combine csv file
raw_data_a_m <- read_csv('data/states_a_m.csv')
raw_data_n_z <- read_csv('data/states_n_z.csv')
raw_data_national <- rbind(raw_data_a_m,raw_data_n_z)

# Now, let's start clean the data

# Step1: delete column with more than 30% missing value
processed_data <- raw_data_national[,!sapply(raw_data_national, function(x) mean(is.na(x)))>0.3]

# Step2: keep useful variable (q8 - q18 safety issue, q25-28 depression& suicide, q33-q41 smoking & alcohol, q45-q57 drugs)
processed_data <- processed_data %>% select(sitename, year,age, sex, grade, race4,race7, stheight,stweight, bmi, bmipct, q8,q9,q12,q13,q15,q16,q17,q18,q25,q26,q27,q28,q32,q33,q38,q40,q41,q45,q46,q50,q51,q52,q53,q56,q57)

# Step3: Rename and unify the value of various variables

processed_data <- processed_data %>% 
  rename("bmi_percentage" = bmipct, 
         "race"= race7, 
         "height_without_shoes" = stheight, 
         "weight_without_shoes" = stweight,
         "seat_belt_use"= q8, 
         "riding_with_a_drinking_driver" = q9, 
         "weapon_carrying"=q12,
         "weapon_carrying_at_school" = q13, 
         "safety_concerns_at_school" = q15,
         "Threatened_at_school"= q16, 
         "Physical_fighting" = q17, 
         "Physical_fighting_at_school" = q18, 
         "feel_hopeless_2+weeks"= q25,
         "serious_consideration_suicide"= q26,
         "plan_suicide"= q27,
         "attempt_suicide"= q28,
         "freq_smoking_cigarette"= q32,
         "num_cigarette_per_day"= q33,
         "freq_smoking_cigar"= q38,
         "first_time_drinking"= q40,
         "freq_drinking"= q41,
         "total_num_marijuana"= q45,
         "first_time_marijuana"= q46,
         "total_num_cocaine"= q50,
         "total_num_get_high"= q51,
         "total_num_heroin"= q52,
         "total_num_methamphetamines"= q53,
         "total_num_needle_for_drug"= q56,
         "has_got_drug_at_school"= q57)

# rename answer for 'age' (As <12, 13 has very few people, we group these two group with age 14, and renamed group '<14')
processed_data$age[processed_data$age == 1 | processed_data$age == 2 | processed_data$age ==  3] <- "<14"
processed_data$age[processed_data$age == 4] <- "15"
processed_data$age[processed_data$age == 5] <- "16"
processed_data$age[processed_data$age == 6] <- "17"
processed_data$age[processed_data$age == 7] <- "18+"

# rename answer for 'race'
processed_data$race[processed_data$race == 1] <- "American Indian/Alaska Native"
processed_data$race[processed_data$race == 2] <- "Asian"
processed_data$race[processed_data$race == 3] <- "Black or African American"
processed_data$race[processed_data$race == 4] <- "Hispanic/Latino"
processed_data$race[processed_data$race == 5] <- "Native Hawaiian/Other Pacific Islander"
processed_data$race[processed_data$race == 6] <- "White"
processed_data$race[processed_data$race == 7] <- "Multiple Races (Non-Hispanic)"


processed_data$race4[processed_data$race4 == 1] <- "White"
processed_data$race4[processed_data$race4 == 2] <- "Black or African American"
processed_data$race4[processed_data$race4 == 3] <- "Hispanic/Latino"
processed_data$race4[processed_data$race4 == 4] <- "All Other Races"

# rename answer for 'sex'
processed_data$sex[processed_data$sex == 1] <- "Female"
processed_data$sex[processed_data$sex == 2] <- "Male"

# rename answer for 'grade'
processed_data$grade[processed_data$grade == 1] <- "9th grade"
processed_data$grade[processed_data$grade == 2] <- "10th grade"
processed_data$grade[processed_data$grade == 3] <- "11th grade"
processed_data$grade[processed_data$grade == 4] <- "12th grade"
processed_data$grade[processed_data$grade == 5] <- "Ungraded or other grade"

# unify answer for other survey questions 
processed_data$seat_belt_use[processed_data$seat_belt_use == 1 | processed_data$seat_belt_use == 2] <- "No"
processed_data$seat_belt_use[processed_data$seat_belt_use == 3 | processed_data$seat_belt_use == 4 | processed_data$seat_belt_use == 5] <- "Yes"

processed_data$riding_with_a_drinking_driver[processed_data$riding_with_a_drinking_driver != 1] <- "Yes"
processed_data$riding_with_a_drinking_driver[processed_data$riding_with_a_drinking_driver == 1] <- "No"

processed_data$weapon_carrying[processed_data$weapon_carrying != 1] <- "Yes"
processed_data$weapon_carrying[processed_data$weapon_carrying == 1] <- "No"

processed_data$weapon_carrying_at_school[processed_data$weapon_carrying_at_school != 1] <- "Yes"
processed_data$weapon_carrying_at_school[processed_data$weapon_carrying_at_school == 1] <- "No"

processed_data$safety_concerns_at_school[processed_data$safety_concerns_at_school != 1] <- "Yes"
processed_data$safety_concerns_at_school[processed_data$safety_concerns_at_school == 1] <- "No"

processed_data$Threatened_at_school[processed_data$Threatened_at_school != 1] <- "Yes"
processed_data$Threatened_at_school[processed_data$Threatened_at_school == 1] <- "No"

processed_data$Physical_fighting[processed_data$Physical_fighting != 1] <- "Yes"
processed_data$Physical_fighting[processed_data$Physical_fighting == 1] <- "No"

processed_data$Physical_fighting_at_school[processed_data$Physical_fighting_at_school != 1] <- "Yes"
processed_data$Physical_fighting_at_school[processed_data$Physical_fighting_at_school == 1] <- "No"

processed_data$'feel_hopeless_2+weeks'[processed_data$'feel_hopeless_2+weeks' == 1] <- "Yes"
processed_data$'feel_hopeless_2+weeks'[processed_data$'feel_hopeless_2+weeks' == 2] <- "No"

processed_data$serious_consideration_suicide[processed_data$serious_consideration_suicide == 1] <- 1
processed_data$serious_consideration_suicide[processed_data$serious_consideration_suicide == 2] <- 0

processed_data$plan_suicide[processed_data$plan_suicide == 1] <- 1
processed_data$plan_suicide[processed_data$plan_suicide == 2] <- 0

processed_data$attempt_suicide[processed_data$attempt_suicide == 1] <- 0
processed_data$attempt_suicide[processed_data$attempt_suicide != 0] <- 1

processed_data$freq_smoking_cigarette[processed_data$freq_smoking_cigarette != 1] <- "Yes"
processed_data$freq_smoking_cigarette[processed_data$freq_smoking_cigarette == 1] <- "No"

processed_data$num_cigarette_per_day[processed_data$num_cigarette_per_day == 1 | processed_data$num_cigarette_per_day == 2 | processed_data$num_cigarette_per_day == 3] <- "no or Light"
processed_data$num_cigarette_per_day[processed_data$num_cigarette_per_day == 4 | processed_data$num_cigarette_per_day == 5] <- "Moderate"
processed_data$num_cigarette_per_day[processed_data$num_cigarette_per_day == 6 | processed_data$num_cigarette_per_day == 7] <- "Heavy"

processed_data$freq_smoking_cigar[processed_data$freq_smoking_cigar != 1] <- "Yes"
processed_data$freq_smoking_cigar[processed_data$freq_smoking_cigar == 1] <- "No"

processed_data$first_time_drinking[processed_data$first_time_drinking == 1] <- "Haven't"
processed_data$first_time_drinking[processed_data$first_time_drinking == 2 | processed_data$first_time_drinking == 3 | processed_data$first_time_drinking == 4] <- "<12"
processed_data$first_time_drinking[processed_data$first_time_drinking == 5 | processed_data$first_time_drinking == 6 | processed_data$first_time_drinking == 7] <- "13+"

processed_data$freq_drinking[processed_data$freq_drinking != 1] <- "Yes"
processed_data$freq_drinking[processed_data$freq_drinking == 1] <- "No"

processed_data$total_num_marijuana[processed_data$total_num_marijuana != 1] <- "Yes"
processed_data$total_num_marijuana[processed_data$total_num_marijuana == 1] <- "No"

processed_data$first_time_marijuana[processed_data$first_time_marijuana == 1] <- "Haven't"
processed_data$first_time_marijuana[processed_data$first_time_marijuana == 2 | processed_data$first_time_marijuana == 3 | processed_data$first_time_marijuana == 4] <- "<12"
processed_data$first_time_marijuana[processed_data$first_time_marijuana == 5 | processed_data$first_time_marijuana == 6 | processed_data$first_time_marijuana == 7] <- "13+"

processed_data$total_num_cocaine[processed_data$total_num_cocaine != 1] <- "Yes"
processed_data$total_num_cocaine[processed_data$total_num_cocaine == 1] <- "No"

processed_data$total_num_get_high[processed_data$total_num_get_high != 1] <- "Yes"
processed_data$total_num_get_high[processed_data$total_num_get_high == 1] <- "No"

processed_data$total_num_heroin[processed_data$total_num_heroin != 1] <- "Yes"
processed_data$total_num_heroin[processed_data$total_num_heroin == 1] <- "No"

processed_data$total_num_methamphetamines[processed_data$total_num_methamphetamines != 1] <- "Yes"
processed_data$total_num_methamphetamines[processed_data$total_num_methamphetamines == 1] <- "No"

processed_data$total_num_needle_for_drug[processed_data$total_num_needle_for_drug != 1] <- "1+"
processed_data$total_num_needle_for_drug[processed_data$total_num_needle_for_drug == 1] <- "0"

processed_data$has_got_drug_at_school[processed_data$has_got_drug_at_school != 1] <- "No"
processed_data$has_got_drug_at_school[processed_data$has_got_drug_at_school == 1] <- "Yes"



#convert BMI percentile into a categorical variable 'weight_status' according to the CDC BMI rules.
processed_data <- processed_data %>% mutate(Weight_status = ifelse(bmi_percentage<5,"underweight", ifelse(bmi_percentage>=5 & bmi_percentage<85,"healthyweight",ifelse(bmi_percentage>=85 & bmi_percentage<95,"overweight",ifelse(bmi_percentage>=95,"obesity",bmi_percentage))))) 

save(processed_data,file = here::here("data/processed_data.RData")) 