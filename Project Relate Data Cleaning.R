####  Startup  ###
## Load packages
library(tidyverse)
library(here)
`%+%` <- paste0


## Load data
t1_raw <- read.csv(here("Data", "802 Person T1 Dataset 7.20.23.csv"))
t2_raw <- read.csv(here("Data", "407 Person T2 Dataset 7.20.23.csv"))
manual_email_lookup <- read.csv(here("Data", "Manual Email Lookup.csv"))



####  Clean Data  ####
## Merge waves
# First, manually correct misspelled emails
for(i in 1:nrow(manual_email_lookup)) {
  
  email_t1 <- manual_email_lookup$email_t1[i]
  email_t2 <- manual_email_lookup$email_t2[i]
  
  # Confirm that email_t2 is, in fact, in t2 data
  if(!email_t2 %in% t2_raw$email_id) stop("t2 email not in t2 data")
  
  t2_raw$email_id[t2_raw$email_id == email_t2] <- email_t1
  
}

# Merge data, keeping only participants who responded at both timepoints
merged_data <- inner_join(
  t1_raw,
  t2_raw,
  by = "email_id",
  relationship = "one-to-one"
)


## Clean merged data
clean_data <- merged_data %>%
  rowwise() %>%
  mutate(
    
    # Detailed condition variables
    condition = factor(condition, levels = c("Control", "Intervention")),
    condition_detailed = if_else(
      condition == "Intervention",
      Module.Started,
      condition
    ) %>%
      factor(levels = c("Control", "Communication Module", "Insight Module", "Stay or Go Module")),
    
    # Demographics
    across(
      c(starts_with("b_dem"), -b_dem_zip_code),
      as.character
    ),
    
    # Recode reverse-coded RDM items
    across(
      c(
        b_rdm_8, b_rdm_12, 
        f_rdm_8, f_rdm_12
      ),
      ~ 6 - .
    ),
    
    # Recode reverse-coded ECR items
    across(
      c(
        b_ecr_1, b_ecr_2, b_ecr_3, b_ecr_4,
        f_ecr_1, f_ecr_2, f_ecr_3, f_ecr_4
      ),
      ~ 8 - .
    ),
    
    # Recode reverse-coded IRI items
    across(
      c(
        b_iri_empcon_2, b_iri_empcon_6, b_iri_empcon_7, b_iri_empcon_8,
        f_iri_empcon_2, f_iri_empcon_6, f_iri_empcon_7, f_iri_empcon_8
      )
    ),
    
    # Create PHQ-9 composites
    b_phq_mean = mean(c_across("b_phq_" %+% 1:9)),
    f_phq_mean = mean(c_across("f_phq_" %+% 1:9)),
    
    # Create GAD-7 composites
    b_gad_mean = mean(c_across("b_gad_" %+% 1:7)),
    f_gad_mean = mean(c_across("f_gad_" %+% 1:7)),
    
    # Create RKQ composites
    b_rkq_mean = mean(c_across("b_rkq_" %+% 1:21)),
    f_rkq_mean = mean(c_across("f_rkq_" %+% 1:21)),
    
    # Create RDM composites
    b_rdm_mean = mean(c_across("b_rdm_" %+% 1:12)),
    f_rdm_mean = mean(c_across("f_rdm_" %+% 1:12)),
    
    # Create BHS-4 composites
    b_bhs_mean = mean(c_across("b_bhs_" %+% 1:4)),
    pi_bhs_mean = mean(c_across("pi_bhs_" %+% 1:4)),
    f_bhs_mean = mean(c_across("f_bhs_" %+% 1:4)),
    
    # Create ECR composites
    b_ecr_mean = mean(c_across("b_ecr_" %+% 1:9)),
    f_ecr_mean = mean(c_across("f_ecr_" %+% 1:9)),
    
    # Create IRI composites
    b_iri_mean = mean(c_across("b_iri_empcon_" %+% 1:13)),
    f_iri_mean = mean(c_across("f_iri_empcon_" %+% 1:13)),
    
    # Create CSI-4 composites
    b_csi_mean = mean(c_across(c("b_csi_1", "b_csi_2_" %+% 1:3))),
    f_csi_mean = mean(c_across(c("f_csi_1", "f_csi_2_" %+% 1:3))),
    
    # Create ULS loneliness composites
    b_uls_mean = mean(c_across("b_lonely_" %+% 1:3)),
    f_uls_mean = mean(c_across("f_lonely_" %+% 1:3)),
    
    # Create PFS composites
    pi_pfs_mean = mean(c_across("pi_feedback_1_" %+% 1:7)),
    
    # Create RLI composites
    pi_rel_learn_mean = mean(c_across("pi_rel_learn_" %+% 1:3)),
    pi_rel_satisfaction_mean = mean(c_across("pi_rel_satisfaction_" %+% 1:3)),
    f_rel_attending_mean = mean(c_across("f_REL_attending_" %+% 1:3))
    
  ) %>%
  ungroup() %>%
  select(
    
    # ID, condition
    email_id, condition, condition_detailed,
    
    # Completion
    b_finished = b_Finished,
    f_finished = Finished,
    
    # Demographics
    age = b_screener_age,
    sex = b_dem_sex,
    gender = b_dem_gender,
    orientation = b_dem_orientation,
    race = b_dem_race,
    language = b_dem_language,
    grade = b_dem_grade,
    zip = b_dem_zip_code,
    
    # ACEs
    starts_with("ACEs"),
    
    # Relationship status
    b_relstat, f_relstat,
    
    # PHQ-9 items and composites
    b_phq_mean, "b_phq_" %+% 1:9,
    f_phq_mean, "f_phq_" %+% 1:9,
    
    # GAD-7 items and composites
    b_gad_mean, "b_gad_" %+% 1:7,
    f_gad_mean, "f_gad_" %+% 1:7,
    
    # RKQ items and composites
    b_rkq_mean, "b_rkq_" %+% 1:21,
    f_rkq_mean, "f_rkq_" %+% 1:21,
    
    # RDM items and composites
    b_rdm_mean, "b_rdm_" %+% 1:12,
    f_rdm_mean, "f_rdm_" %+% 1:12,
    
    # BHS-4 items and composites
    b_bhs_mean, "b_bhs_" %+% 1:4,
    pi_bhs_mean, "pi_bhs_" %+% 1:4,
    f_bhs_mean, "f_bhs_" %+% 1:4,
    
    # ECR items and composites
    b_ecr_mean, "b_ecr_" %+% 1:9,
    f_ecr_mean, "f_ecr_" %+% 1:9,
    
    # IRI items and composites
    b_iri_mean, "b_iri_empcon_" %+% 1:13,
    f_iri_mean, "f_iri_empcon_" %+% 1:13,
    
    # CSI-4 items and composites
    b_csi_mean, c("b_csi_1", "b_csi_2_" %+% 1:3),
    f_csi_mean, c("f_csi_1", "f_csi_2_" %+% 1:3),
    
    # ULS loneliness items and composites
    b_uls_mean, "b_lonely_" %+% 1:3,
    f_uls_mean, "f_lonely_" %+% 1:3,
    
    # PFS items and composites
    pi_pfs_mean, "pi_feedback_1_" %+% 1:7,
    
    # RLI items and compositess
    pi_rel_learn_mean, "pi_rel_learn_" %+% 1:3,
    pi_rel_satisfaction_mean, "pi_rel_satisfaction_" %+% 1:3,
    f_rel_attending_mean, "f_REL_attending_" %+% 1:3
    
    
  ) %>%
  
  # Add state, for fun
  left_join(
    zipcodeR::zip_code_db %>%
      mutate(zip = as.numeric(zipcode)) %>%
      select(zip, state),
    by = "zip"
  )

count(clean_data, condition, condition_detailed)



####  Save Data  ####
saveRDS(clean_data, here("Data", "Clean Project Relate Data.rds"))
