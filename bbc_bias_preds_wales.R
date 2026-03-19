all_objects <- ls(envir = .GlobalEnv)
if (exists("pred_master")) {objects_to_remove <- all_objects[all_objects != "pred_master"]}
if (!exists("pred_master")) {objects_to_remove <- all_objects}
rm(list = objects_to_remove)

listOfPackages <- c("tictoc","tibble","tidyr","brms","dplyr",
                    "readr","data.table","rstan","future",
                    "future.apply","stringr")

for (i in 1:length(listOfPackages)) {
  package <- listOfPackages[i]
  if(!package %in% installed.packages()){install.packages(listOfPackages[i], dependencies = TRUE)} 
}

library(tictoc)
library(tibble)
library(tidyr)
library(brms)
library(dplyr)
library(readr)
library(data.table)
library(rstan)
library(future)
library(future.apply)
library(stringr)

# url to digital ocean space where model is stored

## download.file("https://mrp-psf.ams3.digitaloceanspaces.com/models/bbc_bias_model.rds",
      #        destfile = "bbc_bis_model.rds", mode = "wb")
## fit <- readRDS("bbc_bias_model.rds")


fit <- readRDS("bbc_bias_model.RDS")
## for wales only 

wales_fit <- readRDS("bbc_bias_wales_model.RDS")


try(dir.create("age_level_bbc"))
try(dir.create("edu_level_bbc"))
try(dir.create("ethn_level_bbc"))
try(dir.create("sex_level_bbc"))
try(dir.create("constituency_bbc"))
try(dir.create("age_sex_level_bbc"))



# url to digital ocean space where psf is stored
psf_location <- "https://mrp-psf.ams3.digitaloceanspaces.com/psf-with-ge2024"

ndraws <- 500

#### loading and prepping ####

aux <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/const_data_full_new.csv")
aux <- aux %>% mutate(const_name = gsub("Ynys Mon", "Ynys Môn", const_name))

elex_results <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/ge2024_results.csv")%>%
  mutate(const_name = gsub("Ynys Mon", "Ynys Môn", const_name, useBytes = TRUE)) %>% select(const_name,turnout,ge_con,ge_lab,ge_libdem,ge_ref,ge_green,ge_snp,ge_pc,ge_oth)
aux <- aux %>% left_join(elex_results,by='const_name')

const_names <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/order_for_preds.csv") %>% select(const_name)
const_to_region <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/Const%20to%20region.csv")
const_names <- left_join(const_names, const_to_region)

## for wales only

if(!exists("fit") & exists("wales_fit")){
  const_names <- const_names %>% filter(region == "Wales")
}

const_names <- data.frame(const_name = const_names$const_name)



library(doFuture)
registerDoFuture()
plan(multicore, workers= 5)
#foreach(j = 1:nrow(const_names), .options.future = list(seed = TRUE)) %dofuture% {
for(j in 1:nrow(const_names)){  
i=633-j
  #  gc()
  #  tictoc::tic()
  const_name_i <- const_names[i,1]
  #  print(const_name_i)
  file_check <- paste0("mental_health_preds_",const_name_i,".csv")
  if (file.exists(file_check)) {print(paste0("skipping ",i)); next}
  const_formatted <- str_replace_all(const_name_i, "ô", "%C3%B4")
  const_formatted <- str_replace_all(const_formatted, " ", "%20")
  psf_file <- paste0("https://mrp-psf.ams3.digitaloceanspaces.com/psf-with-ge2024/psf-", const_formatted,".csv")
  data <- read.csv(psf_file)

  region <- data$region[1]
  # this doesn't read in the models each time, it just assignes the pre-read-in models the right way each time
  if (const_name_i == "South Holland and The Deepings") {data$region <- "East Midlands"}
  if (const_name_i == "Queen's Park and Maida Vale") {data$region <- "Greater London"}
  # data <- data %>% mutate(region = ifelse(region == "South East", "South East England", ifelse(region == "North East", "North East England", ifelse(region == "South West", "South West England", ifelse(region == "North West", "North West England", ifelse(region == "Yorkshire & Humber", "Yorkshire and the Humber", ifelse(region == "London", "Greater London", ifelse(region == "Eastern", "East of England", region))))))))

  data <- data %>% select(-c(X))
  # data <- data %>% select(-X)
  # group_vars <- data %>% select(-perc) %>% colnames()
  # data <- data %>%
  #   summarise(perc = sum(perc), .by = all_of(group_vars))
  data <- data %>% left_join(aux, by = 'const_name')
  options(timeout = 600)

  names(data) <- gsub("^(ge_.*)", "\\1.x", names(data))
  data <- data %>% rename(turnout.x=turnout)
  
 # pred <- posterior_epred(fit, newdata = data, allow_new_levels = TRUE, ndraws = ndraws) 
  
  
  # Routes posterior_epred predictions to the Wales-specific model (wales_fit) if  
  # it exists in the environment, otherwise falling back to the main model (fit),
  # allowing a single poststratification script to handle both model runs cleanly.

  if(exists("wales_fit")){
    pred <- posterior_epred(wales_fit, newdata = data, allow_new_levels = TRUE, ndraws = ndraws) 
  }else{
    pred <- posterior_epred(fit, newdata = data, allow_new_levels = TRUE, ndraws = ndraws) 
  }
  
  pred <- as.data.frame(pred)
  data <- data %>% dplyr::mutate(id = as.character(row_number()))
  pred <- t(pred)
  pred <- rownames_to_column(data.frame(pred), var = "names")
  pred <- pred %>%
    rename_with(~str_replace(., "X", "pred"), everything()) %>%
    separate(names,
             into = c("num", "level"),
             extra = "merge") %>%
    dplyr::mutate(level = str_replace_all(level, "[.]", " ")) %>%
    dplyr::mutate(id = str_replace(num, "X", "")) %>% 
    full_join(data, pred, by = "id")

  
  
  ## overall predictions and by demographic subgroups 
  
  
  
  overall_pred <- pred %>%
    select(c(num, level, const_name, perc,starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level,mean)
  
  
  pred_eth <- pred %>%
    select(c(num, level, const_name, ethn_level, perc,starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, ethn_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, ethn_level, mean) %>% 
    group_by(const_name, ethn_level) %>% 
    mutate(group_mean = mean/sum(mean))

  pred_age <- pred %>%
    select(c(num, level, const_name, age_level, perc,starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, age_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, age_level, mean) %>% 
    group_by(const_name, age_level) %>% 
    mutate(group_mean = mean/sum(mean))
  
  
  pred_edu <- pred %>%
    select(c(num, level, const_name, edu_level, perc,starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, edu_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, edu_level, mean) %>% 
    group_by(const_name, edu_level) %>% 
    mutate(group_mean = mean/sum(mean))
  
  pred_sex <- pred %>%
    select(c(num, level, const_name, sex_level, perc,starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, sex_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, sex_level, mean) %>% 
    group_by(const_name, sex_level) %>% 
    mutate(group_mean = mean/sum(mean))
  
  pred_age_sex <- pred %>%
    select(c(num, level, const_name, age_level, sex_level, perc,starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, age_level, sex_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, age_level, sex_level, mean) %>% 
    group_by(const_name, age_level, sex_level) %>% 
    mutate(group_mean = mean/sum(mean))
  
  
  write.csv(pred_age,paste0("age_level_bbc/bbc_bias_preds_",const_name_i,".csv"))
  write.csv(pred_edu,paste0("edu_level_bbc/bbc_bias_preds_",const_name_i,".csv"))
  write.csv(pred_eth,paste0("ethn_level_bbc/bbc_bias_preds_",const_name_i,".csv"))
  write.csv(pred_sex,paste0("sex_level_bbc/bbc_bias_preds_",const_name_i,".csv"))
  write.csv(overall_pred,paste0("constituency_bbc/bbc_bias_preds_",const_name_i,".csv"))
  write.csv(pred_age_sex,paste0("age_sex_level_bbc/bbc_bias_preds_",const_name_i,".csv"))
  
  #  tictoc::toc()
}

const_files <- list.files(path="constituency_bbc/", pattern="\\.csv", full.names=TRUE)
const_dat <- lapply(const_files, \(x)read_csv(x))
const_dat_comb <- bind_rows(const_dat) %>% select(-1)
write.csv(const_dat_comb, "constituency_bbc/bbc_bias_constituency_combined.csv")

age_files <- list.files(path="age_level_bbc/", pattern="\\.csv", full.names=TRUE)
age_dat <- lapply(age_files, \(x)read_csv(x))
age_dat_comb <- bind_rows(age_dat) %>% select(-1)
write.csv(age_dat_comb, "age_level_bbc/bbc_bias_age_combined.csv")

edu_files <- list.files(path="edu_level_bbc/", pattern="\\.csv", full.names=TRUE)
edu_dat <- lapply(edu_files, \(x)read_csv(x))
edu_dat_comb <- bind_rows(edu_dat) %>% select(-1)
write.csv(edu_dat_comb, "edu_level_bbc/bbc_bias_edu_combined.csv")

ethn_files <- list.files(path="ethn_level_bbc/", pattern="\\.csv", full.names=TRUE)
ethn_dat <- lapply(ethn_files, \(x)read_csv(x))
ethn_dat_comb <- bind_rows(ethn_dat) %>% select(-1)
write.csv(ethn_dat_comb, "ethn_level_bbc/bbc_bias_ethn_combined.csv")

sex_files <- list.files(path="sex_level_bbc/", pattern="\\.csv", full.names=TRUE)
sex_dat <- lapply(sex_files, \(x)read_csv(x))
sex_dat_comb <- bind_rows(sex_dat) %>% select(-1)
write.csv(sex_dat_comb, "sex_level_bbc/bbc_bias_sex_combined.csv")

age_sex_files <- list.files(path="age_sex_level_bbc/", pattern="\\.csv", full.names=TRUE)
age_sex_dat <- lapply(age_sex_files, \(x)read_csv(x))
age_sex_dat_comb <- bind_rows(age_sex_dat) %>% select(-1)
write.csv(age_sex_dat_comb, "age_sex_level_bbc/bbc_bias_combined.csv")

