wd <- getwd()
if (wd != "/root") {
  setwd("C:/Users/Zain/More In Common/More In Common Team Site - MRP")
}; rm(wd)


library(brms)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(data.table)
library(rstan)
library(future)
library(future.apply)
library(posterior)
library(parallel)
library(nnet)
library(readr)
library(haven)
library(purrr)



## UK should leave NATO, should remain in NATO, don't know ## 

## edcuation variable is not just degree v non degree 


## remove missing data from welshlang variable 
options(timeout =1200)
polldata <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/polls/winter_10k.csv")

if (detectCores() == 16) {
  threads <- 3
  workers <- 4
}
if (detectCores() == 8) {
  threads <- 2
  workers <- 4
}
if (detectCores() == 4) {
  threads <- 1
  workers <- 4
}
if (detectCores() == 32) {
  threads <- 4
  workers <- 4
}

if (detectCores() == 48) {
  threads <- 11
  workers <- 4
}


options(timeout =1200)
# download.file("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/postcodelookup.csv", 
#               destfile = "postcode_lookup.csv")
postcode_lookup <- read.csv("../mrp_jan_2026/postcode_lookup.csv")


standardize_postcode <- function(postcode) {
  postcode <- as.character(postcode)
  postcode <- toupper(postcode)       
  postcode <- gsub("[[:space:]]", "", postcode) 
  postcode <- gsub("[[:punct:]]", "", postcode) 
  return(postcode)
}

# Standardize postcodes in both datasets
postcode_lookup$postcode_clean <- standardize_postcode(postcode_lookup$postcode)
postcode_lookup <- subset(postcode_lookup , !is.na(postcode_clean))

## this makes it so every name-clean postcode combination only has one entry in the data
postcode_lookup <- postcode_lookup %>%
  dplyr::select(name, postcode_clean) %>%
  dplyr::group_by(name, postcode_clean) %>%
  filter(row_number() == 1)

## this makes it so that the lookup table only has observations where a postcode matches a single constituency
postcode_lookup <- postcode_lookup %>% 
  group_by(postcode_clean) %>% 
  filter(n() == 1)


polldata$postcode_clean <- standardize_postcode(polldata$postcode_1)


# Join to match constituency names
polldata <- left_join(polldata, 
                      postcode_lookup[, c("postcode_clean", "name")], 
                      by = "postcode_clean")

polldata$constituency <- polldata$name
polldata$name <- NULL  

cat("Total rows in polldata:", nrow(polldata), "\n")
cat("Rows with matched constituency:", sum(!is.na(polldata$constituency)), "\n")
cat("Match rate:", round(100 * sum(!is.na(polldata$constituency)) / nrow(polldata), 1), "%\n")


#### Constituency level data ####
aux <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/const_data_full_new.csv")
aux <- aux %>% mutate(const_name = gsub("Ynys Mon", "Ynys Môn", const_name))

elex_results <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/ge2024_results.csv")%>%
  mutate(const_name = gsub("Ynys M\xf4n", "Ynys Môn", const_name, useBytes = TRUE)) %>% select(const_name,turnout,ge_con,ge_lab,ge_libdem,ge_ref,ge_green,ge_snp,ge_pc,ge_oth)
aux <- aux %>% left_join(elex_results,by='const_name')



elex_results <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/ge2024_results.csv")%>%
  mutate(const_name = gsub("Ynys M\xf4n", "Ynys Môn", const_name, useBytes = TRUE)) %>% select(const_name,turnout,ge_con,ge_lab,ge_libdem,ge_ref,ge_green,ge_snp,ge_pc,ge_oth)
aux <- aux %>% left_join(elex_results,by='const_name')

## survey data ## 



col_list <- c('constituency',
              'cid', 'gender','age_1','education','ownrent',
              'married', 'religion','age',
              'ethnicity','workstatus', 
              'leaveremain','ge2024',
              'indyref', 'welshlang', 'region',
              'START_DATE',
              'nato')

polldata <- polldata %>% select(any_of(col_list))


# Standard age handling
polldata$age_1 <- ifelse(is.na(polldata$age_1),polldata$age,polldata$age_1)



polldata <- polldata %>% filter(constituency != "")
polldata$constituency <- gsub("Ynys Mon", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Ynys M\xf4n", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Ynys MÌ«n", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Ynys Mon", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Ynys MÃ´n", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and Glynd  r", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and Glynd_r", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and Glynd?r", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and Glynd_r", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire$", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and GlyndÅµr", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- ifelse(grepl("Montgomeryshire", polldata$constituency),"Montgomeryshire and Glyndwr", polldata$constituency)
polldata$constituency <- ifelse(grepl("Ynys", polldata$constituency),"Ynys Môn", polldata$constituency)

const_name_short <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/const_name_short.csv")
const_name_short <- const_name_short %>% mutate(const_long = ifelse(grepl("Ynys", const_name_short$const_long),"Ynys MÃ´n", const_name_short$const_long))
const_name_short <- const_name_short %>% mutate(const_short = ifelse(grepl("Ynys", const_name_short$const_short),"Ynys MÃ´n", const_name_short$const_short))
polldata <- left_join(polldata, const_name_short, join_by(constituency==const_short))
polldata$constituency <- ifelse(is.na(polldata$const_long), polldata$constituency, polldata$const_long)
rm(const_name_short)

polldata$const_name <- polldata$constituency

polldata <- left_join(polldata,aux,by='const_name')
polldata$region <- polldata$region_name

#recode vars

polldata$age_level <- ifelse(polldata$age_1 < 25,'18-24',
                             ifelse(polldata$age_1 < 35,'25-34',
                                    ifelse(polldata$age_1 < 45,'35-44',
                                           ifelse(polldata$age_1 < 55,'45-54',
                                                  ifelse(polldata$age_1 < 65,'55-64',
                                                         ifelse(polldata$age_1 < 75,'65-74','75+'))))))
##polldata$migrant_level <- recode(polldata$borninuk, 'Yes'='Born in the UK','No'='Born outside the ##UK')
polldata$ethn_level <- recode(polldata$ethnicity,"Asian/Asian British"="Asian",
                              "Asian/ Asian British"="Asian",
                              "Black/Black British"="Black",
                              "Black/ Black British"="Black",
                              "Mixed descent (e.g. White & Asian, White & Black)"="Mixed",
                              "White (British/Irish/Other)"="White")
polldata$ge2024 <- recode(polldata$ge2024,"Reform UK"="Reform UK",
                          "The Green Party"="Green Party",
                          "Another party"="Other",
                          "Scottish National Party (SNP)"="Scottish National Party",
                          "Don't know"="DK_DNV",
                          "Donâ€™t know" = "DK_DNV",
                          "I did not vote"="DK_DNV",
                          "I was too young to vote"="I was too young to vote")
polldata$euref <- recode(polldata$leaveremain,"Don't know"="DK_DNV",
                         "I did not vote"="DK_DNV",
                         "I was too young to vote"="DK_DNV")
polldata <- polldata %>%
  mutate(edu_level = case_when(
    str_detect(education, "Degree|Doctorate|Postgraduate|Masters") ~ "Degree",
    education == "Prefer not to answer" ~ NA_character_,
    TRUE ~ "No degree"
  ))

polldata$econ_level <- recode(polldata$workstatus,
                              "Homemaker/Househusband/Housewife etc"="Inactive",
                              "Homemaker/Househusband/Housewife etc."="Inactive",
                              "Not working and not seeking work"="Inactive",
                              "Not working/temporarily unemployed/sick but seeking work"="Unemployed",
                              "Prefer not to say"="NA",
                              "Retired"="Inactive",
                              "Retired on a state pension only"="Inactive",
                              "Retired with a private pension"="Inactive",
                              "Student"="Inactive",
                              "Working full time - working 30 hours per week or more"="Employed",
                              "Working part time - working less than 30 hours per week"="Employed")
polldata$housing <- recode(polldata$ownrent,
                           "Own outright"='Own',
                           "Owned outright"='Own',
                           "Own with a mortgage or loan"='Own',
                           "Owned with a mortgage or loan"='Own',
                           "Rent from the housing association"='Rent',
                           "Rented from the housing association"='Rent',
                           "Privately rent"='Rent',
                           "Privately rented"="Rent",
                           "Rent free"='Other',
                           "Prefer not to say"='Other',
                           "Rent from the council"='Rent',
                           "Rented from the council"='Rent')
# polldata <- rename(polldata,'sex_level'='gender')
polldata$sex_level <- polldata$gender

##polldata$married_level <- recode(polldata$married,
##                               "Yes - married"='Married',
##                               "No - not married"='Not Married')
polldata$rel_level <- recode(polldata$religion,
                             "Prefer not to say"='Not answered',
                             "Other"='Other religion',
                             "Any other religion, write in"="Other",
                             "Christian (including Church of England, Catholic, Protestant and all other Christian denominations)"="Christian")
polldata$welsh_level <- recode(polldata$welshlang,
                               "I can speak Welsh" = 'Fluent',
                               "I cannot speak Welsh but I can understand, read or write Welsh" = 'Limited',
                               "I can't speak, understand, read or write Welsh" = "None",
                               .missing = "Not Available")  # Handle actual NA values

polldata$welsh_level <- ifelse(polldata$welsh_level == "", "Not Available", polldata$welsh_level)

# polldata$veteran_level <- recode(polldata$veteran,
#                                  "Yes"='Veteran',
#                                  "No"='Not a veteran')
polldata$indyref <- ifelse(polldata$indyref=="","Not Available",polldata$indyref)
polldata$indyref <- recode(polldata$indyref,
                           'I did not vote'='DK_DNV',
                           "Don't know"='DK_DNV',
                           'AGAINST Scottish independence'='No',
                           'FOR Scottish independence'='Yes')
polldata$region <- recode(polldata$region_name, "South East England"="South East",
                          "North East England"="North East",
                          "South West England"="South West",
                          "North West England"="North West",
                          "Yorkshire and the Humber"="Yorkshire & Humber",
                          "Greater London"="London", 
                          "East of England"="Eastern")




polldata <- polldata %>%
  mutate(sex_level = case_when(
    sex_level == "Woman" ~ "Female",
    sex_level == "Man" ~ "Male",
    TRUE ~ sex_level
  ))


###  think about hold to Don't know - this is hte largest group 41% 



## Make DK separate category, use categorical family
##  use this one

polldata <- polldata %>%
  mutate(nato_numeric_dk_sep = case_when(
    nato == "The UK should leave NATO" ~ 1,
    nato == "The UK should remain a member of NATO" ~ 2,
    nato == "Don’t know" ~ 3,
    TRUE ~ NA_real_
  ))



#### check levels
polldata %>% group_by(age_level) %>% count()
polldata %>% group_by(sex_level) %>% count()
polldata %>% group_by(region) %>% count()
polldata %>% group_by(ge2024) %>% count()
polldata %>% group_by(ethn_level) %>% count()
polldata %>% group_by(edu_level) %>% count()
polldata %>% group_by(housing) %>% count()
# polldata %>% group_by(rel_level) %>% count()
polldata %>% group_by(welsh_level) %>% count()
polldata %>% group_by(indyref) %>% count()



overall_nato <- polldata %>% group_by(nato_numeric_dk_sep) %>% count()
overall_nato$pct <- 100*overall_nato$n/nrow(polldata)



data <- polldata %>% select(age_level,sex_level,region,ge2024,ethn_level,edu_level,housing,welsh_level,indyref)
rows_with_na <- data[rowSums(is.na(data)) > 0, ]


nato_data <- polldata


nato_data <- polldata %>%
  mutate(
    nato_cat = factor(
      nato_numeric_dk_sep,
      levels = 1:3,
      labels = c(
        "leave_nato",
        "remain_nato",
        "dont_know"
      )
    )
  )


nato_wales <- nato_data %>% filter(region == "Wales")
nato_eng_scot <- nato_data %>% filter(region != "Wales")

# Define model parameters
iter <- 2000        
chains <- 4         
cores <- 4          
threads <- 1        
treedepth <- 15     

# Priors for model


# Priors for the NATO attitudes categorical model, specified separately for each  
# non-reference response category (don't know; remain in NATO) as the model estimates
# a distinct log-odds equation for each relative to the reference category.
# Normal(0, 1) on intercepts allows moderate baseline probability mass across 
# categories without strongly favouring any outcome a priori; normal(0, 0.5) on 
# coefficients is tighter, reflecting that individual predictors are unlikely to 
# produce very large shifts in log-odds. Exponential(2) on sd parameters (mean = 0.5) 
# is strictly positive and weakly regularises random effect variance, preventing 
# inflation across geographic or demographic groupings where cell counts may be sparse.


priors_nato <- c(
  prior(normal(0, 1), class = "Intercept", dpar = "mudontknow"),          
  prior(normal(0, .5), class = "b", dpar = "mudontknow"),                     
  prior(exponential(2), class = "sd", dpar = "mudontknow"),               
  
  prior(normal(0, 1), class = "Intercept", dpar = "muremainnato"),          
  prior(normal(0, .5), class = "b", dpar = "muremainnato"),                     
  prior(exponential(2), class = "sd", dpar = "muremainnato"))




# Model formulas 
formula_1_es <- nato_cat ~ 
  (1 | age_level * sex_level) +
  (1 | region) +
  (1 | ge2024) +
  (1 | ethn_level) + 
  (1 | edu_level) +
  (1 | euref) + 
  (1 | econ_level) + 
  (1 | housing) +
  (1 | rel_level) + 
  (1 | welsh_level) +
  (1 | indyref) +
  scale(depriv_index_country) + 
  scale(ruc_country) +
  scale(euref_leave) + 
  scale(house_prices_perc) +
  scale(students) + 
  # ge_con + 
  ge_lab.x + 
  ge_libdem.x + 
  ge_green.x +
  ge_ref.x +
  ge_oth.x +
  ge_snp.x + 
  turnout.x

formula_1_wales <- nato_cat ~ 
  (1 | age_level * sex_level) +
  (1 | ge2024) +
  (1 | ethn_level) + 
  (1 | edu_level) +
  (1 | euref) + 
  (1 | econ_level) + 
  (1 | housing) +
  (1 | rel_level) + 
  (1 | welsh_level) +
  (1 | indyref) +
  scale(depriv_index_country) + 
  scale(ruc_country) +
  scale(euref_leave) + 
  scale(house_prices_perc) +
  scale(students) + 
  # ge_con + 
  ge_lab.x + 
  ge_libdem.x + 
  ge_green.x +
  ge_ref.x +
  ge_oth.x +
  ge_pc.x +
  turnout.x


## check family model model for this ordinal model 

## if this complains about the DV, may have to recode it as a factor. 

print(paste("Starting model 1 at:", Sys.time()))

model_1_wales <- brm(
  formula = formula_1_wales,
  data = nato_wales,
  family = categorical(link = "logit"),
  seed = 320,
  silent = 0,
  iter = iter,
  prior = priors_nato,
  chains = chains,
  threads = threading(threads),
  refresh = 50,
  cores = cores,
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = treedepth)
)

model_1_es <- brm(
  formula = formula_1_es,
  data = nato_eng_scot,
  family = categorical(link = "logit"),
  seed = 320,
  silent = 0,
  iter = iter,
  prior = priors_nato,
  chains = chains,
  threads = threading(threads),
  refresh = 50,
  cores = cores,
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.95, max_treedepth = treedepth))
  

print(paste("Starting model 1 at:", Sys.time()))



saveRDS(model_1_wales, "wales_nato_model.rds")
saveRDS(model_1_es, "eng_scot_nato_model.rds")