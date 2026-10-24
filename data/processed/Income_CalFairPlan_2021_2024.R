library(tidyverse)
library(here)
Income_2021 <- read.csv(here("data/raw/ACS_2021_Income_Data.csv"))
Income_2022 <- read.csv(here("data/raw/ACS_2022_Income_Data.csv"))
Income_2023 <- read.csv(here("data/raw/ACS_2023_Income_Data.csv"))
Income_2024 <- read.csv(here("data/raw/ACS_2024_Income_Data.csv"))

#2021
Income_tidied_2021 <- Income_2021 %>% 
  mutate(Geography = GEO_ID, ZCTA = str_extract(NAME, "\\d{5}"), 
         Median_income = B19013_001E, Income_MOE = B19013_001M, 
  ) %>% 
  select(-B19013_001E, -B19013_001M, -NAME, -X, -GEO_ID) %>% 
  filter(!is.na(ZCTA))
#2022/2023/2024 (iterate by year)
Income_tidied_2024 <- Income_2024 %>% 
  mutate(Geography = GEO_ID, ZCTA = str_extract(NAME, "\\d{5}"), 
         Median_income = B19013_001E, Income_MOE = B19013_001M, 
  ) %>% 
  select(-B19013_001E, -B19013_001M, -NAME, -X, -GEO_ID) %>% 
  filter(!is.na(ZCTA))

zip_zcta <- read.csv(here("data/raw/zip_zcta_xref.csv"))
cfp_data <- read.csv(here("data/raw/cfp_data/cfp_policies_by_zip.csv"))

zip_zcta_CA <- zip_zcta %>% 
  filter(zip_code >= 90001 & zip_code <= 96162)

str(Income_tidied_2021$ZCTA)
str(zip_zcta_CA$zcta)
zip_zcta_CA$zcta <- as.character(zip_zcta_CA$zcta)
# 2021
Income2021_ZIP <- zip_zcta_CA %>% 
  left_join(Income_tidied_2021, zip_zcta_CA, by = c("zcta" = "ZCTA"))
# 2022/2023/2024
Income2024_ZIP <- zip_zcta_CA %>% 
  left_join(Income_tidied_2024, zip_zcta_CA, by = c("zcta" = "ZCTA"))



sum(is.na(Income2021_ZIP))

# NAs in each column
colSums(is.na(Income2021_ZIP))

# NAs in each row
sum(!complete.cases(Income2021_ZIP))

# missing income data
sum(is.na(Income2021_ZIP$Median_income))


# ZIP repetitions?
zip_zcta_CA %>%
  count(zip_code) %>%
  filter(n > 1)

# Whether there are ZCTA repetitions in Census Income?
Income_tidied_2021 %>%
  count(ZCTA) %>%
  filter(n > 1)

Income2021_ZIP %>%
  summarise(
    total = n(),
    same_code = sum(zip_code == zcta, na.rm = TRUE),
    different_code = sum(zip_code != zcta, na.rm = TRUE)
  )


#2021
Income2021_ZIP_valid <- Income2021_ZIP %>%
  filter(!is.na(Median_income))
#2022/2023/2024
Income2024_ZIP_valid <- Income2024_ZIP %>%
  filter(!is.na(Median_income))

# 2021
Income2021_ZIP_valid <- Income2021_ZIP_valid %>% 
  mutate(Year = 2021)
# 2022/2023/2024
Income2024_ZIP_valid <- Income2024_ZIP_valid %>% 
  mutate(Year = 2024)

Income2021_ZIP_valid %>%
  filter(if_any(everything(), is.na))

str(Income2021_ZIP_valid$zip_code)
Income2021_ZIP_valid$zip_code <- as.character(Income2021_ZIP_valid$zip_code)
str(cfp_data$zip)
cfp_data$zip <- as.character(cfp_data$zip)

Income2021_ZIP_cfp <- Income2021_ZIP_valid %>%
  left_join(cfp_data, by = c("zip_code" = "zip"))



head(Income2021_ZIP_cfp)
nrow(Income2021_ZIP_cfp)
names(Income2021_ZIP_cfp)

cfp_data %>%
  count(zip) %>%
  filter(n > 1)
Income2021_ZIP_cfp %>%
  count(zip_code) %>%
  filter(n > 1)


cfp_data %>% 
  filter(zip == 96059)
cfp_data_clean <- cfp_data %>%
  group_by(zip) %>%
  summarise(
    across(starts_with("policies_"), max, na.rm = TRUE),
    .groups = "drop"
  )
cfp_data_clean %>%
  filter(zip == "96059")


#2021
Income2021_ZIP_cfp <- Income2021_ZIP_valid %>%
  left_join(cfp_data_clean, by = c("zip_code" = "zip"))
nrow(Income2021_ZIP_cfp)
Income2021_ZIP_cfp <- Income2021_ZIP_cfp %>% 
  select(-source, -policies_2025, -policies_2024, -policies_2023, -policies_2022)

#2022/2023/2024
Income2024_ZIP_cfp <- Income2024_ZIP_valid %>%
  left_join(cfp_data_clean, by = c("zip_code" = "zip"))
nrow(Income2024_ZIP_cfp)
Income2024_ZIP_cfp_final <- Income2024_ZIP_cfp %>% 
  select(-source, -policies_2025, -policies_2023, -policies_2022, -policies_2021)



Income2021_ZIP_cfp_clean <- Income2021_ZIP_cfp %>% 
  filter(!Median_income == "-", !Income_MOE == "**", !is.na(policies_2021))
nrow(Income2021_ZIP_cfp_clean)

Income2021_ZIP_cfp %>%
  summarise(
    total = n(),
    
    missing_income = sum(
      is.na(Median_income) |
        Median_income == "-"
    ),
    
    missing_moe = sum(
      is.na(Income_MOE) |
        Income_MOE == "**"
    ),
    
    missing_policies = sum(is.na(policies_2021)),
    
    zero_policies = sum(
      policies_2021 == 0,
      na.rm = TRUE
    )
  )

Income2021_ZIP_cfp_final <- Income2021_ZIP_cfp %>%
  mutate(
    Median_income = as.numeric(
      na_if(as.character(Median_income), "-")
    ),
    Income_MOE = as.numeric(
      na_if(as.character(Income_MOE), "**")
    )
  )
Income2021_ZIP_cfp_final %>%
  summarise(
    total = n(),
    missing_income = sum(is.na(Median_income)),
    missing_moe = sum(is.na(Income_MOE)),
    missing_policies = sum(is.na(policies_2021)),
    zero_policies = sum(policies_2021 == 0, na.rm = TRUE)
  )
Income2021_ZIP_cfp_clean <- Income2021_ZIP_cfp_final %>%
  filter(
    !is.na(Median_income),
    !is.na(Income_MOE),
    !is.na(policies_2021)
  )
nrow(Income2021_ZIP_cfp_clean)



Income2021_ZIP_cfp %>%
  filter(
    !grepl("^[0-9]+$", Median_income) |
      !grepl("^[0-9]+$", Income_MOE)
  ) %>%
  count(Median_income, Income_MOE, sort = TRUE)




Income2021_ZIP_cfp_final <- Income2021_ZIP_cfp %>%
  mutate(
    Income_status = case_when(
      Median_income == "250,000+" ~ "Upper-censored",
      Median_income == "2,500-" ~ "Lower-censored",
      Median_income == "-" ~ "Missing",
      TRUE ~ "Exact"
    ),
    Median_income = suppressWarnings(as.numeric(Median_income)),
    Income_MOE = suppressWarnings(as.numeric(Income_MOE))
  )
Income2021_ZIP_cfp_clean <- Income2021_ZIP_cfp_final %>%
  filter(
    Income_status == "Exact",
    !is.na(Median_income),
    !is.na(Income_MOE),
    !is.na(policies_2021)
  )


# 1. Repetitive ZIP?
Income2021_ZIP_cfp_clean %>%
  count(zip_code) %>%
  filter(n > 1)

# 2. NA in key variables?
Income2021_ZIP_cfp_clean %>%
  summarise(
    missing_income = sum(is.na(Median_income)),
    missing_moe = sum(is.na(Income_MOE)),
    missing_policies = sum(is.na(policies_2021))
  )

# 3. Anomalies in income & policies
Income2021_ZIP_cfp_clean %>%
  summarise(
    min_income = min(Median_income),
    max_income = max(Median_income),
    min_policies = min(policies_2021),
    max_policies = max(policies_2021)
  )

write.csv(
  Income2021_ZIP_cfp_clean,
  "Income2021_ZIP_cfp_clean.csv",
  row.names = FALSE
)



Incomes_2021_2024 <- bind_rows(Income2021_ZIP_cfp_clean, Income2022_ZIP_cfp_clean,
                               Income2023_ZIP_cfp_clean, Income2024_ZIP_cfp_clean)


write.csv(Incomes_2021_2024, "Income_2021_2024.csv",
          row.names = FALSE)
