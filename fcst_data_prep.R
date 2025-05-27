
# Load libraries
library(vroom)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(here)
library(purrr)
library(fuzzyjoin)
library(stringdist)
library(rlang)
library(slider)


#STEP 1 RTRA LFS DATA 

# Cleaning the most recent LFS data from RTRA
lfs_data <- function(files) {
  # Read and process each file, appending them together
  data_list <- lapply(files, function(file) {
    vroom(file, delim = ",") |>
      filter(ERTAB != "", SYEAR != "", NAICS_5 != "") 
  })
  
  # Combine the processed data frames into one
  combined_data <- bind_rows(data_list)
  
  # Apply the region mapping to the combined dataset
  combined_data <- combined_data |>
    transmute(bc_region = case_when(
      ERTAB == "5910" ~ "Vancouver Island and Coast",
      ERTAB == "5920" ~ "Lower Mainland-Southwest",
      ERTAB == "5930" ~ "Thompson-Okanagan",
      ERTAB == "5940" ~ "Kootenay",
      ERTAB == "5950" ~ "Cariboo",
      ERTAB == "5960" ~ "North Coast",
      ERTAB == "5970" ~ "Nechako",
      ERTAB == "5980" ~ "Northeast",
      TRUE ~ "Unknown"  # Default case for unknown ERTAB
    ),
    year = SYEAR,  
    naics_5 = NAICS_5, 
    employment = `_COUNT_` / 12)
  
  return(combined_data)
}

# Specify the files as a vector of paths
files <- c(
  here("input", "emp_naics_reg_0005.csv"),
  here("input", "emp_naics_reg_0610.csv"),
  here("input", "emp_naics_reg_1115.csv"),
  here("input", "emp_naics_reg_1620.csv"),
  here("input", "emp_naics_reg_2125.csv")
)

# Call the function with the list of files to import and append them
hist_emp_allReg <- lfs_data(files)

# Names need updating every year

hist_last_year = 2024 # lfs data most recent year available
cf_first_year = 2025 # current forecast first year
cf_last_year = cf_first_year + 10

pf_first_year = cf_first_year # previous forecast first year. This should be -1 when the previous forecast is last year

hist_recent_cutoff = hist_last_year - 2 # most recent three years
hist_prev_cutoff1 = hist_last_year - 3 # end of previous ten years
hist_prev_cutoff2 = hist_prev_cutoff1 - 10 # start of previous ten years



#STEP 2: COMBINING LFS HISTORICAL AND INDUSTRY MAPPING

### import industry mapping
industry_mapping <- vroom(here("input", "industry_mapping_2025_with_stokes_agg.csv"), delim = ",") |> 
  transmute(naics_5, aggregate_industry, lmo_ind_code, lmo_industry = lmo_detailed_industry) 
 

# Import the most recent mapping and join with the historical data
hist_empReg_mapped <- hist_emp_allReg |> 
  left_join(industry_mapping) 

# Sum data for BC and join Nechako and North Coast regions
hist_emp_regWide <- hist_empReg_mapped |>
  pivot_wider(
    names_from = "bc_region", 
    values_from = "employment"
  ) |>
  mutate(
    `North Coast and Nechako` = `Nechako` + `North Coast`
  ) |>
  select(-Nechako, -`North Coast`) |>
  mutate(
    `British Columbia` = rowSums(across(c("Vancouver Island and Coast", "Lower Mainland-Southwest", "Thompson-Okanagan", 
                                          "Kootenay", "Cariboo", "North Coast and Nechako", 
                                          "Northeast")), na.rm = TRUE)
  )



# Change data to long
hist_emp_reg <- pivot_longer(hist_emp_regWide, cols = 6:13, names_to = "bc_region", values_to = "historical") |> 
  filter(year <= hist_last_year) # remove the most recent data in lfs as it's a summary of the first few months only

# DATA GROUPING
# group by lmo industry 

historical_data_0 <- hist_emp_reg |> 
  group_by(bc_region, year, aggregate_industry, lmo_industry) |> 
  summarize(historical = sum(historical, na.rm = TRUE))

# Calculate the total for each region and year (sum of all industries)
total_data <- hist_emp_reg |>
  group_by(bc_region, year) |>
  summarize(historical = sum(historical, na.rm = TRUE)) |>
  mutate(lmo_industry = "Total", aggregate_industry = "Total")  # Create a new value to represent the total

# Combine the total data with the original data
historical_data <- bind_rows(historical_data_0, total_data) |> 
  rename(employment = historical) |> 
  mutate(data_type = "lfs_historical") 




#STEP 3 LOAD STOKES INDUSTRY DATA

# Unified data loading function with forecast_type param
load_data <- function(file_path, sheet_name, region_label, data_type) {
  df <- read_excel(file_path, sheet = sheet_name, skip = 1) |>
    rename(lmo_industry = `...1`) |>
    filter(
      !is.na(lmo_industry),
      lmo_industry != "% Change",
      !lmo_industry %in% c(
        "Agriculture and fishing", "Mining and oil and gas extraction", "Construction",
        "Manufacturing", "Retail trade", "Transportation and warehousing",
        "Finance, insurance and real estate", "Professional, scientific and technical services",
        "Business, building and other support services", "Educational services",
        "Health care and Social assistance", "Information, culture and recreation",
        "Accommodation and food services", "Repair, personal and non-profit services",
        "Public administration"
      )
    )
  
  # Identify columns with year-like names (e.g., "2015", "2016", ...)
  year_cols <- grep("^\\d{4}$", names(df), value = TRUE)
  
  # Convert year columns to numeric
  df <- df |>
    mutate(across(all_of(year_cols), as.numeric)) |>
    mutate(bc_region = region_label, data_type = data_type)
  
  return(df)
}


# Region metadata
regions <- data.frame(
  sheet = c("BC", "KOO", "CAR", "NE", "TOK", "NCN", "VIC", "MSW"),
  region_label = c(
  "British Columbia", "Kootenay", "Cariboo", "Northeast",
    "Thompson-Okanagan", "North Coast and Nechako",
    "Vancouver Island and Coast", "Lower Mainland-Southwest"
  ),
  stringsAsFactors = FALSE
)

# File paths
current_file <- here("input", "IndustryEmploymentBC_current.xlsx")
prev_file <- here("input", "IndustryEmploymentBC_previous.xlsx")

# Load and label data with forecast type
current_data <- map2_dfr(regions$sheet, regions$region_label,
                         ~ load_data(current_file, .x, .y, "current_forecast"))
prev_data <- map2_dfr(regions$sheet, regions$region_label,
                      ~ load_data(prev_file, .x, .y, "previous_forecast"))

# Combine into a single data frame
employment_forecast <- bind_rows(prev_data, current_data)

# change to long format
employment_forecast_long <- employment_forecast |> 
  pivot_longer(
    cols = matches("^\\d{4}$"),   # columns named like years
    names_to = "year",
    values_to = "employment"
  ) |>
  mutate(year = as.integer(year), employment = employment*1000) |>    # optional: convert year to integer
  filter(year >= pf_first_year) |> # filter out historical data from Stokes
  select(bc_region, year, data_type, lmo_industry, employment) |> 
  # Renaming some industries to match LFS industry names
  mutate(lmo_industry = case_when(lmo_industry == "Non-Residential building construction" ~ "Non-residential building construction",
                                  lmo_industry == "Social assistance excluding child care" ~ "Social assistance (excluding child care)",
                                  lmo_industry == "Child day care services" ~ "Child day-care services",
                                  lmo_industry == "Other retail trade (excluding cars, online shopping and personal care)" ~ "Other retail trade (excluding cars and personal care)",
                                  TRUE ~ lmo_industry))



 # map aggregate industry but first group to rid of naics code 
industry_map_grp <- industry_mapping |> 
  group_by(aggregate_industry, lmo_industry) |> 
  summarise(count = n(), .groups = "drop") |> 
  select(-count)

# Map aggregate industry to employment forecast
employment_forecast_mapped <- employment_forecast_long |> 
  left_join(industry_map_grp)

# STEP 4: Combine historical and forecast data

# Perform a full join between the three datasets

employment_trend_data0 <- bind_rows(employment_forecast_mapped, historical_data) |> 
  relocate(year, .after = lmo_industry) |> 
  relocate(data_type, .before = bc_region) |> 
  arrange(data_type, bc_region, aggregate_industry, lmo_industry, year) |> 
  group_by(bc_region, data_type, lmo_industry) |> 
  mutate(
    employment = round(employment, 0),
    
    # 10-year moving average (historical only)
    ma10yr = if_else(
      data_type == "lfs_historical",
      slider::slide_dbl(employment, mean, .before = 9, .complete = TRUE),
      NA_real_
    ),
    
    # CAGR logic:
    cagr10yrs = case_when(
      # Historical: CAGR based on moving average
      data_type == "lfs_historical" ~ round(((ma10yr / lag(ma10yr, 10))^(1/10) - 1), 3),
      
      # Forecasts: Standard CAGR, only for most recent year
      data_type %in% c("previous_forecast", "current_forecast") & year == max(year) ~
        round(((employment / lag(employment, 10))^(1/10) - 1), 3),
      
      TRUE ~ NA_real_
    ),
    
    # Annual change (1-year change from raw employment)
    annual_change = round(((employment / lag(employment, 1)) - 1), 3)
  ) |> 
  ungroup()


# Calculate employment shares
employment_share_by_industry <- employment_trend_data0 |> 
  filter(bc_region != "British Columbia") |> 
  group_by(data_type, lmo_industry, year) |> 
  mutate(employment_share = employment/sum(employment))
  
# Merge shares to trend data

employment_trend_data <- employment_trend_data0 |> 
  full_join(employment_share_by_industry)
  

# Subset the data for specific region and industry
# For example, selecting "Vancouver Island and Coast" and "Employment (000s)"
selected_data <- employment_trend_data |>
  filter(lmo_industry == "Total") 



# Create the line plot using ggplot
ggplot(selected_data, aes(x = year, y = employment_share, color = data_type, label = cagr10yrs, group = interaction( lmo_industry, data_type))) +
  geom_line() +
  geom_point() +
  labs(title = "Employment Trends by Region and Industry",
       x = "Year",
       y = "Employment",
       color = "Type") +
  theme_minimal() +
  facet_wrap(~bc_region, scales = "free_y") +  # Optional: If you have multiple industries to facet by
  theme(legend.position = "bottom")





# <><><><><><><><><><><><> Share of REGION by INDUSTRY<><><><><><><><><><><><><><>

# 1) current forecast year (average shares)
current_forecast_shares <- employment_trend_data |>
  filter(data_type == "current_forecast") |>
  arrange(lmo_industry, year, bc_region) |>  
  group_by(aggregate_industry, lmo_industry, bc_region) |>
  summarize(!!paste("1.Forecast", paste0(cf_first_year, "-", cf_last_year)) := round((mean(employment_share, na.rm = TRUE)),3)) |> 
  ungroup()


# 2) Historical average shares

# Last three years
lfs_last3yrs_share <- employment_trend_data |> 
  filter(data_type == "lfs_historical",
         year >= hist_recent_cutoff) |>
  arrange(lmo_industry, year, bc_region) |>  
  group_by(aggregate_industry, lmo_industry, bc_region) |>  
  summarize(!!paste("2.Hist", paste0(hist_recent_cutoff, "-", hist_last_year)) := round((mean(employment_share, na.rm = TRUE)),3)) |>  
  ungroup()

## 10 year average
lfs_prev10yrs_share  <- employment_trend_data |> 
  filter(data_type == "lfs_historical",
         year >= hist_prev_cutoff2 & year <= hist_prev_cutoff1) |>  
  arrange(lmo_industry, year, bc_region) |>  
  group_by(aggregate_industry, lmo_industry, bc_region) |> 
  summarize(!!paste("3.Hist", paste0(hist_prev_cutoff2, "-", hist_prev_cutoff1)) := round((mean(employment_share, na.rm = TRUE)),3)) |>  
  ungroup()


# CHECK employment share calculation
## 10 year average
# lfs_region_by_industry_grps <- historical_data %>%
#   filter(year >= hist_prev_cutoff2 & year <= hist_prev_cutoff1) %>% 
#   group_by(stokes_industry, year) %>%
#   mutate(emp_share = historical/sum(historical)) %>%
#   filter(stokes_industry == "Accommodation & Food Services", year == 2010) %>% 
#   ungroup()



## Merge 3 year and 10 year shares (historical)

lfs_regional_shares <- lfs_last3yrs_share %>% 
  full_join(lfs_prev10yrs_share)

# join historical lfs shares with current forecast average shares
# Dynamically create the column names using paste

# Dynamically create the column names using paste
hist_recent <- paste("2.Hist", paste0(hist_recent_cutoff, "-", hist_last_year))
forecast_1 <- paste("1.Forecast", paste0(cf_first_year, "-", cf_last_year))

# Perform the join and dynamic subtraction
region_shares_by_industry_merged <- current_forecast_shares %>% 
  full_join(lfs_regional_shares) %>% 
  mutate(
    `4.Diff (Forecast - LFS#2)` = !!sym(forecast_1) - !!sym(hist_recent)
  )


# 3) transform data into long format

region_shares_by_industry_long <- pivot_longer(region_shares_by_industry_merged, cols= 4:7, values_to = "share", names_to = "empl_source") %>% 
  arrange(bc_region, empl_source, desc(share))

#wrap data

data_region <- region_shares_by_industry_long
data_region$source <- sapply(data_region$empl_source, 
                             FUN = function(empl_source) {paste(strwrap(empl_source, width = 16), collapse = "<br>")})

# test plot

test_plot <- data_region |> 
  filter(lmo_industry == "Food services and drinking places") |> 
  ggplot(aes(x=source, y=share, fill = bc_region)) + 
  geom_bar(stat="identity")+
  facet_wrap(~bc_region, ncol = 3)

test_plot

test <- plotly::ggplotly(test_plot) %>% 
  plotly::layout(margin = list(b = 10))
test



vroom_write(data_region, here("industryForecast_regionShares.csv"), delim = ",")
vroom_write(employment_trend_data, here("industryForecast_regionTrends.csv"), delim = ",")
