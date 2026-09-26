# NYC Building Decarbonization Portfolio Optimization
# 01 - Import and inspect raw LL84 data

library(tidyverse)
library(janitor)

# Import raw data
raw <- read_csv("data/raw/nyc_ll84_raw.csv")

# Clean column names
raw <- raw %>%
  clean_names()

# Basic checks
dim(raw)
glimpse(raw)
names(raw)[1:30]

raw %>%
  count(calendar_year)
names(raw)[
  str_detect(
    names(raw),
    "site_eui|ghg|greenhouse|electricity|natural_gas|gross_floor_area"
  )
]


names(raw)[
  str_detect(
    names(raw),
    "property_gfa|site_energy_use|source_eui|borough|latitude|longitude"
  )
]

core <- raw %>%
  select(
    calendar_year,
    property_id,
    property_name,
    address_1,
    borough,
    postal_code,
    latitude,
    longitude,
    
    primary_property_type_self_selected,
    year_built,
    number_of_buildings,
    occupancy,
    
    property_gfa_self_reported_ft2,
    property_gfa_calculated_buildings_ft2,
    
    energy_star_score,
    site_eui_k_btu_ft2,
    weather_normalized_site_eui_k_btu_ft2,
    site_energy_use_k_btu,
    
    electricity_use_grid_purchase_k_wh,
    natural_gas_use_therms,
    
    total_location_based_ghg_emissions_metric_tons_co2e,
    total_location_based_ghg_emissions_intensity_kg_co2e_ft2,
    direct_ghg_emissions_metric_tons_co2e,
    indirect_location_based_ghg_emissions_metric_tons_co2e
  )

dim(core)

glimpse(core)


core %>%
  summarise(
    total_rows = n(),
    unique_properties = n_distinct(property_id),
    missing_property_id = sum(is.na(property_id)),
    missing_gfa = sum(is.na(property_gfa_calculated_buildings_ft2)),
    missing_eui = sum(is.na(site_eui_k_btu_ft2)),
    missing_ghg = sum(is.na(total_location_based_ghg_emissions_metric_tons_co2e))
  )

# Check duplicate property-year records
duplicates <- core %>%
  count(property_id, calendar_year) %>%
  filter(n > 1)

nrow(duplicates)

# Check how many years of data each property has
year_coverage <- core %>%
  distinct(property_id, calendar_year) %>%
  count(property_id, name = "years_present")

year_coverage %>%
  count(years_present)

core %>%
  summarise(
    missing_ghg =
      sum(is.na(total_location_based_ghg_emissions_metric_tons_co2e)),
    
    nonpositive_gfa =
      sum(property_gfa_calculated_buildings_ft2 <= 0, na.rm = TRUE),
    
    nonpositive_eui =
      sum(site_eui_k_btu_ft2 <= 0, na.rm = TRUE),
    
    nonpositive_ghg =
      sum(total_location_based_ghg_emissions_metric_tons_co2e <= 0,
          na.rm = TRUE)
  )


# Inspect duplicate property-year records
duplicate_detail <- core %>%
  semi_join(
    duplicates,
    by = c("property_id", "calendar_year")
  ) %>%
  arrange(property_id, calendar_year)

# How many rows are in each duplicate group?
duplicates %>%
  count(n)

duplicate_summary <- core %>%
  group_by(property_id, calendar_year) %>%
  filter(n() > 1) %>%
  summarise(
    rows = n(),
    different_names = n_distinct(property_name),
    different_addresses = n_distinct(address_1),
    different_eui = n_distinct(site_eui_k_btu_ft2),
    different_ghg =
      n_distinct(total_location_based_ghg_emissions_metric_tons_co2e),
    different_gfa =
      n_distinct(property_gfa_calculated_buildings_ft2),
    .groups = "drop"
  )

duplicate_summary %>%

  
  duplicate_summary %>%
  summarise(
    duplicate_groups = n(),
    groups_with_different_eui = sum(different_eui > 1),
    groups_with_different_ghg = sum(different_ghg > 1),
    groups_with_different_gfa = sum(different_gfa > 1),
    groups_with_different_address = sum(different_addresses > 1)
  ) %>%
  print(width = Inf)

names(raw)[
  str_detect(
    names(raw),
    "date|submission|submitted|updated|status|report"
  )
]

# Inspect duplicate records with submission/update dates

duplicate_versions <- raw %>%
  semi_join(
    duplicates,
    by = c("property_id", "calendar_year")
  ) %>%
  select(
    property_id,
    calendar_year,
    property_name,
    address_1,
    
    site_eui_k_btu_ft2,
    property_gfa_calculated_buildings_ft2,
    total_location_based_ghg_emissions_metric_tons_co2e,
    
    report_submission_date,
    report_generation_date,
    last_modified_date_property
  ) %>%
  arrange(property_id, calendar_year)

duplicate_versions %>%
  slice_head(n = 20) %>%

  

  
  # ------------------------------------------------------------
# Keep the latest submission for each property-year
# ------------------------------------------------------------

clean_versions <- raw %>%
  mutate(
    report_submission_dt = mdy_hms(report_submission_date),
    report_generation_dt = mdy_hms(report_generation_date)
  ) %>%
  arrange(
    property_id,
    calendar_year,
    desc(report_submission_dt),
    desc(report_generation_dt)
  ) %>%
  group_by(property_id, calendar_year) %>%
  slice_head(n = 1) %>%
  ungroup()


clean_versions %>%
  count(property_id, calendar_year) %>%
  filter(n > 1) %>%
  nrow()

clean_versions %>%

  
  # ------------------------------------------------------------
# Select core variables and convert numeric metrics
# ------------------------------------------------------------

core_clean <- clean_versions %>%
  select(
    calendar_year,
    property_id,
    property_name,
    address_1,
    borough,
    postal_code,
    latitude,
    longitude,
    
    primary_property_type_self_selected,
    year_built,
    number_of_buildings,
    occupancy,
    
    property_gfa_self_reported_ft2,
    property_gfa_calculated_buildings_ft2,
    
    energy_star_score,
    site_eui_k_btu_ft2,
    weather_normalized_site_eui_k_btu_ft2,
    site_energy_use_k_btu,
    
    electricity_use_grid_purchase_k_wh,
    natural_gas_use_therms,
    
    total_location_based_ghg_emissions_metric_tons_co2e,
    total_location_based_ghg_emissions_intensity_kg_co2e_ft2,
    direct_ghg_emissions_metric_tons_co2e,
    indirect_location_based_ghg_emissions_metric_tons_co2e,
    
    report_submission_dt
  )

numeric_metric_cols <- c(
  "energy_star_score",
  "site_eui_k_btu_ft2",
  "weather_normalized_site_eui_k_btu_ft2",
  "site_energy_use_k_btu",
  "electricity_use_grid_purchase_k_wh",
  "natural_gas_use_therms",
  "total_location_based_ghg_emissions_metric_tons_co2e",
  "total_location_based_ghg_emissions_intensity_kg_co2e_ft2",
  "direct_ghg_emissions_metric_tons_co2e",
  "indirect_location_based_ghg_emissions_metric_tons_co2e"
)

core_clean <- core_clean %>%
  mutate(
    across(
      all_of(numeric_metric_cols),
      ~ parse_number(
        as.character(.x),
        na = c("", "NA", "Not Available")
      )
    )
  )

core_clean %>%
  select(
    energy_star_score,
    site_eui_k_btu_ft2,
    electricity_use_grid_purchase_k_wh,
    natural_gas_use_therms,
    total_location_based_ghg_emissions_metric_tons_co2e
  ) %>%
  glimpse()

core_clean %>%

  
  core_clean %>%
  summarise(
    total_rows = n(),
    
    missing_property_type =
      sum(is.na(primary_property_type_self_selected)),
    
    missing_year_built =
      sum(is.na(year_built)),
    
    missing_occupancy =
      sum(is.na(occupancy)),
    
    missing_gfa =
      sum(is.na(property_gfa_calculated_buildings_ft2)),
    
    missing_eui =
      sum(is.na(site_eui_k_btu_ft2)),
    
    missing_ghg =
      sum(is.na(total_location_based_ghg_emissions_metric_tons_co2e)),
    
    missing_eui_and_ghg =
      sum(
        is.na(site_eui_k_btu_ft2) &
          is.na(total_location_based_ghg_emissions_metric_tons_co2e)
      )
  )

core_clean %>%
  count(primary_property_type_self_selected, sort = TRUE) %>%
  slice_head(n = 20)

core_clean %>%
  summarise(
    total_rows = n(),
    
    missing_property_type =
      sum(is.na(primary_property_type_self_selected)),
    
    missing_year_built =
      sum(is.na(year_built)),
    
    missing_occupancy =
      sum(is.na(occupancy)),
    
    missing_gfa =
      sum(is.na(property_gfa_calculated_buildings_ft2)),
    
    missing_eui =
      sum(is.na(site_eui_k_btu_ft2)),
    
    missing_ghg =
      sum(is.na(total_location_based_ghg_emissions_metric_tons_co2e)),
    
    missing_eui_and_ghg =
      sum(
        is.na(site_eui_k_btu_ft2) &
          is.na(total_location_based_ghg_emissions_metric_tons_co2e)
      )
  ) %>%
  print(width = Inf)

# ------------------------------------------------------------
# Create data quality flags
# ------------------------------------------------------------

full_clean <- core_clean %>%
  mutate(
    valid_gfa =
      !is.na(property_gfa_calculated_buildings_ft2) &
      property_gfa_calculated_buildings_ft2 > 0,
    
    valid_eui =
      !is.na(site_eui_k_btu_ft2) &
      site_eui_k_btu_ft2 > 0,
    
    valid_ghg =
      !is.na(total_location_based_ghg_emissions_metric_tons_co2e) &
      total_location_based_ghg_emissions_metric_tons_co2e > 0
  )

full_clean %>%
  summarise(
    total_rows = n(),
    valid_gfa_rows = sum(valid_gfa),
    valid_eui_rows = sum(valid_eui),
    valid_ghg_rows = sum(valid_ghg),
    valid_eui_and_ghg = sum(valid_eui & valid_ghg)
  )

full_clean %>%
  summarise(
    year_built_zero_or_future =
      sum(year_built <= 0 | year_built > 2024, na.rm = TRUE),
    
    occupancy_outside_range =
      sum(occupancy < 0 | occupancy > 100, na.rm = TRUE)
  )
full_clean %>%
  filter(year_built <= 0 | year_built > 2024) %>%
  select(
    property_id,
    calendar_year,
    property_name,
    address_1,
    primary_property_type_self_selected,
    year_built
  ) %>%
  print(n = Inf)

full_clean %>%
  filter(year_built > calendar_year) %>%
  select(
    property_id,
    calendar_year,
    property_name,
    primary_property_type_self_selected,
    year_built
  ) %>%
  print(n = Inf)

full_clean <- full_clean %>%
  mutate(
    year_built_clean = if_else(
      year_built <= 0 | year_built > calendar_year,
      NA_real_,
      year_built
    )
  )


full_clean %>%
  summarise(
    missing_year_built_clean = sum(is.na(year_built_clean))
  )


full_clean <- full_clean %>%
  mutate(
    building_age = calendar_year - year_built_clean
  )

summary(full_clean$building_age)


model_base <- full_clean %>%
  filter(
    valid_gfa,
    valid_eui,
    !is.na(year_built_clean),
    !is.na(primary_property_type_self_selected),
    occupancy >= 0,
    occupancy <= 100
  )

carbon_base <- full_clean %>%
  filter(
    valid_gfa,
    valid_ghg
  )

retrofit_base <- full_clean %>%
  filter(
    valid_gfa,
    valid_eui,
    valid_ghg
  )

nrow(model_base)
nrow(carbon_base)
nrow(retrofit_base)


balanced_panel <- retrofit_base %>%
  group_by(property_id) %>%
  filter(n_distinct(calendar_year) == 3) %>%
  ungroup()


balanced_panel %>%
  summarise(
    rows = n(),
    buildings = n_distinct(property_id)
  )

balanced_panel %>%
  count(calendar_year)


# ------------------------------------------------------------
# Export cleaned datasets
# ------------------------------------------------------------

write_csv(
  model_base,
  "data/processed/model_base.csv"
)

write_csv(
  carbon_base,
  "data/processed/carbon_base.csv"
)

write_csv(
  retrofit_base,
  "data/processed/retrofit_base.csv"
)




data_quality_summary <- tibble(
  metric = c(
    "Raw records",
    "Records after deduplication",
    "Valid ML records",
    "Valid carbon records",
    "Valid retrofit records",
    "Balanced panel buildings",
    "Balanced panel records"
  ),
  
  value = c(
    nrow(raw),
    nrow(clean_versions),
    nrow(model_base),
    nrow(carbon_base),
    nrow(retrofit_base),
    n_distinct(balanced_panel$property_id),
    nrow(balanced_panel)
  )
)

data_quality_summary

write_csv(
  data_quality_summary,
  "output/data_quality_summary.csv"
)