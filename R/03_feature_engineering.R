# NYC Building Decarbonization Portfolio Optimization
# 03 - Feature engineering for peer benchmarking

library(tidyverse)

# Load ML-ready base dataset
model_base <- read_csv(
  "data/processed/model_base.csv",
  show_col_types = FALSE
)

dim(model_base)



summary(model_base$site_eui_k_btu_ft2)

quantile(
  model_base$site_eui_k_btu_ft2,
  probs = c(
    0,
    0.01,
    0.05,
    0.25,
    0.50,
    0.75,
    0.95,
    0.99,
    1
  ),
  na.rm = TRUE
)




model_base %>%
  summarise(
    property_types =
      n_distinct(primary_property_type_self_selected)
  )



model_base %>%
  count(
    primary_property_type_self_selected,
    sort = TRUE
  ) %>%
  slice_tail(n = 15)



# ------------------------------------------------------------
# Check unrealistic Site EUI values
# ------------------------------------------------------------

model_base %>%
  summarise(
    total_records = n(),
    
    eui_below_5 =
      sum(site_eui_k_btu_ft2 < 5, na.rm = TRUE),
    
    eui_above_1000 =
      sum(site_eui_k_btu_ft2 > 1000, na.rm = TRUE),
    
    eui_between_5_1000 =
      sum(
        site_eui_k_btu_ft2 >= 5 &
          site_eui_k_btu_ft2 <= 1000,
        na.rm = TRUE
      )
  )


model_base %>%
  arrange(desc(site_eui_k_btu_ft2)) %>%
  select(
    property_id,
    calendar_year,
    property_name,
    primary_property_type_self_selected,
    property_gfa_calculated_buildings_ft2,
    site_eui_k_btu_ft2
  ) %>%
  slice_head(n = 20) %>%
  print(n = 20)


# ------------------------------------------------------------
# Check building floor area distribution
# ------------------------------------------------------------

summary(
  model_base$property_gfa_calculated_buildings_ft2
)

quantile(
  model_base$property_gfa_calculated_buildings_ft2,
  probs = c(
    0,
    0.001,
    0.01,
    0.05,
    0.50,
    0.95,
    0.99,
    0.999,
    1
  ),
  na.rm = TRUE
)

model_base %>%
  summarise(
    gfa_le_1 =
      sum(property_gfa_calculated_buildings_ft2 <= 1),
    
    gfa_lt_1000 =
      sum(property_gfa_calculated_buildings_ft2 < 1000),
    
    gfa_lt_5000 =
      sum(property_gfa_calculated_buildings_ft2 < 5000),
    
    gfa_ge_5000 =
      sum(property_gfa_calculated_buildings_ft2 >= 5000)
  )

model_base %>%
  filter(
    property_gfa_calculated_buildings_ft2 <= 1
  ) %>%
  select(
    property_id,
    calendar_year,
    property_name,
    primary_property_type_self_selected,
    property_gfa_calculated_buildings_ft2,
    property_gfa_self_reported_ft2,
    site_eui_k_btu_ft2
  ) %>%
  slice_head(n = 20) %>%
  print(n = 20)




model_base %>%
  filter(property_gfa_calculated_buildings_ft2 <= 1) %>%
  select(
    property_id,
    calendar_year,
    property_name,
    primary_property_type_self_selected,
    property_gfa_calculated_buildings_ft2,
    property_gfa_self_reported_ft2,
    site_eui_k_btu_ft2
  ) %>%
  slice_head(n = 20) %>%
  print(n = 20, width = Inf)


model_base %>%
  filter(property_gfa_calculated_buildings_ft2 <= 1) %>%
  summarise(
    records = n(),
    missing_self_reported =
      sum(is.na(property_gfa_self_reported_ft2)),
    
    self_reported_le_1 =
      sum(
        property_gfa_self_reported_ft2 <= 1,
        na.rm = TRUE
      ),
    
    self_reported_ge_5000 =
      sum(
        property_gfa_self_reported_ft2 >= 5000,
        na.rm = TRUE
      ),
    
    median_self_reported =
      median(
        property_gfa_self_reported_ft2,
        na.rm = TRUE
      )
  )






# ------------------------------------------------------------
# Build final machine-learning dataset
# ------------------------------------------------------------

model_dataset <- model_base %>%
  
  # Remove implausible energy and floor-area records
  filter(
    site_eui_k_btu_ft2 >= 5,
    site_eui_k_btu_ft2 <= 1000,
    property_gfa_calculated_buildings_ft2 >= 1000
  ) %>%
  
  # Create modeling features
  mutate(
    
    # Floor area is highly skewed, so use log(area)
    log_gfa = log(property_gfa_calculated_buildings_ft2),
    
    # Treat year as categorical rather than continuous
    calendar_year = factor(calendar_year),
    
    borough = factor(borough),
    
    # Collapse rare property types
    property_type_model =
      forcats::fct_lump_min(
        factor(primary_property_type_self_selected),
        min = 100,
        other_level = "Other / Rare"
      )
  )


dim(model_dataset)

model_dataset %>%
  summarise(
    records = n(),
    properties = n_distinct(property_id),
    median_eui = median(site_eui_k_btu_ft2),
    min_eui = min(site_eui_k_btu_ft2),
    max_eui = max(site_eui_k_btu_ft2),
    min_gfa = min(property_gfa_calculated_buildings_ft2)
  )



model_dataset %>%
  count(property_type_model, sort = TRUE) %>%
  print(n = Inf)



n_distinct(model_dataset$property_type_model)


# ------------------------------------------------------------
# Export final machine-learning dataset
# ------------------------------------------------------------

write_csv(
  model_dataset,
  "data/processed/model_dataset.csv"
)

saveRDS(
  model_dataset,
  "data/processed/model_dataset.rds"
)