# NYC Building Decarbonization Portfolio Optimization
# 04 - Machine Learning Peer Benchmarking

library(tidyverse)
library(tidymodels)

# Load final modeling dataset
model_dataset <- readRDS(
  "data/processed/model_dataset.rds"
)

dim(model_dataset)




# NYC Building Decarbonization Portfolio Optimization
# 04 - Machine Learning Peer Benchmarking

library(tidyverse)
library(tidymodels)

# Load final modeling dataset
model_dataset <- readRDS(
  "data/processed/model_dataset.rds"
)

dim(model_dataset)






set.seed(2026)

data_split <- group_initial_split(
  model_dataset,
  group = property_id,
  prop = 0.80
)

train_data <- training(data_split)
test_data  <- testing(data_split)



nrow(train_data)
nrow(test_data)



train_data %>%
  summarise(
    records = n(),
    buildings = n_distinct(property_id)
  )

test_data %>%
  summarise(
    records = n(),
    buildings = n_distinct(property_id)
  )



length(
  intersect(
    unique(train_data$property_id),
    unique(test_data$property_id)
  )
)





# ------------------------------------------------------------
# Model 1: Linear Regression Baseline
# ------------------------------------------------------------

linear_recipe <- recipe(
  site_eui_k_btu_ft2 ~
    property_type_model +
    log_gfa +
    building_age +
    occupancy +
    number_of_buildings +
    borough +
    calendar_year,
  data = train_data
) %>%
  
  # Handle possible unseen categories in test data
  step_unknown(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  
  # Convert categorical variables into dummy variables
  step_dummy(all_nominal_predictors()) %>%
  
  # Remove zero-variance predictors
  step_zv(all_predictors())

linear_model <- linear_reg() %>%
  set_engine("lm")


linear_workflow <- workflow() %>%
  add_recipe(linear_recipe) %>%
  add_model(linear_model)



linear_fit <- linear_workflow %>%
  fit(data = train_data)

linear_predictions <- predict(
  linear_fit,
  new_data = test_data
) %>%
  bind_cols(
    test_data %>%
      select(
        property_id,
        calendar_year,
        site_eui_k_btu_ft2
      )
  )



linear_predictions %>%
  slice_head(n = 10)



linear_metrics <- linear_predictions %>%
  metrics(
    truth = site_eui_k_btu_ft2,
    estimate = .pred
  )

linear_metrics



linear_mae <- linear_predictions %>%
  mae(
    truth = site_eui_k_btu_ft2,
    estimate = .pred
  )

linear_mae




# ------------------------------------------------------------
# Model 2: Random Forest
# ------------------------------------------------------------

# Install once if needed:
# install.packages("ranger")

library(ranger)

rf_model <- rand_forest(
  trees = 500,
  mtry = 10,
  min_n = 10
) %>%
  set_engine(
    "ranger",
    importance = "permutation"
  ) %>%
  set_mode("regression")




rf_model <- rand_forest(
  trees = 500,
  mtry = 10,
  min_n = 10
) %>%
  set_engine(
    "ranger",
    importance = "permutation"
  ) %>%
  set_mode("regression")



rf_workflow <- workflow() %>%
  add_recipe(linear_recipe) %>%
  add_model(rf_model)



set.seed(2026)

rf_fit <- rf_workflow %>%
  fit(data = train_data)



set.seed(2026)

rf_fit <- rf_workflow %>%
  fit(data = train_data)




rf_predictions <- predict(
  rf_fit,
  new_data = test_data
) %>%
  bind_cols(
    test_data %>%
      select(
        property_id,
        calendar_year,
        site_eui_k_btu_ft2
      )
  )






rf_metrics <- rf_predictions %>%
  metrics(
    truth = site_eui_k_btu_ft2,
    estimate = .pred
  )

rf_metrics









rf_predictions <- predict(
  rf_fit,
  new_data = test_data
) %>%
  bind_cols(
    test_data %>%
      select(
        property_id,
        calendar_year,
        site_eui_k_btu_ft2
      )
  )




rf_metrics <- rf_predictions %>%
  metrics(
    truth = site_eui_k_btu_ft2,
    estimate = .pred
  )

rf_metrics





library(xgboost)



# ------------------------------------------------------------
# Model 3: XGBoost
# ------------------------------------------------------------

xgb_model <- boost_tree(
  trees = 500,
  tree_depth = 6,
  learn_rate = 0.05,
  min_n = 10,
  sample_size = 0.8
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")



xgb_workflow <- workflow() %>%
  add_recipe(linear_recipe) %>%
  add_model(xgb_model)



set.seed(2026)

xgb_fit <- xgb_workflow %>%
  fit(data = train_data)



xgb_predictions <- predict(
  xgb_fit,
  new_data = test_data
) %>%
  bind_cols(
    test_data %>%
      select(
        property_id,
        calendar_year,
        site_eui_k_btu_ft2
      )
  )


xgb_metrics <- xgb_predictions %>%
  metrics(
    truth = site_eui_k_btu_ft2,
    estimate = .pred
  )

xgb_metrics





# ------------------------------------------------------------
# Compare model performance
# ------------------------------------------------------------

model_comparison <- tibble(
  model = c(
    "Linear Regression",
    "Random Forest",
    "XGBoost"
  ),
  
  rmse = c(
    52.5,
    50.2,
    51.1
  ),
  
  mae = c(
    28.9,
    27.7,
    28.0
  ),
  
  rsq = c(
    0.158,
    0.232,
    0.204
  )
)

model_comparison




write_csv(
  model_comparison,
  "output/model_metrics.csv"
)





p4 <- ggplot(
  model_comparison,
  aes(
    x = reorder(model, rsq),
    y = rsq
  )
) +
  
  geom_col(
    width = 0.6,
    fill = "#155E75"
  ) +
  
  geom_text(
    aes(
      label = paste0(
        "R² = ",
        round(rsq, 3)
      )
    ),
    hjust = -0.15,
    fontface = "bold",
    size = 4.2
  ) +
  
  coord_flip() +
  
  scale_y_continuous(
    limits = c(0, 0.27),
    breaks = seq(0, 0.25, 0.05)
  ) +
  
  labs(
    title = "Random Forest Provided the Best Out-of-Sample Performance",
    subtitle =
      "Models were evaluated on buildings excluded entirely from training",
    x = NULL,
    y = "Test-set R²",
    caption =
      "Train/test split was grouped by Property ID to prevent building-level data leakage."
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      color = "#17252A"
    ),
    
    plot.subtitle = element_text(
      size = 11,
      color = "#66747B",
      margin = margin(b = 15)
    ),
    
    axis.text.y = element_text(
      size = 11,
      face = "bold"
    ),
    
    axis.title.x = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      color = "#777777",
      hjust = 0
    )
  )

p4




ggsave(
  "figures/modeling/04_model_comparison.png",
  plot = p4,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)





# ------------------------------------------------------------
# Out-of-fold predictions for peer benchmarking
# ------------------------------------------------------------

# Add a row ID so predictions can be joined back later
model_oof <- model_dataset %>%
  mutate(row_id = row_number())

set.seed(2026)

rf_folds <- group_vfold_cv(
  model_oof,
  group = property_id,
  v = 5
)


rf_folds




rf_oof_model <- rand_forest(
  trees = 300,
  mtry = 10,
  min_n = 10
) %>%
  set_engine(
    "ranger",
    importance = "permutation"
  ) %>%
  set_mode("regression")



rf_oof_workflow <- workflow() %>%
  add_recipe(linear_recipe) %>%
  add_model(rf_oof_model)


rf_control <- control_resamples(
  save_pred = TRUE,
  verbose = TRUE
)


set.seed(2026)

rf_oof_fit <- fit_resamples(
  rf_oof_workflow,
  resamples = rf_folds,
  metrics = metric_set(rmse, rsq, mae),
  control = rf_control
)

collect_metrics(rf_oof_fit)


rf_oof_predictions <- collect_predictions(rf_oof_fit)

dim(rf_oof_predictions)

rf_oof_predictions %>%
  slice_head(n = 10)


# ------------------------------------------------------------
# Build peer-benchmarking results
# ------------------------------------------------------------

benchmark_results <- rf_oof_predictions %>%
  select(
    .row,
    expected_eui = .pred
  ) %>%
  
  left_join(
    model_oof %>%
      mutate(.row = row_number()),
    by = ".row"
  ) %>%
  
  mutate(
    actual_eui = site_eui_k_btu_ft2,
    
    performance_gap =
      actual_eui - expected_eui,
    
    performance_gap_pct =
      if_else(
        expected_eui > 0,
        performance_gap / expected_eui * 100,
        NA_real_
      )
  )


benchmark_results %>%
  select(
    property_id,
    calendar_year,
    property_name,
    property_type_model,
    actual_eui,
    expected_eui,
    performance_gap,
    performance_gap_pct
  ) %>%
  slice_head(n = 10)

benchmark_results %>%
  summarise(
    records = n(),
    mean_gap = mean(performance_gap, na.rm = TRUE),
    median_gap = median(performance_gap, na.rm = TRUE),
    p90_gap = quantile(performance_gap, 0.90, na.rm = TRUE),
    p95_gap = quantile(performance_gap, 0.95, na.rm = TRUE),
    max_gap = max(performance_gap, na.rm = TRUE)
  )


benchmark_results %>%
  arrange(desc(performance_gap)) %>%
  select(
    property_id,
    calendar_year,
    property_name,
    property_type_model,
    actual_eui,
    expected_eui,
    performance_gap,
    performance_gap_pct
  ) %>%
  slice_head(n = 20) %>%

  
  
  
  write_csv(
    benchmark_results,
    "data/processed/benchmark_results.csv"
  )

saveRDS(
  benchmark_results,
  "data/processed/benchmark_results.rds"
)