# ============================================================
# NYC Building Decarbonization Portfolio Optimization
# 06 - Final Project Summary
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Load final outputs
# ------------------------------------------------------------

priority_candidates <- read_csv(
  "data/processed/decarbonization_priority_candidates.csv",
  show_col_types = FALSE
)

priority_summary <- read_csv(
  "output/priority_group_summary.csv",
  show_col_types = FALSE
)

high_priority_summary <- read_csv(
  "output/high_priority_summary.csv",
  show_col_types = FALSE
)

top15 <- read_csv(
  "output/top_15_priority_buildings.csv",
  show_col_types = FALSE
)

scenario_summary <- read_csv(
  "output/ghg_reduction_scenarios.csv",
  show_col_types = FALSE
)


# ------------------------------------------------------------
# 2. Core project KPIs
# ------------------------------------------------------------

persistent_candidates <-
  nrow(priority_candidates)

high_priority_buildings <-
  priority_candidates %>%
  filter(
    priority_group == "High Gap + High GHG"
  ) %>%
  nrow()

all_candidate_ghg <-
  sum(
    priority_candidates$ghg_2024,
    na.rm = TRUE
  )

high_priority_ghg <-
  priority_candidates %>%
  filter(
    priority_group == "High Gap + High GHG"
  ) %>%
  summarise(
    value = sum(
      ghg_2024,
      na.rm = TRUE
    )
  ) %>%
  pull(value)

top15_ghg <-
  sum(
    top15$ghg_2024,
    na.rm = TRUE
  )

top15_share <-
  top15_ghg /
  all_candidate_ghg *
  100


# ------------------------------------------------------------
# 3. Pull scenario results
# ------------------------------------------------------------

scenario_10 <- scenario_summary %>%
  filter(
    scenario == "10% gap closure"
  )

scenario_25 <- scenario_summary %>%
  filter(
    scenario == "25% gap closure"
  )

scenario_50 <- scenario_summary %>%
  filter(
    scenario == "50% gap closure"
  )


# ------------------------------------------------------------
# 4. Final KPI table
# ------------------------------------------------------------

final_project_kpis <- tibble(
  metric = c(
    "Persistent underperformers with 2024 GHG data",
    "High-priority buildings",
    "Persistent candidate GHG emissions",
    "High-priority GHG emissions",
    "Top 15 candidate GHG emissions",
    "Top 15 share of candidate GHG",
    "10% gap-closure GHG reduction",
    "25% gap-closure GHG reduction",
    "50% gap-closure GHG reduction"
  ),
  
  value = c(
    scales::comma(persistent_candidates),
    
    scales::comma(high_priority_buildings),
    
    paste0(
      round(
        all_candidate_ghg / 1e6,
        2
      ),
      "M tCO2e"
    ),
    
    paste0(
      round(
        high_priority_ghg / 1e6,
        2
      ),
      "M tCO2e"
    ),
    
    paste0(
      scales::comma(
        round(top15_ghg)
      ),
      " tCO2e"
    ),
    
    paste0(
      round(
        top15_share,
        1
      ),
      "%"
    ),
    
    paste0(
      scales::comma(
        round(
          scenario_10$estimated_ghg_reduction
        )
      ),
      " tCO2e"
    ),
    
    paste0(
      scales::comma(
        round(
          scenario_25$estimated_ghg_reduction
        )
      ),
      " tCO2e"
    ),
    
    paste0(
      scales::comma(
        round(
          scenario_50$estimated_ghg_reduction
        )
      ),
      " tCO2e"
    )
  )
)

final_project_kpis


write_csv(
  final_project_kpis,
  "output/final_project_kpis.csv"
)




final_top20 <- priority_candidates %>%
  arrange(
    desc(priority_score)
  ) %>%
  select(
    property_id,
    property_name,
    property_type_model,
    borough,
    mean_gap,
    mean_gap_pct,
    eui_2024,
    expected_eui_2024,
    ghg_2024,
    ghg_intensity_2024,
    priority_score
  ) %>%
  slice_head(
    n = 20
  ) %>%
  mutate(
    rank = row_number(),
    .before = 1
  )

final_top20 %>%
  print(
    n = 20,
    width = Inf
  )

write_csv(
  final_top20,
  "output/final_top20_priority_buildings.csv"
)