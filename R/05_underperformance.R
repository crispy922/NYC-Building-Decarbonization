# NYC Building Decarbonization Portfolio Optimization
# 05 - Persistent Underperformance Detection

library(tidyverse)

benchmark_results <- readRDS(
  "data/processed/benchmark_results.rds"
)

benchmark_3yr <- benchmark_results %>%
  group_by(property_id) %>%
  filter(
    n_distinct(calendar_year) == 3
  ) %>%
  ungroup()


benchmark_3yr %>%
  summarise(
    records = n(),
    buildings = n_distinct(property_id)
  )



building_benchmarks <- benchmark_3yr %>%
  group_by(property_id) %>%
  summarise(
    
    years_underperforming =
      sum(performance_gap > 0),
    
    mean_gap =
      mean(performance_gap),
    
    median_gap =
      median(performance_gap),
    
    mean_gap_pct =
      mean(performance_gap_pct),
    
    eui_2022 =
      actual_eui[calendar_year == 2022][1],
    
    eui_2023 =
      actual_eui[calendar_year == 2023][1],
    
    eui_2024 =
      actual_eui[calendar_year == 2024][1],
    
    expected_eui_2024 =
      expected_eui[calendar_year == 2024][1],
    
    .groups = "drop"
  )




building_identity <- benchmark_3yr %>%
  filter(calendar_year == 2024) %>%
  select(
    property_id,
    property_name,
    property_type_model,
    borough,
    property_gfa_calculated_buildings_ft2
  )

building_benchmarks <- building_benchmarks %>%
  left_join(
    building_identity,
    by = "property_id"
  )



p90_mean_gap <- quantile(
  building_benchmarks$mean_gap,
  0.90,
  na.rm = TRUE
)

p90_mean_gap




persistent_underperformers <- building_benchmarks %>%
  filter(
    years_underperforming == 3,
    mean_gap >= p90_mean_gap
  ) %>%
  arrange(desc(mean_gap))



nrow(persistent_underperformers)



persistent_underperformers %>%
  select(
    property_id,
    property_name,
    property_type_model,
    borough,
    years_underperforming,
    mean_gap,
    mean_gap_pct,
    eui_2022,
    eui_2023,
    eui_2024,
    expected_eui_2024
  ) %>%
  slice_head(n = 20) %>%
  print(n = 20, width = Inf)




# ------------------------------------------------------------
# Add 2024 carbon emissions to persistent underperformers
# ------------------------------------------------------------

carbon_2024 <- read_csv(
  "data/processed/carbon_base.csv",
  show_col_types = FALSE
) %>%
  filter(calendar_year == 2024) %>%
  select(
    property_id,
    ghg_2024 =
      total_location_based_ghg_emissions_metric_tons_co2e,
    ghg_intensity_2024 =
      total_location_based_ghg_emissions_intensity_kg_co2e_ft2
  )


priority_buildings <- persistent_underperformers %>%
  left_join(
    carbon_2024,
    by = "property_id"
  )




priority_buildings %>%
  summarise(
    buildings = n(),
    missing_ghg = sum(is.na(ghg_2024)),
    median_ghg = median(ghg_2024, na.rm = TRUE),
    total_ghg = sum(ghg_2024, na.rm = TRUE)
  )




priority_buildings %>%
  arrange(desc(ghg_2024)) %>%
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
    ghg_intensity_2024
  ) %>%
  slice_head(n = 20) %>%
  print(n = 20, width = Inf)




# ------------------------------------------------------------
# Priority matrix: performance gap x carbon emissions
# ------------------------------------------------------------

priority_matrix_data <- priority_buildings %>%
  filter(
    !is.na(ghg_2024),
    ghg_2024 > 0
  )

gap_cutoff <- median(
  priority_matrix_data$mean_gap,
  na.rm = TRUE
)

ghg_cutoff <- median(
  priority_matrix_data$ghg_2024,
  na.rm = TRUE
)

gap_cutoff
ghg_cutoff




priority_matrix_data <- priority_matrix_data %>%
  mutate(
    priority_group = case_when(
      mean_gap >= gap_cutoff &
        ghg_2024 >= ghg_cutoff ~
        "High Gap + High GHG",
      
      mean_gap >= gap_cutoff &
        ghg_2024 < ghg_cutoff ~
        "High Gap + Lower GHG",
      
      mean_gap < gap_cutoff &
        ghg_2024 >= ghg_cutoff ~
        "Lower Gap + High GHG",
      
      TRUE ~
        "Lower Gap + Lower GHG"
    )
  )




priority_matrix_data %>%
  count(priority_group)




priority_matrix_data <- priority_matrix_data %>%
  mutate(
    gap_rank = percent_rank(mean_gap),
    ghg_rank = percent_rank(ghg_2024),
    
    priority_score =
      0.5 * gap_rank +
      0.5 * ghg_rank
  )









top_priority <- priority_matrix_data %>%
  arrange(desc(priority_score)) %>%
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
  slice_head(n = 15)

top_priority %>%
  print(n = 15, width = Inf)




# ------------------------------------------------------------
# Figure 5: Decarbonization Priority Matrix
# FINAL CLEAN VERSION
# ------------------------------------------------------------

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# 1. Calculate positions for annotations
# ------------------------------------------------------------

x_max <- max(
  priority_matrix_data$mean_gap,
  na.rm = TRUE
)

y_min <- min(
  priority_matrix_data$ghg_2024[
    priority_matrix_data$ghg_2024 > 0
  ],
  na.rm = TRUE
)

y_max <- max(
  priority_matrix_data$ghg_2024,
  na.rm = TRUE
)

# Position for "Top Priority" label
top_priority_x <-
  gap_cutoff +
  0.58 * (x_max - gap_cutoff)

top_priority_y <-
  exp(
    log(ghg_cutoff) +
    0.68 * (
      log(y_max) -
      log(ghg_cutoff)
    )
  )


# ------------------------------------------------------------
# 2. Create figure
# ------------------------------------------------------------

p5 <- ggplot(
  priority_matrix_data,
  aes(
    x = mean_gap,
    y = ghg_2024,
    color = priority_group
  )
) +

  # Building points
  geom_point(
    alpha = 0.50,
    size = 2.3
  ) +

  # Vertical cutoff: performance gap
  geom_vline(
    xintercept = gap_cutoff,
    linetype = "dashed",
    linewidth = 0.8,
    color = "#68747A"
  ) +

  # Horizontal cutoff: GHG emissions
  geom_hline(
    yintercept = ghg_cutoff,
    linetype = "dashed",
    linewidth = 0.8,
    color = "#68747A"
  ) +

  # GHG emissions are highly skewed
  scale_y_log10(
    labels = comma
  ) +

  # Four screening groups
  scale_color_manual(
    values = c(
      "High Gap + High GHG" =
        "#C95F43",

      "High Gap + Lower GHG" =
        "#DDA640",

      "Lower Gap + High GHG" =
        "#4E88A0",

      "Lower Gap + Lower GHG" =
        "#B8C0C4"
    )
  ) +

  # ----------------------------------------------------------
  # Highlight ONLY the most important quadrant
  # ----------------------------------------------------------

  annotate(
    "label",
    x = top_priority_x,
    y = top_priority_y,

    label =
      "TOP PRIORITY\nHigh Gap + High GHG",

    size = 4.2,
    fontface = "bold",

    color = "#963F2D",
    fill = "#FBE9E4"
  ) +

  # ----------------------------------------------------------
  # Cutoff labels
  # ----------------------------------------------------------

  annotate(
    "text",
    x = gap_cutoff + 8,
    y = y_min * 1.25,

    label = paste0(
      "Gap cutoff: ",
      round(gap_cutoff, 1)
    ),

    hjust = 0,
    vjust = 0,

    size = 3.4,
    color = "#66747B"
  ) +

  annotate(
    "text",
    x = x_max * 0.98,
    y = ghg_cutoff * 1.12,

    label = paste0(
      "GHG cutoff: ",
      round(ghg_cutoff, 0),
      " tCO2e"
    ),

    hjust = 1,
    vjust = -0.2,

    size = 3.4,
    color = "#66747B"
  ) +

  # ----------------------------------------------------------
  # Titles and labels
  # ----------------------------------------------------------

  labs(
    title =
      "Decarbonization Priority Matrix",

    subtitle =
      "Persistent underperformers screened by energy-performance gap and 2024 carbon emissions",

    x =
      "Three-year mean EUI performance gap\n(Actual EUI − Expected EUI)",

    y =
      "2024 location-based GHG emissions\n(metric tons CO2e, log scale)",

    color = NULL,

    caption =
      paste0(
        "1,939 buildings with three-year underperformance signals and reported 2024 emissions.\n",
        "Screening results require building-level engineering and financial review before retrofit decisions."
      )
  ) +

  # ----------------------------------------------------------
  # Theme
  # ----------------------------------------------------------

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      size = 19,
      face = "bold",
      color = "#17252A"
    ),

    plot.subtitle = element_text(
      size = 11,
      color = "#66747B",
      margin = margin(
        b = 15
      )
    ),

    axis.title.x = element_text(
      face = "bold",
      size = 11.5,
      margin = margin(
        t = 10
      )
    ),

    axis.title.y = element_text(
      face = "bold",
      size = 11.5,
      margin = margin(
        r = 10
      )
    ),

    axis.text = element_text(
      color = "#566167"
    ),

    panel.grid.minor =
      element_blank(),

    panel.grid.major =
      element_line(
        color = "#E7EAEC",
        linewidth = 0.45
      ),

    legend.position =
      "bottom",

    legend.text = element_text(
      size = 10
    ),

    plot.caption = element_text(
      size = 8.5,
      color = "#777777",
      hjust = 0,
      margin = margin(
        t = 12
      )
    ),

    plot.margin = margin(
      15,
      20,
      15,
      15
    )
  ) +

  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,

      override.aes = list(
        size = 4,
        alpha = 1
      )
    )
  )


# ------------------------------------------------------------
# 3. Display figure
# ------------------------------------------------------------

p5


# ------------------------------------------------------------
# 4. Save final figure
# ------------------------------------------------------------

ggsave(
  filename =
    "figures/modeling/05_decarbonization_priority_matrix.png",

  plot = p5,

  width = 11,
  height = 7.5,

  dpi = 300,

  bg = "white"
)




# ------------------------------------------------------------
# Figure 5: Decarbonization Priority Matrix
# FINAL CLEAN VERSION
# ------------------------------------------------------------

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# 1. Calculate positions for annotations
# ------------------------------------------------------------

x_max <- max(
  priority_matrix_data$mean_gap,
  na.rm = TRUE
)

y_min <- min(
  priority_matrix_data$ghg_2024[
    priority_matrix_data$ghg_2024 > 0
  ],
  na.rm = TRUE
)

y_max <- max(
  priority_matrix_data$ghg_2024,
  na.rm = TRUE
)

# Position for "Top Priority" label
top_priority_x <-
  gap_cutoff +
  0.58 * (x_max - gap_cutoff)

top_priority_y <-
  exp(
    log(ghg_cutoff) +
    0.68 * (
      log(y_max) -
      log(ghg_cutoff)
    )
  )


# ------------------------------------------------------------
# 2. Create figure
# ------------------------------------------------------------

p5 <- ggplot(
  priority_matrix_data,
  aes(
    x = mean_gap,
    y = ghg_2024,
    color = priority_group
  )
) +

  # Building points
  geom_point(
    alpha = 0.50,
    size = 2.3
  ) +

  # Vertical cutoff: performance gap
  geom_vline(
    xintercept = gap_cutoff,
    linetype = "dashed",
    linewidth = 0.8,
    color = "#68747A"
  ) +

  # Horizontal cutoff: GHG emissions
  geom_hline(
    yintercept = ghg_cutoff,
    linetype = "dashed",
    linewidth = 0.8,
    color = "#68747A"
  ) +

  # GHG emissions are highly skewed
  scale_y_log10(
    labels = comma
  ) +

  # Four screening groups
  scale_color_manual(
    values = c(
      "High Gap + High GHG" =
        "#C95F43",

      "High Gap + Lower GHG" =
        "#DDA640",

      "Lower Gap + High GHG" =
        "#4E88A0",

      "Lower Gap + Lower GHG" =
        "#B8C0C4"
    )
  ) +

  # ----------------------------------------------------------
  # Highlight ONLY the most important quadrant
  # ----------------------------------------------------------

  annotate(
    "label",
    x = top_priority_x,
    y = top_priority_y,

    label =
      "TOP PRIORITY\nHigh Gap + High GHG",

    size = 4.2,
    fontface = "bold",

    color = "#963F2D",
    fill = "#FBE9E4"
  ) +

  # ----------------------------------------------------------
  # Cutoff labels
  # ----------------------------------------------------------

  annotate(
    "text",
    x = gap_cutoff + 8,
    y = y_min * 1.25,

    label = paste0(
      "Gap cutoff: ",
      round(gap_cutoff, 1)
    ),

    hjust = 0,
    vjust = 0,

    size = 3.4,
    color = "#66747B"
  ) +

  annotate(
    "text",
    x = x_max * 0.98,
    y = ghg_cutoff * 1.12,

    label = paste0(
      "GHG cutoff: ",
      round(ghg_cutoff, 0),
      " tCO2e"
    ),

    hjust = 1,
    vjust = -0.2,

    size = 3.4,
    color = "#66747B"
  ) +

  # ----------------------------------------------------------
  # Titles and labels
  # ----------------------------------------------------------

  labs(
    title =
      "Decarbonization Priority Matrix",

    subtitle =
      "Persistent underperformers screened by energy-performance gap and 2024 carbon emissions",

    x =
      "Three-year mean EUI performance gap\n(Actual EUI − Expected EUI)",

    y =
      "2024 location-based GHG emissions\n(metric tons CO2e, log scale)",

    color = NULL,

    caption =
      paste0(
        "1,939 buildings with three-year underperformance signals and reported 2024 emissions.\n",
        "Screening results require building-level engineering and financial review before retrofit decisions."
      )
  ) +

  # ----------------------------------------------------------
  # Theme
  # ----------------------------------------------------------

  theme_minimal(
    base_size = 13
  ) +

  theme(

    plot.title = element_text(
      size = 19,
      face = "bold",
      color = "#17252A"
    ),

    plot.subtitle = element_text(
      size = 11,
      color = "#66747B",
      margin = margin(
        b = 15
      )
    ),

    axis.title.x = element_text(
      face = "bold",
      size = 11.5,
      margin = margin(
        t = 10
      )
    ),

    axis.title.y = element_text(
      face = "bold",
      size = 11.5,
      margin = margin(
        r = 10
      )
    ),

    axis.text = element_text(
      color = "#566167"
    ),

    panel.grid.minor =
      element_blank(),

    panel.grid.major =
      element_line(
        color = "#E7EAEC",
        linewidth = 0.45
      ),

    legend.position =
      "bottom",

    legend.text = element_text(
      size = 10
    ),

    plot.caption = element_text(
      size = 8.5,
      color = "#777777",
      hjust = 0,
      margin = margin(
        t = 12
      )
    ),

    plot.margin = margin(
      15,
      20,
      15,
      15
    )
  ) +

  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,

      override.aes = list(
        size = 4,
        alpha = 1
      )
    )
  )


# ------------------------------------------------------------
# 3. Display figure
# ------------------------------------------------------------

p5


# ------------------------------------------------------------
# 4. Save final figure
# ------------------------------------------------------------

ggsave(
  filename =
    "figures/modeling/05_decarbonization_priority_matrix.png",

  plot = p5,

  width = 11,
  height = 7.5,

  dpi = 300,

  bg = "white"
)





# ------------------------------------------------------------
# Figure 5: Decarbonization Priority Matrix
# FINAL CLEAN VERSION
# ------------------------------------------------------------

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# 1. Calculate positions for annotations
# ------------------------------------------------------------

x_max <- max(
  priority_matrix_data$mean_gap,
  na.rm = TRUE
)

y_min <- min(
  priority_matrix_data$ghg_2024[
    priority_matrix_data$ghg_2024 > 0
  ],
  na.rm = TRUE
)

y_max <- max(
  priority_matrix_data$ghg_2024,
  na.rm = TRUE
)

# Position for "Top Priority" label




# ------------------------------------------------------------
# Figure 5: Decarbonization Priority Matrix
# Percentile version
# ------------------------------------------------------------

priority_plot_data <- priority_matrix_data %>%
  mutate(
    gap_percentile = gap_rank * 100,
    ghg_percentile = ghg_rank * 100
  )

p5 <- ggplot(
  priority_plot_data,
  aes(
    x = gap_percentile,
    y = ghg_percentile,
    color = priority_group
  )
) +
  
  geom_point(
    alpha = 0.55,
    size = 2.4
  ) +
  
  # 50th percentile cutoffs
  geom_vline(
    xintercept = 50,
    linetype = "dashed",
    linewidth = 0.8,
    color = "#68747A"
  ) +
  
  geom_hline(
    yintercept = 50,
    linetype = "dashed",
    linewidth = 0.8,
    color = "#68747A"
  ) +
  
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_color_manual(
    values = c(
      "High Gap + High GHG"   = "#C95F43",
      "High Gap + Lower GHG"  = "#DDA640",
      "Lower Gap + High GHG"  = "#4E88A0",
      "Lower Gap + Lower GHG" = "#B8C0C4"
    )
  ) +
  
  annotate(
    "label",
    x = 78,
    y = 88,
    label = "TOP PRIORITY\nHigh Gap + High GHG",
    size = 4.1,
    fontface = "bold",
    color = "#963F2D",
    fill = "#FBE9E4"
  ) +
  
  labs(
    title = "Decarbonization Priority Matrix",
    subtitle =
      "Persistent underperformers ranked by relative energy-performance gap and carbon emissions",
    x =
      "EUI performance-gap percentile\n(higher = worse relative performance)",
    y =
      "2024 GHG emissions percentile\n(higher = larger carbon footprint)",
    color = NULL,
    caption =
      "Percentiles are calculated among 1,939 persistent underperformers.\nScreening results require building-level engineering and financial review before retrofit decisions."
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      size = 19,
      face = "bold",
      color = "#17252A"
    ),
    
    plot.subtitle = element_text(
      size = 11,
      color = "#66747B",
      margin = margin(b = 15)
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      color = "#E7EAEC",
      linewidth = 0.45
    ),
    
    legend.position = "bottom",
    
    plot.caption = element_text(
      size = 8.5,
      color = "#777777",
      hjust = 0
    )
  ) +
  
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE,
      override.aes = list(
        size = 4,
        alpha = 1
      )
    )
  )

p5



ggsave(
  "figures/modeling/05_decarbonization_priority_matrix.png",
  plot = p5,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)



# ------------------------------------------------------------
# Final portfolio screening summary
# ------------------------------------------------------------

priority_summary <- priority_matrix_data %>%
  group_by(priority_group) %>%
  summarise(
    buildings = n(),
    
    total_ghg_2024 =
      sum(ghg_2024, na.rm = TRUE),
    
    median_ghg_2024 =
      median(ghg_2024, na.rm = TRUE),
    
    median_mean_gap =
      median(mean_gap, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    share_of_buildings_pct =
      buildings / sum(buildings) * 100,
    
    share_of_ghg_pct =
      total_ghg_2024 /
      sum(total_ghg_2024) * 100
  )






high_priority_summary <- priority_matrix_data %>%
  filter(
    priority_group == "High Gap + High GHG"
  ) %>%
  summarise(
    buildings = n(),
    
    total_ghg_2024 =
      sum(ghg_2024, na.rm = TRUE),
    
    median_ghg_2024 =
      median(ghg_2024, na.rm = TRUE),
    
    median_mean_gap =
      median(mean_gap, na.rm = TRUE),
    
    median_gap_pct =
      median(mean_gap_pct, na.rm = TRUE)
  )

high_priority_summary


top15_summary <- top_priority %>%
  summarise(
    buildings = n(),
    
    total_ghg_2024 =
      sum(ghg_2024, na.rm = TRUE),
    
    median_ghg_2024 =
      median(ghg_2024, na.rm = TRUE),
    
    median_mean_gap =
      median(mean_gap, na.rm = TRUE)
  )

top15_summary




all_candidate_ghg <- sum(
  priority_matrix_data$ghg_2024,
  na.rm = TRUE
)

top15_ghg_share <- top_priority %>%
  summarise(
    top15_ghg =
      sum(ghg_2024, na.rm = TRUE),
    
    share_pct =
      top15_ghg /
      all_candidate_ghg * 100
  )

top15_ghg_share







write_csv(
  priority_summary,
  "output/priority_group_summary.csv"
)

write_csv(
  high_priority_summary,
  "output/high_priority_summary.csv"
)

write_csv(
  top15_summary,
  "output/top15_summary.csv"
)



# ------------------------------------------------------------
# Scenario analysis:
# Potential GHG reduction from partial EUI-gap closure
# ------------------------------------------------------------

scenario_base <- priority_matrix_data %>%
  filter(
    priority_group == "High Gap + High GHG"
  ) %>%
  mutate(
    
    # 2024 excess EUI above modeled expectation
    excess_eui_2024 =
      pmax(
        eui_2024 - expected_eui_2024,
        0
      ),
    
    # Share of current EUI represented by the excess
    excess_eui_share =
      pmin(
        excess_eui_2024 / eui_2024,
        1
      ),
    
    # Screening estimate:
    # emissions associated with excess EUI
    estimated_excess_ghg =
      ghg_2024 * excess_eui_share
  )
scenario_base %>%
  summarise(
    buildings = n(),
    total_ghg =
      sum(ghg_2024, na.rm = TRUE),
    
    estimated_excess_ghg =
      sum(estimated_excess_ghg, na.rm = TRUE),
    
    median_excess_eui_share =
      median(excess_eui_share, na.rm = TRUE)
  )





scenario_summary <- tibble(
  scenario = c(
    "10% gap closure",
    "25% gap closure",
    "50% gap closure"
  ),
  
  closure_rate = c(
    0.10,
    0.25,
    0.50
  )
) %>%
  mutate(
    estimated_ghg_reduction =
      closure_rate *
      sum(
        scenario_base$estimated_excess_ghg,
        na.rm = TRUE
      ),
    
    share_of_high_priority_ghg =
      estimated_ghg_reduction /
      sum(
        scenario_base$ghg_2024,
        na.rm = TRUE
      ) * 100
  )

scenario_summary




write_csv(
  scenario_summary,
  "output/ghg_reduction_scenarios.csv"
)






# ============================================================
# Figure 6
# LinkedIn / Portfolio Showcase Version - FIXED
# ============================================================

library(tidyverse)
library(scales)

# ------------------------------------------------------------
# 1. Prepare scenario data
# ------------------------------------------------------------

scenario_plot_data <- scenario_summary %>%
  mutate(
    scenario = factor(
      scenario,
      levels = c(
        "10% gap closure",
        "25% gap closure",
        "50% gap closure"
      )
    ),
    
    bar_label = paste0(
      scales::comma(
        round(estimated_ghg_reduction / 1000)
      ),
      "K tCO2e\n",
      round(
        share_of_high_priority_ghg,
        1
      ),
      "% of high-priority GHG"
    )
  )


# ------------------------------------------------------------
# 2. Build figure
# ------------------------------------------------------------

p6_linkedin <- ggplot(
  scenario_plot_data,
  aes(
    x = scenario,
    y = estimated_ghg_reduction,
    fill = scenario
  )
) +
  
  geom_col(
    width = 0.58,
    show.legend = FALSE
  ) +
  
  # Labels above bars
  geom_text(
    aes(
      label = bar_label
    ),
    vjust = -0.45,
    lineheight = 1.25,
    size = 4.3,
    fontface = "bold",
    color = "#163946"
  ) +
  
  # Progressive blue colors
  scale_fill_manual(
    values = c(
      "10% gap closure" = "#A9CEDD",
      "25% gap closure" = "#5E9EB7",
      "50% gap closure" = "#176B87"
    )
  ) +
  
  # Y axis
  scale_y_continuous(
    breaks = c(
      0,
      200000,
      400000,
      600000
    ),
    
    labels = c(
      "0",
      "200K",
      "400K",
      "600K"
    ),
    
    expand = expansion(
      mult = c(0, 0.19)
    )
  ) +
  
  # ----------------------------------------------------------
# Titles
# ----------------------------------------------------------

labs(
  title =
    "Turning Building Energy Gaps Into Carbon Reduction Opportunities",
  
  subtitle =
    paste0(
      "NYC Building Decarbonization Portfolio Optimization\n",
      "579 high-priority buildings  •  ",
      "2.46M tCO2e portfolio emissions  •  ",
      "1.19M tCO2e estimated excess emissions"
    ),
  
  x = NULL,
  
  y =
    "Estimated annual GHG reduction (tCO2e)",
  
  caption =
    paste0(
      "R  •  Random Forest  •  Grouped 5-fold cross-validation  •  NYC Local Law 84 benchmarking data\n",
      "Scenario estimates assume GHG emissions decline proportionally with reductions in excess EUI; ",
      "screening-level estimates, not engineering forecasts."
    )
) +
  
  coord_cartesian(
    clip = "off"
  ) +
  
  # ----------------------------------------------------------
# Styling
# ----------------------------------------------------------

theme_minimal(
  base_size = 14
) +
  
  theme(
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    # IMPORTANT: align everything to whole plot
    plot.title.position = "plot",
    plot.caption.position = "plot",
    
    plot.title = element_text(
      size = 21,
      face = "bold",
      color = "#14252D",
      hjust = 0,
      margin = margin(
        b = 8
      )
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#66747B",
      hjust = 0,
      lineheight = 1.35,
      margin = margin(
        b = 28
      )
    ),
    
    axis.text.x = element_text(
      size = 12,
      face = "bold",
      color = "#35464D",
      margin = margin(
        t = 10
      )
    ),
    
    axis.text.y = element_text(
      size = 10.5,
      color = "#66747B"
    ),
    
    axis.title.y = element_text(
      size = 11.5,
      face = "bold",
      color = "#35464D",
      margin = margin(
        r = 15
      )
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "#E4E9EC",
      linewidth = 0.55
    ),
    
    axis.ticks = element_blank(),
    
    plot.caption = element_text(
      size = 8.5,
      color = "#7B858A",
      hjust = 0,
      lineheight = 1.25,
      margin = margin(
        t = 20
      )
    ),
    
    plot.margin = margin(
      25,
      30,
      22,
      30
    )
  )


# ------------------------------------------------------------
# 3. Display
# ------------------------------------------------------------





dir.create(
  "figures/final",
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  "figures/final/06_ghg_reduction_linkedin.png",
  plot = p6_linkedin,
  width = 12,
  height = 6.75,
  dpi = 300,
  bg = "white"
)