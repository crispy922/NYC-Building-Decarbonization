# NYC Building Decarbonization Portfolio Optimization
# 02 - Three-year panel exploratory analysis

library(tidyverse)
library(scales)

# Load balanced three-year panel
panel <- read_csv(
  "data/processed/balanced_panel_2022_2024.csv",
  show_col_types = FALSE
)

# Check data
dim(panel)

# ------------------------------------------------------------
# Three-year GHG trend
# ------------------------------------------------------------

ghg_trend <- panel %>%
  group_by(calendar_year) %>%
  summarise(
    buildings = n_distinct(property_id),
    
    total_ghg_mtco2e =
      sum(
        total_location_based_ghg_emissions_metric_tons_co2e,
        na.rm = TRUE
      ),
    
    median_ghg_mtco2e =
      median(
        total_location_based_ghg_emissions_metric_tons_co2e,
        na.rm = TRUE
      ),
    
    median_ghg_intensity =
      median(
        total_location_based_ghg_emissions_intensity_kg_co2e_ft2,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

ghg_trend



ghg_change <- ghg_trend %>%
  summarise(
    total_ghg_change_pct =
      (
        total_ghg_mtco2e[calendar_year == 2024] -
          total_ghg_mtco2e[calendar_year == 2022]
      ) /
      total_ghg_mtco2e[calendar_year == 2022] * 100,
    
    median_ghg_change_pct =
      (
        median_ghg_mtco2e[calendar_year == 2024] -
          median_ghg_mtco2e[calendar_year == 2022]
      ) /
      median_ghg_mtco2e[calendar_year == 2022] * 100,
    
    median_intensity_change_pct =
      (
        median_ghg_intensity[calendar_year == 2024] -
          median_ghg_intensity[calendar_year == 2022]
      ) /
      median_ghg_intensity[calendar_year == 2022] * 100
  )

ghg_change


# ------------------------------------------------------------
# Figure 1: Portfolio-style GHG trend
# ------------------------------------------------------------

ghg_plot_data <- ghg_trend %>%
  arrange(calendar_year) %>%
  mutate(
    total_ghg_million = total_ghg_mtco2e / 1e6,
    
    yoy_change = (
      total_ghg_mtco2e /
        lag(total_ghg_mtco2e) - 1
    ) * 100
  )

ghg_plot_data

install.packages("patchwork")

library(patchwork)


p_main <- ggplot(
  ghg_plot_data,
  aes(x = calendar_year, y = total_ghg_million)
) +
  
  
  # Trend line
  geom_line(
    aes(group = 1),
    linewidth = 1.5,
    color = "#155E75"
  ) +
  
  # Points
  geom_point(
    size = 5,
    shape = 21,
    fill = "white",
    color = "#155E75",
    stroke = 1.8
  ) +
  
  # Emission values
  geom_text(
    aes(
      label = paste0(
        round(total_ghg_million, 2),
        "M"
      )
    ),
    vjust = -1.1,
    size = 5,
    fontface = "bold",
    color = "#123B46"
  ) +
  
  # 2022 → 2023 change
  annotate(
    "text",
    x = 2022.5,
    y = 17.75,
    label = "+12.9%",
    size = 4.3,
    fontface = "bold",
    color = "#155E75"
  ) +
  
  # 2023 → 2024 change
  annotate(
    "text",
    x = 2023.5,
    y = 18.72,
    label = "−0.8%",
    size = 4.3,
    fontface = "bold",
    color = "#66747B"
  ) +
  
  # Overall change box
  annotate(
    "label",
    x = 2023,
    y = 16.15,
    label = "+12.0% total GHG\n2022 → 2024",
    size = 4.2,
    fontface = "bold",
    fill = "white",
    color = "#155E75"
  ) +
  
  scale_x_continuous(
    breaks = c(2022, 2023, 2024)
  ) +
  
  scale_y_continuous(
    breaks = seq(16, 19, 0.5),
    labels = function(x) paste0(x, "M")
  ) +
  
  coord_cartesian(
    ylim = c(15.7, 19.25)
  ) +
  
  labs(
    title = "NYC Building GHG Emissions Increased 12% Since 2022",
    subtitle =
      "Location-based emissions from the same 21,649 buildings across all three years",
    x = NULL,
    y = "Total GHG emissions\n(million metric tons CO2e)",
    caption =
      "Source: NYC Local Law 84 Building Energy and Water Benchmarking Data"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      size = 19,
      face = "bold",
      color = "#17252A"
    ),
    
    plot.subtitle = element_text(
      size = 11.5,
      color = "#66747B",
      margin = margin(b = 18)
    ),
    
    axis.text.x = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.title.y = element_text(
      size = 11,
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "#E6EAEC",
      linewidth = 0.5
    ),
    
    plot.caption = element_text(
      color = "#777777",
      size = 9,
      hjust = 0
    ),
    
    plot.margin = margin(15, 20, 15, 15)
  )

p_main




property_type_summary <- panel %>%
  group_by(primary_property_type_self_selected) %>%
  summarise(
    buildings = n_distinct(property_id),
    records = n(),
    median_eui = median(site_eui_k_btu_ft2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(buildings))

property_type_summary %>%
  slice_head(n = 15)

# ------------------------------------------------------------
# Figure 2 data: 2024 EUI benchmark by property type
# ------------------------------------------------------------

eui_2024_summary <- panel %>%
  filter(calendar_year == 2024) %>%
  group_by(primary_property_type_self_selected) %>%
  summarise(
    buildings = n_distinct(property_id),
    median_eui = median(site_eui_k_btu_ft2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Keep property types with enough observations
  filter(buildings >= 100) %>%
  
  arrange(desc(median_eui))

eui_2024_summary



eui_2024_summary %>%
  select(
    primary_property_type_self_selected,
    buildings,
    median_eui
  ) %>%
  print(n = Inf)



plot_types <- c(
  "Other - Recreation",
  "Senior Living Community",
  "Hotel",
  "Mixed Use Property",
  "Multifamily Housing",
  "College/University",
  "K-12 School",
  "Office",
  "Manufacturing/Industrial Plant",
  "Distribution Center",
  "Self-Storage Facility",
  "Parking"
)

eui_plot_data <- eui_2024_summary %>%
  filter(primary_property_type_self_selected %in% plot_types) %>%
  arrange(desc(median_eui))

eui_plot_data


# Overall 2024 median EUI
overall_eui_2024 <- panel %>%
  filter(calendar_year == 2024) %>%
  summarise(
    median_eui = median(site_eui_k_btu_ft2, na.rm = TRUE)
  ) %>%
  pull(median_eui)

overall_eui_2024











p2 <- ggplot(
  eui_plot_data,
  aes(
    y = reorder(primary_property_type_self_selected, median_eui),
    x = median_eui
  )
) +
  
  # Benchmark line
  geom_vline(
    xintercept = overall_eui_2024,
    linetype = "dashed",
    linewidth = 0.8,
    color = "#8A969C"
  ) +
  
  # Lollipop lines
  geom_segment(
    aes(
      x = 0,
      xend = median_eui,
      yend = reorder(primary_property_type_self_selected, median_eui)
    ),
    linewidth = 1.1,
    color = "#BCCBD1"
  ) +
  
  # Points
  geom_point(
    size = 4.8,
    color = "#155E75"
  ) +
  
  # EUI values
  geom_text(
    aes(
      label = round(median_eui, 1)
    ),
    hjust = -0.25,
    size = 4.1,
    fontface = "bold",
    color = "#123B46"
  ) +
  
  # Label benchmark
  annotate(
    "text",
    x = overall_eui_2024 + 2,
    y = 1.3,
    label = paste0(
      "2024 NYC median: ",
      round(overall_eui_2024, 1)
    ),
    hjust = 0,
    size = 3.7,
    color = "#66747B"
  ) +
  
  scale_x_continuous(
    limits = c(0, 160),
    breaks = seq(0, 150, 25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    title = "Energy Intensity Varies Sharply Across NYC Building Types",
    subtitle =
      "Median Site EUI in 2024 | Selected property types with at least 100 buildings",
    x = "Median Site EUI (kBtu/ft2)",
    y = NULL,
    caption =
      "Source: NYC Local Law 84 Building Energy and Water Benchmarking Data"
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
      margin = margin(b = 18)
    ),
    
    axis.text.y = element_text(
      size = 10.5,
      face = "bold"
    ),
    
    axis.text.x = element_text(
      color = "#566167"
    ),
    
    axis.title.x = element_text(
      face = "bold",
      margin = margin(t = 10)
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    panel.grid.major.x = element_line(
      color = "#E6EAEC",
      linewidth = 0.5
    ),
    
    plot.caption = element_text(
      size = 9,
      color = "#777777",
      hjust = 0,
      margin = margin(t = 12)
    ),
    
    plot.margin = margin(15, 25, 15, 15)
  )

p2

ggsave(
  "figures/exploratory/02_eui_benchmark_2024.png",
  plot = p2,
  width = 11,
  height = 7.8,
  dpi = 300,
  bg = "white"
)



# ------------------------------------------------------------
# Figure 3 preparation:
# Keep buildings with stable property type across all 3 years
# ------------------------------------------------------------

type_stability <- panel %>%
  group_by(property_id) %>%
  summarise(
    n_property_types =
      n_distinct(primary_property_type_self_selected),
    .groups = "drop"
  )

type_stability %>%
  count(n_property_types)

stable_panel <- panel %>%
  semi_join(
    type_stability %>%
      filter(n_property_types == 1),
    by = "property_id"
  )

stable_panel %>%
  summarise(
    rows = n(),
    buildings = n_distinct(property_id)
  )


# ------------------------------------------------------------
# Figure 3: EUI change by stable property type, 2022 vs 2024
# ------------------------------------------------------------

eui_change_by_type <- stable_panel %>%
  filter(calendar_year %in% c(2022, 2024)) %>%
  group_by(
    primary_property_type_self_selected,
    calendar_year
  ) %>%
  summarise(
    buildings = n_distinct(property_id),
    median_eui = median(site_eui_k_btu_ft2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = calendar_year,
    values_from = c(buildings, median_eui),
    names_sep = "_"
  ) %>%
  mutate(
    eui_change_pct =
      (median_eui_2024 - median_eui_2022) /
      median_eui_2022 * 100
  ) %>%
  filter(
    buildings_2022 >= 100,
    buildings_2024 >= 100
  ) %>%
  arrange(desc(eui_change_pct))

eui_change_by_type %>%
  select(
    primary_property_type_self_selected,
    buildings_2024,
    median_eui_2022,
    median_eui_2024,
    eui_change_pct
  ) %>%

  
  
  # ------------------------------------------------------------
# Figure 3: Change in median Site EUI by property type
# ------------------------------------------------------------

eui_change_plot <- eui_change_by_type %>%
  mutate(
    change_direction = if_else(
      eui_change_pct < 0,
      "Lower EUI",
      "Higher EUI"
    )
  )


p3 <- ggplot(
  eui_change_plot,
  aes(
    x = eui_change_pct,
    y = reorder(
      primary_property_type_self_selected,
      eui_change_pct
    ),
    fill = change_direction
  )
) +
  
  # Zero reference line
  geom_vline(
    xintercept = 0,
    linewidth = 0.8,
    color = "#7A858B"
  ) +
  
  # Bars
  geom_col(
    width = 0.62
  ) +
  
  # Percentage labels
  geom_text(
    aes(
      label = paste0(
        ifelse(eui_change_pct > 0, "+", ""),
        round(eui_change_pct, 1),
        "%"
      )
    ),
    hjust = ifelse(
      eui_change_plot$eui_change_pct < 0,
      1.15,
      -0.15
    ),
    color = "white",
    fontface = "bold",
    size = 3.8
  ) +
  
  scale_fill_manual(
    values = c(
      "Lower EUI" = "#176B87",
      "Higher EUI" = "#C66B3D"
    )
  ) +
  
  scale_x_continuous(
    limits = c(-20, 22),
    breaks = seq(-20, 20, 5),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    title = "Median Energy Intensity Declined Across Most Major Building Types",
    subtitle =
      "Change in median Site EUI from 2022 to 2024 among buildings with stable property classifications",
    x = "Change in median Site EUI, 2022–2024",
    y = NULL,
    fill = NULL,
    caption =
      "Lower EUI indicates lower energy use intensity; changes should not be interpreted as retrofit effects without further analysis.\nSource: NYC Local Law 84 Building Energy and Water Benchmarking Data"
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
      margin = margin(b = 18)
    ),
    
    axis.text.y = element_text(
      size = 10.5,
      face = "bold"
    ),
    
    axis.title.x = element_text(
      face = "bold",
      margin = margin(t = 10)
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    
    panel.grid.major.x = element_line(
      color = "#E6EAEC",
      linewidth = 0.5
    ),
    
    legend.position = "top",
    
    plot.caption = element_text(
      size = 8.5,
      color = "#777777",
      hjust = 0,
      margin = margin(t = 12)
    ),
    
    plot.margin = margin(15, 25, 15, 15)
  )

p3


ggsave(
  "figures/exploratory/03_eui_change_by_type.png",
  plot = p3,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)


