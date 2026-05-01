############ LIBRARIES
library(dplyr)        # data wrangling
library(tidyr)        # data wrangling
library(ggplot2)      # plotting
library(ggrepel)      # labels that avoid overlap on plots
library(gganimate)    # animate ggplot objects by year
library(ggiraph)      # interactive tooltips for ggplot
library(ggalluvial)   # alluvial/flow plots
library(htmlwidgets)  # save interactive plots as standalone HTML
library(forecast)     # time series forecasting with ARIMA
library(viridis)      # colorblind-safe color palettes
library(scales)       # axis label formatting
library(gifski)       # for saving animation

############ DATA SOURCE
herd = read.csv("data/herd-clean.csv")


############ DATA PREP: FIELD SHARE
# calculate federal share of total by field
# log_total used for bubble size to retain relationship without engineering outsized
# NAs are structural: Non-S&E fields not collected before 2003
# 2000-2009 S&E gaps reflect institutions entering/exiting reporting thresholds as survey population grew
# Before FY 2010, survey included only institutions with R&D expenditures and degree programs in S&E fields
field_share = herd |>
  filter(question %in% c("Expenditures by S&E field",
                         "Expenditures by non-S&E field"),
         !grepl(", all$|^All$", row)) |>
  pivot_wider(names_from = column, values_from = data) |>
  mutate(federal_share = Federal / Total,
         log_total = log(Total))

# remove NAs (structural gaps, not true zeros)
field_share_clean = field_share |>
  filter(!is.na(federal_share))

# split S&E only for main analysis -- handle non-S&E separately
# Non-S&E excluded: data only from 2003, fields are professional programs not research fields
field_share_se = field_share_clean |>
  filter(!grepl("^Non-S&E", row))


############ ANIMATED BUBBLE: FEDERAL DEPENDENCY BY FIELD
# aggregate to field x year means and build labels
field_plot = field_share_se |>
  group_by(row, herd_year) |>
  summarise(
    mean_share = mean(federal_share, na.rm = TRUE),
    total_exp = sum(Total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    log_total = log(total_exp),
    broad_field = sub(",.*", "", row),
    field_label = case_when(
      row == "Engineering, aeronautical and astronautical"  ~ "Aero/Astro",
      row == "Engineering, bioengineering and biomedical"   ~ "Bio",
      row == "Engineering, chemical"                        ~ "Chem",
      row == "Engineering, civil"                           ~ "Civil",
      row == "Engineering, electrical"                      ~ "Elec",
      row == "Engineering, mechanical"                      ~ "Mech",
      row == "Engineering, metallurgical and materials"     ~ "Mats",
      row == "Engineering, other"                           ~ "Eng-other",
      row == "Environmental sciences, atmospheric sciences" ~ "Atmos",
      row == "Environmental sciences, earth sciences"       ~ "Earth",
      row == "Environmental sciences, oceanography"         ~ "Ocean",
      row == "Environmental sciences, other"                ~ "Env-Other",
      row == "Life sciences, agricultural sciences"         ~ "Ag",
      row == "Life sciences, biological sciences"           ~ "Bio",
      row == "Life sciences, medical sciences"              ~ "Med",
      row == "Life sciences, other"                         ~ "Life-Other",
      row == "Physical sciences, astronomy"                 ~ "Astro",
      row == "Physical sciences, chemistry"                 ~ "Chem",
      row == "Physical sciences, other"                     ~ "Phys-Other",
      row == "Physical sciences, physics"                   ~ "Physics",
      row == "Social sciences, economics"                   ~ "Econ",
      row == "Social sciences, other"                       ~ "SocSci-Other",
      row == "Social sciences, political science"           ~ "PoliSci",
      row == "Social sciences, sociology"                   ~ "Soc",
      TRUE ~ row
    )
  )

# y axis limits: use quantiles to avoid outlier years (eng/env enter at low values in 1979)
# pulling from early eng/env years creates empty lower half of plot
y_min = quantile(field_plot$log_total, 0.02, na.rm = TRUE)
y_max = quantile(field_plot$log_total, 0.98, na.rm = TRUE)

# animated plot
p = field_plot |>
  ggplot(aes(x = mean_share,
             y = log_total,
             size = log_total,
             color = broad_field,
             label = field_label)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(size = 6, max.overlaps = 20, nudge_y = 0.15, direction = "x") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis_d(end = .8) +
  scale_size_continuous(
    name = "Total R&D ($M)",
    breaks = log(c(500000, 2000000, 5000000, 10000000)),
    labels = c("$500M", "$2B", "$5B", "$10B")
  ) +
  guides(
  color = guide_legend(override.aes = list(size = 5), nrow = 2),
  size = guide_legend(nrow = 1)
)
  coord_cartesian(ylim = c(y_min, y_max), expand = FALSE) +
  labs(x = "Federal Dependency (Fed Share of Expenditures)",
       y = "Total Expenditures (log $)",
       color = "Field Group") +
  theme_minimal(base_size = 18) +
  theme(legend.position = 'bottom', legend.direction = "vertical", legend.box = "horizontal") +
  transition_time(herd_year) +
  labs(subtitle = "Year: {frame_time}") +
  ease_aes('linear') +
  shadow_wake(0.03, size = 2, alpha = FALSE, colour = 'grey92', exclude_layer = 2)

# animate and save
animate(p, nframes = 100, fps = 2, width = 800, height = 900,
        renderer = gifski_renderer("outputs/field_dependency.gif"),
        detail = 1)


############ LINE CHART: FEDERAL SHARE TRENDS BY SUB-FIELD
# color lines by rank of ending federal share value within each broad field
# darker = higher federal dependency at end of series
field_ranks = field_plot |>
  group_by(broad_field, field_label) |>
  slice_max(herd_year, n = 1) |>
  ungroup() |>
  group_by(broad_field) |>
  mutate(rank = rank(mean_share),
         color = viridis::viridis(n(), direction = -1, end = 0.85)[rank]) |>
  ungroup() |>
  select(broad_field, field_label, color)

field_plot_colored = field_plot |>
  left_join(field_ranks, by = c("field_label", "broad_field"))

# interactive version with tooltip (renders in Quarto HTML)
p_interactive = field_plot_colored |>
  ggplot(aes(x = herd_year, y = mean_share, group = field_label, color = color)) +
  geom_smooth_interactive(
    aes(tooltip = field_label, data_id = field_label),
    method = "loess", se = FALSE, linewidth = 0.6) +
  geom_text_repel(
    data = field_plot_colored |>
      group_by(field_label, broad_field) |>
      slice_max(herd_year, n = 1),
    aes(label = field_label, color = color),
    size = 2.5, nudge_x = 1, direction = "y"
  ) +
  scale_color_identity() +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# render with hover highlighting and save
p_interactive_widget = girafe(
  ggobj = p_interactive,
  options = list(
    opts_sizing(rescale = TRUE, width = 1),
    opts_hover(css = "stroke: #333333; stroke-width: 2px;"),
    opts_hover_inv(css = "opacity: 0.2;"),
    opts_toolbar(saveaspng = FALSE, hidden = c("lasso_select", "lasso_deselect", "zoom_rect", "zoom_reset"))
  )
)
p_interactive_widget
saveWidget(p_interactive_widget, "outputs/federal_share_by_field.html", selfcontained = TRUE)


############ ALLUVIAL: R&D EXPENDITURE BY SOURCE (AGGREGATE)
# note: data labels changed at 2010 with new HERD survey instrument
# pre-2010: Federal/Industry/Institution funds, total
# post-2010: Federal government/Business/Institution funds
source_data = herd |>
  filter(question == "Source",
         row %in% c("Federal", "Federal government",
                    "Industry", "Business",
                    "Institution funds, total", "Institution funds")) |>
  mutate(source = case_when(
    row %in% c("Federal", "Federal government")                 ~ "Federal",
    row %in% c("Industry", "Business")                          ~ "Industry",
    row %in% c("Institution funds, total", "Institution funds") ~ "Institution",
    TRUE ~ row
  )) |>
  group_by(source, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop")

# calculate shares
source_share = source_data |>
  group_by(herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()

# plot shows federal dominance and institution growth
# industry share not increasing meaningfully over time
# label year
label_years = max(source_share$herd_year)

p_source = source_share |>
  ggplot(aes(x = herd_year, y = share, alluvium = source, stratum = source, fill = source, label = source)) +
  geom_alluvium(alpha = .9) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of R&D Expenditures") +
  theme_minimal(base_size = 8)
p_source
ggsave("outputs/source_aggregate.png", p_source, width = 4, height = 2, dpi = 300)


############ ALLUVIAL: NONFEDERAL SOURCES BY BROAD FIELD
# data available from 2010 only (HERD instrument change)
# note: "Nonprofit organziations" is a typo in NCSES source data, corrected below
nonfed_data = herd |>
  filter(question == "Nonfederal expenditures by field and source",
         grepl(", all$", row),
         !grepl("^All$", row),
         column != "Total") |>
  mutate(broad_field = sub(",.*", "", row),
         broad_field = case_when(
           broad_field == "Computer and information sciences"                     ~ "Computer Sciences",
           broad_field == "Geosciences, atmospheric sciences, and ocean sciences" ~ "Environmental Sciences",
           broad_field == "Mathematics and statistics"                            ~ "Mathematics",
           broad_field == "Other sciences"                                        ~ "Other",
           TRUE ~ broad_field
         )) |>
  mutate(column = recode(column,
                         "Nonprofit organziations" = "Nonprofit organizations")) |>
  filter(broad_field != "Non-S&E") |>
  group_by(broad_field, column, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop")

# calculate shares and stack by average share order
nonfed_share = nonfed_data |>
  group_by(broad_field, herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()

# plot nonfed only
label_years = min(nonfed_share$herd_year)

p_nonfed = nonfed_share |>
  mutate(column = factor(column, levels = c("Institution funds",
                                            "Nonprofit organizations",
                                            "Business",
                                            "State and local government",
                                            "All other sources"))) |>
  ggplot(aes(x = herd_year, y = share, alluvium = column, stratum = column, fill = column, label = column)) +
  geom_alluvium(alpha = .9) +
  facet_wrap(~ broad_field,  axes = "all") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of Nonfederal R&D Expenditures") +
  theme_minimal(base_size = 14)
p_nonfed
ggsave("outputs/nonfed_by_field.png", p_nonfed, width = 9, height = 9, dpi = 300)


############ ALLUVIAL: ALL SOURCES (FEDERAL + NONFEDERAL) BY BROAD FIELD
# federal data by field from separate question, extract totals only
fed_data = herd |>
  filter(question == "Federal expenditures by field and agency",
         grepl(", all$", row),
         !grepl("^All$", row),
         column == "Total") |>
  mutate(broad_field = sub(",.*", "", row),
         broad_field = case_when(
           broad_field == "Computer and information sciences"                     ~ "Computer Sciences",
           broad_field == "Geosciences, atmospheric sciences, and ocean sciences" ~ "Environmental Sciences",
           broad_field == "Mathematics and statistics"                            ~ "Mathematics",
           broad_field == "Other sciences"                                        ~ "Other",
           TRUE ~ broad_field
         ),
         column = "Federal") |>
  filter(broad_field != "Non-S&E") |>
  group_by(broad_field, column, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop")

# combine federal and nonfederal
source_field_data = bind_rows(fed_data, nonfed_data) |>
  group_by(broad_field, herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()

# plot with sources stacked by average share order
label_years_field = min(source_field_data$herd_year)

p_source_field = source_field_data |>
  mutate(column = factor(column, levels = c("Federal",
                                            "Institution funds",
                                            "Nonprofit organizations",
                                            "State and local government",
                                            "Business",
                                            "All other sources"))) |>
  ggplot(aes(x = herd_year, y = share, alluvium = column, stratum = column, fill = column, label = column)) +
  geom_alluvium(alpha = .9) +
  geom_text(stat = "stratum", size = 3, color = "white", hjust = 0,
            fontface = "bold", data = ~filter(.x, herd_year %in% label_years)) +
  facet_wrap(~ broad_field,  axes = "all") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of Total R&D Expenditures") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
p_source_field
ggsave("outputs/source_by_field.png", p_source_field, width = 9, height = 9, dpi = 300)


############ R&D TYPE: TRENDS IN BASIC VS APPLIED VS DEVELOPMENT
# data available from 2010 only (HERD instrument change)
# before FY2010, R&D type based on percentage estimates; post-2010 dollar amounts collected directly

# prep data by source (federal vs nonfederal)
rd_type_source = herd |>
  filter(question == "Type of R&D conducted",
         row != "All",
         column != "Total") |>
  group_by(row, column, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop") |>
  group_by(column, herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()

# lm slopes by R&D type and source
# federal dollars shifting away from basic research faster (-0.42%/yr) than nonfederal (-0.19%/yr)
# federal dollars shifting toward applied faster (+0.28%/yr vs +0.11%/yr)
# development increasing in both but slowly
# federal shift toward applied is driving the aggregate trend
# industry is not picking up the basic research gap (see industry section below)
rd_lm_source = rd_type_source |>
  group_by(row, column) |>
  summarise(
    slope = coef(lm(share ~ herd_year))[2],
    intercept = coef(lm(share ~ herd_year))[1],
    .groups = "drop"
  ) |>
  arrange(column, row)
rd_lm_source

# plot with lm lines faceted by federal vs nonfederal
p_rdtype = rd_type_source |>
  mutate(row = factor(row, levels = c("Basic research", "Applied research", "Development"))) |>
  ggplot(aes(x = herd_year, y = share, color = row)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ column) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of R&D Expenditures", color = "R&D Type") +
  theme_minimal(base_size = 16)
p_rdtype
ggsave("outputs/rdtype_lm.png", p_rdtype, width = 10, height = 6, dpi = 300)


############ INDUSTRY INVESTMENT BY FIELD
# test whether business funding is growing to offset federal decline in basic research
# finding: industry growing in Computer Sciences (+8.2%/yr relative), Life sciences (+6.9%/yr)
# declining in Geosciences, Psychology, Social sciences
# fields gaining industry investment are already most applied -- no one filling basic research gap

industry_lm = nonfed_data |>
  filter(column == "Business") |>
  group_by(broad_field) |>
  summarise(
    slope = coef(lm(total ~ herd_year))[2],
    mean_total = mean(total),
    .groups = "drop"
  ) |>
  mutate(pct_growth = slope / mean_total) |>
  arrange(desc(pct_growth))
industry_lm

# plot absolute trends by field
p_industry = nonfed_data |>
  filter(column == "Business") |>
  group_by(broad_field, herd_year) |>
  summarise(total = sum(total), .groups = "drop") |>
  ggplot(aes(x = herd_year, y = total, group = broad_field)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ broad_field, scales = "free_y") +
  labs(x = NULL, y = "Business R&D ($)") +
  theme_minimal(base_size = 16)
p_industry
ggsave("outputs/industry_by_field.png", p_industry, width = 12, height = 8, dpi = 300)


############ FOREIGN FUNDS: SHARE OF TOTAL R&D
# use shares not absolute values -- currency values fluctuate over time
# source breakdown available from 2016 only; total available from 2010
foreign_share = herd |>
  filter(question == "Source", row == "Total") |>
  group_by(herd_year) |>
  summarise(total_rd = sum(data, na.rm = TRUE)) |>
  inner_join(
    herd |>
      filter(question == "Foreign funds", row == "Total") |>
      group_by(herd_year) |>
      summarise(foreign = sum(data, na.rm = TRUE)),
    by = "herd_year"
  ) |>
  mutate(foreign_share = foreign / total_rd)

p_foreign = foreign_share |>
  ggplot(aes(x = herd_year, y = foreign_share)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Foreign Funds as Share of Total R&D") +
  theme_minimal(base_size = 16)
p_foreign
ggsave("outputs/foreign_share.png", p_foreign, width = 8, height = 5, dpi = 300)


############ FORECAST: FEDERAL SHARE BY BROAD FIELD
# ARIMA models fit per broad field using auto.arima
# forecasts assume continuation of historical patterns
# do not account for 2025 federal funding disruptions
field_forecast = field_plot |>
  group_by(broad_field, herd_year) |>
  summarise(mean_share = mean(mean_share, na.rm = TRUE), .groups = "drop")

# fit ARIMA and forecast 10 years per broad field
field_arima = field_forecast |>
  group_by(broad_field) |>
  group_modify(~{
    ts_data = ts(.x$mean_share, start = min(.x$herd_year), frequency = 1)
    fit = auto.arima(ts_data)
    fcast = forecast(fit, h = 10)
    tibble(
      herd_year = seq(max(.x$herd_year) + 1, max(.x$herd_year) + 10),
      forecast  = as.numeric(fcast$mean),
      lower     = as.numeric(fcast$lower[, 2]),
      upper     = as.numeric(fcast$upper[, 2])
    )
  }) |>
  ungroup()

# clip ribbon data to fix variation in panel data
field_arima = field_arima |>
  mutate(lower = pmax(lower, 0),
         upper = pmin(upper, 1))

# plot historical + forecast faceted by broad field
p_forecast = field_forecast |>
  ggplot(aes(x = herd_year, y = mean_share)) +
  geom_line(linewidth = 0.7) +
  geom_ribbon(data = field_arima,
              aes(x = herd_year, ymin = lower, ymax = upper),
              alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = field_arima,
            aes(x = herd_year, y = forecast),
            linetype = "dashed", linewidth = 0.7) +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal(base_size = 16)
p_forecast
ggsave("outputs/forecast_by_field.png", p_forecast, width = 12, height = 8, dpi = 300)

########## TIDY
rm(list = ls())
gc()
