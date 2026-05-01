############ LIBRARIES
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(ggplot2) # plotting
library(ggrepel) # for plot labels
library(gganimate) # animation by year
library(ggiraph) # for animated tooltip
library(ggalluvial) # alluvial plots


############ DATA SOURCE
herd = read.csv("data/herd-clean.csv")

############ EXPLORE
#examine question types and data counts
table(herd$question)

# examine coverage for animated bubble chart by field
herd |>
  filter(question %in% c("Expenditures by S&E field",
                         "Expenditures by non-S&E field")) |>
  distinct(row) |>
  arrange(row)

# examine distinct field types
herd |>
  filter(question %in% c("Expenditures by S&E field",
                         "Expenditures by non-S&E field"),
         !grepl(", all$|^All$", row)) |>
  distinct(row, column) |>
  arrange(row)

#examine R&D type
herd |>
  filter(question == "Type of R&D conducted") |>
  distinct(row)



############ ANIMATED BUBBLE FED SHARE OF TOTAL BY FIELD
#### gather data and calculated federal share of total
# create log for bubbles to retain relationship but not have eng outsized
field_share = herd |>
  filter(question %in% c("Expenditures by S&E field",
                         "Expenditures by non-S&E field"),
         !grepl(", all$|^All$", row)) |>
  pivot_wider(names_from = column, values_from = data) |>
  mutate(federal_share = Federal / Total,
         log_total = log(Total))

# check data for missing values
field_share |>
  filter(is.na(federal_share) | is.infinite(federal_share)) |>
  count(row, sort = TRUE)|>
  print(n=35)

field_share |>
  filter(is.na(federal_share) | is.infinite(federal_share)) |>
  count(herd_year, sort = TRUE)|>
  print(n=40)

# check 2000-2009 spike
field_share |>
  filter(herd_year %in% 2000:2009,
         is.na(federal_share) | is.infinite(federal_share)) |>
  count(row, sort = TRUE)|>
  print(n=35)
### S&E fields: full series 1974–2024 but with some institutional gaps in 2000s
# Before FY 2010, survey included only institutions with R&D expenditures and degree programs in S&E fields — institutions that performed R&D in only non-S&E fields were excluded.
# For FY16, fields were revised to better reflect current R&D being conducted, with minor revisions in FY20.
#The 2000–2009 pattern in S&E fields is likely institutions entering/exiting reporting thresholds as the survey population grew.

# NAs or Inf
field_share |>
  filter(is.na(federal_share) | is.infinite(federal_share)) |>
  mutate(type = case_when(
    is.na(federal_share) ~ "NA",
    is.infinite(federal_share) ~ "Inf")) |>
  count(type)
# 2234 na

### Non-S&E fields: valid data only from 2003 onward:
# Before 2003, the survey covered only S&E fields; non-S&E fields were added in 2003.
# Non-S&E NAs in 1974–2002 are structural

# what fields are represented/potentially lost
field_share |>
  filter(grepl("^Non-S&E", row)) |>
  distinct(row)

# filter na if keeping non S&E
# field_share_clean = field_share |>  filter(!is.na(federal_share))

# what fields are represented/potentially lost
field_share_clean |>
  filter(grepl("^Non-S&E", row)) |>
  distinct(row)

#check coverage by year
field_share_clean |>
  filter(grepl("^Non-S&E", row)) |>
  group_by(row, herd_year) |>
  summarise(
    n = n(),
    mean_share = mean(federal_share, na.rm = TRUE),
    median_share = median(federal_share, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(row, herd_year) |>
  print(n = 100)



#### SPLIT S&E data for analysis across 50 years
# handle non S&S separately for education etc
field_share_se = field_share_clean |>
  filter(!grepl("^Non-S&E", row))

#check coverage by year
field_share_se |>
  count(herd_year) |>
  arrange(herd_year)|>
  print(n=40)


#### prep data for plot
field_share_se |>
  group_by(row, herd_year) |>
  summarise(
    mean_share = mean(federal_share, na.rm = TRUE),
    total_exp = sum(Total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  str()

#### PLOT, minimal by field with mean share on x and log r&d as size
field_plot = field_share_se |>
  group_by(row, herd_year) |>
  summarise(
    mean_share = mean(federal_share, na.rm = TRUE),
    total_exp = sum(Total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    log_total = log(total_exp),
    broad_field = sub(",.*", "", row)
  )
#reuse above for several plots while experimenting

field_plot |>
  filter(herd_year == 2009) |>
  ggplot(aes(x = mean_share,
             y = reorder(row, mean_share),
             size = log_total,
             color = broad_field)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis_d() +
  scale_size_continuous(
    name = "Total R&D ($M)",
    breaks = log(c(500000, 2000000, 5000000, 10000000)),
    labels = c("$500M", "$2B", "$5B", "$10B")
  ) +
  labs(title = "",
       x = 'Federal Share of Total R&D Expenditures' , y = "",
       size = "Log Total Exp",
       color = "Field Group") +
  theme_minimal()
# too much work for viewer


#### PLOT, aggregated scatterplot, with mean against log size
field_plot_agg = field_plot |>
  group_by(broad_field, herd_year) |>
  summarise(
    mean_share = mean(mean_share, na.rm = TRUE),
    total_exp = sum(total_exp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(log_total = log(total_exp))

field_plot_agg |>
  filter(herd_year == 2009) |>
  ggplot(aes(x = mean_share,
             y = log_total,
             size = log_total,
             color = broad_field,
             label = broad_field)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(size = 3.5) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis_d(end = .8) +
  scale_size_continuous(
    name = "Total R&D ($M)",
    breaks = log(c(500000, 2000000, 5000000, 10000000)),
    labels = c("$500M", "$2B", "$5B", "$10B")
  ) +
  labs(x = "Federal Dependency (Fed Share of Expenditures)",
       y = "Total Expenditures (log $)",
       color = "Field Group") +
  theme_minimal()
# lose too much detail, too sparse



#### PLOT, scatter of mean against log
# reuse field_plot above
field_plot |>
  filter(herd_year == 2009) |>
  ggplot(aes(x = mean_share,
             y = log_total,
             size = log_total,
             color = broad_field,
             label = row)) +
  geom_point(alpha = 0.8) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis_d() +
  scale_size_continuous(
    name = "Total R&D ($M)",
    breaks = log(c(500000, 2000000, 5000000, 10000000)),
    labels = c("$500M", "$2B", "$5B", "$10B")
  ) +
  labs(x = "Federal Dependency (Fed Share of Expenditures)",
       y = "Total Expenditures (log $)",
       color = "Field Group") +
  theme_minimal()


#### PLOT, scatter of mean against logwith labels
# reuse field_plot above
field_plot |>
  filter(herd_year == 2009) |>
  mutate(field_label = sub(".*,\\s*", "", row)) |>
  ggplot(aes(x = mean_share,
             y = log_total,
             size = log_total,
             color = broad_field,
             label = field_label)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(size = 3.5, max.overlaps = 20) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis_d(end = .8) +
  scale_size_continuous(
    name = "Total R&D ($M)",
    breaks = log(c(500000, 2000000, 5000000, 10000000)),
    labels = c("$500M", "$2B", "$5B", "$10B")
  ) +
  labs(x = "Federal Dependency (Fed Share of Expenditures)",
       y = "Total Expenditures (log $)",
       color = "Field Group") +
  theme_minimal()


#### PLOT, scatter of mean against log with shorter labels
# replace field_plot data with new version, fields split by comma
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
    field_label = sub(".*,\\s*", "", row)
  )

# unique
field_plot |>
  filter(herd_year == 2009) |>
  distinct(broad_field, field_label) |>
  arrange(broad_field, field_label) |>
  print(n=25)

# reassign subfields shorter names for plot labels
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
      row == "Engineering, aeronautical and astronautical" ~ "Aero/Astro",
      row == "Engineering, bioengineering and biomedical"  ~ "Bio",
      row == "Engineering, chemical"                       ~ "Chem",
      row == "Engineering, civil"                          ~ "Civil",
      row == "Engineering, electrical"                     ~ "Elec",
      row == "Engineering, mechanical"                     ~ "Mech",
      row == "Engineering, metallurgical and materials"    ~ "Mats",
      row == "Engineering, other"                          ~ "Eng-other",
      row == "Environmental sciences, atmospheric sciences"~ "Atmos",
      row == "Environmental sciences, earth sciences"      ~ "Earth",
      row == "Environmental sciences, oceanography"        ~ "Ocean",
      row == "Environmental sciences, other"               ~ "Env-Other",
      row == "Life sciences, agricultural sciences"        ~ "Ag",
      row == "Life sciences, biological sciences"          ~ "Bio",
      row == "Life sciences, medical sciences"             ~ "Med",
      row == "Life sciences, other"                        ~ "Life-Other",
      row == "Physical sciences, astronomy"                ~ "Astro",
      row == "Physical sciences, chemistry"                ~ "Chem",
      row == "Physical sciences, other"                    ~ "Phys-Other",
      row == "Physical sciences, physics"                  ~ "Physics",
      row == "Social sciences, economics"                  ~ "Econ",
      row == "Social sciences, other"                      ~ "SocSci-Other",
      row == "Social sciences, political science"          ~ "PoliSci",
      row == "Social sciences, sociology"                  ~ "Soc",
      TRUE ~ row
    )
  )

# replot
field_plot |>
  filter(herd_year == 2009) |>
  ggplot(aes(x = mean_share,
             y = log_total,
             size = log_total,
             color = broad_field,
             label = field_label)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(size = 3.5, max.overlaps = 20, nudge_y = 0.15, direction = "x") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis_d(end = .6) +
  scale_size_continuous(
    name = "Total R&D ($M)",
    breaks = log(c(500000, 2000000, 5000000, 10000000)),
    labels = c("$500M", "$2B", "$5B", "$10B")
  ) +
  labs(x = "Federal Dependency (Fed Share of Expenditures)",
       y = "Total Expenditures (log $)",
       color = "Field Group") +
  guides(
    color = guide_legend(override.aes = list(size = 4)),
    size = guide_legend()
  ) +
  theme_minimal()


#### FINAL ANIMATED PLOT
# y axis limits to force plot to data
#y_min = min(field_plot$log_total, na.rm = TRUE)
#y_max = max(field_plot$log_total, na.rm = TRUE)
# outlier values keeping most of the data in the upper half of plot area

# try plotting the ranges that are dominant
# eng and env enter at lower values and then migrate to the general values
y_min = quantile(field_plot$log_total, 0.02, na.rm = TRUE)
y_max = quantile(field_plot$log_total, 0.98, na.rm = TRUE)

p = field_plot |>
  ggplot(aes(x = mean_share,
             y = log_total,
             size = log_total,
             color = broad_field,
             label = field_label)) +
  geom_point(alpha = 0.8) +
  geom_text_repel(size = 3.5, max.overlaps = 20, nudge_y = 0.15, direction = "x") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis_d(end = .8) +
  scale_size_continuous(
    name = "Total R&D ($M)",
    breaks = log(c(500000, 2000000, 5000000, 10000000)),
    labels = c("$500M", "$2B", "$5B", "$10B")
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 4)),
    size = guide_legend()
  ) +
  coord_cartesian(ylim = c(y_min, y_max), expand = FALSE) +
  labs(x = "Federal Dependency (Fed Share of Expenditures)",
       y = "Total Expenditures (log $)",
       color = "Field Group") +
  theme_minimal() +
  transition_time(herd_year) +
  labs(subtitle = "Year: {frame_time}") +
  ease_aes('linear')

# animate and save
animate(p, nframes = 100, fps = 2, width = 800, height = 600)
anim_save("outputs/field_dependency.gif")


#### TABLE
field_highlow = field_plot |>
  group_by(row, broad_field, field_label) |>
  summarise(
    min_share = min(mean_share, na.rm = TRUE),
    min_year = herd_year[which.min(mean_share)],
    max_share = max(mean_share, na.rm = TRUE),
    max_year = herd_year[which.max(mean_share)],
    net_change = last(mean_share) - first(mean_share),
    .groups = "drop"
  ) |>
  arrange(desc(max_share))
field_highlow |> print(n=25)

# every field peaked in 1979 (post-Sputnik/Vietnam/Cold War federal research surge)
## The federal share fluctuated over the years — increasing due to national defense and space flight R&D spending in the 1960s — but accounted for over 50% of the total for the entire period. The 1979 peak reflects the tail end of Cold War/space race federal dominance before Reagan-era shifts toward private R&D. When the ratio previously peaked in 1964, federally funded R&D was 1.86% of GDP; by 2021 it was 0.66% of GDP while business funding was 2.52%. The long decline is captured in the net change column.
field_plot |>
  filter(herd_year %in% c(1977, 1978, 1979, 1980, 1981)) |>
  group_by(herd_year) |>
  summarise(mean_federal_share = mean(mean_share, na.rm = TRUE))

# Additional note: The federal proportion fell during the latter part of the 2000–09 decade but rose in 2010 and 2011 with the infusion of ARRA funds, then has been on a steady decline starting in 2012. That ARRA bump should appear as a brief uptick in animation

# Engineering and environmental sciences appear to enterR1 in the 80s
field_plot |>
  filter(broad_field %in% c("Engineering", "Environmental sciences")) |>
  group_by(broad_field, field_label) |>
  summarise(first_year = min(herd_year), .groups = "drop") |>
  arrange(broad_field, first_year)
# Department of Energy was created in 1977 and immediately became a major funder of both engineering and environmental research, which may have triggered NCSES to add those fields to the survey

# Overall, federal dependency has declined across virtually every field over 50 years, with only Engineering Bio showing a slight increase.



############ LINE CHART FOR FEDERAL SHARE BY FIELD
#plot each subfield with lm to field
field_plot |>
  ggplot(aes(x = herd_year, y = mean_share, group = field_label)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  geom_smooth(aes(group = broad_field), method = "lm", se = FALSE,
              linetype = "dashed", linewidth = 0.8, color = "black") +
  geom_text_repel(
    data = field_plot |> group_by(field_label) |> slice_max(herd_year, n = 1),
    aes(label = field_label),
    size = 2.5, nudge_x = 1, direction = "y"
  ) +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal() +
  theme(legend.position = "none")
#not useful b/c all show downtrend

#plot each subfield with raw data and lm to data
field_plot |>
  ggplot(aes(x = herd_year, y = mean_share, group = field_label)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
              linewidth = 0.4, color = "black") +
  geom_text_repel(
    data = field_plot |> group_by(field_label) |> slice_max(herd_year, n = 1),
    aes(label = field_label),
    size = 2.5, nudge_x = 1, direction = "y"
  ) +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal() +
  theme(legend.position = "none")

#plot each subfield with raw data  and lm/loess to data
field_plot |>
  ggplot(aes(x = herd_year, y = mean_share, group = field_label)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed",
              linewidth = 0.4, color = "black") +
  geom_text_repel(
    data = field_plot |> group_by(field_label) |> slice_max(herd_year, n = 1),
    aes(label = field_label),
    size = 2.5, nudge_x = 1, direction = "y"
  ) +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal() +
  theme(legend.position = "none")


#plot each subfield with lm to data
field_plot |>
  ggplot(aes(x = herd_year, y = mean_share, group = field_label)) +
  geom_line(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
              linewidth = 0.4, color = "black") +
  geom_text_repel(
    data = field_plot |> group_by(field_label) |> slice_max(herd_year, n = 1),
    aes(label = field_label),
    size = 2.5, nudge_x = 1, direction = "y"
  ) +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal() +
  theme(legend.position = "none")

#plot subfield with lm
field_plot |>
  ggplot(aes(x = herd_year, y = mean_share, group = field_label)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "solid",
              linewidth = 0.6, color = "steelblue") +
  geom_text_repel(
    data = field_plot |> group_by(field_label) |> slice_max(herd_year, n = 1),
    aes(label = field_label),
    size = 2.5, nudge_x = 1, direction = "y"
  ) +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal() +
  theme(legend.position = "none")

#plot subfield with lm/loess
field_plot |>
  ggplot(aes(x = herd_year, y = mean_share, group = field_label)) +
  geom_smooth(method = "loess", se = FALSE, linetype = "solid",
              linewidth = 0.6, color = "steelblue") +
  geom_text_repel(
    data = field_plot |> group_by(field_label) |> slice_max(herd_year, n = 1),
    aes(label = field_label),
    size = 2.5, nudge_x = 1, direction = "y"
  ) +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal() +
  theme(legend.position = "none")

#plot subfield with lm/loess with colors to follow the lines ranked by end point
# create rank-based color mapping
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

#without tooltip
field_plot_colored |>
  ggplot(aes(x = herd_year, y = mean_share, group = field_label, color = color)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.6) +
  geom_text_repel(
    data = field_plot_colored |> group_by(field_label) |> slice_max(herd_year, n = 1),
    aes(label = field_label, color = color),
    size = 2.5, nudge_x = 1, direction = "y"
  ) +
  scale_color_identity() +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal() +
  theme(legend.position = "none")

#with tooltip
p_interactive = field_plot_colored |>
  ggplot(aes(x = herd_year, y = mean_share, group = field_label, color = color)) +
  geom_smooth_interactive(
    aes(tooltip = field_label, data_id = field_label),
    method = "loess", se = FALSE, linewidth = 0.6) +
  geom_text_repel(
    data = field_plot_colored |> group_by(field_label, broad_field) |> slice_max(herd_year, n = 1),
    aes(label = field_label, color = color),
    size = 2.5, nudge_x = 1, direction = "y"
  ) +
  scale_color_identity() +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Federal Share of R&D Expenditures") +
  theme_minimal() +
  theme(legend.position = "none")

# tooltip that highlights in default yellow
girafe(ggobj = p_interactive, width_svg = 10, height_svg = 7)

# tooltip that fades out non-highlighted
girafe(
  ggobj = p_interactive,
  width_svg = 10,
  height_svg = 7,
  options = list(
    opts_hover(css = "stroke: #333333; stroke-width: 2px;"),
    opts_hover_inv(css = "opacity: 0.2;")
  )
)



############ R&D EXPENDITURE ALLUVIAL
#### data prep
herd |>
  filter(question == "Source",
         row %in% c("Federal", "Federal government",
                    "Industry", "Business",
                    "Institution funds, total", "Institution funds")) |>
  distinct(row, herd_year) |>
  arrange(row, herd_year)
# data labels in question/row changed at 2010 with new survey

# normal data labels in question/row
source_data = herd |>
  filter(question == "Source",
         row %in% c("Federal", "Federal government",
                    "Industry", "Business",
                    "Institution funds, total", "Institution funds")) |>
  mutate(source = case_when(
    row %in% c("Federal", "Federal government") ~ "Federal",
    row %in% c("Industry", "Business") ~ "Industry",
    row %in% c("Institution funds, total", "Institution funds") ~ "Institution",
    TRUE ~ row
  )) |>
  group_by(source, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop")
#check data
source_data |> count(source)



#### PLOT by source only
#calculate shares
source_share = source_data |>
  group_by(herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()

source_share |>
  ggplot(aes(x = herd_year, y = share, alluvium = source, fill = source)) +
  geom_alluvium(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85, alpha = .8) +
  labs(x = NULL, y = "Share of R&D Expenditures", fill = "Source") +
  theme_minimal()
# can use a supp b/c it only shows that institutions are putting more in
# but does show industry is doing little over time

# can I do this by field
herd |> distinct(question)
# only if using fed and nonfed--ratio of values?

#check values
herd |>
  filter(question == "Nonfederal expenditures by field and source") |>
  distinct(row, column) |>
  arrange(row)
# data are rich for the question

# check years
herd |>
  filter(question == "Nonfederal expenditures by field and source") |>
  distinct(herd_year) |>
  arrange(herd_year)



#### Plot by fields using expenditure data for nonfed only
## prep data and fix labels
# exclude nonSE for consistency with above
nonfed_data = herd |>
  filter(question == "Nonfederal expenditures by field and source",
         grepl(", all$", row),
         !grepl("^All$", row),
         column != "Total") |>
  mutate(broad_field = sub(",.*", "", row),
         broad_field = case_when(
           broad_field == "Computer and information sciences" ~ "Computer Sciences",
           broad_field == "Geosciences, atmospheric sciences, and ocean sciences" ~ "Environmental Sciences",
           broad_field == "Mathematics and statistics" ~ "Mathematics",
           broad_field == "Other sciences" ~ "Other",
           TRUE ~ broad_field
         )) |>
  mutate(column = recode(column,
                         "Nonprofit organziations" = "Nonprofit organizations")) |>
  filter(broad_field != "Non-S&E") |>
  group_by(broad_field, column, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop")

nonfed_data |> distinct(broad_field, column)

## plot
nonfed_share = nonfed_data |>
  group_by(broad_field, herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()

nonfed_share |>
  ggplot(aes(x = herd_year, y = share, alluvium = column, fill = column)) +
  geom_alluvium(alpha = 0.8) +
  facet_wrap(~ broad_field, scales = "free_y") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of Nonfederal R&D Expenditures", fill = "Source") +
  theme_minimal()

## Stack alluvial by value not alpha order
# requires ordering within each year which changes across time
# fix: largest to smallest on average.
# check average shares
nonfed_share |>
  group_by(column) |>
  summarise(mean_share = mean(share, na.rm = TRUE)) |>
  arrange(desc(mean_share))

# plot using average shares
nonfed_share |>
  mutate(column = factor(column, levels = c("Institution funds",
                                            "Nonprofit organizations",
                                            "Business", "State and local government",
                                            "All other sources"))) |>
  ggplot(aes(x = herd_year, y = share, alluvium = column, fill = column)) +
  geom_alluvium(alpha = 0.8) +
  facet_wrap(~ broad_field) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of Total R&D Expenditures", fill = "Source") +
  theme_minimal()



#### Plot by fields using expenditure data for fed and nonfed
#### addin fed
herd |>
  filter(question == "Federal expenditures by field and agency") |>
  distinct(row, column) |>
  arrange(row) |>
  head(20)

#build data
fed_data = herd |>
  filter(question == "Federal expenditures by field and agency",
         grepl(", all$", row),
         !grepl("^All$", row),
         column == "Total") |>
  mutate(broad_field = sub(",.*", "", row),
         broad_field = case_when(
           broad_field == "Computer and information sciences" ~ "Computer Sciences",
           broad_field == "Geosciences, atmospheric sciences, and ocean sciences" ~ "Environmental Sciences",
           broad_field == "Mathematics and statistics" ~ "Mathematics",
           broad_field == "Other sciences" ~ "Other",
           TRUE ~ broad_field
         ),
         column = "Federal") |>
  mutate(column = recode(column,
                         "Nonprofit organziations" = "Nonprofit organizations")) |>
  filter(broad_field != "Non-S&E") |>
  group_by(broad_field, column, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop")
#check
distinct(fed_data, herd_year) |> arrange(herd_year)

#bind
source_field_data = bind_rows(fed_data, nonfed_data) |>
  group_by(broad_field, herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()
#check
source_field_data |> distinct(broad_field, column)

#plot
source_field_data |>
  ggplot(aes(x = herd_year, y = share, alluvium = column, fill = column)) +
  geom_alluvium(alpha = 0.8) +
  facet_wrap(~ broad_field) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of Total R&D Expenditures", fill = "Source") +
  theme_minimal()

## Stack alluvial by value not alpha order
# requires ordering within each year which changes across time
# fix: largest to smallest on average.
# check average shares
source_field_data |>
  group_by(column) |>
  summarise(mean_share = mean(share, na.rm = TRUE)) |>
  arrange(desc(mean_share))

# plot using average shares
source_field_data |>
  mutate(column = factor(column, levels = c("Federal", "Institution funds",
                                            "Nonprofit organizations",
                                            "State and local government",
                                            "Business", "All other sources"))) |>
  ggplot(aes(x = herd_year, y = share, alluvium = column, fill = column)) +
  geom_alluvium(alpha = 0.8) +
  facet_wrap(~ broad_field) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of Total R&D Expenditures", fill = "Source") +
  theme_minimal()



# test if industry is increasing funding to partner with data in next
#by field
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

#plot to show trends
nonfed_data |>
  filter(column == "Business") |>
  group_by(broad_field, herd_year) |>
  summarise(total = sum(total), .groups = "drop") |>
  ggplot(aes(x = herd_year, y = total, group = broad_field)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ broad_field, scales = "free_y") +
  labs(x = NULL, y = "Business R&D ($)") +
  theme_minimal()



############ R&D TYPE ALLUVIAL
# explore data
# same as previous, limited to 2010+
herd |>
  filter(question == "Type of R&D conducted",
         row != "All") |>
  distinct(row, column, herd_year) |>
  arrange(herd_year) |>
  head(20)

#prep data
rd_type = herd |>
  filter(question == "Type of R&D conducted",
         row != "All",
         column != "Total") |>
  group_by(row, column, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop") |>
  group_by(column, herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()

#plot faceted by federal and nonfederal to compare
rd_type |>
  mutate(row = factor(row, levels = c("Basic research", "Applied research", "Development"))) |>
  ggplot(aes(x = herd_year, y = share, alluvium = row, fill = row)) +
  geom_alluvium(alpha = 0.8) +
  facet_wrap(~ column) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of R&D Expenditures", fill = "R&D Type") +
  theme_minimal()

#plot by shares
rd_type_total = herd |>
  filter(question == "Type of R&D conducted",
         row != "All",
         column == "Total") |>
  group_by(row, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop") |>
  group_by(herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()

rd_type_total |>
  mutate(row = factor(row, levels = c("Basic research", "Applied research", "Development"))) |>
  ggplot(aes(x = herd_year, y = share, alluvium = row, fill = row)) +
  geom_alluvium(alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of R&D Expenditures", fill = "R&D Type") +
  theme_minimal()

#### TRY LM
#all flat so try lm to find actual change
rd_lm =rd_type_total |>
  group_by(row) |>
  summarise(
    slope = coef(lm(share ~ herd_year))[2],
    intercept = coef(lm(share ~ herd_year))[1],
    .groups = "drop"
  )
rd_lm

rd_type_total |>
  mutate(row = factor(row, levels = c("Basic research", "Applied research", "Development"))) |>
  ggplot(aes(x = herd_year, y = share, color = row)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of R&D Expenditures", color = "R&D Type") +
  theme_minimal()

#### FINAL PLOT WITH LM AND facet fecd and nonfed
# lm for each
rd_lm_source = rd_type_fed |>
  group_by(row, column) |>
  summarise(
    slope = coef(lm(share ~ herd_year))[2],
    intercept = coef(lm(share ~ herd_year))[1],
    .groups = "drop"
  ) |>
  arrange(column, row)
rd_lm_source
# federal dollars are shifting away from basic research faster (-0.42% per year) than nonfederal (-0.19% per year)
# fed dollars are shifting toward applied research faster (+0.28% vs +0.11%).
# Development is increasing in both but slowly.
# federal shift toward applied is driving the aggregate trend more than nonfederal
# bottom line: funding less oriented toward basic science over time, which has implications for the long-term research pipeline at R1 institutions.
# important contrast is that industry funding is not picking up as seen in previous

# plot
rd_type_source = herd |>
  filter(question == "Type of R&D conducted",
         row != "All",
         column != "Total") |>
  group_by(row, column, herd_year) |>
  summarise(total = sum(data, na.rm = TRUE), .groups = "drop") |>
  group_by(column, herd_year) |>
  mutate(share = total / sum(total)) |>
  ungroup()

rd_type_source |>
  mutate(row = factor(row, levels = c("Basic research", "Applied research", "Development"))) |>
  ggplot(aes(x = herd_year, y = share, color = row)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ column) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis_d(end = 0.85) +
  labs(x = NULL, y = "Share of R&D Expenditures", color = "R&D Type") +
  theme_minimal()


########## TIDY
rm(list = ls())
gc()
