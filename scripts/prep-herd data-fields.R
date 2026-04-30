############ LIBRARIES
library(janitor)  # clean names
library(dplyr)    # data wrangling
library(stringr)  # string wrangling


############ DATA SOURCE
# original data from ncses
# https://ncses.nsf.gov/explore-data/microdata/higher-education-research-development
# raw data from https://github.com/kchoover14/r1-federal-funding-dependency
# see scripts/prep-herd data.R for original data from zip to herd-raw.csv
# data/herd-raw.csv not shared due to file size exceeding git limits

herd_raw = read.csv('data/herd-raw.csv')
herd_raw = clean_names(herd_raw)


############ FILTER TO QUESTIONS OF INTEREST
herd_raw |> distinct(question)

herd_filtered = herd_raw |>
  filter(question %in% c(
    "Source",
    "Expenditures by S&E field",
    "Federal expenditures by S&E field and agency",
    "Expenditures by non-S&E field",
    "Externally financed",
    "Type of R&D conducted",
    "Federal expenditures by field and agency",
    "Nonfederal expenditures by field and source",
    "Foreign funds",
    "Inclusion of Institution funds"
  ))

# explore rows
explore = herd_filtered |> distinct(question, row) |> arrange(question, row)


############ PREP DATA
# rename
herd_filtered = herd_filtered |> rename(herd_name = inst_name_long)
herd_filtered = herd_filtered |> rename(herd_state = inst_state_code)
herd_filtered = herd_filtered |> rename(ipeds = ipeds_unitid)

# clean names: lowercase, remove punctuation, standardize ampersand and hyphen
herd_filtered = herd_filtered |>
  mutate(herd_name_clean = str_to_lower(str_squish(herd_name)),
         herd_name_clean = str_replace_all(herd_name_clean, "\\.", ""),
         herd_name_clean = str_replace_all(herd_name_clean, ",", ""),
         herd_name_clean = str_replace_all(herd_name_clean, "&", "and"),
         herd_name_clean = str_replace_all(herd_name_clean, "-", " "))

# relocate
herd_filtered = herd_filtered |> relocate(herd_name_clean, .after = herd_name)

# search for st as abbr for state or saint
herd_filtered |>
  filter(str_detect(herd_name_clean, "\\bst\\b")) |>
  select(herd_name_clean) |>
  distinct() |>
  print(n = Inf)

# normalize st -- expand to full word where st = saint
normalize_st = function(x) {
  x = str_replace_all(x, "\\bst louis\\b", "saint louis")
  x = str_replace_all(x, "\\bst john\\b", "saint john")
  x = str_replace_all(x, "\\bst joseph\\b", "saint joseph")
  x = str_replace_all(x, "\\bst mary\\b", "saint mary")
  x = str_replace_all(x, "\\bst cloud\\b", "saint cloud")
  x = str_replace_all(x, "\\bst olaf\\b", "saint olaf")
  x = str_replace_all(x, "\\bst bonaventure\\b", "saint bonaventure")
  x = str_replace_all(x, "\\bst lawrence\\b", "saint lawrence")
  x = str_replace_all(x, "\\bst andrews\\b", "saint andrews")
  x = str_replace_all(x, "\\bst vincent\\b", "saint vincent")
  x = str_replace_all(x, "\\bst francis\\b", "saint francis")
  x = str_replace_all(x, "\\bst thomas\\b", "saint thomas")
  x = str_replace_all(x, "\\bst michael\\b", "saint michael")
  x = str_replace_all(x, "\\bst edward\\b", "saint edward")
  x = str_replace_all(x, "\\bst catherine\\b", "saint catherine")
  x = str_replace_all(x, "\\bst scholastica\\b", "saint scholastica")
  x
}

herd_filtered = herd_filtered |>
  mutate(herd_name_clean = normalize_st(herd_name_clean))

# check results for any remaining st
herd_filtered |>
  filter(str_detect(herd_name_clean, "\\bst\\b")) |>
  select(herd_name_clean) |>
  distinct()

# final name check
name_check = distinct(herd_filtered, herd_name_clean)


######################## SAVE AND TIDY
write.csv(herd_filtered, 'artifacts/herd.csv', row.names = FALSE)

rm(list = ls())
gc()