############ LIBRARIES
library(dplyr)       # data wrangling
library(readxl)      # read excel files
library(stringr)     # string wrangling
library(fuzzyjoin)   # carnegie join pre-2010 for fuzzy matches on names
library(stringdist)  # for direct matching


############ DATA SOURCE
# read carnegie data filtered to r1
# data generated in https://github.com/kchoover14/r1-federal-funding-dependency
# see scripts/prep-carnegie.R and artifacts/carnegie-r1.csv
carnegie = read.csv('data/carnegie-r1.csv')

# read herd data cleaned
herd = read.csv('artifacts/herd.csv')

# both have 'year' as a var so change to distinguish
carnegie = rename(carnegie, carnegie_year = year)
herd = rename(herd, herd_year = year)

# HERD data are collected every year, carnegie data are in cycles
# create a carnegie cycle year col for herd data for fuzzy match to carnegie
# check years in both datasets
table(herd$herd_year)
table(carnegie$carnegie_year)

# create col
herd = herd |>
  mutate(herd_carnegieCycle = case_when(
    herd_year == 1974 ~ 1973,
    herd_year == 1975 ~ 1973,
    herd_year == 1976 ~ 1976,
    herd_year == 1977 ~ 1976,
    herd_year == 1978 ~ 1976,
    herd_year == 1979 ~ 1976,
    herd_year == 1980 ~ 1976,
    herd_year == 1981 ~ 1976,
    herd_year == 1982 ~ 1976,
    herd_year == 1983 ~ 1976,
    herd_year == 1984 ~ 1976,
    herd_year == 1985 ~ 1976,
    herd_year == 1986 ~ 1976,
    herd_year == 1987 ~ 1987,
    herd_year == 1988 ~ 1987,
    herd_year == 1989 ~ 1987,
    herd_year == 1990 ~ 1987,
    herd_year == 1991 ~ 1987,
    herd_year == 1992 ~ 1987,
    herd_year == 1993 ~ 1987,
    herd_year == 1994 ~ 1994,
    herd_year == 1995 ~ 1994,
    herd_year == 1996 ~ 1994,
    herd_year == 1997 ~ 1994,
    herd_year == 1998 ~ 1994,
    herd_year == 1999 ~ 1994,
    herd_year == 2000 ~ 2000,
    herd_year == 2001 ~ 2000,
    herd_year == 2002 ~ 2000,
    herd_year == 2003 ~ 2000,
    herd_year == 2004 ~ 2000,
    herd_year == 2005 ~ 2005,
    herd_year == 2006 ~ 2005,
    herd_year == 2007 ~ 2005,
    herd_year == 2008 ~ 2005,
    herd_year == 2009 ~ 2005,
    herd_year == 2010 ~ 2010,
    herd_year == 2011 ~ 2010,
    herd_year == 2012 ~ 2010,
    herd_year == 2013 ~ 2010,
    herd_year == 2014 ~ 2010,
    herd_year == 2015 ~ 2015,
    herd_year == 2016 ~ 2015,
    herd_year == 2017 ~ 2015,
    herd_year == 2018 ~ 2018,
    herd_year == 2019 ~ 2018,
    herd_year == 2020 ~ 2018,
    herd_year == 2021 ~ 2021,
    herd_year == 2022 ~ 2021,
    herd_year == 2023 ~ 2021,
    herd_year == 2024 ~ 2021,
    herd_year == 2025 ~ 2025
  ))


############ PREP DATA FOR FUZZY MATCHING
# split herd_source into post and pre 2010
# ipeds data present from 2010 on for exact matching
herd_post = herd |> filter(herd_year >= 2010)
herd_pre  = herd |> filter(herd_year < 2010)


############ JOIN DATA BY EXACT MATCH, POST 2010
# post-2010: join on IPEDS unitid
herd_post = herd_post |>
  inner_join(carnegie, by = c("ipeds" = "unitid", "herd_carnegieCycle" = "carnegie_year"))

# check data
names(herd_post)
head(herd_post)


############ PRE-2010 DATA WRANGLING
# institution names vary in HERD data (e.g., state versus saint, u versus univ etc)
# will eliminate true positives in fuzzy match (ex ca v california)

# STEP 1: filter by threshold
# 1973-1994, Top 50 institutions by federal grants received
table(herd_pre$herd_year)

top50 = herd_pre |>
  filter(herd_year <= 1994, question == "Source",
         row == "Federal") |>
  group_by(herd_year) |>
  slice_max(order_by = data, n = 50, with_ties = FALSE) |>
  ungroup() |>
  select(herd_name_clean, herd_year)

# join to filter first section
herd_pre_early = herd_pre |>
  filter(herd_year <= 1994) |>
  semi_join(top50, by = c("herd_name_clean", "herd_year"))

# filter pre for data parsed in previous section
herd_pre_late = herd_pre |>
  filter(herd_year > 1994)

# bind data
herd_pre = bind_rows(herd_pre_early, herd_pre_late)

# check rows
table(herd_pre$herd_year)

# look for unique names
herd_distinct = distinct(herd_pre, herd_name_clean)
carnegie_distinct = carnegie |> distinct(unitid, carnegie_name_clean)


# STEP 2: handle lingering common problems via string
herd_pre = herd_pre |>
  mutate(
    herd_name_clean = str_replace_all(herd_name_clean, "aandm", "a and m"),
    herd_name_clean = str_replace_all(herd_name_clean, "\\ball campuses\\b", ""),
    herd_name_clean = str_replace_all(herd_name_clean, "\\bmain division\\b", ""),
    herd_name_clean = str_replace_all(herd_name_clean, "\\bmain div\\b", ""),
    herd_name_clean = str_replace_all(herd_name_clean, "^the\\s+", ""),
    herd_name_clean = str_replace_all(herd_name_clean, "\\s+the$", ""),
    herd_name_clean = str_replace_all(herd_name_clean, "\\bsaint\\b", "st"),
    herd_name_clean = str_squish(herd_name_clean)
  )


############ JOIN DATA BY FUZZY MATCHING, PRE 2010
# fuzzy match on name and state and year
herd_pre = stringdist_inner_join(herd_pre, carnegie,
                                 by = c("herd_name_clean" = "carnegie_name_clean"),
                                 method = "jw",
                                 max_dist = 0.15,
                                 distance_col = "dist") |>
  filter(herd_state == stabbr,
         herd_carnegieCycle == carnegie_year) |>
  mutate(flag = ifelse(dist > 0.05, "Possible Bad Match", "Good Match"))

# check results
herd_pre |>
  group_by(flag) |>
  summarise(n_inst = n_distinct(herd_name_clean),
            n_rows = n())
# flag               n_inst n_rows
# 1 Good Match            105   121047
# 2 Possible Bad Match     120    121022

# check that the high number of rows is due to the additional questions included in analysis
table(herd_pre$question)


## INSPECT -- artifacts/fuzzy-match-manual-inspection.xlsx
# multiple values per carnegie cycle per institution because HERD is annual
# filter by distinct to get inst/year pairs to manually inspect
# commented out: inspection completed in r1-federal-funding-dependency
# see scripts/prep-final data and artifacts/fuzzy-match-inspect.csv

# remove all true bad matches
inspect = read.csv("artifacts/fuzzy-match-inspect.csv")

remove_list = inspect |>
  filter(Action == "Remove") |>
  select(herd_name_clean, carnegie_name_clean, herd_carnegieCycle)

herd_pre = herd_pre |>
  anti_join(remove_list, by = c("herd_name_clean", "herd_carnegieCycle"))

# drop carnegie join columns
herd_pre = herd_pre |> select(-carnegie_year, -unitid, -dist, -flag)


######################## JOIN HERD DATA FOR ANALYSIS
herd = bind_rows(herd_pre, herd_post)

# clean up vars for analysis
herd = herd |> select(herd_name_clean, carnegie_city, herd_state, herd_year,
                      herd_carnegieCycle, question, row, column, data)

# rename
herd = herd |> rename(
  inst_name     = herd_name_clean,
  city          = carnegie_city,
  state         = herd_state,
  carnegie_cycle = herd_carnegieCycle
)

# save
write.csv(herd, "data/herd-clean.csv", row.names = FALSE)


######################## TIDY
rm(list = ls())
gc()