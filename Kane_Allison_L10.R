# L10 - joins and missingness ----
# Stat 301-1

## load packages ----

library(tidyverse)
library(nycflights13)
library(fueleconomy)
library(skimr)

## load datasets ----

data("flights")
weather <- read_builtin("weather", "nycflights13")

## Exercises ----

### Exercise 1 ----

# see qmd

### Exercise 2----

# see qmd

### Exercise 3----

names(weather) |> 
  intersect(names(flights))

weather_delay <- flights |> 
  left_join(
    weather, 
    by = c("year", "month", "day", "hour", "origin") # if diff names set = to each other, ex year_dataset1 = year_dataset2
  ) |> 
  select(year, month, day, hour, origin, dep_delay, temp:visib)

# need to scope this more

# aggregate daily info
weather_delay_summarized <- weather_delay |> 
  group_by(origin, year, month, day) |> 
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE),
            mean_temp = mean(temp, na.rm = TRUE),
            mean_dewp = mean(dewp, na.rm = TRUE),
            mean_humid = mean(humid, na.rm = TRUE),
            mean_wind_dir = mean(wind_dir, na.rm = TRUE),
            mean_wind_speed = mean(wind_speed, na.rm = TRUE),
            mean_wind_gust = mean(wind_gust, na.rm = TRUE),
            mean_precip = mean(precip, na.rm = TRUE),
            mean_pressure = mean(pressure, na.rm = TRUE),
            mean_visib = mean(visib, na.rm = TRUE),
            num_fligths = n()
  )


ggplot(weather_delay, aes(x = dep_delay, y = precip)) +
  geom_point(alpha = .1)

weather_delay |> 
ggplot(aes(x = dep_delay, y = wind_speed)) +
  geom_point(alpha = .1)

weather_delay |> 
  ggplot(aes(x = dep_delay, y = temp)) +
  geom_point(alpha = .1)

weather_delay |> 
  ggplot(aes(x = dep_delay, y = visib)) +
  geom_point(alpha = .05)

weather_delay |> 
  ggplot(aes(x = dep_delay, y = wind_gust)) +
  geom_point(alpha = .05)

### Exercise 4----

airports

anti_join(flights, airports, by = c("dest" = "faa"))

# this anti_join is searching the flights dataset for dest observations that do 
# not match faa in the aiports dataset

anti_join(airports, flights, by = c("faa" = "dest"))

# this anti join is searching the airports dataset for faa observations that do 
# not match dest in the flights dataset



### Exercise 5----
### Exploration 1----
# Combine `fueleconomy::vehicles` and `fueleconomy::common` to 
# find the records for only the most common models.

combined_cars <- inner_join(vehicles, common, by = c("model", "make"))

combined_cars |> 
  group_by(model) |> 
  summarize(
    count = n()
  ) |> 
  arrange(desc(count)) |> 
  head(5)
  
### Case Study 1----

rankings_2021 <- read_rds("data/rankings_2021.rds")
rankings_2022 <- read_rds("data/rankings_2022.rds")

# a) What types of explicit missingness exists in `rankings_2021` and `rankings_2022`, if any?

missing_2021 <- is.na(rankings_2021)
rankings_2021 |> 
  skim()

# missing values explicitly in institution fee, room and board, salary after 10, resources_score, 
# engagement_score, outcomes_score, environment_score.

missing_2022 <- is.na(rankings_2022)
rankings_2022 |> 
  skim()

# missing values explicitly in institution's tution fee, room and board, salary after 10, resources_score, 
# engagement_score, outcomes_score, environment_score.

# b) What types of implicit missingness exists in `rankings_2021` and `rankings_2022`, if any?

# none observed?

# c) What is the "key" and is it a primary, compound primary, foreign, or surrogate key?

rankings_2021 |> 
  count(institution) |> 
  filter(n > 1)

rankings_2022 |> 
  count(institution) |> 
  filter(n > 1)

# the key is the variable institution?

### Exploration 2----
# Combine `rankings_2021` and `rankings_2022` to answer the following questions:
  
  # a) Which colleges had tuition increase the most from 2021 to 2022?

combined_rankings <- left_join(rankings_2021, rankings_2022, by = c("institution", "state_abbr", "city"))

combined_rankings |> 
  mutate(
    tuition_increase = tuition_fees.y - tuition_fees.x
  ) |> 
  arrange(desc(tuition_increase)) |> 
  head(5)

# the top 5 colleges with the highest tuition increases were Columbia University, Berea College, Emerson College,
# University of St Thomas and University of Minnesota.

# b) Only considering colleges ranked in the top 400, which college increased the most in ranking from 2021 to 2022?
combined_rankings |> 
  filter(rank_2021 <= 400 & rank_2022 <= 400) |> 
  mutate(
    rank_change = rank_2022 - rank_2021
  ) |> 
  arrange(desc(rank_change)) |> 
  head(1)

# the college with biggest increases in ranking from 2021 to 2022 was Hendrix College

# c) Which colleges were ranked in 2021 but not ranked in 2022? 
combined_rankings |> 
  filter(is.na(rank_2021) == FALSE & is.na(rank_2022) == TRUE)

# Westminster College (Missouri) and Marian University (Indiana) were
# ranked 2021 but not 2022

# which colleges were ranked in 2022 but not ranked in 2021?
combined_rankings |> 
       filter(is.na(rank_2021) == TRUE & is.na(rank_2022) == FALSE)

# no colleges were ranked in 2022 but not 2021.

### Case Study 2----

# Add a new variable called `year` which has the value of `2021` for each 
# observation of `rankings_2021` and the value of `2022` for each observation of 
# rankings_2022`. Then use `bind_rows()` to combine `rankings_2021` and `rankings_2022`.  

rankings_2021 <- rankings_2021 |> 
  mutate(
    year = "2021",
    resources_score = as.numeric(resources_score),
    engagement_score = as.numeric(engagement_score),
    outcomes_score = as.numeric(outcomes_score),
    environment_score = as.numeric(environment_score)
  )

rankings_2022 <- rankings_2022 |> 
  mutate(
    year = "2022",
    resources_score = as.numeric(resources_score),
    engagement_score = as.numeric(engagement_score),
    outcomes_score = as.numeric(outcomes_score),
    environment_score = as.numeric(environment_score)
  )

bind_rows(rankings_2022, rankings_2021)

# When might you use `bind_rows` or `bind_cols` instead of a `join`?

# it might be better to use bind_rows or bind_cols if one would rather have a side-by-side
# comparison of the observations across the same columns or rows. A join is more useful to
# see a merged dataframe based on a common key.