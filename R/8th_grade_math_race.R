library(tidyverse)

data <- read_csv("data/8th_grade_math_scale_scores_juris.csv", 
                 name_repair = janitor::make_clean_names)

year <- juris <- NULL

for (i in 1:nrow(data)) {
  if (!is.na(data[[i,1]])) {
    year <- data[[i,1]]
  } else {
    data[i,1] <- year
  }
  
  if (!is.na(data[[i,2]])) {
    juris <- data[[i,2]]
  } else {
    data[i,2] <- juris
  }
}

change <- data |> 
  mutate(average_scale_score = as.numeric(average_scale_score)) |> 
  pivot_wider(names_from = year, values_from = average_scale_score) |> 
  mutate(diff = `2022` - `2019`)

gap <- data |> 
  filter(year == 2022) |> 
  mutate(average_scale_score = as.numeric(average_scale_score)) |> 
  pivot_wider(names_from = race_ethnicity_used_to_report_trends_school_reported, 
              values_from = average_scale_score) |> 
  mutate(diff = White - Black)

white_black <- map_df(c("2019", "2022"), function(y) {
  data |> 
    filter(year == y) |> 
    mutate(average_scale_score = as.numeric(average_scale_score)) |> 
    pivot_wider(names_from = race_ethnicity_used_to_report_trends_school_reported, 
                values_from = average_scale_score) |> 
    mutate(diff = White - Black)
})

white_black |> 
  ggplot(aes(year, diff, group = jurisdiction,
             color = ifelse(jurisdiction == "Milwaukee", "red", "grey70"))) +
  geom_line() +
  scale_color_identity()
