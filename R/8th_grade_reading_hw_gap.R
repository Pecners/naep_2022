library(tidyverse)

data <- read_csv("data/8th_grade_reading_scale_scores_juris.csv", 
                 name_repair = janitor::make_clean_names)

names(data)[3] <- "race"

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



gap <- map_df(c("2019", "2022"), function(y) {
  data |> 
    filter(year == y) |> 
    mutate(average_scale_score = as.numeric(average_scale_score)) |> 
    pivot_wider(names_from = race, 
                values_from = average_scale_score) |> 
    mutate(diff = White - Hispanic)
})



gg <- gap |> 
  select(year, jurisdiction, diff) |> 
  pivot_wider(names_from = year, values_from = diff) |> 
  mutate(diff = `2022` - `2019`) |> 
  filter(!is.na(diff)) |> 
  arrange(desc(diff))

gg |> 
  ggplot(aes(x = reorder(jurisdiction, `2022`), 
             xend = reorder(jurisdiction, `2022`),
             y = `2019`,
             yend = `2022`,
             color = ifelse(diff < 0, cfc_darkblue, cfc_orange))) +
  geom_segment(arrow = arrow(length = unit(.2, "cm"), type = "closed")) +
  geom_text(aes(y = `2022`, 
                label = ifelse(diff < 0, diff, paste0("+", diff)),
                hjust = ifelse(diff < 0, 1.5, -.5))) +
  coord_flip() +
  scale_color_identity() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 3)) +
  labs(y = "Gap Change (2019 to 2022)",
       x = "Jurisdiction (orderd by 2022 gap)",
       title = "Hispanic-White gap change by Jurisdiction—8th Grade Reading")

ggsave("plots/8th_grade_reading_hw_gap.png", bg = "white")


gap |> 
  select(year, jurisdiction, diff) |> 
  pivot_wider(names_from = year, values_from = diff) |> 
  mutate(diff = `2022` - `2019`) |> 
  filter(!is.na(diff)) |> 
  arrange(desc(diff)) |> 
  ggplot(aes(x = reorder(jurisdiction, diff), 
             xend = reorder(jurisdiction, diff),
             y = `2019`,
             yend = `2022`,
             color = ifelse(diff < 0, cfc_darkblue, cfc_orange))) +
  geom_segment(arrow = arrow(length = unit(.2, "cm"), type = "closed")) +
  geom_text(aes(y = `2022`, 
                label = ifelse(diff < 0, diff, paste0("+", diff)),
                hjust = ifelse(diff < 0, 1.5, -.5))) +
  coord_flip() +
  scale_color_identity() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 3)) +
  labs(y = "Gap Change (2019 to 2022)",
       x = "Jurisdiction (orderd by 2022 gap)",
       title = "Hispanic-White gap change by Jurisdiction—8th Grade Reading")
