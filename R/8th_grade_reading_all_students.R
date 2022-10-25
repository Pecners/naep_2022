library(tidyverse)
library(cityforwardcollective)

data <- read_csv("data/8th_grade_reading_scale_scores_juris_all_students.csv", 
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

change |> 
  filter(!is.na(diff)) |> 
  ggplot(aes(y = `2019`, yend = `2022`,
             x = reorder(jurisdiction, `2022`), 
             xend = reorder(jurisdiction, `2022`),
             color = ifelse(diff < 0, cfc_darkblue, cfc_orange))) +
  geom_segment(arrow = arrow(length = unit(.2, "cm"), type = "closed")) +
  geom_text(aes(y = `2022`, 
                label = ifelse(diff < 0, diff, paste0("+", diff)),
                hjust = ifelse(diff < 0, 1.25, -.25)),
            size= 5) +
  coord_flip() +
  scale_color_identity() +
  theme(text = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = 3)) +
  labs(y = "Score Change (2019 to 2022)",
       x = "Jurisdiction (orderd by 2022 score)",
       title = "Score change by Jurisdiction—8th Grade Reading")

ggsave("plots/8th_grade_reading_all_students.png", bg = "white",
       width = 12, height = 12)

