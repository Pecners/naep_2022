library(tidyverse)
library(glue)
library(ggtext)
library(cityforwardcollective)
library(cowplot)

files <- c(
  "data/clean/8th_grade_math_scale_scores_juris.csv",
  "data/clean/4th_grade_math_scale_scores_juris.csv",
  "data/clean/8th_grade_reading_scale_scores_juris.csv",
  "data/clean/4th_grade_reading_scale_scores_juris.csv"
)

black <- map_df(files, function(f) {
  t <- read_csv(f)
  names(t)[3] <- "race"
  t |> 
    filter(race == "Black") |> 
    mutate(score = as.numeric(average_scale_score),
           test = str_extract(f, "\\dth_grade_[:alpha:]{4,7}"))
})

black |> 
  ggplot(aes(year, score, group = jurisdiction,
             color = ifelse(jurisdiction == "Milwaukee", 
                            cfc_orange, "grey90"))) + 
  geom_line() +
  scale_color_identity() +
  facet_wrap(~ test) +
  scale_x_continuous(breaks = c(2019, 2022)) +
  theme(panel.grid.minor = element_blank())

black |> 
  filter(year == 2022 & !is.na(score) & test == "4th_grade_reading") |> 
  mutate(color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
                           jurisdiction == "National" ~ cfc_darkblue,
                           TRUE ~ "grey90"),
         text_color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
                                jurisdiction == "National" ~ cfc_darkblue,
                                TRUE ~ "grey50"),
         name = glue("<span style='color:{text_color}'>{jurisdiction}</span>")) |> 
  ggplot(aes(reorder(name, score), score,
             fill = color)) +
  geom_col() +
  scale_fill_identity() +
  scale_y_continuous(breaks = c(0, 100, 200)) +
  coord_flip() +
  theme(panel.grid.major.y = element_blank(),
        text = element_text(family = "Verdana"),
        plot.title = element_text(family = "Georgia", size = 24,
                                  margin = margin(b = 20)),
        plot.subtitle = element_text(color = "grey30",
                                     margin = margin(b = 40)),
        axis.text.y = element_markdown(),
        axis.title.x = element_text(margin = margin(t = 10)),
        plot.caption = element_text(color = "grey50", lineheight = 1.1,
                                    margin = margin(t = 15))) +
  labs(title = "Milwaukee is failing its Black students",
       subtitle = "Milwaukee is at the absolute bottom when it comes to Black student proficiency",
       y = "Average scale score for Black students in 4th grade reading",
       x = "",
       caption = glue("We're only showing 4th grade reading here, but Milwaukee ",
                      "was either at the bottom or statistically no different from the ",
                      "bottom scoring districts for Black students on all tests. ",
                      "Data from 2022 NAEP TUDA results. ") |> 
         str_wrap(125))

ggsave(filename =  "plots/black_4th_reading.png", bg = "white")


math_8th <- black |> 
  filter(year == 2022 & !is.na(score) & test == "8th_grade_math") |> 
  mutate(color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
                           jurisdiction == "National" ~ cfc_darkblue,
                           TRUE ~ "grey90"),
         text_color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
                           jurisdiction == "National" ~ cfc_darkblue,
                           TRUE ~ "grey50"),
         name = glue("<span style='color:{text_color}'>{jurisdiction}</span>")) |> 
  ggplot(aes(reorder(name, score), score,
             fill = color)) +
  geom_col() +
  scale_fill_identity() +
  coord_flip() +
  theme(panel.grid.major.y = element_blank(),
        text = element_text(family = "Verdana"),
        plot.title = element_text(family = "Georgia", hjust = .5),
        axis.text.y = element_markdown()) +
  labs(title = "Black students 8th grade math scores",
       y = "Average scale score for Black students",
       x = "")

plot_grid(reading_4th, math_8th, )

ggsave("plots/black_students_comps.png", bg = "white", width = 12, h = 6)


