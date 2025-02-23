library(tidyverse)
library(wisconsink12)
library(cityforwardcollective)

d <- read_csv("data/naep_mke_race.csv")

ys <- c("'09", "'19", "'22", "'24")


# 4th grade

d |> 
  filter(grades == 4) |> 
  ggplot(aes(years, Score, color = groups)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_text(aes(label = Score,
                vjust = ifelse(groups == "Black", 2, -1)),
            color = cfc_darkblue, fontface = "bold") +
  facet_wrap(~subjects, nrow = 1,
             labeller = labeller(
               subjects = c(
                 Math = "4th Grade Math",
                 Reading = "4th Grade Reading"
               )
             )) +
  scale_y_continuous(limits = c(165, 255), breaks = c(170, 210, 250)) +
  scale_x_continuous(breaks = c(2009, 2019, 2022, 2024),
                     labels = ys) +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_line(linetype = 3),
        strip.text = element_text(face = "bold", size = 20)) +
  labs(color = "", x = "")

ggsave("plots/naep_mke_race_trends_4th.png", bg = "transparent",
       w = 7, h = 5)

# 8th grade

d |> 
  filter(grades == 8) |> 
  ggplot(aes(years, Score, color = groups)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_text(aes(label = Score,
                vjust = ifelse(groups == "Black", 2, -1)),
            color = cfc_darkblue, fontface = "bold") +
  facet_wrap(~subjects, nrow = 1,
             labeller = labeller(
               subjects = c(
                 Math = "8th Grade Math",
                 Reading = "8th Grade Reading"
               )
             )) +
  scale_y_continuous(limits = c(220, 290), breaks = c(220, 250, 280)) +
  scale_x_continuous(breaks = c(2009, 2019, 2022, 2024),
                     labels = ys) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_line(linetype = 3),
        strip.text = element_text(face = "bold", size = 20)) +
  labs(color = "", x = "")

ggsave("plots/naep_mke_race_trends_8th.png", bg = "transparent",
       w = 7, h = 5)
