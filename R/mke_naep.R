library(tidyverse)
library(scales)
library(cityforwardcollective)
library(glue)

cs <- c(
  "#ed902d",
  "#395549",
  "#f1be39"
)

these <- tribble(
  ~test, ~grade, ~naep,
  "Math", "4th grade", 12,
  "Reading", "4th grade", 9,
  "Math", "8th grade", 8,
  "Reading", "8th grade", 15
) |> 
  mutate(gg = paste(grade, test),
         gg = factor(gg, levels = c(
           "4th grade Reading",
           "4th grade Math",
           "8th grade Reading",
           "8th grade Math"
         )),
         grade = factor(grade, levels = c(
           "8th grade",
           "4th grade"
         )))

these |> 
  ggplot(aes(test, naep, fill = grade)) +
  geom_col(position = position_dodge(.6), width = .5) +
  geom_text(aes(label = glue("{naep}%"), y = naep + 1),
            position = position_dodge(.6), size = 5,
            family = "Verdana", fontface = "bold") +
  scale_fill_manual(values = c("#348789", cfc_orange),
                    limits = c("4th grade", "8th grade")) +
  scale_y_continuous(breaks = c(0, 10, 20), labels = c("0%", "10%", "20%")) +
  labs(x = "", y = "", fill = "") +
  coord_flip(clip = "off") +
  theme(axis.text.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = "bottom",
        legend.margin = margin(t = -20),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line())

ggsave("plots/mke_naep.png", bg = NA,
       w = 7, h = 4)


