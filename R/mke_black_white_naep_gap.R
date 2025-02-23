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
  ~test, ~grade, ~white, ~black,
  "math", "4th grade", 51, 5,
  "reading", "4th grade", 38, 8,
  "math", "8th grade", 45, 7,
  "reading", "8th grade", 36, 9
) |> 
  mutate(gap = white - black,
         gg = paste(grade, test),
         gg = factor(gg, levels = c(
           "4th grade reading",
           "4th grade math",
           "8th grade reading",
           "8th grade math"
         )))

these |> 
  mutate(white = white / 100,
         black = black / 100) |> 
  ggplot(aes(x = black, xend = white, y = reorder(gg, desc(gg)))) +
  geom_segment(color = cs[3], linewidth = 2) +
  geom_point(inherit.aes = FALSE,
             aes(x = white, y = gg), color = cs[2], size = 15) +
  # geom_text(aes(label = label_percent(1)(white), x = white),
  #           color = "white", size = 4.5) +

  geom_point(inherit.aes = FALSE,
             aes(x = black, y = gg), color = cs[1], size = 15) +
  # geom_text(aes(label = label_percent(1)(black), x = black), size = 5) +
  geom_text(aes(label = glue("{gap} point gap"),
                x = ((white - black)/2)+black),
            nudge_y = .25, family = "Verdana", size = 5) +
  scale_x_continuous(limits = c(0, .6),
                     breaks = seq(from = 0, to = .5, by = .5),
                     labels = label_percent()) +
  theme(panel.grid.major.x = element_line(),
        panel.grid.major.y = element_blank()) +
  labs(y = "", x = "% Proficient")

ggsave("plots/gap_dumbbell_no_percs.png", bg = NA,
       w = 7, h = 4)


