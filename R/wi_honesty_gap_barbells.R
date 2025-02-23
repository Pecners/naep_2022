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
  ~test, ~grade, ~NAEP, ~WI,
  "math", "4th grade", 42, 54,
  "reading", "4th grade", 31, 52,
  "math", "8th grade", 37, 51,
  "reading", "8th grade", 31, 48
) |> 
  mutate(gap = WI - NAEP,
         gg = paste(grade, test),
         gg = factor(gg, levels = c(
           "4th grade reading",
           "4th grade math",
           "8th grade reading",
           "8th grade math"
         )))

these |> 
  mutate(NAEP = NAEP / 100,
         WI = WI / 100) |> 
  ggplot(aes(x = WI, xend = NAEP, y = reorder(gg, desc(gg)))) +
  geom_segment(color = cs[3], linewidth = 2) +
  geom_point(inherit.aes = FALSE,
             aes(x = NAEP, y = gg), color = cs[2], size = 15) +
  # geom_text(aes(label = label_percent(1)(NAEP), x = NAEP),
  #           color = "white", size = 4.5) +
  
  geom_point(inherit.aes = FALSE,
             aes(x = WI, y = gg), color = cs[1], size = 15) +
  # geom_text(aes(label = label_percent(1)(WI), x = WI), size = 5) +
  geom_text(aes(label = glue("{gap} point gap"),
                x = ((NAEP - WI)/2)+WI),
            nudge_y = .25, family = "Verdana", size = 5) +
  scale_x_continuous(limits = c(.295, .55),
                     breaks = c(.3, .5),
                     labels = label_percent()) +
  theme(panel.grid.major.x = element_line(),
        panel.grid.major.y = element_blank()) +
  labs(y = "", x = "% Proficient")

ggsave("plots/honesty_gap_dumbbell_no_percs.png", bg = NA,
       w = 7, h = 4)


