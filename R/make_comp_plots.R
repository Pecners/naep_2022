library(tidyverse)
library(glue)
library(ggtext)
library(cityforwardcollective)
library(patchwork)
library(ggtext)
library(magick)

files <- c(
  "data/clean/8th_grade_math_scale_scores_juris.csv",
  "data/clean/4th_grade_math_scale_scores_juris.csv",
  "data/clean/8th_grade_reading_scale_scores_juris.csv",
  "data/clean/4th_grade_reading_scale_scores_juris.csv"
)

rr <- c("Black", "Hispanic", "White")

walk(rr, function(r) {
  black <- map_df(files, function(f) {
    t <- read_csv(f)
    names(t)[4] <- "race"
    t |> 
      filter(race == r) |> 
      mutate(score = as.numeric(average_scale_score),
             test = str_extract(f, "\\dth_grade_[:alpha:]{4,7}"),
             jurisdiction = ifelse(jurisdiction == "Nation", "National", jurisdiction))
  })
  
  # black |> 
  #   ggplot(aes(year, score, group = jurisdiction,
  #              color = ifelse(jurisdiction == "Milwaukee", 
  #                             cfc_orange, "grey90"))) + 
  #   geom_line() +
  #   scale_color_identity() +
  #   facet_wrap(~ test) +
  #   scale_x_continuous(breaks = c(2024, 2024)) +
  #   theme(panel.grid.minor = element_blank())
  
  black |> 
    filter(year == 2024 & !is.na(score) & test == "4th_grade_reading") |> 
    mutate(color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
                             jurisdiction == "National" ~ cfc_darkblue,
                             TRUE ~ "grey90"),
           text_color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
                                  jurisdiction == "National" ~ cfc_darkblue,
                                  TRUE ~ "grey50"),
           face = case_when(jurisdiction == "Milwaukee" ~ "<strong>",
                            jurisdiction == "National" ~ "<strong>",
                            TRUE ~ ""),
           fend = case_when(jurisdiction == "Milwaukee" ~ "</strong>",
                            jurisdiction == "National" ~ "</strong>",
                            TRUE ~ ""),
           name = glue("<span style='color:{text_color}'>{face}{jurisdiction}{fend}</span>")) |> 
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
    labs(title = glue("Milwaukee is failing its {r} students"),
         subtitle = glue("Milwaukee is at the absolute bottom when it comes to {r} student proficiency"),
         y = "Average scale score for students in 4th grade reading",
         x = "",
         caption = glue("We're only showing 4th grade reading here, but Milwaukee ",
                        "was either at the bottom or statistically no different from the ",
                        "bottom scoring districts for Black students on all tests. ",
                        "Data from 2024 NAEP TUDA results. ") |> 
           str_wrap(125))
  
  ggsave(filename =  glue("plots/{str_to_lower(str_replace_all(r, ' ', '_'))}_4th_reading.png"), 
         bg = "white")
  
  tests <- unique(black$test)
  
  plots <- map(tests, function(t) {
    black |> 
      filter(year == 2024 & !is.na(score) & test == t) |> 
      mutate(color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
                               jurisdiction == "National" ~ cfc_darkblue,
                               TRUE ~ "grey90"),
             text_color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
                                    jurisdiction == "National" ~ cfc_darkblue,
                                    TRUE ~ "grey50"),
             face = case_when(jurisdiction == "Milwaukee" ~ "<strong>",
                              jurisdiction == "National" ~ "<strong>",
                              TRUE ~ ""),
             fend = case_when(jurisdiction == "Milwaukee" ~ "</strong>",
                              jurisdiction == "National" ~ "</strong>",
                              TRUE ~ ""),
             name = glue("<span style='color:{text_color}'>{face}{jurisdiction}{fend}</span>")) |> 
      ggplot(aes(reorder(name, score), score,
                 fill = color)) +
      geom_col() +
      scale_fill_identity() +
      coord_flip() +
      theme(panel.grid.major.y = element_blank(),
            text = element_text(family = "Verdana", size = 10),
            plot.title = element_text(family = "Georgia",
                                      size = 16),
            axis.text.y = element_markdown()) +
      labs(title = glue("{r} students {str_replace_all(t, '_', ' ')} scores"),
           y = "Average scale score for students",
           x = "")
  })
  
  layout <- c(
    area(1,1), area(1,2),
    area(2,1), area(2,2)
  )
  
  wrap_plots(
    plots[[2]], plots[[4]],
    plots[[1]], plots[[3]],
    design = layout
  )
  ggsave(glue("plots/{str_to_lower(str_replace_all(r, ' ', '_'))}_students_comps.png"), 
         bg = "white", width = 12, height = 10)
})


# LIMIT TO 4TH GRADE READING

# plots <- map(rr, function(r) {
#   black <- map_df(files, function(f) {
#     t <- read_csv(f)
#     names(t)[4] <- "race"
#     t |> 
#       filter(race == r) |> 
#       mutate(score = as.numeric(average_scale_score),
#              test = str_extract(f, "\\dth_grade_[:alpha:]{4,7}"))
#   })
#   
#   if (str_detect(r, "Asian")) {
#     r <- "Asian"
#   }
#   
#   black |> 
#     filter(year == 2024 & !is.na(score) & test == "4th_grade_reading") |> 
#     mutate(color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
#                              jurisdiction == "National" ~ cfc_darkblue,
#                              TRUE ~ "grey90"),
#            text_color = case_when(jurisdiction == "Milwaukee" ~ cfc_orange,
#                                   jurisdiction == "National" ~ cfc_darkblue,
#                                   TRUE ~ "grey50"),
#            name = glue("<span style='color:{text_color}'>{jurisdiction}</span>")) |> 
#     ggplot(aes(reorder(name, score), score,
#                fill = color)) +
#     geom_col() +
#     scale_y_continuous(breaks = c(0, 100, 200, 300)) +
#     scale_fill_identity() +
#     coord_flip() +
#     theme(panel.grid.major.y = element_blank(),
#           panel.grid.major.x = element_line(),
#           text = element_text(family = "Verdana", size = 10),
#           plot.title = element_text(family = "Georgia",
#                                     hjust = .5,
#                                     size = 16),
#           axis.text.y = element_markdown(),
#           plot.margin = margin(10, 10, 10, -20)) +
#     labs(title = glue("{r} students"),
#          y = "Average scale score (4th grade reading)",
#          x = "")
#   
# })
# 
# # # set up title and caption ------------------------------------------
# # 
# # title <- ggplot() +
# #   geom_text(aes(x = 0, y = 1.5), label = "Milwaukee is failing its students", size = 15,
# #             hjust = 0, family = "Georgia", fontface = "bold") +
# #   geom_textbox(aes(x = 0, y = .75), 
#                label = glue("Data from 2018 shows habitat use of ",
#                             "at Crane Prairie Reservoir in Oregon. Herbaceous vegetation ",
#                             "was the most popular structure for both sexes, and the river ",
#                             "was the only habitat where males outnumbered females."), 
#                size = 6, width = unit(8.8, "in"), family = "Verdana",
#                hjust = 0, vjust = 1, box.size = 0, color = "grey50") +
#   scale_y_continuous(limits = c(-1,2), expand = c(0,0)) +
#   scale_x_continuous(limits = c(0, 1), expand = c(0,0)) +
#   coord_cartesian(clip = "off") +
#   theme_void() +
#   theme(plot.margin = margin(t = 0, l = -1000, r = 0, b = 0))
# title
# 
# caption <- qplot(x = c(0:10), y = c(0:10), geom = "blank") +
#   geom_text(aes(x = 9, y = 1), 
#             label = "Graphic by Spencer Schien (@MrPecners) | Data from USGS",
#             family = "n", color = "grey70", hjust = 1) +
#   coord_cartesian(clip = "off") +
#   scale_x_continuous(limits = c(0, 10), expand = c(0,0)) +
#   scale_y_continuous(limits = c(0, 10), expand = c(0,0)) +
#   theme_void() +
#   theme(plot.margin = margin(0,0,10,0),
#         aspect.ratio = .1)
# 
# 
# layout <- c(
#   area(1,1), area(1,2),
#   area(2,1), area(2,2)
# )
# 
# wrap_plots(
#   plots[[4]], plots[[1]],
#   plots[[2]], plots[[3]],
#   design = layout
# )
# 
# ggsave("plots/4th_reading_race_students_comps.png", 
#        bg = "white", width = 14, height = 10)

# annot <- glue("Milwaukee is at or near the bottom when it comes to ",
#               "student proficiency, no matter which student subgroup ",
#               "you consider.") |> 
#   str_wrap(80)
# 
# system(
#   glue("convert plots/4th_reading_race_students_comps.png ",
#        "-background White -splice 0x50 ",
#        "-pointsize 75 -font Verdana ",
#        "-fill {'grey50'} label:'{annot}' ",
#        "+swap -gravity northwest ",
#        "-append plots/4th_reading_race_students_comps_.png")
# )
# 
# system(
#   glue("convert plots/4th_reading_race_students_comps_.png ",
#        "-background White -splice 0x30 -background White ", 
#        "-pointsize 125 -font Georgia-Bold ",
#        "label:'Milwaukee is failing its students' ",
#        "+swap -gravity northwest ",
#        "-append ",
#        "plots/4th_reading_race_students_comps_titled_.png")
# )
# 
# this <- glue("We are only showing 4th grade reading here, but Milwaukee ",
#             "was either at or among the bottom scoring districts on all tests. ",
#             "Data from 2024 NAEP TUDA results. Some cities did not have all student ",
#             "subgroups in their testing sample.") |> 
#   str_wrap(125)
# 
# system(
#   glue("convert plots/4th_reading_race_students_comps_titled_.png ",
#        "-pointsize 50 -font Verdana -interline-spacing 10 ",
#        "-fill {'grey70'} label:'{this}' ",
#        "-gravity northwest ",
#        "-append  -bordercolor White -border 30x30 ", 
#        "plots/4th_reading_race_students_comps_titled.png")
# )


annot <- glue("Compared to other urban districts across the country, ",
              "Milwaukee is worst in the nation at educating its Black ",
              "students.") |> 
  str_wrap(80)

system(
  glue("convert plots/black_students_comps.png ",
       "-background White -splice 0x50 ",
       "-pointsize 75 -font Verdana ",
       "-fill {'grey50'} label:'{annot}' ",
       "+swap -gravity northwest ",
       "-append plots/black_students_comps_.png")
)

system(
  glue("convert plots/black_students_comps_.png ",
       "-background White -splice 0x30 -background White ", 
       "-pointsize 125 -font Georgia-Bold ",
       "label:'Milwaukee is failing its Black students' ",
       "+swap -gravity northwest ",
       "-append ",
       "plots/black_students_comps_titled_.png")
)

this <- glue("Data from 2024 NAEP TUDA results.") 

system(
  glue("convert plots/black_students_comps_titled_.png ",
       "-pointsize 50 -font Verdana -interline-spacing 10 ",
       "-fill {'grey70'} label:'{this}' ",
       "-gravity northwest ",
       "-append  -bordercolor White -border 30x30 ", 
       "plots/black_students_comps_titled.png")
)
