math_8th <- read_csv("data/8th_grade_math_scale_scores_juris.csv", 
                 name_repair = janitor::make_clean_names)

math_4th <- read_csv("data/4th_grade_math_scale_scores_juris.csv", 
                     name_repair = janitor::make_clean_names)

reading_8th <- read_csv("data/8th_grade_reading_scale_scores_juris.csv", 
                        name_repair = janitor::make_clean_names)

reading_4th <- read_csv("data/4th_grade_reading_scale_scores_juris.csv", 
                        name_repair = janitor::make_clean_names)


clean <- function(data) {
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
  return(data)
  
}

math_8th <- clean(math_8th)
math_4th <- clean(math_4th)
reading_8th <- clean(reading_8th)
reading_4th <- clean(reading_4th)

