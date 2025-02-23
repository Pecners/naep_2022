subjects <- c("Math", "Reading")
grades <- c(4, 8)
years <- c(2009, 2019, 2022, 2024)
groups <- c("Black", "Hispanic", "White")

these <- expand_grid(subjects,
            grades,
            years,
            groups)

write_csv(these, "data/starting_grid.csv")
