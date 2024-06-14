library(tidyr)
library(dplyr)
library(stats)

data = read.csv('data/lol_ranked_games.csv')

summary(data)

missing_values <- sapply(data, function(x) sum(is.na(x)))
print(missing_values)

data <- data %>%
  mutate(across(where(is.character), ~na_if(.x, "NA"))) %>%
  mutate(across(where(is.character), ~na_if(.x, "na")))

data <- data %>%
  mutate(across(where(is.numeric), ~replace(.x, .x %in% c("NA", "na"), NA)))

adjusted_missing_values <- sapply(data, function(x) sum(is.na(x)))
print(adjusted_missing_values)

data_by_games = data[c(1:3, 5, 6, 9, 10:14, 20, 22, 24:26, 54, 55)]
data_by_games = data_by_games %>% group_by(gameId) %>% slice_tail(n = 1)
data_by_games = data_by_games[-c(1)]
data_by_games <- data_by_games %>% mutate(gameDuration = gameDuration/60000)
data_by_games <- data_by_games %>% mutate(hasWon = as.factor(hasWon),
                                          isFirstBlood = as.factor(isFirstBlood))

data_by_frames = data[c(4, 5, 20, 54, 57)]
data_by_games$kd = ifelse(data_by_games$deaths == 0, data_by_games$kills, data_by_games$kills / data_by_games$deaths)
data_by_frames$towers = apply(data[c(30:32, 36:38, 42:44, 48:50)], 1, sum)

dim(data_by_frames)
dim (data_by_games)


check_negative_values <- function(data) {
  for (column_name in colnames(data)) {
    if (is.numeric(data[[column_name]])) {
      negative_count <- sum(data[[column_name]] < 0, na.rm = TRUE)
      print(paste("Кількість від'ємних значень у стовпці", column_name, ":", negative_count))
    }
  }
}

check_factor_values <- function(data) {
  for (column_name in colnames(data)) {
    if (is.factor(data[[column_name]])) {
      invalid_count <- sum(!data[[column_name]] %in% c(0, 1))
      
      print(paste("Кількість значень, відмінних від 0 та 1 у стовпці", column_name, ":", invalid_count))
    } 
  }
}


check_negative_values(data_by_frames)
check_negative_values(data_by_games)
check_factor_values(data_by_games)



