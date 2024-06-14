library(ggplot2)
library(ggridges)
library(GGally)

# Як різниця в золоті/різниця в досвіді/кількість вбивств/смертей впливають на результат гри?====
ggplot(data_by_games, aes(x = goldDiff, fill = hasWon)) +
  geom_histogram(position = "dodge") +
  labs(x = 'Різниця в золоті', y = 'Частота', title = 'Розподіли різниці в золоті в залежності від результату гри', fill = 'Виграна гра?')

ggplot(data_by_games, aes(x = expDiff, fill = hasWon)) +
  geom_histogram(position = "dodge") +
  labs(x = 'Різниця в досвіді', y = 'Частота', title = 'Розподіли різниці в досвіді в залежності від результату гри', fill = 'Виграна гра?')

ggplot(data_by_games, aes(x = kills, y = deaths, colour = hasWon)) +
  geom_point(alpha = 1/10) +
  labs(x = 'Кількість вбивств', y = 'Кількість смертей', title = 'Виграним матчам відповідають більші значення вбивств та менші смертей', fill = 'Виграна гра?')


# Який тип дракону найбільше впливає на результат гри?====

dragon_kills <- data_by_games[c(2, 6:9)]
fire_dragon <- (dragon_kills[dragon_kills$hasWon == 1, ] %>% group_by(killedFireDrake) %>% count()) /
  (dragon_kills %>% group_by(killedFireDrake) %>% count())
water_dragon <- (dragon_kills[dragon_kills$hasWon == 1, ] %>% group_by(killedWaterDrake) %>% count()) /
  (dragon_kills %>% group_by(killedWaterDrake) %>% count())
air_dragon <- (dragon_kills[dragon_kills$hasWon == 1, ] %>% group_by(killedAirDrake) %>% count()) /
  (dragon_kills %>% group_by(killedAirDrake) %>% count())
earth_dragon <- (dragon_kills[dragon_kills$hasWon == 1, ] %>% group_by(killedEarthDrake) %>% count()) /
  (dragon_kills %>% group_by(killedEarthDrake) %>% count())
dragon_winrate <- data.frame(cbind(c(0, 1, 2, 3, 4), fire_dragon$n, water_dragon$n, air_dragon$n, earth_dragon$n))

ggplot(dragon_winrate) +
  geom_line(aes(x = X1, y = X2), colour = 'red', linewidth = 1) +
  geom_line(aes(x = X1, y = X3), colour = 'blue', linewidth = 1) +
  geom_line(aes(x = X1, y = X4), colour = 'yellow', linewidth = 1) +
  geom_line(aes(x = X1, y = X5), colour = 'green', linewidth = 1) +
  labs(x = 'Кількість вбитих драконів', y = 'Вінрейт', title = 'Залежності вінрейту від кількості й типу дракона') +
  annotate("text", x = 4, y = 1.02, label = 'Earth', colour = 'green') + 
  annotate("text", x = 4, y = 0.87, label = 'Air', colour = 'yellow') + 
  annotate("text", x = 4, y = 0.79, label = 'Fire', colour = 'red') + 
  annotate("text", x = 4, y = 0.71, label = 'Water', colour = 'blue')

# Руйнування якого інгібітору найбільше впливає на ймовірність перемоги? ====

inh_kills <- data_by_games[c(2, 13:15)]
top_inh <- (inh_kills[inh_kills$hasWon == 1, ] %>% group_by(destroyedTopInhibitor) %>% count()) /
  (inh_kills %>% group_by(destroyedTopInhibitor) %>% count())
mid_inh <- (inh_kills[inh_kills$hasWon == 1, ] %>% group_by(destroyedMidInhibitor) %>% count()) /
  (inh_kills %>% group_by(destroyedMidInhibitor) %>% count())
bot_inh <- (inh_kills[inh_kills$hasWon == 1, ] %>% group_by(destroyedBotInhibitor) %>% count()) /
  (inh_kills %>% group_by(destroyedBotInhibitor) %>% count())
inh_winrate <- data.frame(cbind(c(0, 1, 2, 3), top_inh$n, mid_inh[]$n, bot_inh[c(1:4),]$n))

ggplot(inh_winrate) +
  geom_line(aes(x = X1, y = X2), colour = 'red', linewidth = 1) +
  geom_line(aes(x = X1, y = X3), colour = 'blue', linewidth = 1) +
  geom_line(aes(x = X1, y = X4), colour = 'yellow', linewidth = 1) +
  annotate("text", x = 3, y = 0.82, label = 'Top', colour = 'yellow') + 
  annotate("text", x = 3, y = 0.54, label = 'Bot', colour = 'red') + 
  annotate("text", x = 3, y = 0.73, label = 'Mid', colour = 'blue') +
  labs(x = 'Кількість заламаних інгібіторів', y = 'Частка', title = 'Частка виграних ігор в залежності від кількості зламаних інгібіторів.')

# Яким чином поточна тривалість гри впливає на зміну різниці в золоті?====

ggplot(data_by_frames, aes(x = goldDiff, y = as.factor(frame))) +
  geom_density_ridges_gradient(scale = 10, fill = "gold") +
  labs(x = 'Різниця в золоті', y = 'Поточна тривалість гри, хв', title = 'Збільшення відхилення різниці золота при\n більшій тривалості гри')

# Чи існує зв'язок (і який?) між кількістю встановлених вардів/вбиствами====

ggplot(data_by_frames, aes(x = wardsPlaced, y = kills)) +
  geom_point(alpha = 0.03) +
  scale_x_continuous(trans = "log10") +
  labs(x = 'Кількість встановлених вардів', y = 'Кількість вбивств', title = 'Більшій кількості встановених вардів відповідає\n більша кількість вбивств')

ggplot(data_by_games, aes(x = kd, y = gameDuration, colour = hasWon)) +
  geom_point(alpha = 0.05) +
  scale_x_continuous(trans = "log10") +
  labs(x = 'Кд', y = 'Тривалість гра, хв', title = 'Ближчому до 1 кд відповідають довші ігри')


# Як кількість зламаних башт впливає на різницю у золоті?====

ggplot(data_by_frames, aes(x = goldDiff, y = as.factor(towers))) +
  geom_density_ridges_gradient(fill = "gold") +
  labs(x = 'Різниця в золоті', y = 'Кількість зламаних башт', title = 'Збільшення середньої різниці золота при\n більшій кількості зламаних башт')

# Чи існує взаємозв'язок між вбивством Барона та руйнуванням башт?====

ggplot(data_by_frames, aes(x = towers, y = as.factor(killedBaronNashor))) +
  geom_boxplot(fill = 'purple') +
  labs(x = 'Кількість зламаних башт', y = 'Кількість вбитих Баронів', title = 'Збільшення середньої кількості зламаних башт при\n більшій кількості вбитих Баронів*')

# Як вбивство "ключових об'єктів" впливають на остаточний результат гри?====

ggplot(data_by_games, aes(x = killedBaronNashor, fill = hasWon)) +
  geom_bar(position = 'dodge') +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Кількість вбитих Баронів', y = 'Кількість ігор', title = 'Більша кількість перемог при більшій\n кількості вбитих Баронів', fill = 'Виграна гра?')

ggplot(data_by_games, aes(x = killedElderDrake, fill = hasWon)) +
  geom_bar(position = 'dodge') +
  scale_y_continuous(trans = "log10") +
  labs(x = 'Кількість вбитих старших драконів', y = 'Кількість ігор', title = 'Більша кількість перемог при більшій\n кількості вбитих старших драконів', fill = 'Виграна гра?')

ggplot(data_by_games, aes(x = killedRiftHerald, fill = hasWon)) +
  geom_bar(position = 'dodge') +
  labs(x = 'Кількість вбитих Геральдів', y = 'Кількість ігор', title = 'Більша кількість перемог при більшій\n кількості вбитих Геральдів', fill = 'Виграна гра?')

# Корелляційні коефіцієнти====

ggcorr(rev(data_by_games) %>% select(where(is.numeric)), label = TRUE, hjust = 0.9, size = 3)
ggcorr(rev(data_by_games) %>% select(where(is.numeric)), label = TRUE, hjust = 0.9, size = 3, method = c("pairwise", "spearman"))

ggcorr(data_by_frames %>% select(where(is.numeric)), label = TRUE, hjust = 0.9, size = 4)
ggcorr(data_by_frames %>% select(where(is.numeric)), label = TRUE, hjust = 0.9, size = 4, method = c("pairwise", "spearman"))
