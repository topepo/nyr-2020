# Day 3 scratch

# Hands-On: K-NN Grids: 

knn_set <- 
  parameters(neighbors(), weight_func(), dist_power())

updated_set <- 
  knn_set %>% 
  update(neighbors = neighbors(c(1, 50)))

set.seed(8209)
updated_set %>% 
  grid_random(size = 100) %>% 
  ggplot(aes(neighbors, dist_power, color = weight_func)) +
  geom_point(alpha = 0.5)


set.seed(8209)
updated_set %>% 
  grid_max_entropy(size = 100, iter = 20, variogram_range = .1) %>% 
  ggplot(aes(neighbors, dist_power, color = weight_func)) +
  geom_point(alpha = 0.5) + 
  labs(title = "variogram_range = .1")

## -----------------------------------------------------------------------------

# Hands-On: Explore the Data

data("Chicago")

library(lubridate)

Chicago %>% mutate(week_day = wday(date, label = TRUE)) %>%
  ggplot(aes(x = date, y = ridership, colour = week_day)) +
  geom_point() +
  ggthemes::theme_clean()

Chicago %>% ggplot(., aes(x = ridership)) + geom_histogram()

Chicago %>% ggplot(., aes(x = ridership)) + geom_histogram() + scale_x_log10()


Chicago %>%
  mutate(weekday = weekdays(date)) %>%
  group_by(weekday) %>% summarise(MeanRides = mean(ridership))

Chicago %>% ggplot(aes(x = Clark_Lake, y = ridership)) + 
  geom_point(alpha = .3)

Chicago[, 2:21] %>% 
  cor() %>% 
  corrplot::corrplot(order = "hclust")


## -----------------------------------------------------------------------------

# slide 17 of of part 5:

tidy(chi_folds ) %>% 
  ggplot(aes(x = Row, y = Resample, fill = Data)) + 
  geom_tile() + 
  xlim(c(5000, 5700)) + 
  ylab("") + 
  xlab("Day Number (starts at one)")





