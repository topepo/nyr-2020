# Day 1 scratch

library(tidymodels)
data(ames, package = "modeldata")

theme_set(theme_bw())

## -----------------------------------------------------------------------------

# first hands-on code (some of these suggested by participants):

# Gap in houses during WW2
ggplot(ames, aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = .3)

ames %>% 
  count(Year_Built) %>% 
  dplyr::filter(Year_Built > 1940 & Year_Built < 1950 )

# skewed distributions
ames %>% 
  ggplot(aes(x = Gr_Liv_Area)) + 
  geom_histogram()

ames %>% 
  ggplot(aes(x = Sale_Price)) + 
  geom_histogram()

ames %>% 
  ggplot(aes(x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(alpha = .3)

# This looks better then un-logged
ames %>% 
  ggplot(aes(x = Gr_Liv_Area, y = Sale_Price)) + 
  geom_point(alpha = .3) + 
  scale_x_log10() + 
  scale_y_log10()

# unbalanced predictor frequencies
ames %>% 
  ggplot(aes(x=MS_SubClass)) + 
  geom_bar() + 
  coord_flip()

# quadratic patterns?...
ames %>% 
  ggplot(aes(x = Year_Built, y = Gr_Liv_Area)) + 
  geom_point(alpha = .3) + 
  scale_y_log10() + 
  geom_smooth(se = FALSE)

# other other factors (spoiler alter for part 4)
ames %>% 
  ggplot(aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = .3) + 
  scale_y_log10() + 
  geom_smooth(se = FALSE)

# Reduced building in the 80's ¯\_(ツ)_/¯
ames %>% 
  filter(Year_Built >= 1950) %>% 
  mutate(decade_built = Year_Built - (Year_Built %% 10)) %>%
  group_by(decade_built) %>% 
  count()

## -----------------------------------------------------------------------------
