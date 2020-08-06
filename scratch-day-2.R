# Day 2 scratch

library(tidymodels)
data(ames, package = "modeldata")

theme_set(theme_bw())

## -----------------------------------------------------------------------------

recipe(x = ames) %>% 
  update_role(Sale_Price, new_role = "outcome") %>% 
  update_role(-Sale_Price, new_role = "predictor")
  

## -----------------------------------------------------------------------------

# hands-on answer:

mod_rec <- 
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood, 
    data = ames_train
  ) %>%
  
  step_log(Sale_Price, base = 10) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal()) %>% 
  
  step_zv(starts_with("Neighborhood_"))

# or use `step_nzv()`

## -----------------------------------------------------------------------------

# alternate contrasts for dummy variables.
mod_rec <- 
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood, 
    data = ames_train
  ) %>%
  
  step_log(Sale_Price, base = 10) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal(), one_hot = TRUE) 

# ?step_dummy: "To change the type of contrast being used, change the global 
# contrast option via options.

?contr.sum

?embed::step_feature_hash

## -----------------------------------------------------------------------------

ames_rec <- recipe(
  Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
    Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
    Central_Air + Longitude + Latitude,
  data = ames_train
) %>%
  step_BoxCox(Lot_Area, Gr_Liv_Area) %>%
  step_other(Neighborhood, threshold = 0.05)  %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built) %>%
  step_ns(Longitude, Latitude, deg_free = 5)

ames_rec %>% 
  prep() %>% 
  tidy(number = 1)
  
# or step_mutate(Lot_Area = 1/sqrt(Lot_Area))

# step_YeoJohnson() 
