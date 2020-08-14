# Day 4 scratch

# We can't do this yet but will be able to do: 

nb_wflow <- 
  workflow() %>% 
  add_model(nb_mod) %>% 
  add_recipe(nb_recipe) %>% 
  add_prob_threshold(threshold = tune())
