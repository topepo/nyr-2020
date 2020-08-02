## -----------------------------------------------------------------------------
## Code for "Machine Learning" by Max Kuhn for 2020 NYR Conference

library(tidymodels)

thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)

## -----------------------------------------------------------------------------
## Part 1: Introduction

## -----------------------------------------------------------------------------
## Slide 9

## data(penguins, package = "modeldata")
## contains("bill_")
## 
## # instead of
## 
## c("bill_length_mm", "bill_depth_mm")


## merged <- inner_join(a, b)
## 
## # is equal to
## 
## merged <- a %>%
##   inner_join(b)


## -----------------------------------------------------------------------------
## Slice 10

library(tidyverse)

ames_prices <- "http://bit.ly/2whgsQM" %>%
  read_delim(delim = "\t", guess_max = 2000) %>%
  rename_at(vars(contains(' ')), list(~gsub(' ', '_', .))) %>%
  dplyr::rename(Sale_Price = SalePrice) %>%
  dplyr::filter(!is.na(Electrical)) %>%
  dplyr::select(-Order, -PID, -Garage_Yr_Blt)

ames_prices %>%
  group_by(Alley) %>%
  summarize(
    mean_price = mean(Sale_Price / 1000),
    n = sum(!is.na(Sale_Price))
  )


## -----------------------------------------------------------------------------
## Slice 11

# purrr loaded with tidyverse or tidymodels package

mini_ames <- ames_prices %>%
  dplyr::select(Alley, Sale_Price, Yr_Sold) %>%
  dplyr::filter(!is.na(Alley))

head(mini_ames, n = 5)


by_alley <- split(mini_ames, mini_ames$Alley)

# map(.x, .f, ...)
map(by_alley, head, n = 2)

## -----------------------------------------------------------------------------
## Slice 12

map(by_alley, nrow)

map_int(by_alley, nrow)

map(
  by_alley, 
  ~summarise(.x, max_price = max(Sale_Price))
)


## -----------------------------------------------------------------------------
## Slice 13

ames_lst_col <- nest(mini_ames, data = -Alley)
ames_lst_col

ames_lst_col %>%
  mutate(
    n_row = map_int(data, nrow),
    max   = map_dbl(data, ~ max(.x$Sale_Price))
  )


## -----------------------------------------------------------------------------
## Slice 14 - Quick Data Investigation

library(tidymodels)
data(ames, package = "modeldata")

## -----------------------------------------------------------------------------
## Part 2: Data Usage

## -----------------------------------------------------------------------------
## Slide 5

data(ames, package = "modeldata")

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

nrow(ames_train)/nrow(ames)

## -----------------------------------------------------------------------------
## Slide 6

data_split

training(data_split)

## -----------------------------------------------------------------------------
## Slide 11

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

simple_lm_values <- augment(simple_lm)
names(simple_lm_values)

## -----------------------------------------------------------------------------
## Slide 12

head(simple_lm_values, n = 2)

tidy(simple_lm)

# But don't trust this too much!
glance(simple_lm)[1:3]

## -----------------------------------------------------------------------------
## Slide 14

spec_lin_reg <- linear_reg()
spec_lin_reg

lm_mod <- set_engine(spec_lin_reg, "lm")
lm_mod

lm_fit <- fit(
  lm_mod,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

lm_fit

## -----------------------------------------------------------------------------
## Slide 15

ames_train_log <- ames_train %>%
  mutate(Sale_Price_Log = log10(Sale_Price))

fit_xy(
  lm_mod,
  y = dplyr::pull(ames_train_log, Sale_Price_Log),
  x = dplyr::select(ames_train_log, Latitude, Longitude)
)

## -----------------------------------------------------------------------------
## Slide 16

spec_stan <- 
  spec_lin_reg %>%
  # Engine specific arguments are passed through here
  set_engine("stan", chains = 4, iter = 1000)

# Otherwise, looks exactly the same!
fit_stan <- fit(
  spec_stan,
  log10(Sale_Price) ~ Longitude + Latitude,
  data = ames_train
)

coef(fit_stan$fit)

coef(lm_fit$fit)

## -----------------------------------------------------------------------------
## Slide 17

fit_knn <- 
  nearest_neighbor(mode = "regression", neighbors = 5) %>%
  set_engine("kknn") %>% 
  fit(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)

fit_knn

## -----------------------------------------------------------------------------
## Slide 21

# Numeric predictions always in a df
# with column `.pred`
test_pred <- 
  lm_fit %>%
  predict(ames_test) %>%
  bind_cols(ames_test) %>%
  mutate(log_price = log10(Sale_Price))

test_pred %>% 
  dplyr::select(log_price, .pred) %>% 
  slice(1:3)

## -----------------------------------------------------------------------------
## Slide 22

# yardstick loaded by tidymodels

perf_metrics <- metric_set(rmse, rsq, ccc)

# A tidy result back:
test_pred  %>% 
  perf_metrics(truth = log_price, estimate = .pred)


## -----------------------------------------------------------------------------
## Part 3: Feature Engineering

## -----------------------------------------------------------------------------
## Slide 3

library(tidymodels)
data(ames, package = "modeldata")

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

lm_mod <- linear_reg() %>% 
  set_engine("lm")

perf_metrics <- metric_set(rmse, rsq, ccc)

## -----------------------------------------------------------------------------
## Slide 13

mod_rec <- recipe(Sale_Price ~ Longitude + Latitude, ames_train) %>%
  step_log(Sale_Price, base = 10)

## -----------------------------------------------------------------------------
## Slide 14

mod_rec <- 
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood, 
    data = ames_train
  ) %>%
  
  step_log(Sale_Price, base = 10) %>%
  
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

mod_rec

## -----------------------------------------------------------------------------
## Slide 16

mod_rec_trained <- prep(mod_rec, training = ames_train, verbose = TRUE)

## -----------------------------------------------------------------------------
## Slide 17

mod_rec_trained 

## -----------------------------------------------------------------------------
## Slide 18

# Extracts processed version of `ames_train`
juice(mod_rec_trained)

## -----------------------------------------------------------------------------
## Slide 19

bake(mod_rec_trained, new_data = ames_test)

## -----------------------------------------------------------------------------
## Hands-On: Zero-Variance Filter

# Code needed: 

library(tidymodels)
data(ames, package = "modeldata")

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

lm_mod <- linear_reg() %>% 
  set_engine("lm")

perf_metrics <- metric_set(rmse, rsq, ccc)

# * Instead of using step_other(), take 10 minutes and research how to eliminate 
#   any zero-variance predictors using the recipe reference site.
# * Re-run the recipe with this step.
# 
# * What were the results?
#
# * Do you prefer either of these approaches to the other?

# Modify this recipe: 

mod_rec <- 
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood, 
    data = ames_train
  ) %>%
  
  step_log(Sale_Price, base = 10) %>%
  
  # Lump factor levels that occur in 
  # <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  
  # Create dummy variables for _any_ factor variables
  step_dummy(all_nominal())

## -----------------------------------------------------------------------------
## Slide 24

price_breaks <- (1:6)*(10^5)

ames_train %>%
  ggplot(aes(x = Year_Built, y = Sale_Price)) + 
  geom_point(alpha = 0.4) +
  scale_y_log10() + 
  geom_smooth(method = "loess")

## -----------------------------------------------------------------------------
## Slide 25

ames_train %>%
  group_by(Central_Air) %>%
  count() %>%
  ungroup() %>% 
  mutate(percent = n / sum(n) * 100)

ames_train %>%
 ggplot(aes(x = Year_Built, y = Sale_Price)) +
 geom_point(alpha = 0.4) +
 scale_y_log10() +
 facet_wrap(~ Central_Air, nrow = 2) +
 geom_smooth(method = "lm")

## -----------------------------------------------------------------------------
## Slide 26

mod1 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air,                          data = ames_train)
mod2 <- lm(log10(Sale_Price) ~ Year_Built + Central_Air + Year_Built:Central_Air, data = ames_train)
anova(mod1, mod2)

## -----------------------------------------------------------------------------
## Slide 27

interact_rec <- recipe(Sale_Price ~ Year_Built + Central_Air, data = ames_train) %>%
  step_log(Sale_Price) %>%
  step_dummy(Central_Air) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built)

interact_rec %>%
  prep(training = ames_train) %>%
  juice() %>%
  # select a few rows with different values
  slice(153:157)

## -----------------------------------------------------------------------------
## Slide 33

data(ames, package = "modeldata")

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")

ames_train <- training(data_split)
ames_test  <- testing(data_split)

mod_rec <- 
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood + Year_Built + Central_Air,
    data = ames_train
  ) %>%
  step_other(Neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  step_interact(~ starts_with("Central_Air"):Year_Built)

## -----------------------------------------------------------------------------
## Slide 47

ggplot(ames_train, 
       aes(x = Longitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::bs(x, 5), 
    se = FALSE
  ) + 
  scale_y_log10()

## -----------------------------------------------------------------------------
## Slide 48

ggplot(ames_train, 
       aes(x = Latitude, y = Sale_Price)) + 
  geom_point(alpha = .5) + 
  geom_smooth(
    method = "lm", 
    formula = y ~ splines::ns(x, df = 5), 
    se = FALSE
  ) + 
  scale_y_log10()


## -----------------------------------------------------------------------------
## Slide 49

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

## -----------------------------------------------------------------------------
## Slide 50

ames_rec <- prep(ames_rec)

lm_fit <- 
  lm_mod %>% 
  fit(Sale_Price ~ ., data = juice(ames_rec))   # Sale_Price is already on the log scale

glance(lm_fit$fit)

ames_test_processed <- bake(ames_rec, ames_test, all_predictors())

## -----------------------------------------------------------------------------
## Slide 53

ames_wfl <- workflow() %>% 
  add_recipe(ames_rec) %>% 
  add_model(lm_mod)

ames_wfl

## -----------------------------------------------------------------------------
## Slide 54

ames_wfl_fit <- fit(ames_wfl, ames_train)
predict(ames_wfl_fit, ames_test) %>% slice(1:5)

## -----------------------------------------------------------------------------
## Part 4: Resampling and Grid Search

## -----------------------------------------------------------------------------
## Slide 3

library(tidymodels)
library(modeldata)

data(ames, package = "modeldata")

ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(4595)

data_split <- initial_split(ames, strata = "Sale_Price")
ames_train <- training(data_split)
ames_test  <- testing(data_split)

perf_metrics <- metric_set(rmse, rsq, ccc)

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
  step_bs(Longitude, Latitude, deg_free = 5)

## -----------------------------------------------------------------------------
## Slide 10

set.seed(2453)

cv_splits <- vfold_cv(ames_train) #10-fold is default

cv_splits

## -----------------------------------------------------------------------------
## Slide 11

cv_splits$splits[[1]]

cv_splits$splits[[1]] %>% 
  analysis() %>%
  dim()

cv_splits$splits[[1]] %>% 
  assessment() %>%
  dim()

## -----------------------------------------------------------------------------
## Slide 13

knn_mod <- 
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

knn_wfl <- 
  workflow() %>% 
  add_model(knn_mod) %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)

fit(knn_wfl, data = ames_train)

## -----------------------------------------------------------------------------
## Slide 15

knn_res <- cv_splits %>%
  mutate(
    workflows = map(
      splits, 
      ~ fit(knn_wfl, data = analysis(.x))
    )
  ) 

knn_res


## -----------------------------------------------------------------------------
## Slides 16-22

knn_pred <- map2_dfr(
  knn_res$workflows, 
  knn_res$splits,     
  ~ predict(.x, assessment(.y)),         
  .id = "fold"
)                                   
prices <- map_dfr(
  knn_res$splits,  
  ~ assessment(.x) %>% select(Sale_Price)
)
rmse_estimates <- knn_pred %>%
  bind_cols(prices) %>% 
  group_by(fold) %>% 
  do(rmse = rmse(., Sale_Price, .pred)) %>% 
  tidyr::unnest(cols = c(rmse)) 
mean(rmse_estimates$.estimate)

## -----------------------------------------------------------------------------
## Slide 28

library(tune)
easy_eval <-
  fit_resamples(knn_wfl,
                resamples = cv_splits,
                control = control_resamples(save_pred = TRUE))
easy_eval

## -----------------------------------------------------------------------------
## Slide 29

collect_predictions(easy_eval) %>% 
  arrange(.row) %>% 
  slice(1:5) 

collect_metrics(easy_eval)

collect_metrics(easy_eval, summarize = FALSE) %>% 
  slice(1:10)

## -----------------------------------------------------------------------------
## Slide 37

penalty()
mixture()

glmn_param <- parameters(penalty(), mixture())

glmn_param

glmn_grid <- 
  grid_regular(glmn_param, levels = c(penalty = 10, mixture = 5))
glmn_grid %>% slice(1:4)

## -----------------------------------------------------------------------------
## Slide 38

set.seed(7454)

glmn_sfd <- grid_max_entropy(glmn_param, size = 50)

glmn_sfd %>% slice(1:4)

# grid_latin_hypercube() can also be used
# grid_random() too

## -----------------------------------------------------------------------------
## Slide 39

glmn_set <- parameters(lambda = penalty(), mixture())

# The ranges can also be set by their name:
glmn_set <- 
  update(glmn_set, lambda = penalty(c(-5, -1)))

# Some parameters depend on data dimensions:
mtry()
rf_set <- parameters(mtry(), trees())

rf_set

# Sets the range of mtry to
# be the number of predictors
finalize(rf_set, mtcars %>% dplyr::select(-mpg))

## -----------------------------------------------------------------------------
## Slide 40 Hands-On: K-NN Grids

# * Look at the help file ?nearest_neighbors and find the names of the three tuning parameters.
# 
# * Create a parameter set for these three, make at least one grid, and plot them.

## -----------------------------------------------------------------------------
## Slide 42

library(tune)

knn_mod <- 
  nearest_neighbor(neighbors = tune(), weight_func = tune()) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

parameters(knn_mod)


## -----------------------------------------------------------------------------
## Slide 43

nearest_neighbor(neighbors = tune("K"), weight_func = tune("weights")) %>% 
  set_engine("kknn") %>% 
  set_mode("regression") %>% 
  parameters()

## -----------------------------------------------------------------------------
## Slide 44

set.seed(522)

knn_grid <- knn_mod %>% 
  parameters() %>% 
  grid_regular(levels = c(penalty = 15, mixture = 5))

ctrl <- control_grid(verbose = TRUE)

knn_tune <- 
  knn_mod %>% 
  tune_grid(
    ames_rec,
    resamples = cv_splits, 
    grid = knn_grid, 
    control = ctrl
  )

## -----------------------------------------------------------------------------
## Slide 46

knn_tune

knn_tune$.metrics[[1]]

## -----------------------------------------------------------------------------
## Slide 47

show_best(knn_tune, metric = "rmse", n = 3)

best_res <- select_best(knn_tune, metric = "rmse")
final_knn_mod <- finalize_model(knn_mod, best_res)
final_knn_mod


## -----------------------------------------------------------------------------
## Part 5: Regression Models

## -----------------------------------------------------------------------------
## Slide 6 Hands-On: Explore the Data

library(tidymodels)
data("Chicago")

## -----------------------------------------------------------------------------
## Slide 9 - 15

library(stringr)
# define a few holidays
us_hol <- 
  timeDate::listHolidays() %>% 
  str_subset("(^US)|(Easter)")
chi_rec <-
  recipe(ridership ~ ., data = Chicago) %>%
  step_holiday(date, holidays = us_hol) %>%
  step_date(date) %>%
  step_rm(date) %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())
# step_normalize(one_of(!!stations))
# step_pca(one_of(!!stations), num_comp = tune())

## -----------------------------------------------------------------------------
## Slide 16

chi_folds <- rolling_origin(
  Chicago, 
  initial = 364 * 15, 
  assess = 7 * 4, 
  skip = 7 * 4, 
  cumulative = FALSE
)

chi_folds %>% nrow()

## -----------------------------------------------------------------------------
## Slide 20

lm(ridership ~ . - date, data = Chicago)


## -----------------------------------------------------------------------------
## Slide 27

glmn_grid <- expand.grid(
  penalty = 10 ^ seq(-3, -1, length = 20), 
  mixture = c(0.05, 0.25, 0.50, 0.75, 1.00)
)

## -----------------------------------------------------------------------------
## Slide 28

# We need to normalize the predictors:
glmn_rec <- chi_rec %>% 
  step_normalize(all_predictors())

glmn_mod <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

# Save the assessment set predictions
ctrl <- control_grid(save_pred = TRUE)

glmn_tune <- 
  glmn_mod %>% 
  tune_grid(
    glmn_rec,
    resamples = chi_folds,
    grid = glmn_grid,
    control = ctrl
  )

## -----------------------------------------------------------------------------
## Slide 30

library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)
# run `tune_grid()`...
stopCluster(cl)


## -----------------------------------------------------------------------------
## Slide 31

rmse_vals <-
  collect_metrics(glmn_tune) %>%
  filter(.metric == "rmse")

autoplot(glmn_tune)

## -----------------------------------------------------------------------------
## Slide 32

show_best(glmn_tune, metric = "rmse")

best_glmn <-
  select_best(glmn_tune, metric = "rmse")
best_glmn

## -----------------------------------------------------------------------------
## Slide 33

glmn_pred <- collect_predictions(glmn_tune)
glmn_pred


## -----------------------------------------------------------------------------
## Slide 34

glmn_pred <-
  glmn_pred %>%
  inner_join(best_glmn, by = c("penalty", "mixture"))

ggplot(glmn_pred, aes(x = .pred, y = ridership)) +
  geom_abline(col = "green") +
  geom_point(alpha = .3) +
  coord_obs_pred()

## -----------------------------------------------------------------------------
## Slide 35

large_resid <- 
  glmn_pred %>% 
  mutate(resid = ridership - .pred) %>% 
  arrange(desc(abs(resid))) %>% 
  slice(1:4)

library(lubridate)
Chicago %>% 
  slice(large_resid$.row) %>% 
  select(date) %>% 
  mutate(day = wday(date, label = TRUE)) %>% 
  bind_cols(large_resid)

## -----------------------------------------------------------------------------
## Slide 37

glmn_rec_final <- prep(glmn_rec)

glmn_mod_final <- finalize_model(glmn_mod, best_glmn)

glmn_mod_final

glmn_fit <- glmn_mod_final %>% 
  fit(ridership ~ ., data = juice(glmn_rec_final))

glmn_fit

## -----------------------------------------------------------------------------
## Slide 38

library(glmnet)
plot(glmn_fit$fit, xvar = "lambda")


## -----------------------------------------------------------------------------
## Slide 39-40

# Get the set of coefficients across penalty values
tidy_coefs <-
  broom::tidy(glmn_fit) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::select(-step, -dev.ratio)

# Get the lambda closest to tune's optimal choice
delta <- abs(tidy_coefs$lambda - best_glmn$penalty)
lambda_opt <- tidy_coefs$lambda[which.min(delta)]

# Keep the large values
label_coefs <-
  tidy_coefs %>%
  mutate(abs_estimate = abs(estimate)) %>%
  dplyr::filter(abs_estimate >= 1.1) %>%
  distinct(term) %>%
  inner_join(tidy_coefs, by = "term") %>%
  dplyr::filter(lambda == lambda_opt)

# plot the paths and highlight the large values
tidy_coefs %>%
  ggplot(aes(x = lambda, y = estimate, group = term, col = term, label = term)) +
  geom_vline(xintercept = lambda_opt, lty = 3) +
  geom_line(alpha = .4) +
  theme(legend.position = "none") +
  scale_x_log10() +
  ggrepel::geom_text_repel(data = label_coefs, aes(x = .005))

## -----------------------------------------------------------------------------
## Slide 41

library(vip)

vip(glmn_fit, num_features = 20L, 
    # Needs to know which coefficients to use
    lambda = best_glmn$penalty)

## -----------------------------------------------------------------------------
## Slide 55

# Let MARS decide the number of terms but tune the term dimensions
mars_mod <-  mars(prod_degree = tune())

# We'll decide via search:
mars_mod <-  
  mars(num_terms = tune("mars terms"), prod_degree = tune(), prune_method = "none") %>% 
  set_engine("earth") %>% 
  set_mode("regression")

mars_rec <- 
  chi_rec %>% 
  step_normalize(one_of(!!stations)) %>% 
  step_pca(one_of(!!stations), num_comp = tune("pca comps"))

## -----------------------------------------------------------------------------
## Slide 72

chi_wflow <-
  workflow() %>%
  add_recipe(mars_rec) %>%
  add_model(mars_mod)

chi_set <-
  parameters(chi_wflow) %>%
  update(
    `pca comps`  = num_comp(c(0, 20)), # 0 comps => PCA is not used 
    `mars terms` = num_terms(c(2, 100))
  )

## -----------------------------------------------------------------------------
## Slide 73

# library(doMC)
# registerDoMC(cores = 8)

ctrl <- control_bayes(verbose = TRUE, save_pred = TRUE)

# Some defaults:
#   - Uses expected improvement with no trade-off. See ?exp_improve().
#   - RMSE is minimized
set.seed(7891)
mars_tune <-
  tune_bayes(
    chi_wflow,
    resamples = chi_folds,
    iter = 25,
    param_info = chi_set,
    metrics = metric_set(rmse),
    initial = 4,
    control = ctrl
  )

## -----------------------------------------------------------------------------
## Slide 75

autoplot(mars_tune, type = "performance")

## -----------------------------------------------------------------------------
## Slide 76

autoplot(mars_tune, type = "marginals")

## -----------------------------------------------------------------------------
## Slide 77

autoplot(mars_tune, type = "parameters")

## -----------------------------------------------------------------------------
## Slide 78

show_best(mars_tune, metric = "rmse")

## -----------------------------------------------------------------------------
## Slide 82

mars_pred <- 
  mars_tune %>% 
  collect_predictions() %>% 
  inner_join(
    select_best(mars_tune, metric = "rmse"), 
    by = c("mars terms", "prod_degree", "pca comps")
  ) 

ggplot(mars_pred, aes(x = .pred, y = ridership)) + 
  geom_abline(col = "green") + 
  geom_point(alpha = .3) + 
  coord_obs_pred()

## -----------------------------------------------------------------------------
## Slide 83

best_mars <- select_best(mars_tune, "rmse")
best_mars

final_mars_wfl <- finalize_workflow(chi_wflow, best_mars)

# No formula is needed since a recipe is embedded in the workflow
final_mars_wfl <- fit(final_mars_wfl, data = Chicago)

## -----------------------------------------------------------------------------
## Slide 84

final_mars_wfl %>% 
  # Pull out the model
  pull_workflow_fit() %>%
  vip(num_features = 20L, type = "gcv")


## -----------------------------------------------------------------------------
## Part 6: Classification Models

## -----------------------------------------------------------------------------
## Slide 3

library(tidymodels)

## -----------------------------------------------------------------------------
## Slide 5

two_class_example %>% head(4)

## -----------------------------------------------------------------------------
## Slide 6

two_class_example %>% 
  conf_mat(truth = truth, estimate = predicted)

two_class_example %>% 
  accuracy(truth = truth, estimate = predicted)

## -----------------------------------------------------------------------------
## Slide 10

roc_obj <- 
  two_class_example %>% 
  roc_curve(truth, Class1)

two_class_example %>% roc_auc(truth, Class1)

autoplot(roc_obj) + thm

## -----------------------------------------------------------------------------
## Slide 15

data(ad_data)

# There's not much data so we'll use just 10% for testing. 
set.seed(1293)
split <- initial_split(ad_data, strata = Class, prop = .9)

ad_train <- training(split)  # n = 300
ad_test  <- testing(split)   # n =  33

count(ad_data, Class)

## -----------------------------------------------------------------------------
## Slide 16

set.seed(9599)
ad_folds <- vfold_cv(ad_train, strata = Class, repeats = 3)

## -----------------------------------------------------------------------------
## Slide 24

# 'save_workflow' will be used in the extra slides.
ctrl <- control_grid(save_pred = TRUE, save_workflow = TRUE)

cart_mod <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

cart_grid <- 
  crossing(cost_complexity = 10^seq(-3, -1, length.out = 10), 
           min_n = c(5, 10, 20))

cart_wflow <- 
  workflow() %>% 
  add_model(cart_mod) %>% 
  add_formula(Class ~ .)


## -----------------------------------------------------------------------------
## Slide 25

set.seed(2553)
cart_tune <- tune_grid(
  cart_wflow,
  resamples = ad_folds,
  grid = cart_grid,
  metrics = metric_set(roc_auc),
  control = ctrl
)

show_best(cart_tune, metric = "roc_auc")

## -----------------------------------------------------------------------------
## Slide 26

autoplot(cart_tune)

## -----------------------------------------------------------------------------
## Slide 27

cart_pred <- collect_predictions(cart_tune)
cart_pred %>% slice(1:5)
 
cart_pred %>% 
  inner_join(
    select_best(cart_tune, metric = "roc_auc"), 
    by = c("cost_complexity", "min_n")
  ) %>% 
  group_by(id, id2) %>% 
  roc_curve(Class, .pred_Impaired) %>% 
  autoplot() + 
  # Remove huge legend
  theme(legend.position = "none") 

## -----------------------------------------------------------------------------
## Slide 28

# For  tun_* object, get the right predictions 
# and extract the ROC curve data
auc_curve_data <- function(x) {
  collect_predictions(x) %>% 
    inner_join(select_best(x, "roc_auc")) %>% 
    roc_curve(Class, .pred_Impaired)
}

# Apply the `auc_roc_data()` function across
# models. 
approx_roc_curves <- function(...) {
  curves <- map_dfr(
    list(...), auc_curve_data, .id = "model"
  ) %>% 
    arrange(desc(specificity))
  
  default_cut <- curves %>% 
    group_by(model) %>% 
    arrange(abs(.threshold - .5)) %>% 
    slice(1)
  
  ggplot(curves) +
    aes(y = sensitivity, x = 1 - specificity, 
        col = model) +
    geom_abline(lty = 3) + 
    geom_step(direction = "vh") + 
    geom_point(data = default_cut) + 
    coord_equal()
}

## -----------------------------------------------------------------------------
## Slide 29

# Use named arguments for better labels
approx_roc_curves(CART = cart_tune)

## -----------------------------------------------------------------------------
## Slide 30 Hands-On: Down-Sampling

# * Looking at the ROC curve, the default cutoff may not be optimal if FP and FN 
#   errors are about equal.
# 
# * We could pick a better cutoff or fit another model using sub-class sampling.
# 
# * The latter approach would balance the data prior to model fitting.
# 
#   - The most common method would be to down-sample the data.
#  
#   - This is fairly controversial (at least in statistical circles).
# 
# * These types of steps are contained in the themis package.
# 
# * Let's take 20m and refit the model code above with a recipe that includes 
#   downsampling.
# 
# link to themis package documentation: https://themis.tidymodels.org/reference/index.html

# Code needed: 

library(tidymodels)

data(ad_data)

# There's not much data so we'll use just 10% for testing. 
set.seed(1293)
split <- initial_split(ad_data, strata = Class, prop = .9)

ad_train <- training(split)  # n = 300
ad_test  <- testing(split)   # n =  33

set.seed(9599)
ad_folds <- vfold_cv(ad_train, strata = Class, repeats = 3)

cart_wflow <- 
  workflow() %>% 
  add_model(cart_mod) %>% 
  add_recipe(`_CREATE_A_NEW_RECIPE_`)


## -----------------------------------------------------------------------------
## Slide 41

boost_mod <-
  boost_tree(
    trees = tune(),
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune()
  ) %>%
  set_engine("xgboost") %>% 
  set_mode("classification")

boost_wflow <- update_model(cart_wflow, boost_mod)
 
# We will just modify our CART grid and add 
# a new parameter: 
set.seed(5793)
boost_tune <-
  tune_grid(
    boost_wflow,
    ad_folds,
    grid = 20,
    metrics = metric_set(roc_auc),
    control = ctrl
  )

## -----------------------------------------------------------------------------
## Slide 42

approx_roc_curves(CART = cart_tune, xgb = boost_tune)

show_best(boost_tune, metric = "roc_auc")
autoplot(boost_tune)

## -----------------------------------------------------------------------------
## Slide 43

best_xgb <- select_best(boost_tune, metric = "roc_auc")
best_xgb

# no prep-juice calls!
boost_wflow_final <- 
  boost_wflow %>%
  finalize_workflow(best_xgb) %>% 
  fit(data = ad_train)

boost_wflow_final


## -----------------------------------------------------------------------------
## Slide 44

library(vip)
boost_wflow_final %>% 
  pull_workflow_fit() %>% 
  vip(num_features = 15)

## -----------------------------------------------------------------------------
## Slide 57

library(discrim)

nb_mod <- naive_Bayes(smoothness = tune()) %>% set_engine("klaR")

nb_recipe <- 
  recipe(Class ~ ., data = ad_train)  %>% 
  
  # To make sure that `male` will be treated as a binomial random variable instead of 
  # a Gaussian random variable with two distinct values of 0 and 1. 
  step_bin2factor(male) %>% 
  
  # See if removing redundant predictors improves performance. 
  step_corr(all_numeric(), threshold = tune())

nb_wflow <- 
  workflow() %>% 
  add_model(nb_mod) %>% 
  add_recipe(nb_recipe)

grid <- expand.grid(smoothness = 1:3, threshold = seq(0, .9, length.out = 10))


## -----------------------------------------------------------------------------
## Slide 59

set.seed(2553)
nb_tune <- tune_grid(
  nb_wflow,
  resamples = ad_folds,
  metrics = metric_set(roc_auc),
  control = ctrl,
  grid = grid
)

show_best(nb_tune)

## -----------------------------------------------------------------------------
## Slide 60

autoplot(nb_tune)  

approx_roc_curves(CART = cart_tune, xgb = boost_tune, 
                  "Naive Bayes" = nb_tune)

## -----------------------------------------------------------------------------
## Slide 61

test_probs <- boost_wflow_final %>%
  predict(ad_test, type = "prob") %>% 
  bind_cols(ad_test %>% dplyr::select(Class)) %>% 
  bind_cols(predict(boost_wflow_final, ad_test))

roc_auc(test_probs, Class, .pred_Impaired)
conf_mat(test_probs, Class, .pred_class)

roc_values <- 
  roc_curve(test_probs, Class, .pred_Impaired)

autoplot(roc_values)

