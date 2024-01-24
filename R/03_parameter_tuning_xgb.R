
# splitting test/train data -----------------------------------------------
set.seed(123)
df_split <- initial_split(df_houseprice, strata = price)
df_train <- training(df_split)
df_test <- testing(df_split)


## spec --------------------------------------------------------------------
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_spec

## workflow ----------------------------------------------------------------
xgb_wf <- workflow() %>%
  add_formula(price ~ type + beds + bath + propertysqft + locality + gruppe) %>%
  add_model(xgb_spec)

xgb_wf

## grid --------------------------------------------------------------------
set.seed(23)
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_houseprice),
  learn_rate(),
  size = 30
)

xgb_grid                     

## cv ----------------------------------------------------------------------
set.seed(123)
df_folds <- vfold_cv(df_train, strata = price, v = 5)

df_folds


## tuning ------------------------------------------------------------------
doParallel::registerDoParallel()

# set.seed(234)
# xgb_res <- tune_grid(
#   xgb_wf,
#   resamples = df_folds,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE)
# )

# xgb_res %>% write_rds("data/xgb_res.rds")
# xgb_res <- read_rds("data/xgb_res.rds")

## plot results ------------------------------------------------------------
collect_metrics(xgb_res)

xgb_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

show_best(xgb_res, "rmse")


# iteration 2 -------------------------------------------------------------
# fix learn_rate = 0.01  
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = 0.01                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_spec

## workflow ----------------------------------------------------------------
xgb_wf <- workflow() %>%
  add_formula(price ~ type + beds + bath + propertysqft + locality + gruppe) %>%
  add_model(xgb_spec)

xgb_wf

## grid --------------------------------------------------------------------
set.seed(23)
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_houseprice),
  size = 30
)

xgb_grid                     

## cv ----------------------------------------------------------------------
set.seed(123)
df_folds <- vfold_cv(df_train, strata = price, v = 5)

df_folds


## tuning ------------------------------------------------------------------
doParallel::registerDoParallel()

# set.seed(234)
# xgb_res2 <- tune_grid(
#   xgb_wf,
#   resamples = df_folds,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE)
# )

# xgb_res2 %>% write_rds("data/xgb_res2.rds")
# xgb_res2 <- read_rds("data/xgb_res2.rds")

## plot results ------------------------------------------------------------
collect_metrics(xgb_res2)

xgb_res2 %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

# iteration 3 -------------------------------------------------------------
# fix loss_reduction = 0
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = 0,                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = 0.01                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_spec

## workflow ----------------------------------------------------------------
xgb_wf <- workflow() %>%
  add_formula(price ~ type + beds + bath + propertysqft + locality + gruppe) %>%
  add_model(xgb_spec)

xgb_wf

## grid --------------------------------------------------------------------
set.seed(28)
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  sample_size = sample_prop(),
  finalize(mtry(), df_houseprice),
  size = 30
)

xgb_grid                     

## cv ----------------------------------------------------------------------
set.seed(109)
df_folds <- vfold_cv(df_train, strata = price, v = 5)

df_folds


## tuning ------------------------------------------------------------------
doParallel::registerDoParallel()

# set.seed(234)
# xgb_res3 <- tune_grid(
#   xgb_wf,
#   resamples = df_folds,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE)
# )

# xgb_res3 %>% write_rds("data/xgb_res3.rds")
# xgb_res3 <- read_rds("data/xgb_res3.rds")

## plot results ------------------------------------------------------------
collect_metrics(xgb_res1)

xgb_res3 %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

# iteration 4 -------------------------------------------------------------
# fix sample_size = 0.8
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(),
  loss_reduction = 0,                     ## first three: model complexity
  sample_size = 0.8, mtry = tune(),         ## randomness
  learn_rate = 0.01                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_spec

## workflow ----------------------------------------------------------------
xgb_wf <- workflow() %>%
  add_formula(price ~ type + beds + bath + propertysqft + locality + gruppe) %>%
  add_model(xgb_spec)

xgb_wf

## grid --------------------------------------------------------------------
set.seed(280)
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  finalize(mtry(), df_houseprice),
  size = 30
)

xgb_grid                     

## cv ----------------------------------------------------------------------
set.seed(198)
df_folds <- vfold_cv(df_train, strata = price, v = 5)

df_folds


## tuning ------------------------------------------------------------------
doParallel::registerDoParallel()

# set.seed(234)
# xgb_res4 <- tune_grid(
#   xgb_wf,
#   resamples = df_folds,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE)
# )
# 
# xgb_res4 %>% write_rds("data/xgb_res4.rds")
# xgb_res4 <- read_rds("data/xgb_res4.rds")

## plot results ------------------------------------------------------------
collect_metrics(xgb_res1)

xgb_res4 %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, mtry:tree_depth) %>%
  pivot_longer(mtry:tree_depth,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")


# final model -------------------------------------------------------------

# fix min_n = 10,
# mtry = 8
# tree_depth = 9
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = 9, min_n = 10,
  loss_reduction = 0,                     ## first three: model complexity
  sample_size = 0.8, mtry = 8,         ## randomness
  learn_rate = 0.01                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

## workflow ----------------------------------------------------------------
xgb_wf <- workflow() %>%
  add_formula(price ~ type + beds + bath + propertysqft + locality + gruppe) %>%
  add_model(xgb_spec)

xgb_wf

set.seed(967)
xgb_res_final <- xgb_wf %>%
  fit(data = df_train)


# test error --------------------------------------------------------------
df_test %>% 
  bind_cols(xgb_res_final %>% predict(df_test)) %>% 
  yardstick::rmse(price, .pred)

df_test %>% 
  bind_cols(xgb_res_final %>% predict(df_test)) %>% 
  yardstick::mae(price, .pred)

rm(ls())
