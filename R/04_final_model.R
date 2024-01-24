

# spec --------------------------------------------------------------------
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = 9, min_n = 10,
  loss_reduction = 0,                     ## first three: model complexity
  sample_size = 0.8, mtry = 8,         ## randomness
  learn_rate = 0.01                          ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_spec

# workflow ----------------------------------------------------------------
xgb_wf <- workflow() %>%
  add_formula(price ~ type + beds + bath + propertysqft + locality + gruppe) %>%
  add_model(xgb_spec)


# fit model ---------------------------------------------------------------
set.seed(967)
xgb_final_model <- xgb_wf %>%
  fit(data = df_houseprice)

xgb_final_model %>% write_rds("data/xgb_final_model.rds")


# vip ---------------------------------------------------------------------
xgb_res_final %>% 
  pull_workflow_fit() %>%
  vip(geom = "point")
