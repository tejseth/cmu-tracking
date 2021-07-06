set.seed(2014) #go lions

simple_data_model <- plays_select %>%
  filter(!is.na(width)) %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber,
         is_shotgun, half_seconds_remaining, score_differential)

simple_data_model$pass_or_run <- as.factor(simple_data_model$pass_or_run)

simple_split_pbp <- initial_split(simple_data_model, 0.75, strata = pass_or_run)

simple_train_data <- training(simple_split_pbp)

simple_test_data <- testing(simple_split_pbp)

simple_pbp_rec <- recipe(pass_or_run ~ ., data = simple_train_data) %>% 
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors()) # remove zero-variance predictors

simple_lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")

simple_lr_wflow <- workflow() %>% 
  add_model(simple_lr_mod) %>% # parsnip model
  add_recipe(simple_pbp_rec)   # recipe from recipes

simple_pbp_fit_lr <- simple_lr_wflow %>% 
  fit(data = simple_train_data) # fit the model against the training data

simple_pbp_pred_lr <- predict(simple_pbp_fit_lr, simple_test_data) %>% 
  # Get probabilities for the class for each observation
  bind_cols(predict(simple_pbp_fit_lr, simple_test_data, type = "prob")) %>% 
  # Add back a "truth" column for what the actual play_type was
  bind_cols(simple_test_data %>% select(pass_or_run))

simple_pbp_pred_lr %>% 
  group_by(.pred_class, pass_or_run) %>%
  summarize(perc = n() / nrow(simple_pbp_pred_lr)) %>%
  arrange(-perc)

table("Predictions" = simple_pbp_pred_lr$.pred_class, "Observed" = simple_pbp_pred_lr$pass_or_run)

`simple_pbp_pred_lr` %>% # Simple Logistic Regression predictions
  metrics(truth = pass_or_run, .pred_class)
#71.8%

###########################################################################
set.seed(2016) #go lions

plays_model_data <- plays_select %>%
  filter(!is.na(width)) %>%
  select(pass_or_run, quarter, week, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, is_wildcat, defendersInTheBox,
         half_seconds_remaining, score_differential, width, linemen_width, prev_pass,
         linemen_height, rb_deep, is_fullback)

plays_model_data$pass_or_run <- as.factor(plays_model_data$pass_or_run)

split_pbp <- initial_split(plays_model_data, 0.75, strata = pass_or_run)

train_data <- training(split_pbp)

test_data <- testing(split_pbp)

pbp_rec <- recipe(pass_or_run ~ ., data = train_data) %>% 
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors()) # remove zero-variance predictors

lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")

lr_wflow <- workflow() %>% 
  add_model(lr_mod) %>% # parsnip model
  add_recipe(pbp_rec)   # recipe from recipes

pbp_fit_lr <- lr_wflow %>% 
  fit(data = train_data) # fit the model against the training data

pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>% 
  # Get probabilities for the class for each observation
  bind_cols(predict(pbp_fit_lr, test_data, type = "prob")) %>% 
  # Add back a "truth" column for what the actual play_type was
  bind_cols(test_data %>% select(pass_or_run))

pbp_pred_lr %>% 
  group_by(.pred_class, pass_or_run) %>%
  summarize(perc = n() / nrow(pbp_pred_lr)) %>%
  arrange(-perc)

table("Predictions" = pbp_pred_lr$.pred_class, "Observed" = pbp_pred_lr$pass_or_run)

model_stats <- lr_wflow %>% 
  finalize_workflow(pbp_fit_lr) %>%
  fit(train_data) %>%
  pull_workflow_fit() %>%
  tidy()

model_stats %>%
  mutate(ord_term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = estimate, y = ord_term)) +
  geom_bar(aes(fill = ifelse(estimate > 0, "darkblue", "darkred")), stat = "identity") +
  scale_color_identity(aesthetics = c("fill")) +
  theme_bw() +
  theme(legend.position = "none")

rf_mod <- rand_forest(trees = 1000) %>% 
  set_engine("ranger", 
             importance = "impurity", # variable importance
             num.threads = 4) %>%     # Parallelize
  set_mode("classification")

rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>%  # New model
  add_recipe(pbp_rec)    # Same recipe

pbp_fit_rf <- rf_wflow %>% # New workflow
  fit(data = train_data)   # Fit the Random Forest
# Get predictions and check metrics

pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>% 
  bind_cols(test_data %>% select(pass_or_run)) %>% 
  bind_cols(predict(pbp_fit_rf, test_data, type = "prob"))

`pbp_pred_rf` %>% # Random Forest predictions
  metrics(truth = pass_or_run, .pred_class)
#73.2

`pbp_pred_lr` %>% # Logistic Regression predictions
  metrics(truth = pass_or_run, .pred_class)
#73.3%

roc_rf <- pbp_pred_rf %>% 
  roc_curve(truth = pass_or_run, .pred_Pass) %>% 
  mutate(model = "Random Forest")

roc_lr <- pbp_pred_lr %>% 
  roc_curve(truth = pass_or_run, .pred_Pass) %>% 
  mutate(model = "Logistic Regression")

bind_rows(roc_rf, roc_lr) %>% 
  # Note that autoplot() would also work here!
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = model)) + 
  geom_path(lwd = 1, alpha = 0.5) +
  geom_abline(lty = 3) + 
  theme_bw() +
  scale_color_manual(values = c("#374785", "#E98074")) +
  theme(legend.position = "top")

############################################################################
set.seed(1991) #go lions

xg_model_data <- plays_model_data %>%
  mutate(label = ifelse(pass_or_run == "Pass", 1, 0)) %>%
  select(-pass_or_run)

str(xg_model_data)

nrounds <- 1121
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("error", "logloss"),
    eta = .015,
    gamma = 2,
    subsample = 0.8,
    colsample_bytree = 0.8,
    max_depth = 7,
    min_child_weight = 0.9
  )

cv_results <- map_dfr(1:6, function(x) {
  test_data <- xg_model_data %>%
    filter(week == x) %>%
    select(-week)
  train_data <- xg_model_data %>%
    filter(week != x) %>%
    select(-week)
  
  full_train <- xgboost::xgb.DMatrix(model.matrix(~ . + 0, data = train_data %>% select(-label)),
                                     label = train_data$label
  )
  xp_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)
  
  preds <- as.data.frame(
    matrix(predict(xp_model, as.matrix(test_data %>% select(-label))))
  ) %>%
    dplyr::rename(xp = V1)
  
  cv_data <- bind_cols(test_data, preds) %>% mutate(week = x)
  return(cv_data)
})

cv_results <- cv_results %>%
  mutate(actual_result = ifelse(label == 1, "Pass", "Run"),
         pred = ifelse(xp >= 0.50, "Pass", "Run"),
         is_right = ifelse(actual_result == pred, 1, 0))

mean(cv_results$is_right)