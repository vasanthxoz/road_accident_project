# MACHINE LEARNING: Random Forest severity prediction
model_features <- c("vehicle_type","driver_age","speed","weather","road_condition",
                    "light_condition","num_vehicles","hour")   # 'casualties' excluded (it would leak the answer)

train_severity_model <- function(df, model_path = "model/rf_model.rds") {
  set.seed(123)
  d <- df[, c(model_features, "severity")]
  idx <- sample(nrow(d), 0.8 * nrow(d))                        # 80% train / 20% test
  train <- d[idx, ]; test <- d[-idx, ]
  rf <- randomForest(severity ~ ., data = train, ntree = 200, importance = TRUE)
  pred <- predict(rf, test)
  cm <- table(Actual = test$severity, Predicted = pred)
  out <- list(model = rf, confusion = cm,
              accuracy = sum(diag(cm)) / sum(cm),
              baseline = max(table(test$severity)) / nrow(test),   # always guess the biggest class
              recall = round(diag(cm) / rowSums(cm), 3),
              importance = importance(rf),
              levels = lapply(train[model_features], function(x) if (is.factor(x)) levels(x) else NULL))
  dir.create(dirname(model_path), showWarnings = FALSE)
  saveRDS(out, model_path)
  out
}

predict_severity <- function(m, vehicle_type, driver_age, speed, weather, road_condition,
                             light_condition, num_vehicles, hour) {
  nd <- data.frame(vehicle_type = factor(vehicle_type, levels = m$levels$vehicle_type),
                   driver_age = driver_age, speed = speed,
                   weather = factor(weather, levels = m$levels$weather),
                   road_condition = factor(road_condition, levels = m$levels$road_condition),
                   light_condition = factor(light_condition, levels = m$levels$light_condition),
                   num_vehicles = num_vehicles, hour = hour)
  p <- predict(m$model, nd, type = "prob")[1, ]
  list(class = names(which.max(p)), probs = p)
}
