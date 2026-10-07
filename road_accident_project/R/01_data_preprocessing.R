# DATA LOADING, CLEANING AND PREPROCESSING
required_cols <- c("accident_id","date","time","latitude","longitude","location","vehicle_type",
                   "driver_age","speed","weather","road_condition","light_condition",
                   "num_vehicles","casualties","severity")

mode_value <- function(x) { x <- x[!is.na(x)]; names(sort(table(x), decreasing = TRUE))[1] }

# Helper for raw datasets that have no severity column
classify_severity <- function(killed, seriously_injured)
  ifelse(killed > 0, "Fatal", ifelse(seriously_injured > 0, "Serious", "Minor"))

load_clean_data <- function(path = "data/road_accidents.csv") {
  df <- read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA", "NULL", "Unknown"))
  miss <- setdiff(required_cols, names(df))
  if (length(miss) > 0) stop("Dataset is missing columns: ", paste(miss, collapse = ", "))
  df <- distinct(df)                                           # 1. remove duplicates
  cats <- c("location","vehicle_type","weather","road_condition","light_condition","severity")
  df[cats] <- lapply(df[cats], trimws)
  df$date <- as.Date(df$date)
  df <- df[!is.na(df$date) & !is.na(df$latitude) & !is.na(df$longitude) & !is.na(df$severity), ]
  for (col in c("driver_age","speed","num_vehicles","casualties"))   # 2. numeric NA -> median
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  for (col in setdiff(cats, "severity"))                              # 3. categorical NA -> most common
    df[[col]][is.na(df[[col]])] <- mode_value(df[[col]])
  df$speed      <- pmin(pmax(df$speed, 5), 200)                # 4. clip unrealistic values
  df$driver_age <- pmin(pmax(df$driver_age, 16), 90)
  # 5. derived features
  df$hour <- suppressWarnings(as.integer(substr(df$time, 1, 2)))
  df$hour[is.na(df$hour)] <- median(df$hour, na.rm = TRUE)
  df$year <- as.integer(format(df$date, "%Y"))
  df$month <- factor(month.abb[as.integer(format(df$date, "%m"))], levels = month.abb)
  days <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  df$day_of_week <- factor(days[as.integer(format(df$date, "%u"))], levels = days)
  df$age_group  <- cut(df$driver_age, c(0,24,34,44,59,Inf), labels = c("18-24","25-34","35-44","45-59","60+"))
  df$speed_band <- cut(df$speed, c(-Inf,40,60,80,100,Inf), labels = c("<=40","41-60","61-80","81-100",">100"))
  df$severity <- factor(df$severity, levels = c("Minor","Serious","Fatal"))
  for (col in c("location","vehicle_type","weather","road_condition","light_condition"))
    df[[col]] <- factor(df[[col]])
  # 6. risk score
  df$risk_score <- calculate_risk_score(df$speed, df$weather, df$road_condition,
                                        df$light_condition, df$vehicle_type, df$num_vehicles)
  df$risk_level <- risk_level(df$risk_score)
  df
}
