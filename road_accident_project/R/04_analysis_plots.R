# STATISTICS, ANALYSIS TABLES AND GGPLOT2 CHARTS
ggplot2::theme_set(ggplot2::theme_minimal(base_size = 12))
sev_cols   <- c(Minor = "#2ca02c", Serious = "#ff9800", Fatal = "#d62728")
level_cols <- c(Low = "#2ca02c", Medium = "#f1c40f", High = "#ff9800", Critical = "#d62728")

# ---- statistics ----
overall_stats <- function(df) {
  pct <- function(s) paste0(round(100 * mean(df$severity == s), 1), "%")
  span <- as.numeric(max(df$date) - min(df$date)) + 1
  data.frame(Statistic = c("Total accidents","Minor accidents","Serious accidents","Fatal accidents",
      "Total casualties","Avg casualties / accident","Max casualties (one accident)","Average speed (km/h)",
      "Median speed (km/h)","Average driver age","Avg vehicles involved","Accidents per day","Average risk score"),
    Value = as.character(c(nrow(df), pct("Minor"), pct("Serious"), pct("Fatal"), sum(df$casualties),
      round(mean(df$casualties), 2), max(df$casualties), round(mean(df$speed), 1), median(df$speed),
      round(mean(df$driver_age), 1), round(mean(df$num_vehicles), 2), round(nrow(df) / span, 2),
      round(mean(df$risk_score), 1))))
}

group_summary <- function(df, var) {
  df %>% group_by(across(all_of(var))) %>%
    summarise(Accidents = n(), Minor = sum(severity == "Minor"), Serious = sum(severity == "Serious"),
              Fatal = sum(severity == "Fatal"), Fatal_Pct = round(100 * mean(severity == "Fatal"), 1),
              Severe_Pct = round(100 * mean(severity != "Minor"), 1), Avg_Speed = round(mean(speed), 1),
              Avg_Casualties = round(mean(casualties), 2), Avg_Risk = round(mean(risk_score), 1),
              .groups = "drop") %>% arrange(desc(Accidents))
}

top_of <- function(x, flag = NULL) { if (!is.null(flag)) x <- x[flag]; names(which.max(table(x))) }
danger_summary <- function(df) {
  sev <- df$severity != "Minor"; fat <- df$severity == "Fatal"
  data.frame(Period = c("Hour of day","Day of week","Month"),
    Most_Accidents = c(paste0(top_of(df$hour), ":00"), top_of(df$day_of_week), top_of(df$month)),
    Most_Severe    = c(paste0(top_of(df$hour, sev), ":00"), top_of(df$day_of_week, sev), top_of(df$month, sev)),
    Most_Fatal     = c(paste0(top_of(df$hour, fat), ":00"), top_of(df$day_of_week, fat), top_of(df$month, fat)))
}

location_ranking <- function(df) {   # severity score: Minor = 1, Serious = 3, Fatal = 5
  df %>% group_by(location) %>%
    summarise(Accidents = n(), Serious = sum(severity == "Serious"), Fatal = sum(severity == "Fatal"),
              Avg_Risk = round(mean(risk_score), 1),
              Severity_Score = sum(case_when(severity == "Minor" ~ 1, severity == "Serious" ~ 3, TRUE ~ 5)),
              .groups = "drop") %>%
    arrange(desc(Severity_Score)) %>% rename(Location = location) %>%
    mutate(Rank = row_number(), Location = as.character(Location)) %>% select(Rank, Location, everything())
}

trend_summary <- function(df) {
  m <- df %>% mutate(ym = format(date, "%Y-%m")) %>% group_by(ym) %>%
    summarise(total = n(), fatal = sum(severity == "Fatal"), .groups = "drop") %>% mutate(t = row_number())
  say <- function(name, s) sprintf("%s are %s by about %.2f per month.", name,
                                   ifelse(s > 0, "INCREASING", "DECREASING"), abs(s))
  list(text = c(say("All accidents", coef(lm(total ~ t, m))[2]), say("Fatal accidents", coef(lm(fatal ~ t, m))[2])),
       yearly = df %>% group_by(Year = year) %>%
         summarise(Accidents = n(), Fatal = sum(severity == "Fatal"),
                   Fatal_Pct = round(100 * mean(severity == "Fatal"), 1), Avg_Risk = round(mean(risk_score), 1)))
}

# ---- charts ----
plot_severity_pie <- function(df) {
  d <- df %>% count(severity) %>% mutate(pct = round(100 * n / sum(n), 1))
  ggplot(d, aes(x = "", y = n, fill = severity)) + geom_col(width = 1, color = "white") +
    coord_polar("y") + geom_text(aes(label = paste0(pct, "%")), position = position_stack(vjust = .5), color = "white") +
    scale_fill_manual(values = sev_cols) + theme_void() + labs(fill = "Severity")
}
plot_severity_bar <- function(df)
  ggplot(count(df, severity), aes(severity, n, fill = severity)) + geom_col() + geom_text(aes(label = n), vjust = -.3) +
    scale_fill_manual(values = sev_cols) + labs(x = NULL, y = "Accidents") + theme(legend.position = "none")

plot_count_by <- function(df, var, title, flip = FALSE) {      # stacked bars by severity
  p <- ggplot(df, aes(x = as.factor(.data[[var]]), fill = severity)) + geom_bar() +
    scale_fill_manual(values = sev_cols) + labs(title = title, x = NULL, y = "Accidents", fill = "Severity")
  if (flip) p + coord_flip() else p
}
plot_share_by <- function(df, var, title)                      # severity percentage within each group
  ggplot(df, aes(x = as.factor(.data[[var]]), fill = severity)) + geom_bar(position = "fill") +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) + scale_fill_manual(values = sev_cols) +
    labs(title = title, x = NULL, y = "Share of accidents", fill = "Severity")

plot_heatmap <- function(df)
  ggplot(count(df, day_of_week, hour), aes(factor(hour), day_of_week, fill = n)) + geom_tile(color = "white") +
    scale_fill_gradient(low = "#fff3cd", high = "#d62728") + labs(title = "Accidents by day and hour", x = "Hour", y = NULL, fill = "Accidents")

plot_speed_box <- function(df)
  ggplot(df, aes(severity, speed, fill = severity)) + geom_boxplot() + scale_fill_manual(values = sev_cols) +
    labs(title = "Speed by severity", x = NULL, y = "Speed (km/h)") + theme(legend.position = "none")

plot_trend <- function(df) {
  m <- df %>% mutate(ym = as.Date(format(date, "%Y-%m-01"))) %>% group_by(ym) %>%
    summarise(`All accidents` = n(), `Fatal accidents` = sum(severity == "Fatal"), .groups = "drop") %>%
    pivot_longer(-ym, names_to = "Type", values_to = "Count")
  ggplot(m, aes(ym, Count, color = Type)) + geom_line(linewidth = 1) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed") +
    facet_wrap(~Type, scales = "free_y", ncol = 1) + labs(title = "Monthly trend (dashed = trend line)", x = NULL) +
    theme(legend.position = "none")
}

plot_risk_hist <- function(df)
  ggplot(df, aes(risk_score)) + geom_histogram(binwidth = 5, fill = "#3c8dbc", color = "white") +
    labs(title = "Risk score distribution", x = "Risk score (0-100)", y = "Accidents")
plot_risk_levels <- function(df)
  ggplot(count(df, risk_level), aes(risk_level, n, fill = risk_level)) + geom_col() + scale_fill_manual(values = level_cols) +
    labs(title = "Accidents by risk level", x = NULL, y = "Accidents") + theme(legend.position = "none")

plot_location <- function(df)
  ggplot(head(location_ranking(df), 10), aes(reorder(Location, Severity_Score), Severity_Score)) +
    geom_col(fill = "#d62728") + coord_flip() + labs(title = "Top accident-prone roads (severity score)", x = NULL, y = "Severity score")

plot_importance <- function(m) {
  imp <- data.frame(Variable = rownames(m$importance), Importance = m$importance[, "MeanDecreaseGini"])
  ggplot(imp, aes(reorder(Variable, Importance), Importance)) + geom_col(fill = "#3c8dbc") + coord_flip() +
    labs(title = "What drives severity? (variable importance)", x = NULL)
}
