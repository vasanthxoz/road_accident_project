# RISK SCORE (0-100): speed 40 + weather 20 + road 15 + light 10 + vehicle 10 + vehicles involved 5
weather_pts <- c(Clear = 0, Cloudy = 3, Rain = 10, Fog = 16, Storm = 20)
road_pts    <- c(Dry = 0, Wet = 6, Damaged = 10, Oily = 15)
light_pts   <- c(Daylight = 0, Dusk = 4, "Dark-Lit" = 7, "Dark-Unlit" = 10)
vehicle_pts <- c(Car = 4, Motorcycle = 10, Bus = 6, Truck = 8, Other = 5)

lookup_pts <- function(tbl, x) { v <- unname(tbl[as.character(x)]); v[is.na(v)] <- mean(tbl); v }

calculate_risk_score <- function(speed, weather, road_condition, light_condition, vehicle_type, num_vehicles) {
  score <- pmin(pmax(speed, 0), 140) / 140 * 40 +
           lookup_pts(weather_pts, weather) + lookup_pts(road_pts, road_condition) +
           lookup_pts(light_pts, light_condition) + lookup_pts(vehicle_pts, vehicle_type) +
           pmin(num_vehicles - 1, 4) * 1.25
  round(pmin(score, 100), 1)
}

risk_level <- function(score)
  cut(score, c(-Inf, 25, 50, 75, Inf), labels = c("Low","Medium","High","Critical"), right = FALSE)
