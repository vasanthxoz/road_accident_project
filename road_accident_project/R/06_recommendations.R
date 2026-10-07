# SAFETY RECOMMENDATION SYSTEM
# (a) for one set of conditions (used by the What-If calculator)
recommend_for_conditions <- function(speed, weather, road_condition, light_condition, vehicle_type, driver_age = 35, num_vehicles = 1) {
  r <- c()
  if (speed >= 80) r <- c(r, "High speed: enforce speed limits, add speed cameras, rumble strips and speed breakers.")
  else if (speed >= 60) r <- c(r, "Moderate-high speed: display speed-limit signs and use speed-control measures near junctions.")
  if (weather %in% c("Rain","Storm")) r <- c(r, "Rain/storm: slow down, keep a longer gap, improve drainage and anti-skid road surfaces.")
  if (weather == "Fog") r <- c(r, "Fog: use fog lights, reflective road markings and fog-warning signs; reduce speed.")
  if (road_condition %in% c("Wet","Oily")) r <- c(r, "Slippery road: use skid-resistant surfacing and clean oil spills quickly.")
  if (road_condition == "Damaged") r <- c(r, "Damaged road: repair potholes and put up hazard warning signs.")
  if (light_condition %in% c("Dark-Lit","Dark-Unlit","Dusk")) r <- c(r, "Low light: install better street lighting, reflectors and cat-eye road studs.")
  if (light_condition == "Dark-Unlit") r <- c(r, "Unlit road: this is a priority location for new street lights.")
  if (vehicle_type == "Motorcycle") r <- c(r, "Motorcycle: enforce helmet use, provide dedicated lanes and rider-safety awareness.")
  if (vehicle_type %in% c("Truck","Bus")) r <- c(r, "Heavy vehicle: enforce driver rest hours, vehicle fitness checks and lower speed limits.")
  if (driver_age < 25) r <- c(r, "Young driver: stricter licensing and road-safety training programmes.")
  if (driver_age > 65) r <- c(r, "Older driver: regular vision and fitness checks.")
  if (num_vehicles >= 3) r <- c(r, "Multi-vehicle risk: keep safe following distance and use lane discipline.")
  if (length(r) == 0) r <- "Conditions look low-risk. Continue regular maintenance and safe-driving awareness."
  r
}

# (b) from the whole dataset: flag conditions whose accidents are more often serious/fatal
dataset_recommendations <- function(df) {
  base <- mean(df$severity != "Minor")
  chk <- function(flag, text) {
    if (sum(flag) < 30) return(NULL)
    rate <- mean(df$severity[flag] != "Minor")
    if (rate > base + 0.03) sprintf("%s (%.0f%% serious/fatal here vs %.0f%% overall)", text, 100 * rate, 100 * base)
  }
  r <- c(
    chk(df$speed >= 80, "Speed control: enforce limits and install speed cameras"),
    chk(df$weather %in% c("Rain","Fog","Storm"), "Bad weather: add weather warning signs, fog lights and better drainage"),
    chk(df$road_condition %in% c("Wet","Damaged","Oily"), "Road surface: repair damage and improve skid resistance"),
    chk(df$light_condition %in% c("Dark-Lit","Dark-Unlit"), "Night safety: install better street lighting and reflectors"),
    chk(df$vehicle_type == "Motorcycle", "Two-wheelers: enforce helmets and provide safe lanes"),
    chk(df$vehicle_type %in% c("Truck","Bus"), "Heavy vehicles: enforce driver rest hours and speed limits"),
    chk(df$driver_age < 25, "Young drivers: training and stricter licensing"))
  top <- head(location_ranking(df)$Location, 3)
  c(r, paste("Priority road audits and police patrolling on:", paste(top, collapse = ", ")))
}
