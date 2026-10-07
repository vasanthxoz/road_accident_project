# Creates a realistic SAMPLE accident dataset (synthetic) so the project runs out of the box.
# To use a real dataset, replace data/road_accidents.csv (see README for the column format).
generate_sample_data <- function(n = 6000, path = "data/road_accidents.csv", seed = 42) {
  set.seed(seed)
  roads <- data.frame(
    location = c("Avinashi Road","Trichy Road","Mettupalayam Road","Pollachi Road","Sathy Road",
                 "Palakkad Road","Ooty Road","Race Course Road","Cross Cut Road","Bypass Road"),
    lat = c(11.018,10.999,11.051,10.930,11.070,10.960,11.030,10.995,11.017,10.980),
    lon = c(76.990,77.020,76.940,76.960,76.990,76.950,76.960,76.970,76.961,77.030),
    w   = c(.20,.16,.12,.10,.10,.08,.07,.06,.06,.05))
  r <- sample(nrow(roads), n, replace = TRUE, prob = roads$w)
  date <- sample(seq(as.Date("2019-01-01"), as.Date("2023-12-31"), by = "day"), n, replace = TRUE)
  hp <- c(.01,.01,.01,.01,.015,.02,.04,.07,.075,.05,.04,.04,.045,.04,.04,.05,.06,.08,.085,.07,.05,.04,.03,.02)
  hour <- sample(0:23, n, replace = TRUE, prob = hp / sum(hp))
  vehicle <- sample(c("Car","Motorcycle","Bus","Truck","Other"), n, TRUE, c(.35,.35,.07,.15,.08))
  base <- c(Car = 55, Motorcycle = 50, Bus = 45, Truck = 50, Other = 40)
  speed <- round(pmin(pmax(rnorm(n, base[vehicle], 18), 10), 140))
  weather <- sample(c("Clear","Cloudy","Rain","Fog","Storm"), n, TRUE, c(.60,.15,.15,.06,.04))
  road <- sample(c("Dry","Wet","Damaged","Oily"), n, TRUE, c(.65,.20,.10,.05))
  light <- ifelse(hour >= 7 & hour <= 17, "Daylight",
           ifelse(hour %in% c(6, 18), "Dusk", sample(c("Dark-Lit","Dark-Unlit"), n, TRUE, c(.6,.4))))
  nveh <- sample(1:5, n, TRUE, c(.30,.45,.15,.07,.03))
  age <- pmin(pmax(round(rnorm(n, 36, 12)), 18), 80)
  z <- 0.035 * speed + c(Clear=0,Cloudy=.2,Rain=.6,Fog=1,Storm=1.2)[weather] +
       c(Dry=0,Wet=.4,Damaged=.6,Oily=.8)[road] +
       c(Daylight=0,Dusk=.3,"Dark-Lit"=.5,"Dark-Unlit"=.9)[light] +
       c(Car=0,Motorcycle=.7,Bus=.3,Truck=.5,Other=.2)[vehicle] + 0.15 * nveh +
       ifelse(age < 25 | age > 65, .3, 0) + rnorm(n, 0, 1)
  sev <- as.character(cut(z, c(-Inf, quantile(z, .55), quantile(z, .88), Inf),
                          labels = c("Minor","Serious","Fatal")))
  cas <- ifelse(sev == "Minor", rpois(n, .5), ifelse(sev == "Serious", 1 + rpois(n, 1), 1 + rpois(n, 1.2)))
  df <- data.frame(accident_id = 1:n, date = date, time = sprintf("%02d:%02d", hour, sample(0:59, n, TRUE)),
    latitude = round(roads$lat[r] + rnorm(n, 0, .004), 5), longitude = round(roads$lon[r] + rnorm(n, 0, .004), 5),
    location = roads$location[r], vehicle_type = vehicle, driver_age = age, speed = speed, weather = weather,
    road_condition = road, light_condition = light, num_vehicles = nveh, casualties = cas, severity = sev)
  for (col in c("speed","weather","driver_age"))            # add ~2% missing values to practise cleaning
    df[[col]][sample(n, round(.02 * n))] <- NA
  dir.create(dirname(path), showWarnings = FALSE)
  write.csv(df, path, row.names = FALSE)
  invisible(df)
}
