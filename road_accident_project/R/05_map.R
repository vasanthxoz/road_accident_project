# INTERACTIVE ACCIDENT MAP (leaflet)
make_accident_map <- function(df, severities = c("Minor","Serious","Fatal"), max_points = 3000) {
  df <- df[df$severity %in% severities, ]
  if (nrow(df) == 0) return(leaflet() %>% addTiles())
  if (nrow(df) > max_points) df <- df[sample(nrow(df), max_points), ]   # keep the map fast
  pal <- colorFactor(unname(sev_cols), levels = names(sev_cols))
  m <- leaflet() %>% addTiles() %>% setView(mean(df$longitude), mean(df$latitude), zoom = 12)
  for (s in severities) {
    d <- df[df$severity == s, ]
    if (nrow(d) == 0) next
    m <- m %>% addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude, group = s,
      radius = c(Minor = 4, Serious = 6, Fatal = 9)[[s]], color = unname(sev_cols[s]), stroke = FALSE, fillOpacity = .7,
      popup = ~paste0("<b>", severity, "</b><br>", location, "<br>", date, " ", time, "<br>Vehicle: ", vehicle_type,
                      "<br>Speed: ", speed, " km/h<br>Weather: ", weather, "<br>Casualties: ", casualties))
  }
  m %>% addLayersControl(overlayGroups = severities, options = layersControlOptions(collapsed = FALSE)) %>%
    addLegend("bottomright", pal = pal, values = names(sev_cols), title = "Severity")
}
