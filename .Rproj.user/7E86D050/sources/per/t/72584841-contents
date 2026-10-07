# Road Accident Risk Assessment and Severity Prediction System Using R
library(shiny); library(shinydashboard); library(ggplot2); library(dplyr)
library(tidyr); library(leaflet); library(randomForest); library(DT)

for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)

# ---- load data (generate the sample dataset on first run) ----
if (!file.exists("data/road_accidents.csv")) { source("data/generate_sample_data.R"); generate_sample_data() }
accidents <- load_clean_data("data/road_accidents.csv")

# ---- load or train the model (delete model/rf_model.rds after changing the dataset) ----
model_obj <- if (file.exists("model/rf_model.rds")) readRDS("model/rf_model.rds") else train_severity_model(accidents)

tbl <- function(x) datatable(x, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
pbox <- function(id, title, width = 6) box(title = title, width = width, status = "primary", solidHeader = TRUE, plotOutput(id, height = 300))
tbox <- function(id, title, width = 12) box(title = title, width = width, status = "primary", solidHeader = TRUE, DTOutput(id))

# ============================== UI ==============================
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Road Accident Risk"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Accident Dashboard", tabName = "dash", icon = icon("chart-bar")),
      menuItem("Severity Analysis", tabName = "sev", icon = icon("exclamation-triangle")),
      menuItem("Risk Assessment", tabName = "risk", icon = icon("tachometer-alt")),
      menuItem("Prediction", tabName = "pred", icon = icon("robot")),
      menuItem("What-If Calculator", tabName = "whatif", icon = icon("calculator")),
      menuItem("Location Map", tabName = "map", icon = icon("map-marked-alt")),
      menuItem("Time Analysis", tabName = "time", icon = icon("clock")),
      menuItem("Weather Analysis", tabName = "weather", icon = icon("cloud-rain")),
      menuItem("Vehicle Analysis", tabName = "vehicle", icon = icon("car")),
      menuItem("Safety Recommendations", tabName = "safety", icon = icon("shield-alt"))),
    hr(),
    sliderInput("years", "Year range", min(accidents$year), max(accidents$year),
                value = range(accidents$year), step = 1, sep = "")),
  dashboardBody(tabItems(
    tabItem("home",
      box(width = 12, status = "primary", solidHeader = TRUE, title = "Road Accident Risk Assessment and Severity Prediction System",
        p("This system analyses road accident records, scores accident risk from 0 to 100, predicts accident severity
          (Minor / Serious / Fatal) using a Random Forest model, locates accident-prone roads and suggests safety measures."),
        p("Use the menu on the left. The year slider filters every page except the Prediction model (trained on all data).")),
      box(width = 6, title = "Dataset summary", status = "info", solidHeader = TRUE, tableOutput("home_stats")),
      box(width = 6, title = "Modules", status = "info", solidHeader = TRUE,
        tags$ul(tags$li("Data cleaning & preprocessing"), tags$li("Statistics & visualisation"),
                tags$li("Risk score engine"), tags$li("Severity prediction (ML)"), tags$li("What-if calculator"),
                tags$li("Location, time, weather, vehicle analysis"), tags$li("Interactive map"),
                tags$li("Trend analysis"), tags$li("Safety recommendations")))),
    tabItem("dash",
      fluidRow(valueBoxOutput("vb_total", 3), valueBoxOutput("vb_fatal", 3), valueBoxOutput("vb_speed", 3), valueBoxOutput("vb_cas", 3)),
      fluidRow(pbox("sev_pie", "Severity share"), pbox("sev_bar", "Severity count")),
      fluidRow(box(width = 12, title = "Monthly trend", status = "primary", solidHeader = TRUE, plotOutput("trend", height = 380))),
      fluidRow(box(width = 6, title = "Trend result", status = "warning", solidHeader = TRUE, uiOutput("trend_text"), tableOutput("yearly")),
               box(width = 6, title = "Statistics", status = "primary", solidHeader = TRUE, tableOutput("stats")))),
    tabItem("sev",
      fluidRow(pbox("sev_speed", "Speed vs severity"), pbox("sev_band", "Severity by speed band")),
      fluidRow(pbox("sev_age", "Severity by driver age"), pbox("sev_light", "Severity by light condition")),
      fluidRow(pbox("sev_road", "Severity by road condition"), tbox("sev_tbl", "Casualty statistics by severity", 6))),
    tabItem("risk",
      fluidRow(valueBoxOutput("vb_risk", 4), valueBoxOutput("vb_crit", 4), valueBoxOutput("vb_high", 4)),
      fluidRow(pbox("risk_hist", "Risk score distribution"), pbox("risk_lvl", "Risk levels")),
      fluidRow(tbox("risk_tbl", "Highest-risk accidents"))),
    tabItem("pred",
      fluidRow(valueBoxOutput("vb_acc", 4), valueBoxOutput("vb_base", 4), valueBoxOutput("vb_rf", 4)),
      fluidRow(pbox("imp", "Variable importance"),
               box(width = 6, title = "Confusion matrix (test data)", status = "primary", solidHeader = TRUE,
                   tableOutput("cm"), h5("Recall per severity class"), tableOutput("recall")))),
    tabItem("whatif",
      fluidRow(
        box(width = 5, title = "Enter accident conditions", status = "primary", solidHeader = TRUE,
          sliderInput("w_speed", "Speed (km/h)", 10, 140, 70),
          selectInput("w_weather", "Weather", levels(accidents$weather)),
          selectInput("w_road", "Road condition", levels(accidents$road_condition)),
          selectInput("w_light", "Light condition", levels(accidents$light_condition)),
          selectInput("w_vehicle", "Vehicle type", levels(accidents$vehicle_type)),
          sliderInput("w_nveh", "Vehicles involved", 1, 10, 2),
          sliderInput("w_age", "Driver age", 18, 80, 30),
          sliderInput("w_hour", "Hour of day", 0, 23, 20),
          actionButton("calc", "Calculate", icon = icon("calculator"), class = "btn-primary")),
        column(7, valueBoxOutput("wi_score", 6), valueBoxOutput("wi_sev", 6),
          box(width = 12, title = "Predicted probability", status = "primary", solidHeader = TRUE, plotOutput("wi_prob", height = 220)),
          box(width = 12, title = "Safety recommendations", status = "warning", solidHeader = TRUE, uiOutput("wi_rec"))))),
    tabItem("map",
      fluidRow(box(width = 12, status = "primary", solidHeader = TRUE, title = "Accident map",
        checkboxGroupInput("map_sev", "Show severity:", c("Minor","Serious","Fatal"), c("Minor","Serious","Fatal"), inline = TRUE),
        leafletOutput("map", height = 450))),
      fluidRow(pbox("loc_plot", "Most dangerous roads"), tbox("loc_tbl", "Road ranking (Minor=1, Serious=3, Fatal=5)", 6))),
    tabItem("time",
      fluidRow(pbox("t_hour", "By hour"), pbox("t_day", "By day of week")),
      fluidRow(pbox("t_month", "By month"), pbox("t_heat", "Day x hour heatmap")),
      fluidRow(tbox("t_tbl", "Most dangerous periods"))),
    tabItem("weather",
      fluidRow(pbox("w_freq", "Accidents by weather"), pbox("w_share", "Severity share by weather")),
      fluidRow(tbox("w_tbl", "Weather summary"))),
    tabItem("vehicle",
      fluidRow(pbox("v_freq", "Accidents by vehicle"), pbox("v_share", "Severity share by vehicle")),
      fluidRow(tbox("v_tbl", "Vehicle summary"))),
    tabItem("safety",
      box(width = 12, title = "Recommendations generated from the accident data", status = "warning", solidHeader = TRUE, uiOutput("rec_data")),
      box(width = 12, title = "Quick guide", status = "info", solidHeader = TRUE,
          p("For a specific situation, open the What-If Calculator: it generates recommendations for the conditions you enter."))))))

# ============================ SERVER ============================
server <- function(input, output, session) {
  df <- reactive(accidents %>% filter(year >= input$years[1], year <= input$years[2]))

  # Home
  output$home_stats <- renderTable(overall_stats(accidents))

  # Accident dashboard
  output$vb_total <- renderValueBox(valueBox(format(nrow(df()), big.mark = ","), "Total accidents", icon("car-crash"), "blue"))
  output$vb_fatal <- renderValueBox(valueBox(paste0(round(100 * mean(df()$severity == "Fatal"), 1), "%"), "Fatal accidents", icon("skull"), "red"))
  output$vb_speed <- renderValueBox(valueBox(paste(round(mean(df()$speed), 1), "km/h"), "Average speed", icon("gauge-high"), "yellow"))
  output$vb_cas   <- renderValueBox(valueBox(format(sum(df()$casualties), big.mark = ","), "Total casualties", icon("user-injured"), "purple"))
  output$sev_pie <- renderPlot(plot_severity_pie(df()))
  output$sev_bar <- renderPlot(plot_severity_bar(df()))
  output$trend   <- renderPlot(plot_trend(df()))
  output$trend_text <- renderUI(tags$ul(lapply(trend_summary(df())$text, tags$li)))
  output$yearly  <- renderTable(trend_summary(df())$yearly)
  output$stats   <- renderTable(overall_stats(df()))

  # Severity analysis
  output$sev_speed <- renderPlot(plot_speed_box(df()))
  output$sev_band  <- renderPlot(plot_share_by(df(), "speed_band", "Severity by speed band (km/h)"))
  output$sev_age   <- renderPlot(plot_share_by(df(), "age_group", "Severity by driver age"))
  output$sev_light <- renderPlot(plot_share_by(df(), "light_condition", "Severity by light condition"))
  output$sev_road  <- renderPlot(plot_share_by(df(), "road_condition", "Severity by road condition"))
  output$sev_tbl   <- renderDT(tbl(df() %>% group_by(Severity = severity) %>%
    summarise(Accidents = n(), Total_Casualties = sum(casualties), Avg_Casualties = round(mean(casualties), 2),
              Avg_Speed = round(mean(speed), 1), .groups = "drop")))

  # Risk assessment
  output$vb_risk <- renderValueBox(valueBox(round(mean(df()$risk_score), 1), "Average risk score", icon("tachometer-alt"), "blue"))
  output$vb_crit <- renderValueBox(valueBox(sum(df()$risk_level == "Critical"), "Critical-risk accidents", icon("exclamation-circle"), "red"))
  output$vb_high <- renderValueBox(valueBox(sum(df()$risk_level == "High"), "High-risk accidents", icon("exclamation-triangle"), "orange"))
  output$risk_hist <- renderPlot(plot_risk_hist(df()))
  output$risk_lvl  <- renderPlot(plot_risk_levels(df()))
  output$risk_tbl  <- renderDT(tbl(df() %>% arrange(desc(risk_score)) %>% head(50) %>%
    select(accident_id, location, vehicle_type, speed, weather, road_condition, light_condition, risk_score, risk_level, severity)))

  # Prediction (model evaluation)
  output$vb_acc  <- renderValueBox(valueBox(paste0(round(100 * model_obj$accuracy, 1), "%"), "Model accuracy (test set)", icon("bullseye"), "green"))
  output$vb_base <- renderValueBox(valueBox(paste0(round(100 * model_obj$baseline, 1), "%"), "Baseline (guess biggest class)", icon("minus"), "yellow"))
  output$vb_rf   <- renderValueBox(valueBox("Random Forest", "200 trees, 80/20 split", icon("tree"), "blue"))
  output$imp    <- renderPlot(plot_importance(model_obj))
  output$cm     <- renderTable(as.data.frame.matrix(model_obj$confusion), rownames = TRUE)
  output$recall <- renderTable(data.frame(Severity = names(model_obj$recall), Recall = as.numeric(model_obj$recall)))

  # What-if calculator
  wi <- eventReactive(input$calc, {
    score <- calculate_risk_score(input$w_speed, input$w_weather, input$w_road, input$w_light, input$w_vehicle, input$w_nveh)
    list(score = score, level = as.character(risk_level(score)),
         pred = predict_severity(model_obj, input$w_vehicle, input$w_age, input$w_speed, input$w_weather,
                                 input$w_road, input$w_light, input$w_nveh, input$w_hour))
  }, ignoreNULL = FALSE)
  output$wi_score <- renderValueBox({
    r <- wi(); valueBox(paste0(r$score, " / 100"), paste("Risk level:", r$level), icon("tachometer-alt"),
                        c(Low = "green", Medium = "yellow", High = "orange", Critical = "red")[[r$level]])
  })
  output$wi_sev <- renderValueBox({
    r <- wi(); valueBox(r$pred$class, "Predicted severity", icon("car-crash"),
                        c(Minor = "green", Serious = "orange", Fatal = "red")[[r$pred$class]])
  })
  output$wi_prob <- renderPlot({
    p <- wi()$pred$probs
    ggplot(data.frame(Severity = factor(names(p), levels = names(sev_cols)), Probability = as.numeric(p)),
           aes(Severity, Probability, fill = Severity)) + geom_col() + scale_fill_manual(values = sev_cols) +
      geom_text(aes(label = paste0(round(100 * Probability), "%")), vjust = -.3) + ylim(0, 1) + theme(legend.position = "none")
  })
  output$wi_rec <- renderUI({
    input$calc
    isolate(tags$ul(lapply(recommend_for_conditions(input$w_speed, input$w_weather, input$w_road, input$w_light,
                                                    input$w_vehicle, input$w_age, input$w_nveh), tags$li)))
  })

  # Map and location ranking
  output$map <- renderLeaflet(make_accident_map(df(), input$map_sev))
  output$loc_plot <- renderPlot(plot_location(df()))
  output$loc_tbl  <- renderDT(tbl(location_ranking(df())))

  # Time analysis
  output$t_hour  <- renderPlot(plot_count_by(df(), "hour", "Accidents by hour"))
  output$t_day   <- renderPlot(plot_count_by(df(), "day_of_week", "Accidents by day"))
  output$t_month <- renderPlot(plot_count_by(df(), "month", "Accidents by month"))
  output$t_heat  <- renderPlot(plot_heatmap(df()))
  output$t_tbl   <- renderDT(tbl(danger_summary(df())))

  # Weather and vehicle analysis
  output$w_freq  <- renderPlot(plot_count_by(df(), "weather", "Accidents by weather"))
  output$w_share <- renderPlot(plot_share_by(df(), "weather", "Severity share by weather"))
  output$w_tbl   <- renderDT(tbl(group_summary(df(), "weather")))
  output$v_freq  <- renderPlot(plot_count_by(df(), "vehicle_type", "Accidents by vehicle"))
  output$v_share <- renderPlot(plot_share_by(df(), "vehicle_type", "Severity share by vehicle"))
  output$v_tbl   <- renderDT(tbl(group_summary(df(), "vehicle_type")))

  # Safety recommendations
  output$rec_data <- renderUI(tags$ul(lapply(dataset_recommendations(df()), tags$li)))
}

shinyApp(ui, server)
