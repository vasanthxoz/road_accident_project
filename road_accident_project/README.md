# Road Accident Risk Assessment and Severity Prediction System Using R

## Abstract
Road accidents cause heavy loss of life every year. This project analyses road accident records using R to find
when, where and why severe accidents happen. Accidents are classified as Minor, Serious or Fatal, a Random Forest
model predicts severity from accident details, and a 0-100 risk score (Low / Medium / High / Critical) is calculated
from speed, weather, road, light and vehicle factors. An R Shiny dashboard presents statistics, charts, an
interactive Leaflet map, trend analysis, a what-if calculator and automatic safety recommendations to support
better road-safety decisions.

## Objectives
1. Clean and analyse road accident data using R.
2. Classify accidents into Minor, Serious and Fatal.
3. Build and evaluate a machine-learning model to predict severity.
4. Calculate an accident risk score and risk level.
5. Identify dangerous hours/days/months and accident-prone roads.
6. Study the effect of weather, light, road condition and vehicle type.
7. Show accidents on an interactive map and analyse monthly/yearly trends.
8. Generate automatic safety recommendations.
9. Deliver everything in a simple Shiny dashboard.

## Modules
| Module | File |
|---|---|
| Data cleaning and preprocessing | `R/01_data_preprocessing.R` |
| Risk score engine | `R/02_risk_score.R` |
| Machine learning (Random Forest) | `R/03_model.R` |
| Statistics, tables and ggplot2 charts | `R/04_analysis_plots.R` |
| Interactive map (leaflet) | `R/05_map.R` |
| Safety recommendations | `R/06_recommendations.R` |
| Shiny UI + server (11 tabs) | `app.R` |
| Sample data generator | `data/generate_sample_data.R` |

## Folder structure
```
road_accident_project/
├── app.R                      # Shiny UI + server (run this)
├── install_packages.R         # installs required packages
├── README.md
├── data/
│   ├── generate_sample_data.R # creates the sample dataset
│   └── road_accidents.csv     # created automatically on first run
├── model/
│   └── rf_model.rds           # trained model (created automatically)
└── R/                         # modules 01-06 listed above
```

## Install and run (RStudio)
1. Unzip the folder and open `app.R` in RStudio.
2. Run once in the console: `source("install_packages.R")`
3. Click **Run App** (top-right of the editor). First launch creates the data and trains the model (~10-20 seconds).

## Dataset
The project reads `data/road_accidents.csv`. A **synthetic sample** (6,000 records, 2019-2023, 10 roads, ~2% missing
values added on purpose) is generated automatically so the app runs immediately. For the final submission, use a
real dataset (for example the UK STATS19 road safety data on data.gov.uk, "US Accidents" on Kaggle, or India's
road accident data on data.gov.in), rename its columns to the format below, save as `data/road_accidents.csv`,
and delete `model/rf_model.rds` so the model is retrained.

| Column | Example | Notes |
|---|---|---|
| accident_id | 101 | unique id |
| date | 2022-08-14 | YYYY-MM-DD |
| time | 21:35 | HH:MM |
| latitude, longitude | 11.018, 76.990 | for the map |
| location | Avinashi Road | road / area name |
| vehicle_type | Motorcycle | Car, Motorcycle, Bus, Truck, Other |
| driver_age | 28 | years |
| speed | 75 | km/h |
| weather | Rain | Clear, Cloudy, Rain, Fog, Storm |
| road_condition | Wet | Dry, Wet, Damaged, Oily |
| light_condition | Dark-Unlit | Daylight, Dusk, Dark-Lit, Dark-Unlit |
| num_vehicles | 2 | vehicles involved |
| casualties | 1 | injured + killed |
| severity | Serious | Minor, Serious, Fatal |

If your dataset has other category names, edit the point tables in `R/02_risk_score.R`.
If it has no severity column, use `classify_severity(killed, seriously_injured)` in `R/01_data_preprocessing.R`.

## How it works
* **Risk score (0-100):** speed (max 40) + weather (20) + road condition (15) + light (10) + vehicle type (10) + vehicles involved (5).
  Levels: Low < 25, Medium 25-49, High 50-74, Critical 75+.
* **Model:** Random Forest (200 trees), 80/20 train-test split, evaluated with accuracy, confusion matrix and
  per-class recall, compared against a "guess the biggest class" baseline. `casualties` is not used as an input
  because it would reveal the answer.
* **Location ranking:** severity score = Minor x1 + Serious x3 + Fatal x5 per road.
* **Trend:** a straight line fitted to monthly counts shows if accidents are increasing or decreasing.

## Future enhancements
* Add XGBoost / SVM models and compare them; handle class imbalance (SMOTE).
* Live weather and traffic APIs; real-time risk alerts.
* Black-spot clustering (DBSCAN) and heat-map layers on the map.
* Time-series forecasting (ARIMA / Prophet) for future accident counts.
* PDF report download, user login, and a cloud deployment (shinyapps.io).
* Mobile app / SMS alerts for high-risk roads and hours.
