pkgs <- c("shiny","shinydashboard","ggplot2","dplyr","tidyr","leaflet","randomForest","DT")
new <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
cat("All packages installed.\n")
