#install.packages('here')
#install.packages("ggplot2")
#install.packages("magrittr")
#install.packages("incidence")
#install.packages("projections")
#install.packages("distcrete")

library(here)
library(ggplot2)
library(magrittr)
library(incidence)
library(projections)
library(distcrete)

simulate_cases <- readRDS(here("rds", "simulate_cases.rds"))

scripts <- list.files("scripts", pattern = "projections[.]R$", full.names = TRUE)
lapply(scripts, source)

casos <- read.csv(here("caso.csv"))
deaths_rj <- casos[(casos$state == "RJ" & casos$deaths > 0 & casos$place_type == "state"),]
deaths_rj <- deaths_rj[order(deaths_rj[,1]),]

dates <- c()
total_deaths <- 0
for (i in 1:nrow(deaths_rj)) {
  new_deaths <- deaths_rj[i, "deaths"] - total_deaths
  total_deaths <- total_deaths + new_deaths
  date_deaths <- rep(paste(deaths_rj[i, "date"]), new_deaths)
  dates <- c(dates, date_deaths)
}

clean_date <- as.Date(unlist(dates))
clean_date <- clean_date[!is.na(clean_date)]
clean_date <- clean_date[order(clean_date)]
date_range <- clean_date[length(clean_date)] - clean_date[1]
date_death = clean_date

result <- simulate_cases(date_death,
               R = 2,
               cfr = 0.02,
               duration = 1,
               n_sim = 100)

projections <- result$projections
projections_df <- as.data.frame(projections)
last_row <- projections_df[nrow(projections_df) - 1,]
x <- unlist(tail(as.list(last_row), -1))

quantile(x, probs = c(0.01, 0.05, 0.95, 0.99))

