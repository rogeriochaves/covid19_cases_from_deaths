#install.packages('here')
#install.packages("ggplot2")
#install.packages("magrittr")
#install.packages("incidence")
#install.packages("projections")
#install.packages("distcrete")
#install.packages("rjson")

library(here)
library(ggplot2)
library(magrittr)
library(incidence)
library(projections)
library(distcrete)
library(rjson)

simulate_cases <- readRDS(here("rds", "simulate_cases.rds"))

scripts <- list.files("scripts", pattern = "projections[.]R$", full.names = TRUE)
lapply(scripts, source)

states_with_death <- casos[(casos$deaths > 0 & casos$place_type == "state"),"state"]
states_with_death <- unique(states_with_death)

estimates <- data.frame("RJ",0,0,0,0, stringsAsFactors=FALSE)
names(estimates) <- c("state", "1%", "5%", "95%", "99%")

for (state in states_with_death) {
  casos <- read.csv(here("caso.csv"))
  deaths_state <- casos[(casos$state == state & casos$deaths > 0 & casos$place_type == "state"),]
  deaths_state <- deaths_state[order(deaths_state[,1]),]
  
  dates <- c()
  total_deaths <- 0
  for (i in 1:nrow(deaths_state)) {
    new_deaths <- deaths_state[i, "deaths"] - total_deaths
    total_deaths <- total_deaths + new_deaths
    date_deaths <- rep(paste(deaths_state[i, "date"]), new_deaths)
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
  
  quantiles <- quantile(x, probs = c(0.01, 0.05, 0.95, 0.99))
  estimates[nrow(estimates) + 1,] = c(state, quantiles[1], quantiles[2], quantiles[3], quantiles[4])
}

estimates = estimates[-1,]
estimates

json <- toJSON(unname(split(estimates, 1:nrow(estimates))))
json

write(json, file = "result.json")
