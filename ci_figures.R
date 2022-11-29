library(data.table)
library(trending) # for PI calculation
library(ggplot2)

# From trending package vignette: https://cran.r-project.org/web/packages/trending/vignettes/prediction_intervals.html
# Once we have calculated the confidence interval on the response we feed the upper and lower bounds 
# into the quantile function associated with the relevant distribution.
# The maximum and minimum values of the output are then used as the upper and lower bounds of our prediction interval.

event_table <- fread("./data/spillover_data_v2.csv")
event_table <- event_table[!Pathogen %in% c("MERS-CoV", "MERS Coronavirus"),]
event_table <- event_table[Event_start_year < 2020,] # exclude Covid
event_table <- event_table[order(Event_start_year, Pathogen)]
event_table[Pathogen == "SARS Coronavirus 2"]

lossDatYear <- event_table[, list(reported_deaths = sum(reported_deaths),
                                  reported_cases = sum(reported_cases),
                                  reported_events = uniqueN(Event_name)),
                           by = .(Event_start_year)]
lossDatYear <- lossDatYear[,.SD[CJ(Event_start_year = seq(min(Event_start_year), max(Event_start_year), by = 1), 
                                   unique = T),  on = .(Event_start_year)]]
lossDatYear[is.na(reported_deaths), reported_deaths := 0]
lossDatYear[is.na(reported_cases), reported_cases := 0]
lossDatYear[is.na(reported_events), reported_events := 0]


x <- lossDatYear$Event_start_year
y_death <- lossDatYear$reported_deaths

death_mod <- glm_nb_model(y_death ~ x)

death_dat <- rbind(data.table(x = x, y_death = y_death), data.table(x = seq(2020, 2035, 1), y_death = NA))
fitted_death <- fit(death_mod, death_dat)

DeathPredDat <- predict(fitted_death, simulate_pi = FALSE, alpha = 0.05)
DeathPredDat
ggplot() + 
  geom_point(data = DeathPredDat,
             aes(x = x, y = y_death), size = 0.5) + 
  geom_line(data = DeathPredDat[x < 2020], aes(x = x, y = estimate), color = "navy")+
  geom_line(data = DeathPredDat[x >= 2020], aes(x = x, y = estimate), 
            color = "navy", linetype = "dotted")+
  geom_line(data = DeathPredDat,
            aes(x = x, y = lower_pi), color = "steelblue",  linetype = "dashed")+
  geom_line(data = DeathPredDat, 
            aes(x = x, y = upper_pi), color = "steelblue",  linetype = "dashed")+
  ylab("Reported deaths") + xlab("Year")+ coord_cartesian(ylim = c(0, 10000)) 
ggsave(filename = "figures/death_projection.jpeg", device = "jpeg", width = 4, height = 4, units = "in")

y_event <- lossDatYear$reported_events

event_mod <- glm_nb_model(y_event ~ x)

event_dat <- rbind(data.table(x = x, y_event = y_event), data.table(x = seq(2020, 2035, 1), y_event = NA))
fitted_event <- fit(event_mod, event_dat)

EventPredDat <- predict(fitted_event, simulate_pi = FALSE, alpha = 0.05)
EventPredDat
ggplot() + 
  geom_point(data = EventPredDat,
             aes(x = x, y = y_event), size = 0.5) + 
  geom_line(data = EventPredDat[x < 2020], aes(x = x, y = estimate), color = "navy")+
  geom_line(data = EventPredDat[x >= 2020], aes(x = x, y = estimate), 
            color = "navy", linetype = "dotted")+
  geom_line(data = EventPredDat,
            aes(x = x, y = lower_pi), color = "steelblue",  linetype = "dashed")+
  geom_line(data = EventPredDat, 
            aes(x = x, y = upper_pi), color = "steelblue",  linetype = "dashed")+
  ylab("Reported events") + xlab("Year")
ggsave(filename = "figures/event_projection.jpeg", device = "jpeg", width = 4, height = 4, units = "in")


comprss <- function(tx) {
  div <- findInterval(as.numeric(gsub("\\,", "", tx)),
                      c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
  paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2),
        c("","K","M","B","T")[div] )}

logybreak <- c(10^seq(0, 8, 1))
logylab <- comprss(logybreak)

ggplot() + 
  geom_point(data = DeathPredDat,
             aes(x = x, y = y_death), size = 0.5) + 
  geom_line(data = DeathPredDat[x < 2020], aes(x = x, y = estimate), color = "navy")+
  geom_line(data = DeathPredDat[x >= 2020], aes(x = x, y = estimate), 
            color = "navy", linetype = "dotted")+
  geom_line(data = DeathPredDat,
            aes(x = x, y = lower_pi), color = "steelblue",  linetype = "dashed")+
  geom_line(data = DeathPredDat, 
            aes(x = x, y = upper_pi), color = "steelblue",  linetype = "dashed")+
  ylab("Reported deaths") + xlab("Year")+

  geom_point(aes(x = 2020, y = 1928508), color = "red")+
  geom_point(aes(x = 2021, y = 3521649), color = "red")+
  scale_y_log10(labels = logylab, breaks = logybreak) 
ggsave(filename = "figures/death_projection_covid.jpeg", device = "jpeg", width = 4, height = 4, units = "in")
