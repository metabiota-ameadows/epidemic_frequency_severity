library(data.table)
# updates: 
# 1) excluded non-spillover events 
# 2) used global sums instead of summing AL0 data (AL0 data was needed for g20 report because we applied underreporting correction)
# 3) including event end year... might allocate cases to event end year instead of event start year

included_pathogens <- c("Zaire ebolavirus", "SARS-CoV", "MERS Coronavirus", 
                        "MERS-CoV","Marburg virus",
                        "Machupo virus", "Sudan ebolavirus", "Nipah virus", 
                        "SARS Coronavirus 2",
                        "Bundibugyo ebolavirus", "Tai Forest ebolavirus")

# exclude non-spillover events (i.e lab accidents or reintroductions)
exclude_events <- c("Sudan ebolavirus_United Kingdom_1976", "Zaire ebolavirus_Russia_1996", 
                    "SARS-CoV_Singapore_2003", "SARS-CoV_Taiwan_2003",
                    "Zaire ebolavirus_Liberia_2015a", "Zaire ebolavirus_Liberia_2015b")

event_table <- fread("./data/20220712_event_table.csv")

event_table <- event_table[AL0_CODE == "global"] # only interested in global counts

event_table[, Pathogen := unlist(sapply(FUN = strsplit, 
                                        X = EVENT_NAME, split = "_"))[1], by = EVENT_NAME]
event_table[EVENT_NAME == "Novel coronavirus_China_2019", Pathogen := "SARS Coronavirus 2"]
event_table[, Spark_Country := unlist(sapply(FUN = strsplit, 
                                             X = EVENT_NAME, split = "_"))[2], by = EVENT_NAME]
event_table[, Spark_ISO2 := countrycode::countrycode(sourcevar = Spark_Country, 
                                                     origin = "country.name", 
                                                     destination = "iso2c")]

# subset to included pathogens, but exclude certain events not pertaining to spillover
event_table <- event_table[Pathogen %in% included_pathogens]
event_table <- event_table[!(EVENT_NAME %in% exclude_events),]

event_table[, DATE_START_ALL := as.Date(DATE_START_ALL)]
event_table[, DATE_END_ALL := as.Date(DATE_END_ALL)]
event_table[, Event_start_year := format(DATE_START_ALL, "%Y")]
event_table[, Event_end_year := format(DATE_END_ALL, "%Y")]
event_table[is.na(CUML_DEATHS_ALL), CUML_DEATHS_ALL := 0]

event_table <- event_table[Event_start_year >= 1960, ]
event_table <- event_table[, .(Event_name = EVENT_NAME, Pathogen, Event_start_year, 
                               Event_end_year, reported_cases = CUML_CASES_ALL, reported_deaths = CUML_DEATHS_ALL)]
fwrite(event_table, "data/spillover_data_v2.csv")

