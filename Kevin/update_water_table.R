setwd("~/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/weather_file_creation")

source("weatherFunctions.R")
source("methods.R")
library(hisafer)
library(tidyverse)

##### BASE RESTINCLIERES WEATHER #####
## Data sources:
## Restinclieres Chatau/Plots: temp, humidity, rain, wind
## Lavalette: radiation
## Fixed: CO2
## File appendix refers to when the data was last updated
base.wth <- read_csv(paste0("./restinclieres_weather_base-1994-2018_20180416.csv"),
                     guess_max = 10000,
                     col_types = cols())

## Clean up errors
#ggplot(base.wth, aes(x = JULIAN, y = RG)) + geom_line() + facet_wrap(~YR)
base.wth$RG[base.wth$YR == 1996 & base.wth$MONTH == 12 & base.wth$DAY == 3] <- 5
base.wth$RG[base.wth$YR == 2008 & base.wth$MONTH == 3 & base.wth$DAY == 31] <- 5
base.wth$RG[base.wth$YR == 2008 & base.wth$MONTH == 4 & base.wth$DAY == 4] <- 5
base.wth$RG[base.wth$YR == 2015 & base.wth$MONTH == 9 & base.wth$DAY == 11] <- 5
base.wth$RG[base.wth$YR == 2007 & base.wth$RG > 35] <- 30

names(base.wth) <- c("doy", "year", "month", "day", "Tmax", "Tmin", "RHmax", "RHmin", "Rg", "precip", "wind", "watertable", "CO2")

conv.wth <- base.wth %>%
  mutate(Date = paste(year, month, day, sep = "-")) %>%
  select(Date, everything(), -CO2) %>%
  as.data.frame()

##### PIEZOMETER DATA #####
piezo.info <- read_csv("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-calibration/raw_data/restinclieres_piezometer_info.csv",
                       col_types = cols()) %>%
  select(-tube.height.orig, -tube.height.6603)

piezo <- read_csv("/Users/kevinwolz/Desktop/RESEARCH/ACTIVE_PROJECTS/HI-SAFE/hisafe-calibration/raw_data/restinclieres_piezometer_data.csv",
                  col_types = cols()) %>%
  left_join(piezo.info, by = c("plot", "rowtree.id")) %>%
  mutate(depth = pmin(-(depth - tube.height) / 100, 0)) %>%
  rename(watertable = depth) %>%
  mutate(date  = mdy(date)) %>%
  mutate(doy   = yday(date)) %>%
  mutate(year  = year(date)) #%>%
#mutate(month = month(date)) %>%
#mutate(day   = day(date))


##### CALCULATE WATER TABLE #####
for(PLOT in paste0("A", 2:4)) {
  old.data <- read_weather(paste0("./OLD/restinclieres_", PLOT, "-1994-2018_OLD.wth"))

  out <- update_water_table(conv.wth, PLOT)

  new.data <- base.wth %>%
    as_tibble() %>%
    mutate(watertable = out$watertable) #%>%
  #write_weather(paste0("./NEW/restinclieres_", PLOT, "-1994-2018.wth"))

  old.data <- old.data %>%
    mutate(type = "OLD")
  new.data <- new.data %>%
    mutate(type = "NEW")

  comb.data <- bind_rows(old.data, new.data) %>%
    mutate(type = factor(type, levels = c("OLD", "NEW")))

  comp.plot <- ggplot(comb.data, aes(x        = doy,
                                     y        = watertable,
                                     color    = type,
                                     linetype = type)) +
    scale_y_continuous(limits = c(-7, 0)) +
    geom_line() +
    facet_wrap(~year) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_color_manual(values    = c("black", "red"))
  ggsave_fitmax(paste0("./", PLOT, "_Comparison.png"), comp.plot)
}


##### MEASURED vs. MODELED #####
for(PLOT in paste0("A", 2:4)) {
  old.data <- read_weather(paste0("./OLD/restinclieres_", PLOT, "-1994-2018_OLD.wth"))
  out <- update_water_table(conv.wth, PLOT)
  new.data <- base.wth %>%
    as_tibble() %>%
    mutate(watertable = pmin(0, out$watertable))

  piezo.data <- piezo %>%
    filter(plot == PLOT)

  MVM.plot <- ggplot(new.data, aes(x = doy, y = watertable)) +
    labs(color = "Location") +
    scale_y_continuous(limits = c(-7, 0)) +
    facet_wrap(~year) +
    geom_line() +
    geom_point(data = piezo.data, aes(color = rowtree.id))
    #scale_color_manual(values    = c("black", "red"))
  ggsave_fitmax(paste0("./", PLOT, "_MvM.png"), MVM.plot)

}

##### PLOT CHECK #####
# new.data %>%
#   group_by(year) %>%
#   summarize(rain = sum(precip),
#             wt   = sum(watertable)) %>%
#   ggplot(aes(x = rain, y = wt, label = year)) + geom_text()

##### FUNCTIONS #####
recompute_nappe = function(dati){
  etpGen <- getETP( (dati$tmax+dati$tmin)/2,
                    dati$rg,
                    dati$wind,
                    getDoy(dati$year,dati$month,dati$day),
                    (dati$hrmax+dati$hrmin)/2,
                    latitude = 43.6)
  nappe <- predNappe(-300, dati$rain, etpGen, a=1.4e-05, b=2.25, l=120, m=6, n=0.6, ru=300, prof=540)
  dati$water <- nappe/100
  return(dati)
}

nappe_isomorphism <- function(dati, parcel_name){
  # compute isomorphism from parcel A2 to one among parcels A1, A3, A4 in Restinclieres (copied from an Excel file)
  fluctuations <- data.frame(name = c("A1","A2","A3","A4"),
                             min  = c(-1,-3,-1,-0.5),
                             max  = c(-4,-6,-4,-3),
                             a    = numeric(4),
                             b    = numeric(4))
  fluctuations$a <- (fluctuations$max - fluctuations$min) / (fluctuations$max[fluctuations$name == "A3"] - fluctuations$min[fluctuations$name == "A3"])
  fluctuations$b <- (fluctuations$min - (fluctuations$a * fluctuations$min[fluctuations$name == "A3"]) )
  dati$water <- dati$water * fluctuations$a[fluctuations$name == parcel_name] + fluctuations$b[fluctuations$name == parcel_name]
  return (dati)
}

update_water_table <- function(dati, parcel_name) {
  names(dati) <- c("Date", "DOY","year","month","day","tmax","tmin","hrmax","hrmin","rg","rain","wind","water")
  dati_water <- recompute_nappe(dati)
  dati_water_isomorph <- nappe_isomorphism(dati_water, parcel_name)
  names(dati_water_isomorph) <- c("Date", "doy", "year", "month", "day", "Tmax", "Tmin", "RHmax", "RHmin", "Rg", "precip", "wind", "watertable")
  return(dati_water_isomorph)
}
