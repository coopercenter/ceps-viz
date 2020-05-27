library(jsonlite)
library(eia)
library(here)
library(tidyverse)
library(stringr)
library(jsonlite)
library(data.table)
library(ggplot2)
library(dplyr)
library(RPostgreSQL)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

table="whole_electric_industry_capacity"
script  <- paste0("select * from ",table," ;")
capacity <- data.table(dbGetQuery(db,script))

table="va_gats_data"
script  <- paste0("select * from ",table," ;")
gats_data <- data.table(dbGetQuery(db,script))

dbDisconnect(db)

source(here::here("my_eia_api_key.R"))

get_EIA_series <- function(eiaKey,series_id) {
  require(jsonlite)
  require(data.table)
  # This function retrieves one EIA time-series with metadata
  # The function returns a list of parts of the series:
  #     seriesID,name,units,frequency,data (as data table)
  
  # eiaKey is your EIA API key
  eiaBase = paste0("http://api.eia.gov/series/?api_key=",eiaKey,"&series_id=") 
  
  vv = paste0(eiaBase,series_id)
  temp = readLines(vv, warn = "F")
  rd <- fromJSON(temp)
  print(paste0("Retrieving: ",rd$series$series_id))
  print(paste0(rd$series$name))
  
  # Now take the 'data' element from the list and make a data frame
  rd2 = data.frame(rd$series$data,stringsAsFactors = F)
  rd2 = data.table(rd2)
  
  setnames(rd2,1,"year"); setnames(rd2,2,'value')
  rd2[,value:=as.numeric(value)]
  rd2[,year:=as.numeric(year)]
  returnList = list(
    series_id = rd$series$series_id,
    name = rd$series$name,
    units = rd$series$units,
    frequency = rd$series$f,
    data = rd2
  )
  return(rd2) 
}

series_list = data.frame(
  series_id=c("ELEC.GEN.NUC-VA-99.A",
              "ELEC.GEN.SUN-VA-99.A",
              "ELEC.GEN.DPV-VA-99.A",
              "ELEC.GEN.TSN-VA-99.A",
              "ELEC.GEN.HYC-VA-99.A",
              "ELEC.GEN.ALL-VA-99.A"),
  fuel=c("nuclear",
         "utility_solar", #note utility_solar currently includes all utility-scale solar but this can be changed to just utility-scale PV if needed
         "distributed_solar",
         "all_solar",
         "hydropower",
         "total"))

series_list$fuel<-as.character(series_list$fuel)

va_generation <- NULL

for(row in 1:nrow(series_list)){
  table <- series_list[row,"series_id"]
  fuel <- series_list[row,"fuel"]
  
  dt <- get_EIA_series(eiaKey,table)
  setnames(dt,old="value",new=fuel)
  
  if (is.null(va_generation))
  {va_generation <- dt}
  else
  {va_generation <-  merge(va_generation, dt[], by ="year", all=TRUE)}
  
}

source(here::here("ggplot2","viz_functions.R"))

#plotting donut figure of progress towards renewable generation goal
renewable_percent_gen_2019 = va_generation[year==2019,(all_solar+hydropower)/total]
renewable_percent_gen_2030_goal = .3 #30% of Virginia’s electricity from renewables by 2030

renewable_donut_p <- donut_figure_p(renewable_percent_gen_2019,"2019","2.6%",renewable_percent_gen_2030_goal,"2030","30%","Renewable Generation","skyblue","steelblue")
renewable_donut_p

single_ring_renewable_donut_p <- single_ring_donut_figure_p(renewable_percent_gen_2019,"2019","2.6%",renewable_percent_gen_2030_goal,"2030","30%","Renewable Generation","skyblue","steelblue")
single_ring_renewable_donut_p

#plotting donut figure of progress towards carbon-free generation goal
carbon_free_percent_gen_2019 = va_generation[year==2019,(all_solar+hydropower+nuclear)/total]
carbon_free_percent_gen_2050_goal = 1 #100% of Virginia’s electricity from carbon-free sources by 2050

carbon_free_donut_p <- donut_figure_p(carbon_free_percent_gen_2019,"2019","32.9%",carbon_free_percent_gen_2050_goal,"2050","100%","Carbon-Free Generation","mediumseagreen","seagreen")
carbon_free_donut_p

single_ring_carbon_free_donut_p <- single_ring_donut_figure_p(carbon_free_percent_gen_2019,"2019","32.9%",carbon_free_percent_gen_2050_goal,"2050","100%","Carbon-Free Generation","mediumseagreen","seagreen")
single_ring_carbon_free_donut_p

#plotting donut figure of progess towards wind and solar capacity goals
solar_capacity_2018_mw = capacity[Year==2018,as.numeric(Solar)]
sw_capacity_2028_goal_mw = 5500 #5,500 MW of onshore wind and solar energy total [in operation] by 2028
sw_capacity_2030_goal_mw = 13600 #13,600 MW of onshore wind and solar energy total by 2030 (from 'Virginia Clean Economy progress dashboard -- UPDATED DRAFT')

solar_capacity_percent_2018 = solar_capacity_2018_mw/sw_capacity_2030_goal_mw
sw_capacity_percent_goal_2028 = sw_capacity_2028_goal_mw/sw_capacity_2030_goal_mw
sw_capacity_percent_goal_2030 = sw_capacity_2030_goal_mw/sw_capacity_2030_goal_mw

sw_capacity_donut_p <- donut_figure_p(solar_capacity_percent_2018,"2018","392.5 MW",sw_capacity_percent_goal_2028,"2028","5,500 MW in Operation","Wind & Solar Energy","lightcoral","indianred",sw_capacity_percent_goal_2030,"2030","13,600 MW Total","maroon")
sw_capacity_donut_p

single_ring_sw_capacity_donut_p <- single_ring_donut_figure_p(solar_capacity_percent_2018,"2018","392.5 MW",sw_capacity_percent_goal_2028,"2028","5,500 MW in Operation","Wind & Solar Energy","lightcoral","indianred",sw_capacity_percent_goal_2030,"2030","13,600 MW Total","maroon")
single_ring_sw_capacity_donut_p

#16,100 MW of solar and onshore wind by January 1, 2024 (from VCEA Summary 3.0)
va_gats_data <- gats_data[state=="VA"]
va_gats_data[,date:=as.Date(date)]

distributed_solar_nameplate <- va_gats_data[primary_fuel_type=="SUN"&pjm_unit=="No"&date>=2010-01-01,sum(nameplate)] #data more relevant after 2010
utility_solar_nameplate <- va_gats_data[primary_fuel_type=="SUN"&pjm_unit=="Yes"&date>=2010-01-01,sum(nameplate)]
nameplate_goal = 16100

sw_ring = data.frame(category=c("2019 distributed solar capacity","2019 utility solar capacity","additional onshore wind & solar necessary to reach target"),
                     value=c(distributed_solar_nameplate,utility_solar_nameplate,nameplate_goal-(distributed_solar_nameplate+utility_solar_nameplate)))

VCEA_single_ring_sw_capacity_donut_p <- plot_ly(textinfo="none",hoverinfo="label+value") %>%
  add_pie(data = sw_ring, values = ~value, labels = ~category, sort = F,hole = 0.7,
          domain = list(x = c(0, 1), y = c(0, 1)),
          marker=list(colors=c("lightcoral","indianred","maroon"),
                      line=list(color="white",width=1))) %>%
  layout(title=list(text="807.65 MW of Solar Capacity as of 2019",font = list(color="black",size = 15),x=0.5),showlegend = F) %>%
  add_annotations(x=0.5,y=0.5,text="Onshore Wind & Solar Capacity",showarrow=F,font = list(color = "black",size = 14)) %>%
  add_annotations(x=0.5,y=-0.1,text="16,100 MW of Onshore Wind & Solar Capacity by 2024",showarrow=F,font = list(color = "maroon",size = 15))
VCEA_single_ring_sw_capacity_donut_p
