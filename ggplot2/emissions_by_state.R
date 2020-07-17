if(!("jsonlite" %in% installed.packages()))  install.packages("jsonlite")
if(!("eia" %in% installed.packages()))       install.packages("eia")
if(!("RPostgres" %in% installed.packages())) install.packages("RPostgres")
library(here)
library(tidyverse)
library(stringr)
library(jsonlite)
library(data.table)
library(ggplot2)
library(eia)
library(RPostgreSQL)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#load in metadata
metadata <- data.table(dbGetQuery(db,"select * from metadata ;"))

dbDisconnect(db)

source(here::here("my_eia_api_key.R"))
source(here::here("ggplot2","viz_functions.R"))

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

states = c("AK","AL","AR","AZ","CA",
           "CO","CT","DE","FL","GA",
           "HI","IA","ID","IL","IN",
           "KS","KY","LA","MA","MD",
           "ME","MI","MN","MO","MS",
           "MT","NC","ND","NE","NH",
           "NJ","NM","NV","NY","OH",
           "OK","OR","PA","RI","SC",
           "SD","TN","TX","UT","VA",
           "VT","WA","WI","WV","WY")

#I used just the first two states below to test the code while I was writing it to avoid waiting for all the datasets to be retrieved from EIA
#states = c("AK","AL") 

for(state in states){
  series_id = paste0("EMISS.CO2-TOTV-TT-TO-",state,".A")
  
  dt <- get_EIA_series(eiaKey,series_id)
  setnames(dt,old="value",new="CO2_emissions")
  
  lf_dt <- melt(dt,id="year")
  
  emissions_by_state_figure <- line_figure(list(lf_dt),
                                           "year","Emissions (Million Metric Tons CO2)",paste(state,"Annual CO2 Emissions"),
                                           list("eia_emiss_co2_totv_ec_to_va_a"),
                                           modifications=theme(legend.position = "none")) #same source for all, so choosing random data table that will pull correct source for all
  
  figure_name <- str_to_lower(paste(state,"emissions_figure",sep="_"))
  assign(figure_name,emissions_by_state_figure) #could eventually utilize ggsave() here, but wanted to wait until code was finalized
  
  dt_name <- str_to_lower(paste("eia_emissions",state,"a",sep="_"))
  assign(dt_name,dt)
}
