library(jsonlite)
library(eia)
library(RPostgres)
library(tidyverse)
library(here)
library(stringr)
library(jsonlite)
library(data.table)
library(ggplot2)
library(eia)

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
  series_id=c(paste0("EMISS.CO2-TOTV-TT-CO-VA.A"),
              paste0("EMISS.CO2-TOTV-TT-NG-VA.A"),
              paste0("EMISS.CO2-TOTV-TT-PE-VA.A")),
  fuel=c("coal_emissions",
         "natural_gas_emissions",
         "petroleum_emissions"))
series_list$fuel<-as.character(series_list$fuel)


VA_emissions_by_fuel <- NULL

for(row in 1:nrow(series_list)){
  table <- series_list[row,"series_id"]
  fuel <- series_list[row,"fuel"]
  
  dt <- get_EIA_series(eiaKey,table)
  setnames(dt,old="value",new=fuel)
  
  if (is.null(VA_emissions_by_fuel))
  {VA_emissions_by_fuel <- dt}
  else
  {VA_emissions_by_fuel <-  merge(VA_emissions_by_fuel, dt[], by ="year", all=TRUE)}
  
}

source(here::here("ggplot2","viz_functions.R"))

melted_emissions <- melt(VA_emissions_by_fuel[,.(year,coal_emissions,natural_gas_emissions,petroleum_emissions)],id="year")

emissions_by_fuel_type_line<-line_figure(melted_emissions,"million metric tons CO2","CO2 Emissions by Fuel Type")
emissions_by_fuel_type_line

