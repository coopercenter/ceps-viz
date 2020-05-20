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
  series_id=c(paste0("ELEC.GEN.ALL-VA-99.A"),
              paste0("ELEC.GEN.HYC-VA-99.A"),
              paste0("ELEC.GEN.SUN-VA-99.A"),
              paste0("ELEC.GEN.TSN-VA-99.A"),
              paste0("ELEC.GEN.DPV-VA-99.A"),
              paste0("ELEC.GEN.NUC-VA-99.A")),
  fuel=c("total_generation",
         "hydro_generation",
         "utility_solar_generation",
         "all_solar_generation",
         "distributed_solar_generation",
         "nuclear_generation"))
series_list$fuel<-as.character(series_list$fuel)


VA_annual_energy_generation <- NULL

for(row in 1:nrow(series_list)){
  table <- series_list[row,"series_id"]
  fuel <- series_list[row,"fuel"]
  
  dt <- get_EIA_series(eiaKey,table)
  setnames(dt,old="value",new=fuel)
  
  if (is.null(VA_annual_energy_generation))
  {VA_annual_energy_generation <- dt}
  else
  {VA_annual_energy_generation <-  merge(VA_annual_energy_generation, dt[], by ="year", all=TRUE)}
  
}

VA_annual_energy_generation[is.na(VA_annual_energy_generation)]<-0

# Finding sum of total annual renewable generation
VA_annual_energy_generation<- VA_annual_energy_generation%>%
  mutate(renewable_generation=(hydro_generation+all_solar_generation))

# Finding total annual renewable generation as a percent of total energy generation
VA_annual_energy_generation<- VA_annual_energy_generation%>%
  mutate(percent_renewable=((renewable_generation/total_generation)*100))

# Finding sum of total annual carbon-free generation
VA_annual_energy_generation<- VA_annual_energy_generation%>%
  mutate(carbon_free_generation=hydro_generation+all_solar_generation+nuclear_generation)

# Finding total annual carbon-free generation as a percent of total energy generation
VA_annual_energy_generation<- VA_annual_energy_generation%>%
  mutate(percent_carbon_free=((carbon_free_generation/total_generation)*100))

# Graphing % of VA power generation (in GWh/yr) from renewables & carbon-free sources 
percent_renewable_and_carbon_free<-VA_annual_energy_generation[,c(1,9,11)]
melted_percent_renewable_and_carbon_free<-melt(percent_renewable_and_carbon_free,id="year")

source(here::here("ggplot2","viz_functions.R"))

percent_renewable_and_carbon_free_line<-line_figure(melted_percent_renewable_and_carbon_free,"% of Total Annual VA Energy Generation","% of VA Energy Generation (GWh/yr) from Renewables and Carbon-Free Sources",annual=TRUE,x_label="Year",subtitle_name=NULL,lower_limit=0)
percent_renewable_and_carbon_free_line

path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="percent_renewable_and_carbon_free_line.png")



# Solar, Hydro, and Nuclear Generation over Time
carbon_free_generation_by_type<-VA_annual_energy_generation[,c(1,2,3,5,7)]
melted_generation <- melt(carbon_free_generation_by_type,id="year")

annual_carbon_free_generation_by_type_line<-line_figure(melted_generation,"Annual VA Energy Generation (GWh)","Annual Energy Generation in VA by Fuel Type",annual=TRUE,x_label="Year",subtitle_name=NULL,lower_limit=0)
annual_carbon_free_generation_by_type_line

ggsave(path=path2graphics, filename="annual_carbon_free_generation_by_type_line.png")



# Solar (broken into distributed and utility), Hydro, and Nuclear Generation over Time
carbon_free_generation_by_type2<-VA_annual_energy_generation[,c(1,2,3,4,6,7)]
melted_generation2 <- melt(carbon_free_generation_by_type2,id="year")

annual_carbon_free_generation_by_type_line2<-line_figure(melted_generation2,"Annual VA Energy Generation (GWh)","Annual Energy Generation in VA by Type",annual=TRUE,x_label="Year",subtitle_name=NULL,lower_limit=0)
annual_carbon_free_generation_by_type_line2

ggsave(path=path2graphics, filename="annual_carbon_free_generation_by_type_line2.png")



#Stacked Annual Carbon Free Generation Broken Out by Type
carbon_free_generation_by_type3<-VA_annual_energy_generation[,c(1,3,5,7)]
melted_generation3 <- melt(carbon_free_generation_by_type3,id="year")

carbon_free_generation_by_type_stacked<-stacked_area_figure(melted_generation3,"Annual VA Energy Generation (GWh)","Annual Energy Generation in VA by Type",annual=TRUE,x_label="Year",subtitle_name=NULL,lower_limit=0)
carbon_free_generation_by_type_stacked

ggsave(path=path2graphics, filename="carbon_free_generation_by_type_stacked.png")



# Stacked Annual Renewable Generation Broken Out by Type (hydro, utility solar, distributed solar)
renewable_generation_by_type<-VA_annual_energy_generation[,c(1,3,4,6)]
melted_generation4 <- melt(renewable_generation_by_type,id="year")

renewable_generation_by_type_stacked<-stacked_area_figure(melted_generation4,"Annual VA Energy Generation (GWh)","Annual Renewable Generation in VA by Type",annual=TRUE,x_label="Year",subtitle_name=NULL,lower_limit=0)
renewable_generation_by_type_stacked

ggsave(path=path2graphics, filename="renewable_generation_by_type_stacked.png")



# Stacked Renewable versus Non-renewable Generation
VA_annual_energy_generation<-VA_annual_energy_generation%>%
  mutate(non_renewable_generation=(total_generation-renewable_generation))
renewable_and_non_renewable<-VA_annual_energy_generation[,c(1,8,12)]
melted_renewable_and_non_renewable<-melt(renewable_and_non_renewable,id="year")

renewable_versus_non_renewable_stacked<-stacked_area_figure(melted_renewable_and_non_renewable,"Annual VA Energy Generation (GWh)","Annual Renewable and Non-Renewable Generation in VA",annual=TRUE,x_label="Year",subtitle_name=NULL,lower_limit=0)
renewable_versus_non_renewable_stacked

ggsave(path=path2graphics, filename="renewable_versus_non_renewable_stacked.png")



# Stacked Carbon versus Carbon Free Generation
VA_annual_energy_generation<-VA_annual_energy_generation%>%
  mutate(carbon_generation=(total_generation-carbon_free_generation))
carbon_and_carbon_free<-VA_annual_energy_generation[,c(1,10,13)]
melted_carbon_and_carbon_free<-melt(carbon_and_carbon_free,id="year")

carbon_versus_carbon_free_stacked<-stacked_area_figure(melted_carbon_and_carbon_free,"Annual VA Energy Generation (GWh)","Annual Carbon and Carbon Free Generation in VA",annual=TRUE,x_label="Year",subtitle_name=NULL,lower_limit=0)
carbon_versus_carbon_free_stacked

ggsave(path=path2graphics, filename="carbon_versus_carbon_free_stacked.png")



# Total Renewable and Total Carbon Free Generation Over Time
renewable_and_carbon_free<-VA_annual_energy_generation[,c(1,2,8,10)]
melted_renewable_and_carbon_free<-melt(renewable_and_carbon_free,id="year")

renewable_and_carbon_free_line<-line_figure(melted_renewable_and_carbon_free,"Annual VA Energy Generation (GWh)","Annual Energy Generation in VA by Type",annual=TRUE,x_label="Year",subtitle_name=NULL,lower_limit=0)
renewable_and_carbon_free_line

ggsave(path=path2graphics, filename="renewable_and_carbon_free_line.png")


