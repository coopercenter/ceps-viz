library(here)
library(tidyverse)
library(stringr)
library(jsonlite)
library(data.table)
library(ggplot2)
library(eia)
library(scales)

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

series_list_gen = data.frame(
  series_id=c("ELEC.GEN.COW-VA-99.A",
              "ELEC.GEN.PEL-VA-99.A",
              "ELEC.GEN.NG-VA-99.A",
              "ELEC.GEN.NUC-VA-99.A",
              "ELEC.GEN.SUN-VA-99.A",
              "ELEC.GEN.DPV-VA-99.A",
              "ELEC.GEN.HYC-VA-99.A",
              "ELEC.GEN.WWW-VA-99.A",
              "ELEC.GEN.WAS-VA-99.A"),
  fuel=c("coal",
         "oil",
         "gas",
         "nuclear",
         "utility_solar", #note utility_solar currently includes all utility-scale solar but this can be changed to just utility-scale PV if needed
         "distributed_solar",
         "hydropower",
         "wood",
         "other_biomass"))

series_list_gen$fuel<-as.character(series_list_gen$fuel)

#building data table `va_annual_generation` by merging data on generation by several fuel types
va_annual_generation <- NULL

for(row in 1:nrow(series_list_gen)){
  table <- series_list_gen[row,"series_id"]
  fuel <- series_list_gen[row,"fuel"]
  
  dt <- get_EIA_series(eiaKey,table)
  setnames(dt,old="value",new=fuel)
  
  if (is.null(va_annual_generation))
  {va_annual_generation <- dt}
  else
  {va_annual_generation <-  merge(va_annual_generation, dt[], by ="year", all=TRUE)}
}

series_list_con = data.frame(
  series_id=c("SEDS.TERCB.VA.A",
              "SEDS.TECCB.VA.A",
              "SEDS.TEICB.VA.A",
              "SEDS.TEACB.VA.A"),
  sector=c("residential",
           "commercial",
           "industrial",
           "transportation")
)

series_list_con$sector<-as.character(series_list_con$sector)

#building data table `va_annual_consumption` by merging data on consumption by sector
va_annual_consumption <- NULL

for(row in 1:nrow(series_list_con)){
  table <- series_list_con[row,"series_id"]
  sector <- series_list_con[row,"sector"]
  
  dt <- get_EIA_series(eiaKey,table)
  setnames(dt,old="value",new=sector)
  
  if (is.null(va_annual_consumption))
  {va_annual_consumption <- dt}
  else
  {va_annual_consumption <-  merge(va_annual_consumption, dt[], by ="year", all=TRUE)}
}

source(here::here("ggplot2","viz_functions.R"))

#production figures:
lf_va_annual_generation <- melt(va_annual_generation,id="year")

va_annual_production_area = stacked_area_figure(lf_va_annual_generation,"GWh","VA Annual Generation",subtitle_name = "By Fuel Type")
va_annual_production_area

path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="va_annual_production_area.png")

va_annual_production_2019_pie_chart = pie_chart_figure(lf_va_annual_generation[year==2019],"VA 2019 Generation",percent_label_size = 0) #setting percent_label_size = 0 to remove percent labels because slivers are so small

#finding location of labels and percent label for each fuel type so that labels for the larger pie slices (gas and nuclear) can be manually added
va_2019_gen = lf_va_annual_generation[year==2019]

va_2019_gen <- va_2019_gen %>% 
  arrange(desc(variable)) %>%
  mutate(prop = value / sum(va_2019_gen$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )
va_2019_gen=data.table(va_2019_gen)

va_annual_production_2019_pie_chart = va_annual_production_2019_pie_chart +
  geom_text(aes(y=va_2019_gen[variable=="gas",ypos],label=paste0(as.character(va_2019_gen[variable=="gas",round(prop,1)]),"%")),color="white",size=4)+
  geom_text(aes(y=va_2019_gen[variable=="nuclear",ypos],label=paste0(as.character(va_2019_gen[variable=="nuclear",round(prop,1)]),"%")),color="white",size=4) 
va_annual_production_2019_pie_chart

ggsave(path=path2graphics, filename="va_annual_production_2019_pie_chart.png")

#consumption figures: 
lf_va_annual_consumption <- melt(va_annual_consumption,id="year")

va_annual_consumption_area = stacked_area_figure(lf_va_annual_consumption,"Billion Btu","VA Annual Consumption",subtitle_name = "By Sector") + scale_y_continuous(labels = comma)
va_annual_consumption_area

ggsave(path=path2graphics, filename="va_annual_consumption_area.png")

va_annual_consumption_2017_pie_chart = pie_chart_figure(lf_va_annual_consumption[year==2017],"VA 2017 Consumption")
va_annual_consumption_2017_pie_chart

ggsave(path=path2graphics, filename="va_annual_consumption_2017_pie_chart.png")






  