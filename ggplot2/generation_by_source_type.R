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
  fuel=c("total_production",
         "hydro_production",
         "utility_solar_production",
         "all_solar_production",
         "distrib_solar_production",
         "nuclear_production"))
series_list$fuel<-as.character(series_list$fuel)


VA_annual_energy_production_mwh <- NULL

for(row in 1:nrow(series_list)){
  table <- series_list[row,"series_id"]
  fuel <- series_list[row,"fuel"]
  
  dt <- get_EIA_series(eiaKey,table)
  setnames(dt,old="value",new=fuel)
  
  if (is.null(VA_annual_energy_production_mwh))
  {VA_annual_energy_production_mwh <- dt}
  else
  {VA_annual_energy_production_mwh <-  merge(VA_annual_energy_production_mwh, dt[], by ="year", all=TRUE)}
  
}

# Converting units to GWh
VA_annual_energy_production_gwh<-VA_annual_energy_production_mwh[,`:=`(total_production=total_production/1000,
                                                                       hydro_production=hydro_production/1000,
                                                                       nuclear_production=nuclear_production/1000,
                                                                       all_solar_production=all_solar_production/1000,
                                                                       utility_solar_production=utility_solar_production/1000,
                                                                       distrib_solar_production=distrib_solar_production/1000)]

VA_annual_energy_production_gwh[is.na(VA_annual_energy_production_gwh)]<-0

# Finding sum of total annual renewable production
VA_annual_energy_production_gwh<- VA_annual_energy_production_gwh%>%
  mutate(renewable_production=(hydro_production+all_solar_production))

# Finding total annual renewable production as a percent of total energy production
VA_annual_energy_production_gwh<- VA_annual_energy_production_gwh%>%
  mutate(percent_renewable=((renewable_production/total_production)*100))

# Finding sum of total annual carbon-free production
VA_annual_energy_production_gwh<- VA_annual_energy_production_gwh%>%
  mutate(carbon_free_production=hydro_production+all_solar_production+nuclear_production)

# Finding total annual carbon-free production as a percent of total energy production
VA_annual_energy_production_gwh<- VA_annual_energy_production_gwh%>%
  mutate(percent_carbon_free=((carbon_free_production/total_production)*100))

# Graphing % of VA power production (in GWh/yr) from renewables & carbon-free sources 
percent_renewable_and_carbon_free<-VA_annual_energy_production_gwh[,c(1,9,11)]
melted_percent_renewable_and_carbon_free<-melt(percent_renewable_and_carbon_free,id="year")
names(melted_percent_renewable_and_carbon_free)[c(2,3)]<-c("source_type","percentage_energy_production")

percent_renewable_and_carbon_free_line<-ggplot(data=melted_percent_renewable_and_carbon_free,mapping=aes(x=year,y=percentage_energy_production,color=source_type))+
  geom_line()+
  geom_point()+
  xlab("Year")+ylab("% of Total Annual VA Power Production")+
  scale_color_discrete(name=NULL,breaks=c("percent_carbon_free",
                                          "percent_renewable"),labels=c("Carbon Free (hydro,solar,wind,nuclear)","Renewable (hydro,solar,wind)"))+
  labs(title="% of VA Power Production (in GWh/yr) from Renewables and Carbon-Free Sources")

percent_renewable_and_carbon_free_line
path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="percent_renewable_and_carbon_free_line.png")

# Solar, Hydro, and Nuclear Generation over Time
carbon_free_generation_by_type<-VA_annual_energy_production_gwh[,c(1,2,3,5,7)]
melted_generation <- melt(carbon_free_generation_by_type,id="year")
names(melted_generation)[c(2,3)]<-c("source_type","annual_energy_production_gwh")

annual_carbon_free_generation_by_type_line <- ggplot(data=melted_generation,mapping=aes(x=year,y=annual_energy_production_gwh,color=source_type, shape=source_type))+
  geom_line()+
  geom_point()+
  xlab("Year")+ylab("Annual VA Power Production (GWh)")+
  scale_color_discrete(name=NULL,breaks=c("total_production",
                                          "nuclear_production",
                                          "hydro_production",
                                          "all_solar_production"),labels=c("Total Energy Production",
                                                                           "Nuclear Production",
                                                                           "Hydro Production",
                                                                           "Solar Production"))+
  scale_shape_manual(name=NULL,breaks=c("total_production",
                                        "nuclear_production",
                                        "hydro_production",
                                        "all_solar_production"),labels=c("Total Energy Production",
                                                                         "Nuclear Production",
                                                                         "Hydro Production",
                                                                         "Solar Production"),values=c(15,8,17,16))+
  labs(title="Annual Energy Generation in VA by Type (GWh)")

annual_carbon_free_generation_by_type_line
ggsave(path=path2graphics, filename="annual_carbon_free_generation_by_type_line.png")

# Solar (broken into distributed and utility), Hydro, and Nuclear Generation over Time
carbon_free_generation_by_type2<-VA_annual_energy_production_gwh[,c(1,2,3,4,6,7)]
melted_generation2 <- melt(carbon_free_generation_by_type2,id="year")
names(melted_generation2)[c(2,3)]<-c("source_type","annual_energy_production_gwh")

annual_carbon_free_generation_by_type_line2 <- ggplot(data=melted_generation2,mapping=aes(x=year,y=annual_energy_production_gwh,color=source_type, shape=source_type))+
  geom_line()+
  geom_point()+
  xlab("Year")+ylab("Annual VA Power Production (GWh)")+
  scale_color_discrete(name=NULL,breaks=c("total_production",
                                          "nuclear_production",
                                          "hydro_production",
                                          "utility_solar_production",
                                          "distrib_solar_production"),labels=c("Total Energy Production",
                                                                               "Nuclear Production",
                                                                               "Hydro Production",
                                                                               "Utility Solar Production",
                                                                               "Distributed Solar Production"))+
  scale_shape_manual(name=NULL,breaks=c("total_production",
                                        "nuclear_production",
                                        "hydro_production",
                                        "utility_solar_production",
                                        "distrib_solar_production"),labels=c("Total Energy Production",
                                                                             "Nuclear Production",
                                                                             "Hydro Production",
                                                                             "Utility Solar Production",
                                                                             "Distributed Solar Production"),values=c(15,8,17,16,4))+
  labs(title="Annual Energy Generation in VA by Type (GWh)")

annual_carbon_free_generation_by_type_line2
ggsave(path=path2graphics, filename="annual_carbon_free_generation_by_type_line2.png")

#Stacked Annual Carbon Free Generation Broken Out by Type
carbon_free_generation_by_type3<-VA_annual_energy_production_gwh[,c(1,3,5,7)]
melted_generation3 <- melt(carbon_free_generation_by_type3,id="year")
names(melted_generation3)[c(2,3)]<-c("source_type","annual_energy_production_gwh")

carbon_free_generation_by_type_stacked <- ggplot()+
  geom_area(data=melted_generation3,mapping=aes(x=year,y=annual_energy_production_gwh,fill=source_type))+
  xlab("Year")+ylab("Annual VA Power Production (GWh)")+
  scale_fill_discrete(name=NULL,breaks=c("hydro_production",
                                         "all_solar_production",
                                         "nuclear_production"),labels=c("Hydro Production",
                                                                        "Solar Production",
                                                                        "Nuclear Production"))+
  labs(title="Annual Carbon Free Generation in VA by Type (GWh)")

carbon_free_generation_by_type_stacked
ggsave(path=path2graphics, filename="carbon_free_generation_by_type_stacked.png")

# Stacked Annual Renewable Generation Broken Out by Type (hydro, utility solar, distributed solar)
renewable_generation_by_type<-VA_annual_energy_production_gwh[,c(1,3,4,6)]
melted_generation4 <- melt(renewable_generation_by_type,id="year")
names(melted_generation4)[c(2,3)]<-c("source_type","annual_energy_production_gwh")

renewable_generation_by_type_stacked <- ggplot()+
  geom_area(data=melted_generation4,mapping=aes(x=year,y=annual_energy_production_gwh,fill=source_type))+
  xlab("Year")+ylab("Annual VA Power Production (GWh)")+
  scale_fill_discrete(name=NULL,breaks=c("hydro_production",
                                         "utility_solar_production",
                                         "distrib_solar_production"),labels=c("Hydro Production",
                                                                              "Utility Solar Production",
                                                                              "Distributed Solar Production"))+
  labs(title="Annual Renewable Generation in VA by Type (GWh)")

renewable_generation_by_type_stacked
ggsave(path=path2graphics, filename="renewable_generation_by_type_stacked.png")


# Stacked Renewable versus Non-renewable Production
VA_annual_energy_production_gwh<-VA_annual_energy_production_gwh%>%
  mutate(non_renewable_production=(total_production-renewable_production))
renewable_and_non_renewable<-VA_annual_energy_production_gwh[,c(1,8,12)]
melted_renewable_and_non_renewable<-melt(renewable_and_non_renewable,id="year")
names(melted_renewable_and_non_renewable)[c(2,3)]<-c("source_type","annual_energy_production_gwh")

renewable_versus_non_renewable_stacked<-ggplot()+
  geom_area(data=melted_renewable_and_non_renewable,mapping=aes(x=year,y=annual_energy_production_gwh,fill=source_type))+
  xlab("Year")+ylab("Annual VA Power Production (GWh)")+
  scale_fill_discrete(name=NULL,breaks=c("renewable_production",
                                         "non_renewable_production"),labels=c("Renewable Production",
                                                                              "Non-Renewable Production"))+
  labs(title="Annual Renewable and Non-Renewable Generation in VA (GWh)")

renewable_versus_non_renewable_stacked
ggsave(path=path2graphics, filename="renewable_versus_non_renewable_stacked.png")

# Stacked Carbon versus Carbon Free Production
VA_annual_energy_production_gwh<-VA_annual_energy_production_gwh%>%
  mutate(carbon_production=(total_production-carbon_free_production))
carbon_and_carbon_free<-VA_annual_energy_production_gwh[,c(1,10,13)]
melted_carbon_and_carbon_free<-melt(carbon_and_carbon_free,id="year")
names(melted_carbon_and_carbon_free)[c(2,3)]<-c("source_type","annual_energy_production_gwh")

carbon_versus_carbon_free_stacked<-ggplot()+
  geom_area(data=melted_carbon_and_carbon_free,mapping=aes(x=year,y=annual_energy_production_gwh,fill=source_type))+
  xlab("Year")+ylab("Annual VA Power Production (GWh)")+
  scale_fill_discrete(name=NULL,breaks=c("carbon_free_production",
                                         "carbon_production"),labels=c("Carbon-Free Production",
                                                                       "Carbon Production"))+
  labs(title="Annual Carbon and Carbon Free Generation in VA (GWh)")

carbon_versus_carbon_free_stacked
ggsave(path=path2graphics, filename="carbon_versus_carbon_free_stacked.png")

# Total Renewable and Total Carbon Free Generation Over Time
renewable_and_carbon_free<-VA_annual_energy_production_gwh[,c(1,2,8,10)]
melted_renewable_and_carbon_free<-melt(renewable_and_carbon_free,id="year")
names(melted_renewable_and_carbon_free)[c(2,3)]<-c("source_type","annual_energy_production_gwh")

renewable_and_carbon_free_line<- ggplot(data=melted_renewable_and_carbon_free,mapping=aes(x=year,y=annual_energy_production_gwh,color=source_type, shape=source_type))+
  geom_line()+
  geom_point()+
  xlab("Year")+ylab("Annual VA Power Production (GWh)")+
  scale_color_discrete(name=NULL,breaks=c("total_production",
                                          "carbon_free_production",
                                          "renewable_production"),labels=c("Total Energy Production",
                                                                           "Carbon Free Production",
                                                                           "Renewable Production"))+
  scale_shape_manual(name=NULL,breaks=c("total_production",
                                        "carbon_free_production",
                                        "renewable_production"),labels=c("Total Energy Production",
                                                                         "Carbon Free Production",
                                                                         "Renewable Production"),values=c(15,8,17))+
  labs(title="Annual Energy Generation in VA by Type (GWh)")

renewable_and_carbon_free_line
ggsave(path=path2graphics, filename="renewable_and_carbon_free_line.png")


