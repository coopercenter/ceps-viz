library(here)
library(tidyverse)
library(stringr)
library(jsonlite)
library(data.table)
library(ggplot2)
library(dplyr)
library(scales)
library(eia)
library(zoo)
library(RPostgreSQL)
library(lubridate)
library(plotly)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#load in metadata
metadata <- data.table(dbGetQuery(db,"select * from metadata ;"))

dbDisconnect(db)

source(here::here("derived_values","capacity_factor_calculations.R"))

#-------------------------------PLOTTING CAPACITY FACTORS VISUALLY-----------------------------------------------

source(here::here("ggplot2","viz_functions.R"))

####mean capacity factor of coal
mean_coal_cf_2019<- capacity_factors[year(date)==2019,mean(coal_cf)]
mean_coal_cf_2014<- capacity_factors[year(date)==2014,mean(coal_cf)]

####data tables for coal
mean_coal_cf_2019_data_table<-data.table(year=2019,variable=c("coal", " "),value=c(mean_coal_cf_2019, 100-mean_coal_cf_2019))
mean_coal_cf_2014_data_table<-data.table(year=2014,variable=c("coal", " "),value=c(mean_coal_cf_2014, 100-mean_coal_cf_2014))


####2019 Pie chart
title_name<- "Coal Average Capacity Factor 2019"
theme_colors <- c("orange","white")
coal_cf_2019_piechart <- plot_ly(mean_coal_cf_2019_data_table,labels=~variable,values=~value,type='pie',textinfo="percent",hoverinfo="none",insidetextfont = list(color = 'white'),marker=list(colors=theme_colors),sort=F) %>%
  layout(title=list(text=title_name,x=0.55),showlegend=F,annotations=list(x=0.5,y=-0.1,text=paste0("<i>","U.S. Energy Information Administration","</i>"),showarrow=F,xref='paper',yref='paper',font=list(size=10))) 
coal_cf_2019_piechart


######2014 Pie chart
title_name<- "Coal Average Capacity Factor 2014"
theme_colors <- c("orange","white")
coal_cf_2014_piechart <- plot_ly(mean_coal_cf_2014_data_table,labels=~variable,values=~value,type='pie',textinfo="percent",hoverinfo="none",insidetextfont = list(color = 'white'),marker=list(colors=theme_colors),sort=F) %>%
  layout(title=list(text=title_name,x=0.55),showlegend=F,annotations=list(x=0.5,y=-0.1,text=paste0("<i>","U.S. Energy Information Administration","</i>"),showarrow=F,xref='paper',yref='paper',font=list(size=10))) 
coal_cf_2014_piechart


#######Coal and Natural Gas Capacity Factors Over Time
lf_coal_cf_and_gas_cf<- melt(capacity_factors[,.(date,coal_cf,natural_gas_cf)],id="date")
coal_cf_and_gas_cf_over_time<-line_figure(list(lf_coal_cf_and_gas_cf),"date","Capacity Factor","Coal and Natural Gas Capacity Factors Over Time",
                        list("eia_elec_gen_cow_va_99_m","eia_elec_gen_ng_va_99_m"),return_static = F)
coal_cf_and_gas_cf_over_time
coal_cf_and_gas_cf_over_time_p<- ggplotly_wrapper(coal_cf_and_gas_cf_over_time)
coal_cf_and_gas_cf_over_time_p
####Coal and Natural Gas Capacity Factors Over Time Faceted
coal_cf_and_gas_cf_over_time+facet_grid(rows=vars(variable), scales="free")


lf_all_capacity_factors<-melt(capacity_factors[,.(date,biomass_cf,coal_cf,natural_gas_cf,nuclear_cf,petroleum_liquids_cf,hydroelctric_cf, wood_cf)],id="date")
mean_coal_cf_2019<- capacity_factors[year(date)==2019,mean(coal_cf)]
mean_biomass_cf_2019<- capacity_factors[year(date)==2019,mean(biomass_cf)]
mean_natural_gas_cf_2019<- capacity_factors[year(date)==2019,mean(natural_gas_cf)]
mean_nuclear_cf_2019<- capacity_factors[year(date)==2019,mean(nuclear_cf)]
mean_petroleum_liquids_cf_2019<- capacity_factors[year(date)==2019,mean(petroleum_liquids_cf)]
mean_hydroelctric_cf_2019<- capacity_factors[year(date)==2019,mean(hydroelctric_cf)]
mean_wood_cf_2019<- capacity_factors[year(date)==2019,mean(wood_cf)]
###data table excluding biomass bc value doesn't make sense
mean_all_cf_2019_data_table<-data.table(year=2019,variable=c("coal", "natural gas","nuclear", "petroleum liquids", "hydroelectric","wood","wind"),
                                        value=c(mean_coal_cf_2019,mean_natural_gas_cf_2019,mean_nuclear_cf_2019,mean_petroleum_liquids_cf_2019,mean_hydroelctric_cf_2019,mean_wood_cf_2019,44.8))

###bar graph of average capacity factor of all fuel types in 2019
capicity_factors_2019_bar<-ggplot(data=mean_all_cf_2019_data_table, aes(x=variable, y=value)) + 
  geom_bar(stat="identity")+xlab("Fuel Type")+ylab("Capacity Factor")+
  labs(title="Average Capacity Factor in 2019",caption="Wind data is estimated")
capicity_factors_2019_bar

###load in offshore wind
net_capacity_factor_offshore_wind <- data.table(dbGetQuery(db,"select * from net_capacity_factor_offshore_wind ;"))

###Offshore wind net capacity factors over time

lf_net_cf_offshore_wind<- melt(net_capacity_factor_offshore_wind[,.(Year,Pilot,Stage_I,Stage_II,Stage_III)],id="Year")
net_cf_offshore_wind_line<-line_figure(list(lf_net_cf_offshore_wind),"Year", "Capacity Factor", "Forcasted Net Capacity Factor of Offshore Wind",
                                       list("net_capacity_factor_offshore_wind"),return_static = F)
net_cf_offshore_wind_line
net_cf_offshore_wind_line_p<- ggplotly_wrapper(net_cf_offshore_wind_line)
net_cf_offshore_wind_line_p

###Offshore wind net capacity over time diff y axis limits
net_cf_offshore_wind_line_axis<-line_figure(list(lf_net_cf_offshore_wind),"Year", "Capacity Factor", "Forcasted Net Capacity Factor of Offshore Wind",lower_limit=35,upper_limit=45,
                                       list("net_capacity_factor_offshore_wind"),return_static = F)
net_cf_offshore_wind_line_axis
net_cf_offshore_wind_line_axis_p<-ggplotly_wrapper(net_cf_offshore_wind_line_axis)
net_cf_offshore_wind_line_axis_p

