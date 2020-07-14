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
source <- metadata[db_table_name=="eia_elec_gen_cow_va_99_m",data_source_full_name] #doesn't matter which table is used-it's all EIA data
source_full <- paste("Source:",source)

####2019 Coal Average Capacity Factor Pie chart
title_name<- "Coal Average Capacity Factor 2019"
theme_colors <- c("orange","white")
coal_cf_2019_piechart <- plot_ly(mean_coal_cf_2019_data_table,labels=~variable,values=~value,type='pie',textinfo="percent",hoverinfo="none",insidetextfont = list(color = 'white'),marker=list(colors=theme_colors),sort=F) %>%
  layout(title=list(text=title_name,x=0.55),showlegend=F,
add_annotations(x=0.5,y=-0.13,text=paste0(bottom_description,"<br>","<i>","<sub>",source_full,"<sub>","</i>"),showarrow=F,xref='paper',yref='paper',font=list(size=10))) 
coal_cf_2019_piechart
path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="capacity_factor_coal_2019.png")

######2014 Pie chart
title_name<- "Coal Average Capacity Factor 2014"
theme_colors <- c("orange","white")
coal_cf_2014_piechart <- plot_ly(mean_coal_cf_2014_data_table,labels=~variable,values=~value,type='pie',textinfo="percent",hoverinfo="none",insidetextfont = list(color = 'white'),marker=list(colors=theme_colors),sort=F) %>%
  layout(title=list(text=title_name,x=0.55),showlegend=F,
         add_annotations(x=0.5,y=-0.13,text=paste0(bottom_description,"<br>","<i>","<sub>",source_full,"<sub>","</i>"),showarrow=F,xref='paper',yref='paper',font=list(size=10))) 
coal_cf_2014_piechart
path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="capacity_factor_coal_2014.png")

#######Coal and Natural Gas Capacity Factors Over Time
coal_cf_and_gas_cf_over_time<-line_figure(list(lf_coal_cf_and_gas_cf),"date","Capacity Factor","Coal and Natural Gas Capacity Factors Over Time",
                        list("eia_elec_gen_cow_va_99_m","eia_elec_gen_ng_va_99_m","generator_2018","generator_2017","generator_2016","generator_2015","generator_2014","generator_2019"),return_static = F)
coal_cf_and_gas_cf_over_time
coal_cf_and_gas_cf_over_time_p<- ggplotly_wrapper(coal_cf_and_gas_cf_over_time)
coal_cf_and_gas_cf_over_time_p
path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="coal_gas_capacity_factors.png")

###bar graph of average capacity factor of all fuel types in 2019
capicity_factors_2019_bar<-ggplot(data=mean_all_cf_2019_data_table, aes(x=Variable, y=value, fill=Variable)) + 
  geom_bar(stat="identity")+xlab("Fuel Type")+ylab("Capacity Factor")+
  labs(title="Average Capacity Factor in 2019",caption=source_full,subtitle="Wind data is estimated")
capicity_factors_2019_bar
path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="capacity_factors_2019_bar.png")

###Offshore wind net capacity factors over time
net_cf_offshore_wind_line<-line_figure(list(lf_net_cf_offshore_wind),"Year", "Capacity Factor", "Forcasted Net Capacity Factor of Offshore Wind",
                                       list("net_capacity_factor_offshore_wind"),return_static = F)
net_cf_offshore_wind_line
net_cf_offshore_wind_line_p<- ggplotly_wrapper(net_cf_offshore_wind_line)
net_cf_offshore_wind_line_p
path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="forcasted_net_capacity_factor_offshore_wind.png")

###Offshore wind net capacity over time diff y axis limits
net_cf_offshore_wind_line_axis<-line_figure(list(lf_net_cf_offshore_wind),"Year", "Capacity Factor", "Forcasted Net Capacity Factor of Offshore Wind",lower_limit=35,upper_limit=45,
                                       list("net_capacity_factor_offshore_wind"),return_static = F)
net_cf_offshore_wind_line_axis
net_cf_offshore_wind_line_axis_p<-ggplotly_wrapper(net_cf_offshore_wind_line_axis)
net_cf_offshore_wind_line_axis_p
path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="forcasted_net_capacity_factor_offshore_wind_smaller_axis.png")
