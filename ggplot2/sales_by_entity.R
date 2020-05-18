library(data.table)
library(lubridate)
library(dbConnect)
library("RPostgreSQL")
library(ggplot2)
library(scales)
library(here)

read_sheet <- function(path2xls, sheet=1, cols) {
  require(readxl)
  thisData = data.table(read_excel(path2xls, 
                                   sheet=sheet,
                                   cell_cols(cols)))
  return(thisData)
}

path2forecast <- here::here("ggplot2","elec_sales_forecasts_2019-12.xlsx")

annual_sales  = read_sheet(path2forecast,sheet=2,cols=1:18)
monthly_sales = read_sheet(path2forecast,cols=1:22)
monthly_sales[date>=2020-01-01,va_total_sales_fc_gwh:=dom_total_exdc_fc_gwh+dom_dc_sales_fc_gwh+apco_total_fc_gwh+ros_total_fc_gwh]

#plotting total electricity generation
total_sales_2019 <- ggplot()+
  geom_line(data=monthly_sales[year=="2019"],mapping=aes(month,va_total_sales_gwh))+
  ylab("Sales (GWh)")+xlab(NULL)+scale_x_discrete(limits=month.abb)+
  labs(title = "2019 VA Monthly Total Electricity Sales")
total_sales_2019

path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="total_sales_2019.png")

total_sales_2018 <- ggplot()+
  geom_line(data=monthly_sales[year=="2018"],mapping=aes(month,va_total_sales_gwh))+
  ylab("Sales (GWh)")+xlab(NULL)+scale_x_discrete(limits=month.abb)+
  labs(title = "2018 VA Monthly Total Electricity Sales")
total_sales_2018

ggsave(path=path2graphics, filename="total_sales_2018.png")

total_sales_annual <- ggplot()+
  geom_line(data=annual_sales[year<"2020"],mapping=aes(year,va_total_sales_gwh))+
  ylab("Sales (GWh)")+xlab(NULL)+
  labs(title = "VA Annual Total Electricity Sales", subtitle = "2001-2019")
total_sales_annual

ggsave(path=path2graphics, filename="total_sales_annual.png")

total_sales_monthly <- ggplot()+
  geom_line(data=monthly_sales[year<"2020"],mapping=aes(date,va_total_sales_gwh))+
  ylab("Sales (GWh)")+xlab(NULL)+
  labs(title = "VA Monthly Total Electricity Sales", subtitle = "2001-2019")
total_sales_monthly

ggsave(path=path2graphics, filename="total_sales_monthly.png")

#plotting electricity generation by load-serving entity
lf_monthly_sales_by_entity <-melt(monthly_sales[year<"2020",.(date,month,year,apco_total_gwh,ros_total_gwh,dom_total_gwh)],id=c("date","month","year"))
setnames(lf_monthly_sales_by_entity,old=c("variable","value"),new=c("load_serving_entity","sales_gwh"))

sales_by_entity_2019_line <- ggplot()+
  geom_line(data=lf_monthly_sales_by_entity[year=="2019"],mapping=aes(x=month,y=sales_gwh,color=load_serving_entity))+
  scale_color_discrete(name="Load Serving Entity",breaks=c("apco_total_gwh",
                                                           "dom_total_gwh",
                                                           "ros_total_gwh"),labels=c("APCO","Dominion","Rest-of-state"))+
  ylab("Sales(GWh)")+xlab(NULL)+scale_x_discrete(limits=month.abb)+
  labs(title="2019 VA Monthly Electricity Sales",subtitle = "By Load Serving Entity")
sales_by_entity_2019_line

ggsave(path=path2graphics, filename="sales_by_entity_2019_line.png")

sales_by_entity_2018_line <- ggplot()+
  geom_line(data=lf_monthly_sales_by_entity[year=="2018"],mapping=aes(x=month,y=sales_gwh,color=load_serving_entity))+
  scale_color_discrete(name="Load Serving Entity",breaks=c("apco_total_gwh",
                                                           "dom_total_gwh",
                                                           "ros_total_gwh"),labels=c("APCO","Dominion","Rest-of-state"))+
  ylab("Sales(GWh)")+xlab(NULL)+scale_x_discrete(limits=month.abb)+
  labs(title="2018 VA Monthly Electricity Sales",subtitle = "By Load Serving Entity")
sales_by_entity_2018_line

ggsave(path=path2graphics, filename="sales_by_entity_2018_line.png")

monthly_sales_by_entity_line <- ggplot()+
  geom_line(data=lf_monthly_sales_by_entity,mapping=aes(x=date,y=sales_gwh,color=load_serving_entity))+
  scale_color_discrete(name="Load Serving Entity",breaks=c("apco_total_gwh",
                                                           "dom_total_gwh",
                                                           "ros_total_gwh"),labels=c("APCO","Dominion","Rest-of-state"))+
  ylab("Sales(GWh)")+xlab(NULL)+
  labs(title="VA Monthly Electricity Sales",subtitle = "By Load Serving Entity, 2001-2019")
monthly_sales_by_entity_line

ggsave(path=path2graphics, filename="monthly_sales_by_entity_line.png")

lf_annual_sales_by_entity <-melt(annual_sales[year<"2020",.(year,apco_total_gwh,ros_total_gwh,dom_total_gwh)],id=c("year"))
setnames(lf_annual_sales_by_entity,old=c("variable","value"),new=c("load_serving_entity","sales_gwh"))

annual_sales_by_entity_line <- ggplot()+
  geom_line(data=lf_annual_sales_by_entity,mapping=aes(x=year,y=sales_gwh,color=load_serving_entity))+
  scale_color_discrete(name="Load Serving Entity",breaks=c("apco_total_gwh",
                                                           "dom_total_gwh",
                                                           "ros_total_gwh"),labels=c("APCO","Dominion","Rest-of-state"))+
  ylab("Sales(GWh)")+xlab(NULL)+
  labs(title="VA Annual Electricity Sales",subtitle = "By Load Serving Entity, 2001-2019")
annual_sales_by_entity_line

ggsave(path=path2graphics, filename="annual_sales_by_entity_line.png")

#plotting electricity generation by load-serving entity with stacked area
sales_by_entity_2019_stacked_area <- ggplot()+
  geom_area(data=lf_monthly_sales_by_entity[year=="2019"],mapping=aes(x=month,y=sales_gwh,fill=load_serving_entity))+
  scale_fill_discrete(name="Load Serving Entity",breaks=c("apco_total_gwh",
                                                           "dom_total_gwh",
                                                           "ros_total_gwh"),labels=c("APCO","Dominion","Rest-of-state"))+
  ylab("Sales(GWh)")+xlab(NULL)+scale_x_discrete(limits=month.abb)+
  labs(title="2019 VA Monthly Electricity Sales",subtitle = "By Load Serving Entity")
sales_by_entity_2019_stacked_area

ggsave(path=path2graphics, filename="sales_by_entity_2019_stacked_area.png")

sales_by_entity_2018_stacked_area <- ggplot()+
  geom_area(data=lf_monthly_sales_by_entity[year=="2018"],mapping=aes(x=month,y=sales_gwh,fill=load_serving_entity))+
  scale_fill_discrete(name="Load Serving Entity",breaks=c("apco_total_gwh",
                                                          "dom_total_gwh",
                                                          "ros_total_gwh"),labels=c("APCO","Dominion","Rest-of-state"))+
  ylab("Sales(GWh)")+xlab(NULL)+scale_x_discrete(limits=month.abb)+
  labs(title="2018 VA Monthly Electricity Sales",subtitle = "By Load Serving Entity")
sales_by_entity_2018_stacked_area

ggsave(path=path2graphics, filename="sales_by_entity_2018_stacked_area.png")

monthly_sales_by_entity_stacked_area <- ggplot()+
  geom_area(data=lf_monthly_sales_by_entity,mapping=aes(x=date,y=sales_gwh,fill=load_serving_entity))+
  scale_fill_discrete(name="Load Serving Entity",breaks=c("apco_total_gwh",
                                                           "dom_total_gwh",
                                                           "ros_total_gwh"),labels=c("APCO","Dominion","Rest-of-state"))+
  ylab("Sales(GWh)")+xlab(NULL)+
  labs(title="VA Monthly Electricity Sales",subtitle = "By Load Serving Entity, 2001-2019")
monthly_sales_by_entity_stacked_area

ggsave(path=path2graphics, filename="monthly_sales_by_entity_stacked_area.png")

annual_sales_by_entity_stacked_area <- ggplot()+
  geom_area(data=lf_annual_sales_by_entity,mapping=aes(x=year,y=sales_gwh,fill=load_serving_entity))+
  scale_fill_discrete(name="Load Serving Entity",breaks=c("apco_total_gwh",
                                                           "dom_total_gwh",
                                                           "ros_total_gwh"),labels=c("APCO","Dominion","Rest-of-state"))+
  ylab("Sales(GWh)")+xlab(NULL)+
  labs(title="VA Annual Electricity Sales",subtitle = "By Load Serving Entity, 2001-2019")
annual_sales_by_entity_stacked_area

ggsave(path=path2graphics, filename="annual_sales_by_entity_stacked_area.png")





