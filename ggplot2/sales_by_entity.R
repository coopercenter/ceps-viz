library(here)

source(here::here("derived_values","sales_by_entity_calculations.R"))
source(here::here("ggplot2","viz_functions.R"))

#plotting total electricity generation
total_sales_2019 <- line_figure(list(lf_2019_monthly_sales),"month","Sales (GWh)","Virginia Monthly Electricity Sales (2019)",x_label = "Month",return_static = F, modifications=scale_x_discrete(limits=month.abb))
total_sales_2019
path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="total_sales_2019.png")

total_sales_2018<-line_figure(list(lf_2018_monthly_sales),"month","Sales (GWh)","Virginia Monthly Electricity Sales (2018)",x_label = "Month",return_static = F, modifications=scale_x_discrete(limits=month.abb))
total_sales_2018
ggsave(path=path2graphics, filename="total_sales_2018.png")

total_sales_annual<-line_figure(list(lf_total_sales_annual),"year","Sales (GWh)","VA Annual Total Electricity Sales",return_static = F,subtitle_description = "2001-2019")
total_sales_annual
ggsave(path=path2graphics, filename="total_sales_annual.png")

total_sales_monthly<-line_figure(list(lf_total_sales_monthly),"date","Sales (GWh)","VA Monthly Total Electricity Sales",return_static = F,subtitle_description = "2001-2019")
total_sales_monthly
ggsave(path=path2graphics, filename="total_sales_monthly.png")


#plotting electricity generation by load-serving entity
sales_by_entity_2019_line<-line_figure(list(lf_monthly_sales_by_entity_2019),"month","Sales (GWh)","2019 VA Monthly Electricity Sales",x_label="Month", modifications= scale_x_discrete(limits=month.abb),subtitle_description ="By Load Serving Entity")
sales_by_entity_2019_line
ggsave(path=path2graphics, filename="sales_by_entity_2019_line.png")

sales_by_entity_2018_line<-line_figure(list(lf_monthly_sales_by_entity_2018),"month","Sales (GWh)","2018 VA Monthly Electricity Sales",x_label="Month", modifications= scale_x_discrete(limits=month.abb),subtitle_description ="By Load Serving Entity")
sales_by_entity_2018_line
ggsave(path=path2graphics, filename="sales_by_entity_2018_line.png")

monthly_sales_by_entity_line<-line_figure(list(lf_monthly_sales_by_entity),"date","Sales (GWh)","VA Monthly Electricity Sales",subtitle_description = "By Load Serving Entity, 2001-2019")
monthly_sales_by_entity_line
ggsave(path=path2graphics, filename="monthly_sales_by_entity_line.png")

annual_sales_by_entity_line<-line_figure(list(lf_annual_sales_by_entity),"year","Sales (GWh)","VA Annual Electricity Sales",subtitle_description="By Load Serving Entity, 2001-2019")
annual_sales_by_entity_line
ggsave(path=path2graphics, filename="annual_sales_by_entity_line.png")


#plotting electricity generation by load-serving entity with stacked area
sales_by_entity_2019_stacked_area <- stacked_area_figure(list(lf_monthly_sales_by_entity_2019),"month","Sales (GWh)","2019 VA Monthly Electricity Sales",x_label="Month", modifications= scale_x_discrete(limits=month.abb),subtitle_description ="By Load Serving Entity")
sales_by_entity_2019_stacked_area
ggsave(path=path2graphics, filename="sales_by_entity_2019_stacked_area.png")

sales_by_entity_2018_stacked_area <- stacked_area_figure(list(lf_monthly_sales_by_entity_2018),"month","Sales (GWh)","2018 VA Monthly Electricity Sales",x_label="Month", modifications= scale_x_discrete(limits=month.abb),subtitle_description ="By Load Serving Entity")
sales_by_entity_2018_stacked_area
ggsave(path=path2graphics, filename="sales_by_entity_2018_stacked_area.png")

monthly_sales_by_entity_stacked_area <- stacked_area_figure(list(lf_monthly_sales_by_entity),"date","Sales (GWh)","VA Monthly Electricity Sales",subtitle_description ="By Load Serving Entity, 2001-2019")
monthly_sales_by_entity_stacked_area
ggsave(path=path2graphics, filename="monthly_sales_by_entity_stacked_area.png")

annual_sales_by_entity_stacked_area<-stacked_area_figure(list(lf_annual_sales_by_entity),"year","Sales (GWh)","VA Annual Electricity Sales",subtitle_description="By Load Serving Entity, 2001-2019")
annual_sales_by_entity_stacked_area
ggsave(path=path2graphics, filename="annual_sales_by_entity_stacked_area.png")






