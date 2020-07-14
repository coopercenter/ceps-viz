library(here)

source(here::here("derived_values","consumption_by_sector_calculations.R")) #sourcing in data and reformatted data tables & calculations ready to serve as input to viz functions
source(here::here("ggplot2","viz_functions.R")) #sourcing in viz functions


#plotting monthly consumption by sector
consumption_by_sector_monthly_line<-line_figure(list(lf_consumption_by_sector_monthly),
                                                merge_variable = "date",
                                                value_unit="Consumption(GWh)",
                                                title_name = "VA Monthly Electricity Consumption by Sector",
                                                return_static = F,
                                                subtitle_description="2001-2019")
consumption_by_sector_monthly_line

consumption_by_sector_monthly_stacked_area<-stacked_area_figure(list(lf_consumption_by_sector_monthly),
                                                                merge_variable = "date",
                                                                value_unit="Consumption(GWh)",
                                                                title_name = "VA Monthly Electricity Consumption by Sector",
                                                                return_static = F,
                                                                subtitle_description="2001-2019")
consumption_by_sector_monthly_stacked_area
 

#plotting annual consumption by sector
consumption_by_sector_annual_line<-line_figure(list(lf_consumption_by_sector_annual),
                                                merge_variable = "year",
                                                value_unit="Consumption(GWh)",
                                                title_name = "VA Annual Electricity Consumption by Sector",
                                                return_static = F,
                                                subtitle_description="2001-2018")
consumption_by_sector_annual_line 

consumption_by_sector_annual_stacked_area<-stacked_area_figure(list(lf_consumption_by_sector_annual),
                                                                merge_variable = "year",
                                                                value_unit="Consumption(GWh)",
                                                                title_name = "VA Annual Electricity Consumption by Sector",
                                                                return_static = F,
                                                                subtitle_description="2001-2018")
consumption_by_sector_annual_stacked_area


#plotting 2019 monthly consumption by sector
consumption_by_sector_2019_line<-line_figure(list(lf_consumption_by_sector_monthly[year(date)=="2019"]),
                                               merge_variable = "date",
                                               value_unit="Consumption (GWh)",
                                               title_name = "VA 2019 Monthly Electricity Consumption by Sector",
                                               x_label="Date",
                                               return_static = F)
consumption_by_sector_2019_line

consumption_by_sector_2019_stacked_area<-stacked_area_figure(list(lf_consumption_by_sector_monthly[year(date)=="2019"]),
                                             merge_variable = "date",
                                             value_unit="Consumption (GWh)",
                                             title_name = "VA 2019 Monthly Electricity Consumption by Sector",
                                             x_label="Date",
                                             return_static = F)
consumption_by_sector_2019_stacked_area







