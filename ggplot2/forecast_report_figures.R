library(here)

source(here::here("derived_values","forecast_report_calculations.R"))
source(here::here("ggplot2","viz_functions.R"))

#plotting total monthly statewide sales since 1990
total_monthly_sales <- line_figure(list(lf_total_va_utility_sales),
                                   "date","Sales (GWh)","Virginia Total Monthly Sales",
                                   list("va_utility_sales"),
                                   modifications=theme(legend.position = "none"))
total_monthly_sales

path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="total_monthly_sales.png")

#plotting annual percent change in sales (statewide and by utility)
annual_change_by_utility <- line_figure(list(lf_annual_sales_change_by_utility),
                                        "year","Percent Change","Virginia Electricity Sales Annual Percent Change",
                                        list("va_utility_sales"),lower_limit =-5)
annual_change_by_utility

ggsave(path=path2graphics, filename="annual_change_by_utility.png")

annual_change_by_utility_facet <- ggplot()+
  geom_line(data=lf_annual_sales_change_by_utility,mapping=aes(x=year,y=value,color=variable))+
  facet_grid(rows=vars(variable),scales="free")+
  xlab("Year")+ylab("Percent Change")+
  scale_color_manual(name=NULL,breaks=c("apco","dom","ros","total"),labels=c("APCO","Dominion","Rest of State","Total"),values=ceps_pal[1:4])+
  labs(title="Annual Percent Change in VA Electricity Sales")+
  theme_ceps()
annual_change_by_utility_facet

ggsave(path=path2graphics, filename="annual_change_by_utility_facet.png")

annual_change_by_utility_bar <- ggplot()+
  geom_bar(data=lf_annual_sales_change_by_utility,mapping=aes(x=year,y=value,fill=variable),position = "dodge", stat="identity")+
  xlab("Year")+ylab("Percent Change")+
  scale_fill_manual(name=NULL,breaks=c("apco","dom","ros","total"),labels=c("APCO","Dominion","Rest of State","Total"),values=ceps_pal[1:4])+
  labs(title="Annual Percent Change in VA Electricity Sales")+
  theme_ceps()
annual_change_by_utility_bar

ggsave(path=path2graphics, filename="annual_change_by_utility_bar.png")

annual_change_by_utility_bar_facet <- ggplot()+
  geom_bar(data=lf_annual_sales_change_by_utility,mapping=aes(x=year,y=value,fill=variable),position = "dodge", stat="identity")+
  facet_grid(rows=vars(variable),scales="free")+
  xlab("Year")+ylab("Percent Change")+
  scale_fill_manual(name=NULL,breaks=c("apco","dom","ros","total"),labels=c("APCO","Dominion","Rest of State","Total"),values=ceps_pal[1:4])+
  labs(title="Annual Percent Change in VA Electricity Sales")+
  theme_ceps()
annual_change_by_utility_bar_facet

ggsave(path=path2graphics, filename="annual_change_by_utility_bar_facet.png")

annual_change_by_utility_bar_extot <- ggplot()+
  geom_bar(data=lf_annual_sales_change_by_utility[variable!="total"],mapping=aes(x=year,y=value,fill=variable),position = "dodge", stat="identity")+
  xlab("Year")+ylab("Percent Change")+
  scale_fill_manual(name=NULL,breaks=c("apco","dom","ros"),labels=c("APCO","Dominion","Rest of State"),values=ceps_pal[1:3])+
  labs(title="Annual Percent Change in Virginia Electricity Sales")+
  theme_ceps()
annual_change_by_utility_bar_extot

ggsave(path=path2graphics, filename="annual_change_by_utility_bar_extot.png")

#Percent share of total annual historical sales by category: residential, industrial, commercial (commerical includes commercial and other category)
annual_sales_by_category_line <- line_figure(list(lf_annual_sales_by_category),
                                             "year","Percent of Total Sales","Percent Share of Virginia Annual Sales by Sector",
                                             list("va_utility_sales"))
annual_sales_by_category_line

ggsave(path=path2graphics, filename="annual_sales_by_category_line.png")

#recreating the above graph with Dominion data centers as its own category
annual_sales_by_category_with_dc_line <- line_figure(list(lf_annual_sales_by_category_with_dc[year>=2001]),
                                                     "year","Percent of Total Sales","Percent Share of Virginia Annual Sales by Sector",
                                                     list("va_utility_sales"))
annual_sales_by_category_with_dc_line

ggsave(path=path2graphics, filename="annual_sales_by_category_with_dc_line.png")

annual_sales_by_category_with_dc_all_line <- line_figure(list(lf_annual_sales_by_category_with_dc),
                                                         "year","Percent of Total Sales","Percent Share of Virginia Annual Sales by Sector",
                                                         list("va_utility_sales"))
annual_sales_by_category_with_dc_all_line

ggsave(path=path2graphics, filename="annual_sales_by_category_with_dc_all_line.png")

#annual sales by category in GWh rather than percentage of total
sales_by_category_gwh_line <- line_figure(list(lf_annual_sales_gwh_by_category_with_dc),
                                          "year","Sales (GWh)","Virginia Annual Sales By Sector",
                                          list("va_utility_sales"))
sales_by_category_gwh_line

ggsave(path=path2graphics, filename="sales_by_category_gwh_line.png")

annual_sales_by_category_area <- stacked_area_figure(list(lf_annual_sales_by_category),
                                                     "year","Percent of Total Sales","Percent Share of Virginia Annual Sales by Sector",
                                                     list("va_utility_sales"))
annual_sales_by_category_area

ggsave(path=path2graphics, filename="annual_sales_by_category_area.png")

#Annual electricity use per customer 
annual_elec_per_customer_line <- line_figure(list(lf_annual_elec_per_customer),
                                             "year","GWh/customer","Electricity Use per Customer",
                                             list("va_utility_sales"))
annual_elec_per_customer_line

ggsave(path=path2graphics, filename="annual_elec_per_customer_line.png")

#annual electricity use per customer, but for residential sales only 
annual_elec_per_customer_res_line <- line_figure(list(lf_annual_elec_per_customer_res),
                                             "year","MWh/customer","Residential Electricity Use per Customer",
                                             list("va_utility_sales"))
annual_elec_per_customer_res_line

ggsave(path=path2graphics, filename="annual_elec_per_customer_res_line.png")

#same graph as above, but with ymin=0.75
annual_elec_per_customer_res_line_scaled <- line_figure(list(lf_annual_elec_per_customer_res),
                                                        "year","MWh/customer","Residential Electricity Use per Customer",
                                                        list("va_utility_sales"),lower_limit = 0.75)
annual_elec_per_customer_res_line_scaled

ggsave(path=path2graphics, filename="annual_elec_per_customer_res_line_scaled.png")

#annual electricity use per customer (all res sales and res cutomers combined)
annual_total_elec_per_cust_res_line <- line_figure(list(lf_annual_total_sales_per_total_cust[variable=="sales_mwh_per_cust"]),
                                                   "year","MWh/customer","Total Residential Electricity Use per Customer",
                                                   list("va_electricity_sales"),modifications = theme(legend.position = "none"))
annual_total_elec_per_cust_res_line

ggsave(path=path2graphics, filename="annual_total_elec_per_cust_res_line.png")

#annual percent change in customers by utility
#jump in ROS in 2008 because prior to 2008 the other category of utility_name2 had 0 customers reported,
#then jumped to 516,737 customers in 2008 when it seems customers for "other" category began to be counted
#so not plotting ROS percent change prior to 2009
cust_change_by_utility_line <- line_figure(list(lf_annual_cust_change),
                                           "year","Percent Change","Annual Percent Change in Customers by Utility",
                                           list("va_utility_sales"),lower_limit=-8)
cust_change_by_utility_line

ggsave(path=path2graphics, filename="cust_change_by_utility_line.png")

#annual historical sales for apco, ros and dom 
annual_sales_by_utility_line <- line_figure(list(lf_annual_sales_by_utility),
                                            "year","Sales (GWh)","Annual Virginia Electricity Sales by Utility",
                                            list("va_utility_sales"))
annual_sales_by_utility_line

ggsave(path=path2graphics, filename="annual_sales_by_utility_line.png")

#plotting annual sales and rate of change of sales for 3 utilities
annual_sales_and_rate_by_utility <- ggarrange(annual_sales_by_utility_line,annual_change_by_utility_bar_extot,
                                              ncol=1,nrow=2,align="v")
annual_sales_and_rate_by_utility

ggsave(path=path2graphics, filename="annual_sales_and_rate_by_utility.png")

#Dominion sales by category: residential, industrial, commercial
dominion_sales_by_category_area <- stacked_area_figure(list(lf_dominion_sales_by_category),
                                                       "year","Sales (GWh)","Dominion Sales by Sector",
                                                       list("va_utility_sales"))
dominion_sales_by_category_area

ggsave(path=path2graphics, filename="dominion_sales_by_category_area.png")

#Dominion sales by category: residential, industrial, commercial, and data centers
dominion_sales_by_category_w_dc_line <- line_figure(list(lf_dominion_sales_by_category_w_dc),
                                                    "year","Sales (GWh)","Dominion Annual Sales by Sector",
                                                    list("va_utility_sales"))
dominion_sales_by_category_w_dc_line

ggsave(path=path2graphics, filename="dominion_sales_by_category_w_dc_line.png")

#plotting total annual sales with and without data centers
annual_sales_dc <- line_figure(list(lf_annaul_sales_dc),
                               "year","Sales (GWh)", "Virginia Annual Electricity Sales",
                               list("annaul_sales"))
annual_sales_dc

ggsave(path=path2graphics, filename="annual_sales_dc.png")  

#plotting coincident index
va_indx <- line_figure(list(lf_va_indx),
                       "date","Index 2007 = 100","Philadelphia Fed Coincident Economic Index for Virginia",
                       "monthly_sales",modifications = theme(legend.position = "none"))
va_indx

ggsave(path=path2graphics, filename="va_indx.png")  

#plotting electricity use per GDP
#with and without data centers
#with GDP meausred by coincident index
elec_per_gdp <- line_figure(list(lf_annual_elec_per_gdp),
                            "year","GWh/GDP","Electricity Use per GDP",
                            list("monthly_sales"),subtitle_description = "With GDP measured by coincident index")
elec_per_gdp

ggsave(path=path2graphics, filename="elec_per_gdp.png") 

#plotting VA pop growth
annual_pop_growth<-line_figure(list(lf_va_pop_growth),
                               "date","Percent Growth Rate","Virginia Annual Population Growth Rate",
                               list("residential_population_va"),modifications = theme(legend.position = "none"))
annual_pop_growth

ggsave(path=path2graphics, filename="annual_pop_growth.png")

dom_2019_share_of_total_sales = annual_sales_by_utility[year==2019,dominion/total]
apco_2019_share_of_total_sales = annual_sales_by_utility[year==2019,apco/total]
ros_2019_share_of_total_sales = annual_sales_by_utility[year==2019,rest_of_state/total]

