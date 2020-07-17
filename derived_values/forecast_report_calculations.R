library(data.table)
library(lubridate)
library(dbConnect)
library("RPostgreSQL")
library(ggplot2)
library(scales)
library(here)
library(dplyr)
library(readxl)
library(ggpubr)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#load in metadata
metadata <- data.table(dbGetQuery(db,"select * from metadata ;"))

#load in va sales data
va_utility_sales <- data.table(dbGetQuery(db,"select * from va_utility_sales ;"))

#load in population data
residential_population_va <- data.table(dbGetQuery(db,"select * from residential_population_va ;"))

dbDisconnect(db)

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

#summing total monthly sales
#converting units to GWh as it is standard in other data sets
total_va_utility_sales = va_utility_sales[,.(total_sales_gwh=sum(tot_sales_mwh)/1000),by=date]
total_va_utility_sales[,date:=as.Date(date)]
lf_total_va_utility_sales <- melt(total_va_utility_sales,id="date")

#Annual year-over-year percent change in statewide sales and same for the three utility groupings: dom, apco, ros 
annual_sales_by_utility = dcast(va_utility_sales[,.(year,tot_sales_mwh,utility_name)],year~utility_name,fun=(sum),value.var = "tot_sales_mwh")
#converting to gwh
annual_sales_by_utility[,`:=`(apco=apco/1000,
                              dominion=dominion/1000,
                              rest_of_state=rest_of_state/1000)]
annual_sales_by_utility[,total:=apco+dominion+rest_of_state]
annual_sales_by_utility[,`:=`(apco_change=(apco-lag(apco))/lag(apco)*100,
                              dominion_change=(dominion-lag(dominion))/lag(dominion)*100,
                              ros_change=(rest_of_state-lag(rest_of_state))/lag(rest_of_state)*100,
                              total_change=(total-lag(total))/lag(total)*100)]

lf_annual_sales_change_by_utility <- melt(annual_sales_by_utility[,.(apco_change,dominion_change,ros_change,total_change,year)],id="year")
lf_annual_sales_change_by_utility[variable=="apco_change",variable:="apco"] #for legend purposes
lf_annual_sales_change_by_utility[variable=="dominion_change",variable:="dom"]
lf_annual_sales_change_by_utility[variable=="ros_change",variable:="ros"]
lf_annual_sales_change_by_utility[variable=="total_change",variable:="total"]

#Percent share of total annual historical sales by category: residential, industrial, commercial (commerical includes commercial and other category)
#note: no data for sales by category for Tennesse Valley Authority, so I excluded it
#note: no data for commercial sales for Shenandoah_Valley_Elec_Coop in Aug 2010, so I estimated it by subtracting(res+ind+oth) from total
#note: no data for other sales for some uitlities in a few years (APCO in Dec 2010 for example), so I estimated it 
va_utility_sales[year==2010&utility_name2=="Shenandoah_Valley_Elec_Coop"&is.na(com_sales_mwh),com_sales_mwh:=tot_sales_mwh-res_sales_mwh-ind_sales_mwh-oth_sales_mwh]
va_utility_sales[is.na(oth_sales_mwh),oth_sales_mwh:=tot_sales_mwh-res_sales_mwh-ind_sales_mwh-com_sales_mwh]
annual_sales_by_category = va_utility_sales[utility_name2!="Tennessee_Valley_Authority",.(res_sales_share=sum(res_sales_mwh)/sum(tot_sales_mwh)*100,
                                                                                          com_sales_share=sum(com_sales_mwh+oth_sales_mwh)/sum(tot_sales_mwh)*100,
                                                                                          ind_sales_share=sum(ind_sales_mwh)/sum(tot_sales_mwh)*100),by="year"]

lf_annual_sales_by_category <- melt(annual_sales_by_category,id="year")
lf_annual_sales_by_category[variable=="com_sales_share",variable:="commercial"] #for legend label purposes
lf_annual_sales_by_category[variable=="res_sales_share",variable:="residential"]
lf_annual_sales_by_category[variable=="ind_sales_share",variable:="Industrial"]

#Same as above, but separating out data centers
annual_sales_by_category_with_dc = va_utility_sales[utility_name2!="Tennessee_Valley_Authority",.(res_sales_gwh=sum(res_sales_mwh)/1000,
                                                                                                  com_sales_gwh=sum(com_sales_mwh+oth_sales_mwh)/1000,
                                                                                                  ind_sales_gwh=sum(ind_sales_mwh)/1000,
                                                                                                  tot_sales_gwh=sum(tot_sales_mwh)/1000),by="year"]
annual_sales_by_category_with_dc <- merge(annual_sales_by_category_with_dc,annual_sales[year<=2019,.(year,dom_dc_sales_gwh)],id="year",all=TRUE)
annual_sales_by_category_with_dc[,`:=`(res_sales_share=res_sales_gwh/tot_sales_gwh*100,
                                       com_sales_exdc_share=(com_sales_gwh-dom_dc_sales_gwh)/tot_sales_gwh*100,
                                       ind_sales_share=ind_sales_gwh/tot_sales_gwh*100,
                                       dc_sales_share=dom_dc_sales_gwh/tot_sales_gwh*100)]
annual_sales_by_category_with_dc[year<=2000,com_sales_exdc_share:=com_sales_gwh/tot_sales_gwh*100]

lf_annual_sales_by_category_with_dc <-melt(annual_sales_by_category_with_dc[,.(year,
                                                                               res_sales_share,
                                                                               com_sales_exdc_share,
                                                                               ind_sales_share,
                                                                               dc_sales_share)],id="year")

lf_annual_sales_by_category_with_dc[variable=="res_sales_share",variable:="residential"] #for legend label purposes
lf_annual_sales_by_category_with_dc[variable=="com_sales_exdc_share",variable:="commercial"]
lf_annual_sales_by_category_with_dc[variable=="ind_sales_share",variable:="industrial"]
lf_annual_sales_by_category_with_dc[variable=="dc_sales_share",variable:="Data Centers"]

#annual sales by category in GWh rather than percentage of total
annual_sales_by_category_with_dc[year<=2000,com_sales_exdc_gwh:=com_sales_gwh]
annual_sales_by_category_with_dc[year>2000,com_sales_exdc_gwh:=com_sales_gwh-dom_dc_sales_gwh]

lf_annual_sales_gwh_by_category_with_dc <- melt(annual_sales_by_category_with_dc[,.(year,res_sales_gwh,com_sales_exdc_gwh,ind_sales_gwh,dom_dc_sales_gwh)],id="year")
lf_annual_sales_gwh_by_category_with_dc[variable=="res_sales_gwh",variable:="residential"] #for legend label purposes
lf_annual_sales_gwh_by_category_with_dc[variable=="com_sales_exdc_gwh",variable:="commercial"]
lf_annual_sales_gwh_by_category_with_dc[variable=="ind_sales_gwh",variable:="industrial"]
lf_annual_sales_gwh_by_category_with_dc[variable=="dom_dc_sales_gwh",variable:="Data Centers"]

#Annual electricity use per customer 
#customer data begins in 1999
#note: only plotting ROS data stating in 2008 because prior data is not meaningful
annual_elec_per_customer = dcast(va_utility_sales[year>=1999&!is.na(tot_cust),.(year,tot_sales_mwh,tot_cust,utility_name)],year~utility_name,fun=(sum),value.var=c("tot_sales_mwh","tot_cust"))
annual_elec_per_customer[,`:=`(apco_gwh_elec_per_cust=(tot_sales_mwh_apco/1000)/tot_cust_apco,
                               dominion_gwh_elec_per_cust=(tot_sales_mwh_dominion/1000)/tot_cust_dominion,
                               ros_gwh_elec_per_cust=(tot_sales_mwh_rest_of_state/1000)/tot_cust_rest_of_state)]

lf_annual_elec_per_customer <- melt(annual_elec_per_customer[,.(year,apco=apco_gwh_elec_per_cust,dom=dominion_gwh_elec_per_cust,ros=ros_gwh_elec_per_cust)],id="year")
lf_annual_elec_per_customer[year<2008&variable=="ros",value:=NA] #removing ros data prior to 2008

#annual electricity use per customer, but for residential sales only 
annual_elec_per_customer_res = dcast(va_utility_sales[year>=1999&!is.na(res_cust),.(year,res_sales_mwh,res_cust,utility_name)],year~utility_name,fun=(sum),value.var=c("res_sales_mwh","res_cust"))
annual_elec_per_customer_res[,`:=`(apco_mwh_elec_per_cust=(res_sales_mwh_apco)/res_cust_apco,
                                   dominion_mwh_elec_per_cust=(res_sales_mwh_dominion)/res_cust_dominion,
                                   ros_mwh_elec_per_cust=(res_sales_mwh_rest_of_state)/res_cust_rest_of_state)]

lf_annual_elec_per_customer_res <- melt(annual_elec_per_customer_res[,.(year,apco=apco_mwh_elec_per_cust,dom=dominion_mwh_elec_per_cust,ros=ros_mwh_elec_per_cust)],id="year")
lf_annual_elec_per_customer_res[year<2008&variable=="ros",value:=NA] #removing ros data prior to 2008

#annual electricity use per customer (all res sales and res cutomers combined)
annual_total_sales_per_total_cust = va_utility_sales[year>=1999&!is.na(res_cust),.(res_sales_mwh=sum(res_sales_mwh),
                                                                                   res_cust=sum(res_cust)),by="year"]
annual_total_sales_per_total_cust[,sales_mwh_per_cust:=res_sales_mwh/res_cust]
lf_annual_total_sales_per_total_cust <- melt(annual_total_sales_per_total_cust,id="year")

#annual percent change in customers by utility
#jump in ROS in 2008 because prior to 2008 the other category of utility_name2 had 0 customers reported,
#then jumped to 516,737 customers in 2008 when it seems customers for "other" category began to be counted
#so not plotting ROS percent change prior to 2009
annual_elec_per_customer[,`:=`(apco_cust_change=(tot_cust_apco-lag(tot_cust_apco))/lag(tot_cust_apco)*100,
                               dominion_cust_change=(tot_cust_dominion-lag(tot_cust_dominion))/lag(tot_cust_dominion)*100,
                               ros_cust_change=(tot_cust_rest_of_state-lag(tot_cust_rest_of_state))/lag(tot_cust_rest_of_state)*100)]
lf_annual_cust_change <- melt(annual_elec_per_customer[,.(year,apco=apco_cust_change,dom=dominion_cust_change,ros=ros_cust_change)],id="year")
lf_annual_cust_change[year<=2008&variable=="ros",value:=NA]

#annual historical sales for apco, ros and dom 
lf_annual_sales_by_utility <- melt(annual_sales_by_utility[,.(apco,dom=dominion,rest_of_state,year)],id="year")

#Dominion sales by category: residential, industrial, commercial
dominion_sales_by_category =va_utility_sales[utility_name=="dominion",.(res_sales_gwh=sum(res_sales_mwh)/1000,com_sales_gwh=sum(com_sales_mwh+oth_sales_mwh)/1000,ind_sales_gwh=sum(ind_sales_mwh)/1000),by="year"]
lf_dominion_sales_by_category<-melt(dominion_sales_by_category[,.(year,residential=res_sales_gwh,commercial=com_sales_gwh,industrial=ind_sales_gwh)],id="year")

#Dominion sales by category: residential, industrial, commercial, and data centers
dominion_sales_by_category_w_dc = dominion_sales_by_category
dominion_sales_by_category_w_dc <- merge(dominion_sales_by_category_w_dc,annual_sales[year<=2019,.(year,dom_dc_sales_gwh)],id="year",all=TRUE)
dominion_sales_by_category_w_dc[year>=2001,com_sales_gwh_exdc:=com_sales_gwh-dom_dc_sales_gwh]
dominion_sales_by_category_w_dc[year<2001,com_sales_gwh_exdc:=com_sales_gwh]

lf_dominion_sales_by_category_w_dc <- melt(dominion_sales_by_category_w_dc[,.(year,residential=res_sales_gwh,commercial=com_sales_gwh_exdc,industrial=ind_sales_gwh,data_Centers=dom_dc_sales_gwh)],id="year")

#plotting total annual sales with and without data centers
lf_annaul_sales_dc <- melt(annual_sales[year<="2019",.(year,total_Sales=va_total_sales_gwh,total_Sales_Excluding_Data_Centers=va_total_sales_gwh-dom_dc_sales_gwh)],id="year")

#plotting coincident index
lf_va_indx <- melt(monthly_sales[date<="2019-12-01",.(date,va_indx)],id="date")

#plotting electricity use per GDP
#with and without data centers
#with GDP meausred by coincident index
monthly_sales[,total_elec_per_gdp:=va_total_sales_gwh/va_indx]
monthly_sales[,elec_exdc_per_gdp:=(va_total_sales_gwh-dom_dc_sales_gwh)/va_indx]

annual_elec_per_gdp <- monthly_sales[date<="2019-12-01",.(total_Electricity_Use_per_GDP=mean(total_elec_per_gdp),total_Electricity_Use_Excluding_Data_Centers_per_GDP=mean(elec_exdc_per_gdp)),by="year"]
lf_annual_elec_per_gdp <-melt(annual_elec_per_gdp,id="year")

#plotting VA pop growth
residential_population_va[,value:=as.numeric(value)]
residential_population_va[,date:=as.Date(date)]
residential_population_va[,pop_growth:=(value-lag(value))/lag(value)*100]
lf_va_pop_growth <- melt(residential_population_va[date>="1990-01-01",.(date,pop_growth)],id="date")


