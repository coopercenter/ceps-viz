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

table = "va_utility_sales"
script  <- paste0("select * from ",table ," ;")
va_utility_sales = data.table(dbGetQuery(db, script))

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

#plotting total monthly statewide sales since 1990
#converting units to GWh as it is standard in other data sets
total_va_utility_sales = va_utility_sales[,.(total_sales_gwh=sum(tot_sales_mwh)/1000),by=date]
total_va_utility_sales[,date:=as.Date(date)]

total_monthly_sales <- ggplot()+
  geom_line(data=total_va_utility_sales,mapping=aes(x=date,y=total_sales_gwh))+
  xlab("Year")+ylab("Sales (GWh)")+ylim(0,NA)+
  labs(title="VA Total Monthly Sales")
total_monthly_sales

path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="total_monthly_sales.png")

#Annual year-over-year percent change in statewide sales and same for the three utility groupings: dom, apco, ros 
annual_sales_by_utility = dcast(va_utility_sales[,.(year,tot_sales_mwh,utility_name)],year~utility_name,fun=(sum),value.var = "tot_sales_mwh")
#converting to gwh
annual_sales_by_utility[,`:=`(apco=apco/1000,
                              dominion=dominion/1000,
                              rest_of_state=rest_of_state/1000)]
annual_sales_by_utility[,total:=apco+dominion+rest_of_state]
annual_sales_by_utility[,`:=`(apco_change=(apco-lag(apco))/lag(apco)*100,
                              domion_change=(dominion-lag(dominion))/lag(dominion)*100,
                              ros_change=(rest_of_state-lag(rest_of_state))/lag(rest_of_state)*100,
                              total_change=(total-lag(total))/lag(total)*100)]

lf_annual_sales_change_by_utility <- melt(annual_sales_by_utility[,.(apco_change,domion_change,ros_change,total_change,year)],id="year")
setnames(lf_annual_sales_change_by_utility,old=c("variable","value"),new=c("utility","percent_change"))

annual_change_by_utility <- ggplot()+
  geom_line(data=lf_annual_sales_change_by_utility,mapping=aes(x=year,y=percent_change,color=utility))+
  xlab("Year")+ylab("Percent Change")+
  scale_color_discrete(name=NULL,breaks=c("apco_change",
                                          "domion_change",
                                          "ros_change",
                                          "total_change"),labels=c("APCO",
                                                                   "Dominion",
                                                                   "Rest of state",
                                                                   "Total"))+
  labs(title="Annual Percent Change in VA Electricity Sales")
annual_change_by_utility

ggsave(path=path2graphics, filename="annual_change_by_utility.png")

annual_change_by_utility_facet <- ggplot()+
  geom_line(data=lf_annual_sales_change_by_utility,mapping=aes(x=year,y=percent_change,color=utility))+
  facet_grid(rows=vars(utility),scales="free")+
  xlab("Year")+ylab("Percent Change")+
  scale_color_discrete(name=NULL,breaks=c("apco_change",
                                          "domion_change",
                                          "ros_change",
                                          "total_change"),labels=c("APCO",
                                                                   "Dominion",
                                                                   "Rest of state",
                                                                   "Total"))+
  labs(title="Annual Percent Change in VA Electricity Sales")
annual_change_by_utility_facet

ggsave(path=path2graphics, filename="annual_change_by_utility_facet.png")

annual_change_by_utility_bar <- ggplot()+
  geom_bar(data=lf_annual_sales_change_by_utility,mapping=aes(x=year,y=percent_change,fill=utility),position = "dodge", stat="identity")+
  xlab("Year")+ylab("Percent Change")+
  scale_fill_discrete(name=NULL,breaks=c("apco_change",
                                         "domion_change",
                                         "ros_change",
                                         "total_change"),labels=c("APCO",
                                                                  "Dominion",
                                                                  "Rest of state",
                                                                  "Total"))+
  labs(title="Annual Percent Change in VA Electricity Sales")
annual_change_by_utility_bar

ggsave(path=path2graphics, filename="annual_change_by_utility_bar.png")

annual_change_by_utility_bar_facet <- ggplot()+
  geom_bar(data=lf_annual_sales_change_by_utility,mapping=aes(x=year,y=percent_change,fill=utility),position = "dodge", stat="identity")+
  facet_grid(rows=vars(utility),scales="free")+
  xlab("Year")+ylab("Percent Change")+
  scale_fill_discrete(name=NULL,breaks=c("apco_change",
                                         "domion_change",
                                         "ros_change",
                                         "total_change"),labels=c("APCO",
                                                                  "Dominion",
                                                                  "Rest of state",
                                                                  "Total"))+
  labs(title="Annual Percent Change in VA Electricity Sales")
annual_change_by_utility_bar_facet

ggsave(path=path2graphics, filename="annual_change_by_utility_bar_facet.png")

annual_change_by_utility_bar_extot <- ggplot()+
  geom_bar(data=lf_annual_sales_change_by_utility[utility!="total_change"],mapping=aes(x=year,y=percent_change,fill=utility),position = "dodge", stat="identity")+
  xlab("Year")+ylab("Percent Change")+
  scale_fill_discrete(name=NULL,breaks=c("apco_change",
                                         "domion_change",
                                         "ros_change"),labels=c("APCO",
                                                                "Dominion",
                                                                "Rest of state"))+
  labs(title="Annual Percent Change in VA Electricity Sales")
annual_change_by_utility_bar_extot

ggsave(path=path2graphics, filename="annual_change_by_utility_bar_extot.png")

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
setnames(lf_annual_sales_by_category,old=c("variable","value"),new=c("category","share_of_sales"))

annual_sales_by_category_line <- ggplot()+
  geom_line(data=lf_annual_sales_by_category,mapping=aes(x=year,y=share_of_sales,color=category))+
  xlab("Year")+ylab("Percent of Total Sales")+
  scale_color_discrete(name=NULL,breaks=c("res_sales_share",
                                          "com_sales_share",
                                          "ind_sales_share"),labels=c("Residential",
                                                                      "Commercial",
                                                                      "Industrial"))+
  labs(title="Percent Share of Annual Sales by Category")
annual_sales_by_category_line

ggsave(path=path2graphics, filename="annual_sales_by_category_line.png")

#recreating the above graph with Dominion data centers as its own category
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
setnames(lf_annual_sales_by_category_with_dc,old=c("variable","value"),new=c("category","share_of_sales"))

annual_sales_by_category_with_dc_line <- ggplot()+
  geom_line(data=lf_annual_sales_by_category_with_dc[year>=2001],mapping=aes(x=year,y=share_of_sales,color=category))+
  xlab("Year")+ylab("Percent of Total Sales")+
  scale_color_discrete(name=NULL,breaks=c("res_sales_share",
                                          "com_sales_exdc_share",
                                          "ind_sales_share",
                                          "dc_sales_share"),labels=c("Residential",
                                                                     "Commercial",
                                                                     "Industrial",
                                                                     "Data Centers"))+
  labs(title="Percent Share of Annual Sales by Category")
annual_sales_by_category_with_dc_line

ggsave(path=path2graphics, filename="annual_sales_by_category_with_dc_line.png")

annual_sales_by_category_with_dc_all_line <- ggplot()+
  geom_line(data=lf_annual_sales_by_category_with_dc,mapping=aes(x=year,y=share_of_sales,color=category))+
  xlab("Year")+ylab("Percent of Total Sales")+
  scale_color_discrete(name=NULL,breaks=c("res_sales_share",
                                          "com_sales_exdc_share",
                                          "ind_sales_share",
                                          "dc_sales_share"),labels=c("Residential",
                                                                     "Commercial",
                                                                     "Industrial",
                                                                     "Data Centers"))+
  labs(title="Percent Share of Annual Sales by Category")
annual_sales_by_category_with_dc_all_line

ggsave(path=path2graphics, filename="annual_sales_by_category_with_dc_all_line.png")

#annual sales by category in GWh rather than percentage of total
annual_sales_by_category_with_dc[year<=2000,com_sales_exdc_gwh:=com_sales_gwh]
annual_sales_by_category_with_dc[year>2000,com_sales_exdc_gwh:=com_sales_gwh-dom_dc_sales_gwh]

lf_annual_sales_gwh_by_category_with_dc <- melt(annual_sales_by_category_with_dc[,.(year,res_sales_gwh,com_sales_exdc_gwh,ind_sales_gwh,dom_dc_sales_gwh)],id="year")
setnames(lf_annual_sales_gwh_by_category_with_dc,old=c("variable","value"),new=c("category","sales_gwh"))

sales_by_category_gwh_line <- ggplot(data=lf_annual_sales_gwh_by_category_with_dc,mapping=aes(x=year,y=sales_gwh,color=category,shape=category))+
  geom_line()+
  geom_point()+
  xlab("Year")+ylab("sales (GWh)")+ylim(0,NA)+
  labs(title="Annual Sales By Category")+
  scale_color_discrete(name=NULL,breaks=c("res_sales_gwh",
                                          "com_sales_exdc_gwh",
                                          "ind_sales_gwh",
                                          "dom_dc_sales_gwh"),labels=c("Residential",
                                                                       "Commercial",
                                                                       "Industrial",
                                                                       "Data Centers"))+
  scale_shape_manual(name=NULL,breaks=c("res_sales_gwh",
                                        "com_sales_exdc_gwh",
                                        "ind_sales_gwh",
                                        "dom_dc_sales_gwh"),labels=c("Residential",
                                                                     "Commercial",
                                                                     "Industrial",
                                                                     "Data Centers"),values=c(15,8,17,16))
sales_by_category_gwh_line

ggsave(path=path2graphics, filename="sales_by_category_gwh_line.png")

annual_sales_by_category_area <- ggplot()+
  geom_area(data=lf_annual_sales_by_category,mapping=aes(x=year,y=share_of_sales,fill=category))+
  xlab("Year")+ylab("Percent of Total Sales")+
  scale_fill_discrete(name=NULL,breaks=c("res_sales_share",
                                          "com_sales_share",
                                          "ind_sales_share"),labels=c("Residential",
                                                                      "Commercial",
                                                                      "Industrial"))+
  labs(title="Percent Share of Annual Sales by Category")
annual_sales_by_category_area

ggsave(path=path2graphics, filename="annual_sales_by_category_area.png")

#Annual electricity use per customer 
#customer data begins in 1999
#note: only plotting ROS data stating in 2008 because prior data is not meaningful
annual_elec_per_customer = dcast(va_utility_sales[year>=1999&!is.na(tot_cust),.(year,tot_sales_mwh,tot_cust,utility_name)],year~utility_name,fun=(sum),value.var=c("tot_sales_mwh","tot_cust"))
annual_elec_per_customer[,`:=`(apco_gwh_elec_per_cust=(tot_sales_mwh_apco/1000)/tot_cust_apco,
                               dominion_gwh_elec_per_cust=(tot_sales_mwh_dominion/1000)/tot_cust_dominion,
                               ros_gwh_elec_per_cust=(tot_sales_mwh_rest_of_state/1000)/tot_cust_rest_of_state)]


annual_elec_per_customer_line <- ggplot()+
  geom_line(data=annual_elec_per_customer,mapping=aes(x=year,y=apco_gwh_elec_per_cust,color="APCO"))+
  geom_line(data=annual_elec_per_customer,mapping=aes(x=year,y=dominion_gwh_elec_per_cust,color="Dominion"))+
  geom_line(data=annual_elec_per_customer[year>=2008],mapping=aes(x=year,y=ros_gwh_elec_per_cust,color="Rest of state"))+
  xlab("Year")+ylab("GWh")+ylim(0,NA)+
  scale_color_discrete(name=NULL)+
  labs(title="Electricity Use per Customer")
annual_elec_per_customer_line

ggsave(path=path2graphics, filename="annual_elec_per_customer_line.png")

#annual electricity use per customer, but for residential sales only 
annual_elec_per_customer_res = dcast(va_utility_sales[year>=1999&!is.na(res_cust),.(year,res_sales_mwh,res_cust,utility_name)],year~utility_name,fun=(sum),value.var=c("res_sales_mwh","res_cust"))
annual_elec_per_customer_res[,`:=`(apco_mwh_elec_per_cust=(res_sales_mwh_apco)/res_cust_apco,
                                   dominion_mwh_elec_per_cust=(res_sales_mwh_dominion)/res_cust_dominion,
                                   ros_mwh_elec_per_cust=(res_sales_mwh_rest_of_state)/res_cust_rest_of_state)]

annual_elec_per_customer_res_line <- ggplot()+
  geom_line(data=annual_elec_per_customer_res,mapping=aes(x=year,y=apco_mwh_elec_per_cust,color="APCO"))+
  geom_line(data=annual_elec_per_customer_res,mapping=aes(x=year,y=dominion_mwh_elec_per_cust,color="Dominion"))+
  geom_line(data=annual_elec_per_customer_res[year>=2008],mapping=aes(x=year,y=ros_mwh_elec_per_cust,color="Rest of state"))+
  geom_point(data=annual_elec_per_customer_res,mapping=aes(x=year,y=apco_mwh_elec_per_cust,shape="APCO"))+
  geom_point(data=annual_elec_per_customer_res,mapping=aes(x=year,y=dominion_mwh_elec_per_cust,shape="Dominion"))+
  geom_point(data=annual_elec_per_customer_res[year>=2008],mapping=aes(x=year,y=ros_mwh_elec_per_cust,shape="Rest of state"))+
  xlab("Year")+ylab("MWh")+ylim(0,NA)+
  scale_color_discrete(name=NULL,breaks=c("APCO","Dominion","Rest of state"),labels=c("APCO","Dominion","Rest of state"))+
  scale_shape_manual(name=NULL,breaks=c("APCO","Dominion","Rest of state"),labels=c("APCO","Dominion","Rest of state"),values=c(15,8,17))+
  labs(title="Residential Electricity Use per Customer")
annual_elec_per_customer_res_line

ggsave(path=path2graphics, filename="annual_elec_per_customer_res_line.png")

#same graph as above, but with ymin=0.75
annual_elec_per_customer_res_line_scaled <- ggplot()+
  geom_line(data=annual_elec_per_customer_res,mapping=aes(x=year,y=apco_mwh_elec_per_cust,color="APCO"))+
  geom_line(data=annual_elec_per_customer_res,mapping=aes(x=year,y=dominion_mwh_elec_per_cust,color="Dominion"))+
  geom_line(data=annual_elec_per_customer_res[year>=2008],mapping=aes(x=year,y=ros_mwh_elec_per_cust,color="Rest of state"))+
  geom_point(data=annual_elec_per_customer_res,mapping=aes(x=year,y=apco_mwh_elec_per_cust,shape="APCO"))+
  geom_point(data=annual_elec_per_customer_res,mapping=aes(x=year,y=dominion_mwh_elec_per_cust,shape="Dominion"))+
  geom_point(data=annual_elec_per_customer_res[year>=2008],mapping=aes(x=year,y=ros_mwh_elec_per_cust,shape="Rest of state"))+
  xlab("Year")+ylab("MWh")+ylim(0.75,NA)+
  scale_color_discrete(name=NULL,breaks=c("APCO","Dominion","Rest of state"),labels=c("APCO","Dominion","Rest of state"))+
  scale_shape_manual(name=NULL,breaks=c("APCO","Dominion","Rest of state"),labels=c("APCO","Dominion","Rest of state"),values=c(15,8,17))+
  labs(title="Residential Electricity Use per Customer")
annual_elec_per_customer_res_line_scaled

ggsave(path=path2graphics, filename="annual_elec_per_customer_res_line_scaled.png")

#annual electricity use per customer (all res sales and res cutomers combined)
annual_total_sales_per_total_cust = va_utility_sales[year>=1999&!is.na(res_cust),.(res_sales_mwh=sum(res_sales_mwh),
                                                                                   res_cust=sum(res_cust)),by="year"]
annual_total_sales_per_total_cust[,sales_mwh_per_cust:=res_sales_mwh/res_cust]

annual_total_elec_per_cust_res_line <- ggplot()+
  geom_line(data=annual_total_sales_per_total_cust,mapping=aes(x=year,y=sales_mwh_per_cust))+
  xlab("Year")+ylab("MWh")+ylim(0,NA)+
  labs(title="Total Residential Electricity Use per Customer")
annual_total_elec_per_cust_res_line

ggsave(path=path2graphics, filename="annual_total_elec_per_cust_res_line.png")

#annual percent change in customers by utility
#jump in ROS in 2008 because prior to 2008 the other category of utility_name2 had 0 customers reported,
#then jumped to 516,737 customers in 2008 when it seems customers for "other" category began to be counted
#so not plotting ROS percent change prior to 2009
annual_elec_per_customer[,`:=`(apco_cust_change=(tot_cust_apco-lag(tot_cust_apco))/lag(tot_cust_apco)*100,
                               dominion_cust_change=(tot_cust_dominion-lag(tot_cust_dominion))/lag(tot_cust_dominion)*100,
                               ros_cust_change=(tot_cust_rest_of_state-lag(tot_cust_rest_of_state))/lag(tot_cust_rest_of_state)*100)]

cust_change_by_utility_line <- ggplot()+
  geom_line(data=annual_elec_per_customer,mapping=aes(x=year,y=apco_cust_change,color="APCO"))+
  geom_line(data=annual_elec_per_customer,mapping=aes(x=year,y=dominion_cust_change,color="Dominion"))+
  geom_line(data=annual_elec_per_customer[year>2008],mapping=aes(x=year,y=ros_cust_change,color="Rest of state"))+
  xlab("Year")+ylab("Percent Change")+
  scale_color_discrete(name=NULL)+
  labs(title="Annual Percentage Change in Customers by Utility")
cust_change_by_utility_line

ggsave(path=path2graphics, filename="cust_change_by_utility_line.png")

#annual historical sales for apco, ros and dom 
lf_annual_sales_by_utility <- melt(annual_sales_by_utility[,.(apco,dominion,rest_of_state,year)],id="year")
setnames(lf_annual_sales_by_utility,old=c("variable","value"),new=c("utility","sales_gwh"))

annual_sales_by_utility_line <- ggplot(data=lf_annual_sales_by_utility,mapping=aes(x=year,y=sales_gwh,color=utility,shape=utility))+
  geom_line()+
  geom_point()+
  xlab("Year")+ylab("Sales (GWh)")+ylim(0,NA)+
  scale_color_discrete(name=NULL,breaks=c("apco",
                                          "dominion",
                                          "rest_of_state"),labels=c("APCO","Dominion","Rest of state"))+
  scale_shape_manual(name=NULL,breaks=c("apco",
                                        "dominion",
                                        "rest_of_state"),labels=c("APCO","Dominion","Rest of state"),values=c(15,8,17))+
  
  labs(title="Annual VA Electricity Sales by Utility")
annual_sales_by_utility_line

ggsave(path=path2graphics, filename="annual_sales_by_utility_line.png")

#plotting annual sales and rate of change of sales for 3 utilities
annual_sales_and_rate_by_utility <- ggarrange(annual_sales_by_utility_line,annual_change_by_utility_bar_extot,
                                              ncol=1,nrow=2,align="v")
annual_sales_and_rate_by_utility

ggsave(path=path2graphics, filename="annual_sales_and_rate_by_utility.png")


#Dominion sales by category: residential, industrial, commercial
dominion_sales_by_category =va_utility_sales[utility_name=="dominion",.(res_sales_gwh=sum(res_sales_mwh)/1000,com_sales_gwh=sum(com_sales_mwh+oth_sales_mwh)/1000,ind_sales_gwh=sum(ind_sales_mwh)/1000),by="year"]

lf_dominion_sales_by_category<-melt(dominion_sales_by_category,id="year")
setnames(lf_dominion_sales_by_category,old=c("variable","value"),new=c("category","sales_gwh"))

dominion_sales_by_category_area <- ggplot()+
  geom_area(data=lf_dominion_sales_by_category,mapping=aes(x=year,y=sales_gwh,fill=category))+
  xlab("Year")+ylab("Sales (GWh)")+
  scale_fill_discrete(name=NULL,breaks=c("res_sales_gwh",
                                         "com_sales_gwh",
                                         "ind_sales_gwh"),labels=c("Residential","Commercial","Industrial"))+
  labs(title="Dominion Sales by Category")
dominion_sales_by_category_area

ggsave(path=path2graphics, filename="dominion_sales_by_category_area.png")

#Dominion sales by category: residential, industrial, commercial, and data centers
dominion_sales_by_category_w_dc = dominion_sales_by_category
dominion_sales_by_category_w_dc <- merge(dominion_sales_by_category_w_dc,annual_sales[year<=2019,.(year,dom_dc_sales_gwh)],id="year",all=TRUE)
dominion_sales_by_category_w_dc[year>=2001,com_sales_gwh_exdc:=com_sales_gwh-dom_dc_sales_gwh]
dominion_sales_by_category_w_dc[year<2001,com_sales_gwh_exdc:=com_sales_gwh]

lf_dominion_sales_by_category_w_dc <- melt(dominion_sales_by_category_w_dc[,.(year,res_sales_gwh,com_sales_gwh_exdc,ind_sales_gwh,dom_dc_sales_gwh)],id="year")
setnames(lf_dominion_sales_by_category_w_dc,old=c("variable","value"),new=c("category","sales_gwh"))

dominion_sales_by_category_w_dc_line <- ggplot(data=lf_dominion_sales_by_category_w_dc,mapping=aes(x=year,y=sales_gwh,color=category,shape=category))+
  geom_line()+
  geom_point()+
  xlab("Year")+ylab("Sales (GWh)")+
  scale_color_discrete(name=NULL,breaks=c("res_sales_gwh",
                                          "com_sales_gwh_exdc",
                                          "ind_sales_gwh",
                                          "dom_dc_sales_gwh"),labels=c("Residential","Commercial","Industrial","Data Centers"))+
  scale_shape_manual(name=NULL,breaks=c("res_sales_gwh",
                                        "com_sales_gwh_exdc",
                                        "ind_sales_gwh",
                                        "dom_dc_sales_gwh"),labels=c("Residential","Commercial","Industrial","Data Centers"),values=c(15,8,17,16))+
  labs(title="Dominion Annual Sales by Category")
dominion_sales_by_category_w_dc_line

ggsave(path=path2graphics, filename="dominion_sales_by_category_w_dc_line.png")


#plotting total annual sales with and without data centers
annual_sales_dc <- ggplot()+
  geom_line(data=annual_sales[year<="2019"],mapping=aes(x=year,y=va_total_sales_gwh,color="Total Sales"))+
  geom_line(data=annual_sales[year<="2019"],mapping=aes(x=year,y=va_total_sales_gwh-dom_dc_sales_gwh,color="Total Sales Excluding Data Centers"))+
  scale_color_discrete(name=NULL)+xlab("Year")+ylab("Sales (GWh)")+ylim(0,NA)+
  labs(title="VA Annual Electricity Sales",subtitle="2001-2019")
annual_sales_dc

ggsave(path=path2graphics, filename="annual_sales_dc.png")  

#plotting coincident index
va_indx <- ggplot() +
  geom_line(data=monthly_sales[date<="2019-12-01"],mapping=aes(x=date,y=va_indx))+
  labs(title="Philadelphia Fed Coincident Economic Index for Virginia",subtitle="2001-2019")+
  ylab("Index 2007 = 100")+xlab("Year")+ylim(0,NA)
va_indx

ggsave(path=path2graphics, filename="va_indx.png")  

#plotting electricity use per GDP
#with and without data centers
#with GDP meausred by coincident index
monthly_sales[,total_elec_per_gdp:=va_total_sales_gwh/va_indx]
monthly_sales[,elec_exdc_per_gdp:=(va_total_sales_gwh-dom_dc_sales_gwh)/va_indx]

annual_elec_per_gdp <- monthly_sales[date<="2019-12-01",.(total_elec_per_gdp=mean(total_elec_per_gdp),elec_exdc_per_gdp=mean(elec_exdc_per_gdp)),by="year"]
lf_annual_elec_per_gdp <-melt(annual_elec_per_gdp,id="year")
setnames(lf_annual_elec_per_gdp,old=c("variable","value"),new=c("category","elec_per_gdp"))

elec_per_gdp <- ggplot()+
  geom_line(data=lf_annual_elec_per_gdp,mapping=aes(x=year,y=elec_per_gdp,color=category))+
  scale_color_discrete(name=NULL,breaks=c("total_elec_per_gdp","elec_exdc_per_gdp"),labels=c("Total Electricity Use per GDP","Total Electricity Use Excluding Data Denters per GDP"))+
  xlab("Year")+ylab("GWh per GDP")+ylim(0,NA)+
  labs(title="Electricity Use per GDP",subtitle="With GDP measured by coincident index, 2001-2019")
elec_per_gdp 

ggsave(path=path2graphics, filename="elec_per_gdp.png") 

#plotting VA pop growth
#(will adjust code when FRED data is in database, for now I have saved data in excel sheet to ggplot2 folder)
path2xls <- here::here("ggplot2","VAPOP.xls")

va_pop = data.table(read_excel(path2xls,skip=10))
va_pop[,pop_growth:=(VAPOP-lag(VAPOP))/lag(VAPOP)*100]

annual_pop_growth <- ggplot()+
  geom_line(data=va_pop[observation_date>="1990-01-01"],mapping=aes(x=observation_date,y=pop_growth))+
  ylim(0,NA) + xlab("Year") + ylab("Percent Growth Rate") +
  labs(title="VA Annual Population Growth Rate", subtitle = "1990-2019")
annual_pop_growth

ggsave(path=path2graphics, filename="annual_pop_growth.png")

dom_2019_share_of_total_sales = annual_sales_by_utility[year==2019,dominion/total]
apco_2019_share_of_total_sales = annual_sales_by_utility[year==2019,apco/total]
ros_2019_share_of_total_sales = annual_sales_by_utility[year==2019,rest_of_state/total]

