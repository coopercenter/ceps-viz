library(data.table)
library(lubridate)
library(here)
library(ggplot2)
library(scales)
library(dbConnect)
library("RPostgreSQL")

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

table="va_electricity_data"
script  <- paste0("select date,elec_sales_all_gwh,elec_sales_res_gwh,elec_sales_com_gwh,elec_sales_ind_gwh,elec_sales_tra_gwh,year from ",table," where date>='2001-01-01';")
consumption_by_sector_monthly <- data.table(dbGetQuery(db,script))

dbDisconnect(db)

lf_consumption_by_sector_monthly <- melt(consumption_by_sector_monthly[,.(date,elec_sales_res_gwh,elec_sales_com_gwh,elec_sales_ind_gwh,elec_sales_tra_gwh)],id="date")
setnames(lf_consumption_by_sector_monthly,old=c("variable","value"),new=c("sector","consumption_gwh"))
lf_consumption_by_sector_monthly[,date:=as.Date(date)]
lf_consumption_by_sector_monthly[,sector:=as.character(sector)]

#plotting monthly consumption by sector
consumption_by_sector_monthly_line <- ggplot() +
  geom_line(data=lf_consumption_by_sector_monthly,mapping=aes(x=date,y=consumption_gwh,color=sector))+
  scale_color_discrete(name="Sector",breaks=c("elec_sales_com_gwh",
                                              "elec_sales_ind_gwh",
                                              "elec_sales_res_gwh",
                                              "elec_sales_tra_gwh"),labels=c("Commercial","Industrial","Residential","Transportation"))+
  ylab("Consumption(GWh)")+xlab(NULL)+
  labs(title="VA Monthly Electricity Consumption by Sector",subtitle="2001-2019")
consumption_by_sector_monthly_line

path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="consumption_by_sector_monthly_line.png")

consumption_by_sector_monthly_stacked_area <- ggplot() +
  geom_area(data=lf_consumption_by_sector_monthly,mapping=aes(x=date,y=consumption_gwh,fill=sector))+
  scale_fill_discrete(name="Sector",breaks=c("elec_sales_com_gwh",
                                              "elec_sales_ind_gwh",
                                              "elec_sales_res_gwh",
                                              "elec_sales_tra_gwh"),labels=c("Commercial","Industrial","Residential","Transportation"))+
  ylab("Consumption(GWh)")+xlab(NULL)+
  labs(title="VA Monthly Electricity Consumption by Sector",subtitle="2001-2019")
consumption_by_sector_monthly_stacked_area

ggsave(path=path2graphics, filename="consumption_by_sector_monthly_stacked_area.png")

#creating annual consumption by sector table
#excluding 2019 because there is only data for 11 months in 2019
consumption_by_sector_annual <- consumption_by_sector_monthly[year<"2019",.(annual_elec_sales_all_gwh=sum(elec_sales_all_gwh),
                                                                            annual_elec_sales_com_gwh=sum(elec_sales_com_gwh),
                                                                            annual_elec_sales_ind_gwh=sum(elec_sales_ind_gwh),
                                                                            annual_elec_sales_res_gwh=sum(elec_sales_res_gwh),
                                                                            annual_elec_sales_tra_gwh=sum(elec_sales_tra_gwh)),
                                                              by="year"]

lf_consumption_by_sector_annual <- melt(consumption_by_sector_annual[,.(year,annual_elec_sales_com_gwh,annual_elec_sales_ind_gwh,annual_elec_sales_res_gwh,annual_elec_sales_tra_gwh)],id="year")
setnames(lf_consumption_by_sector_annual,old=c("variable","value"),new=c("sector","consumption_gwh"))
lf_consumption_by_sector_annual[,sector:=as.character(sector)]

#plotting annual consumptio by sector
consumption_by_sector_annual_line <- ggplot() +
  geom_line(data=lf_consumption_by_sector_annual,mapping=aes(x=year,y=consumption_gwh,color=sector))+
  scale_color_discrete(name="Sector",breaks=c("annual_elec_sales_com_gwh",
                                              "annual_elec_sales_ind_gwh",
                                              "annual_elec_sales_res_gwh",
                                              "annual_elec_sales_tra_gwh"),labels=c("Commercial","Industrial","Residential","Transportation"))+
  ylab("Consumption(GWh)")+xlab(NULL)+
  labs(title="VA Annual Electricity Consumption by Sector",subtitle="2001-2018")
consumption_by_sector_annual_line

ggsave(path=path2graphics, filename="consumption_by_sector_annual_line.png")

consumption_by_sector_annual_stacked_area <- ggplot() +
  geom_area(data=lf_consumption_by_sector_annual,mapping=aes(x=year,y=consumption_gwh,fill=sector))+
  scale_fill_discrete(name="Sector",breaks=c("annual_elec_sales_com_gwh",
                                              "annual_elec_sales_ind_gwh",
                                              "annual_elec_sales_res_gwh",
                                              "annual_elec_sales_tra_gwh"),labels=c("Commercial","Industrial","Residential","Transportation"))+
  ylab("Consumption(GWh)")+xlab(NULL)+
  labs(title="VA Annual Electricity Consumption by Sector",subtitle="2001-2018")
consumption_by_sector_annual_stacked_area

ggsave(path=path2graphics, filename="consumption_by_sector_annual_stacked_area.png")

#plotting 2019 monthly consumption by sector
consumption_by_sector_2019_line <- ggplot()+
  geom_line(data=lf_consumption_by_sector_monthly[year(date)=="2019"],mapping=aes(x=month(date),y=consumption_gwh,color=sector))+
  scale_color_discrete(name="Sector",breaks=c("elec_sales_com_gwh",
                                              "elec_sales_ind_gwh",
                                              "elec_sales_res_gwh",
                                              "elec_sales_tra_gwh"),labels=c("Commercial","Industrial","Residential","Transportation"))+
  ylab("Consumption(GWh)")+xlab(NULL)+
  scale_x_discrete(limits=month.abb)+
  labs(title="VA 2019 Monthly Electricity Consumption by Sector")
consumption_by_sector_2019_line

ggsave(path=path2graphics, filename="consumption_by_sector_2019_line.png")

consumption_by_sector_2019_stacked_area <- ggplot()+
  geom_area(data=lf_consumption_by_sector_monthly[year(date)=="2019"],mapping=aes(x=month(date),y=consumption_gwh,fill=sector))+
  scale_fill_discrete(name="Sector",breaks=c("elec_sales_com_gwh",
                                             "elec_sales_ind_gwh",
                                             "elec_sales_res_gwh",
                                             "elec_sales_tra_gwh"),labels=c("Commercial","Industrial","Residential","Transportation"))+
  ylab("Consumption(GWh)")+xlab(NULL)+
  scale_x_discrete(limits=month.abb[1:11])+
  labs(title="VA 2019 Monthly Electricity Consumption by Sector")
consumption_by_sector_2019_stacked_area

ggsave(path=path2graphics, filename="consumption_by_sector_2019_stacked_area.png")





