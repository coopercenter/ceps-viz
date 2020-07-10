library(data.table)
library(lubridate)
library(here)
library(ggplot2)
library(scales)
library("RPostgreSQL")

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

table="va_electricity_data"
script  <- paste0("select date,elec_sales_all_gwh,elec_sales_res_gwh,elec_sales_com_gwh,elec_sales_ind_gwh,elec_sales_tra_gwh,year from ",table," where date>='2001-01-01';")
consumption_by_sector_monthly <- data.table(dbGetQuery(db,script))

dbDisconnect(db)


lf_consumption_by_sector_monthly <- melt(consumption_by_sector_monthly[,.(date,elec_sales_res_gwh,elec_sales_com_gwh,elec_sales_ind_gwh,elec_sales_tra_gwh)],id="date")
lf_consumption_by_sector_monthly[,date:=as.Date(date)]
lf_consumption_by_sector_monthly[,variable:=as.character(variable)]

lf_consumption_by_sector_monthly[,variable:=gsub("elec_sales_com_gwh","Commercial",variable)]
lf_consumption_by_sector_monthly[,variable:=gsub("elec_sales_ind_gwh","Industrial",variable)]
lf_consumption_by_sector_monthly[,variable:=gsub("elec_sales_res_gwh","Residential",variable)]
lf_consumption_by_sector_monthly[,variable:=gsub("elec_sales_tra_gwh","Transportation",variable)]

#creating annual consumption by sector table
#excluding 2019 because there is only data for 11 months in 2019
consumption_by_sector_annual <- consumption_by_sector_monthly[year<"2019",.(annual_elec_sales_all_gwh=sum(elec_sales_all_gwh),
                                                                            annual_elec_sales_com_gwh=sum(elec_sales_com_gwh),
                                                                            annual_elec_sales_ind_gwh=sum(elec_sales_ind_gwh),
                                                                            annual_elec_sales_res_gwh=sum(elec_sales_res_gwh),
                                                                            annual_elec_sales_tra_gwh=sum(elec_sales_tra_gwh)),
                                                              by="year"]

lf_consumption_by_sector_annual <- melt(consumption_by_sector_annual[,.(year,annual_elec_sales_com_gwh,annual_elec_sales_ind_gwh,annual_elec_sales_res_gwh,annual_elec_sales_tra_gwh)],id="year")
lf_consumption_by_sector_annual[,variable:=as.character(variable)]

lf_consumption_by_sector_annual[,variable:=gsub("elec_sales_com_gwh","Commercial",variable)]
lf_consumption_by_sector_annual[,variable:=gsub("elec_sales_ind_gwh","Industrial",variable)]
lf_consumption_by_sector_annual[,variable:=gsub("elec_sales_res_gwh","Residential",variable)]
lf_consumption_by_sector_annual[,variable:=gsub("elec_sales_tra_gwh","Transportation",variable)]


