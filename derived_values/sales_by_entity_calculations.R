library(data.table)
library(lubridate)
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
lf_2019_monthly_sales <-melt(monthly_sales[year=="2019",.(month,va_total_sales_gwh)],id="month")
lf_2019_monthly_sales[,variable:=gsub("va_total_sales_gwh","VA Total Sales",variable)]

lf_2018_monthly_sales <-melt(monthly_sales[year=="2018",.(month,va_total_sales_gwh)],id="month")
lf_2018_monthly_sales[,variable:=gsub("va_total_sales_gwh","VA Total Sales",variable)]

lf_total_sales_annual<-melt(annual_sales[year<"2018",.(year,va_total_sales_gwh)],id="year")
lf_total_sales_annual[,variable:=gsub("va_total_sales_gwh","VA Total Sales",variable)]

lf_total_sales_monthly<-melt(monthly_sales[year<"2020",.(date,va_total_sales_gwh)],id="date")
lf_total_sales_monthly[,variable:=gsub("va_total_sales_gwh","VA Total Sales",variable)]

#plotting electricity generation by load-serving entity
lf_monthly_sales_by_entity_2019 <-melt(monthly_sales[year=="2019",.(month,apco_total_gwh,ros_total_gwh,dom_total_gwh)],id=c("month"))
lf_monthly_sales_by_entity_2019[,variable:=gsub("apco_total_gwh","APCO",variable)]
lf_monthly_sales_by_entity_2019[,variable:=gsub("dom_total_gwh","Dominion",variable)]
lf_monthly_sales_by_entity_2019[,variable:=gsub("ros_total_gwh","Rest of state",variable)]

lf_monthly_sales_by_entity_2018 <-melt(monthly_sales[year=="2018",.(month,apco_total_gwh,ros_total_gwh,dom_total_gwh)],id=c("month"))
lf_monthly_sales_by_entity_2018[,variable:=gsub("apco_total_gwh","APCO",variable)]
lf_monthly_sales_by_entity_2018[,variable:=gsub("dom_total_gwh","Dominion",variable)]
lf_monthly_sales_by_entity_2018[,variable:=gsub("ros_total_gwh","Rest of state",variable)]


lf_monthly_sales_by_entity<-melt(monthly_sales[,.(date,apco_total_gwh,ros_total_gwh,dom_total_gwh)],id="date")
lf_monthly_sales_by_entity[,variable:=gsub("apco_total_gwh","APCO",variable)]
lf_monthly_sales_by_entity[,variable:=gsub("dom_total_gwh","Dominion",variable)]
lf_monthly_sales_by_entity[,variable:=gsub("ros_total_gwh","Rest of state",variable)]
lf_monthly_sales_by_entity <- lf_monthly_sales_by_entity[!is.na(value)]

lf_annual_sales_by_entity <-melt(annual_sales[year<"2020",.(year,apco_total_gwh,ros_total_gwh,dom_total_gwh)],id=c("year"))
lf_annual_sales_by_entity[,variable:=gsub("apco_total_gwh","APCO",variable)]
lf_annual_sales_by_entity[,variable:=gsub("dom_total_gwh","Dominion",variable)]
lf_annual_sales_by_entity[,variable:=gsub("ros_total_gwh","Rest of state",variable)]
lf_annual_sales_by_entity <- lf_annual_sales_by_entity[!is.na(value)]
