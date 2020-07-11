library(data.table)
library(lubridate)
library("RPostgreSQL")
library(ggthemes)
library(ggplot2)
library(zoo)
library(scales)
library(here)

#setting initial global variables
globals <- function() {
  HOURS_PER_YEAR <<- 8760
  HOURS_PER_DAY <<- 24
  DAYS_PER_YEAR <<- 365
  WIND_NAMEPLATE_GW <<- 3
  WIND_CF <<- .42
  ACRES_PER_GW_SOLAR_NAMEPLATE <<- 10000
  COST_PER_GW_SOLAR_NAMEPLATE <<- 1000000
}
globals()

read_sheet <- function(path2xls, sheet=1, cols) {
  require(readxl)
  thisData = data.table(read_excel(path2xls, 
                                   sheet=sheet,
                                   cell_cols(cols)))
  return(thisData)
}

path2forecast <- here::here("ggplot2","elec_sales_forecasts_2019-12.xlsx")
source(here::here("ggplot2","viz_functions.R")) #sourcing in viz functions

annual_sales  = read_sheet(path2forecast,sheet=2,cols=1:18)
monthly_sales = read_sheet(path2forecast,cols=1:22)
monthly_sales[date>=2020-01-01,va_total_sales_fc_gwh:=dom_total_exdc_fc_gwh+dom_dc_sales_fc_gwh+apco_total_fc_gwh+ros_total_fc_gwh]

db_driver = dbDriver("PostgreSQL")
#insert your filepath here
source(here::here("my_postgres_credentials.R"))
table = "pjm_hourly_generation"
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)
script  <- paste0("select * from ",table ," where datetime_beginning_ept>='2016' and fuel_type = 'Solar';")
hourly_generation = data.table(dbGetQuery(db, script))
dbDisconnect(db)

#the below section of code is how daily capacity factors were estimated
#maximum hourly generation to date is used as a proxy for nameplate 
#hourly generation summed up for each day 
#then divided by (the nameplate proxy estimation * 24) to get estimates for daily capacity factors
hourly_generation[,date := as.Date(date)]
hourly_generation[,cum_hourly_max:=cummax(mw)]
daily_generation <- hourly_generation[,.(year=last(year),month=last(month), day=last(day),
                                         daily_capacity=last(cum_hourly_max)*HOURS_PER_DAY,
                                         total_gen=sum(mw,na.rm=TRUE)
),by=date]
daily_generation[,solar_cf := total_gen/daily_capacity]

#daily,monthly, and annual capacity factors by taking the average of daily capacity factors over that period
cf_daily = daily_generation[,.(daily_cf=mean(solar_cf),date=first(date)), by=.(month,day)]
cf_monthly = daily_generation[,.(monthly_cf = mean(solar_cf)),by=month]
cf_annual = daily_generation[year<year(now()),.(annual_cf = mean(solar_cf)),by=year]

AVG_ANNUAL_SOLAR_CF = cf_annual[year<year(now()),mean(annual_cf)]

#calculations for renewables to supply 30% of 2030 annual electricity sales
#(and associated costs/solar additions required to reach this 30% supply)
ANNUAL_RENEWABLES_SUPPLY_GWH = .3 * annual_sales[year==2030,total_virginia_sales_fc_gwh]
ANNUAL_WIND_SUPPLY_GWH = HOURS_PER_YEAR*WIND_CF*WIND_NAMEPLATE_GW
ANNUAL_SOLAR_SUPPLY_GWH = ANNUAL_RENEWABLES_SUPPLY_GWH - ANNUAL_WIND_SUPPLY_GWH
ANNUAL_REQUIRED_SOLAR_NAMEPLATE_GW = ANNUAL_SOLAR_SUPPLY_GWH/(HOURS_PER_YEAR*AVG_ANNUAL_SOLAR_CF)
ANNUAL_CAP_INVESTMENT = COST_PER_GW_SOLAR_NAMEPLATE * ANNUAL_REQUIRED_SOLAR_NAMEPLATE_GW
ANNUAL_REQUIRED_ACREAGE = ACRES_PER_GW_SOLAR_NAMEPLATE * ANNUAL_REQUIRED_SOLAR_NAMEPLATE_GW

#doing the same for each month of 2030...
monthly_renewables_supply <- merge(monthly_sales[year==2030,.(date,month,days_in_month,va_total_sales_fc_gwh)],cf_monthly,by="month")
monthly_renewables_supply[,m_demand_sup_by_renewables_gwh:=.3*va_total_sales_fc_gwh]
monthly_renewables_supply[,wind_m_sup_gwh:=WIND_NAMEPLATE_GW*WIND_CF*HOURS_PER_DAY*days_in_month]
monthly_renewables_supply[,solar_m_sup_gwh:=m_demand_sup_by_renewables_gwh-wind_m_sup_gwh]
monthly_renewables_supply[,solar_nameplate_gw:=solar_m_sup_gwh/(HOURS_PER_DAY*monthly_cf*days_in_month)]
monthly_renewables_supply[,cap_investment:=COST_PER_GW_SOLAR_NAMEPLATE*solar_nameplate_gw]
monthly_renewables_supply[,req_acreage:=ACRES_PER_GW_SOLAR_NAMEPLATE*solar_nameplate_gw]

# Find the extremes for 3-day and 5-day moving averages
##########
# Here is a nice little moving average function based on the stats::filter fumction
mav <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
# find the 3 and 5 day moving average solar capacity factors
daily_generation[,`:=`(solar_cf_ma3 = mav(solar_cf,n=3),
                       solar_cf_ma5 = mav(solar_cf,n=5)) ]
monthlyCapacityFactors = daily_generation[!is.na(solar_cf),.(
  avg_solar_cf = mean(solar_cf,na.rm=TRUE),
  med_solar_cf = median(solar_cf,na.rm=TRUE),
  min_ma3_solar_cf = min(solar_cf_ma3,na.rm=TRUE)
),by=.(year,month)]
monthlyCapacityFactors[,date:=as.Date(paste(year,month,'01',sep='-'))]


#looking at mothly cfs visually
cf_monthly_melted <- melt(cf_monthly,id=c("month"))

high_monthly_cf=cf_monthly[,max(monthly_cf)]
low_monthly_cf=cf_monthly[,min(monthly_cf)]

#looking at monthly renewable generation 
#vs 30% of monthly demand, which should ideally be covered by renewables
#assuming we have naemplate in place to supply 30% of annual demand on avg with renewables
cf_monthly[,monthly_solar_supply_gwh:=monthly_cf*ANNUAL_REQUIRED_SOLAR_NAMEPLATE_GW*HOURS_PER_DAY*days_in_month(month)]
cf_monthly[,monthly_renewable_supply_gwh:=WIND_CF*WIND_NAMEPLATE_GW*HOURS_PER_DAY*days_in_month(month)+monthly_solar_supply_gwh]
cf_monthly<-merge(cf_monthly,monthly_renewables_supply[,.(va_total_sales_fc_gwh,month,m_demand_sup_by_renewables_gwh)],by="month")
cf_monthly_melted2 <- melt(cf_monthly[,c("month","monthly_renewable_supply_gwh","m_demand_sup_by_renewables_gwh","va_total_sales_fc_gwh")],id="month")
cf_monthly_melted2[,variable:=gsub("monthly_renewable_supply_gwh","Monthly Renewable Supply (Wind and Solar)",variable)]
cf_monthly_melted2[,variable:=gsub("m_demand_sup_by_renewables_gwh","30% of Monthly Demand",variable)]
cf_monthly_melted2[,variable:=gsub("va_total_sales_fc_gwh","Monthly Electricity Demand",variable)]


#comparing renewable generation by week
#vs target of 30% 2030 weekly demand
#ignoring week 53 as it is only 1 day of the year
daily_generation[, week:=week(date)]
cf_weekly = daily_generation[week<=52,.(weekly_cf = mean(solar_cf)),by=week]
cf_weekly[,weekly_solar_generation_gwh:=weekly_cf*HOURS_PER_DAY*7*ANNUAL_REQUIRED_SOLAR_NAMEPLATE_GW]
cf_weekly[,weekly_ANNUAL_RENEWABLES_SUPPLY_GWH:=weekly_solar_generation_gwh+WIND_CF*WIND_NAMEPLATE_GW*7*HOURS_PER_DAY]

monthly_renewables_supply[,week:=week(date)]

week<-merge(cf_weekly,monthly_renewables_supply[,.(va_total_sales_fc_gwh,week,month)],by="week",all=TRUE)
week[,va_total_sales_fc_gwh:=na.locf(va_total_sales_fc_gwh)]
week[,month:=na.locf(month)]
week[,weekly_demand_gwh:=va_total_sales_fc_gwh*7/days_in_month(month)]
week[,thirty_p_of_weekly_demand_gwh:=.3*weekly_demand_gwh]
week_melted <- melt(week[,c("week","weekly_ANNUAL_RENEWABLES_SUPPLY_GWH","weekly_demand_gwh","thirty_p_of_weekly_demand_gwh")],id="week")
week_melted[,variable:=gsub("weekly_ANNUAL_RENEWABLES_SUPPLY_GWH","Weekly Renewables Supply (Wind and Solar)",variable)]
week_melted[,variable:=gsub("weekly_demand_gwh","Weekly Demand",variable)]
week_melted[,variable:=gsub("thirty_p_of_Weekly Demand","30% of Weekly Demand",variable)]
