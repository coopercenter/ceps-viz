library(here)
library(tidyverse)
library(stringr)
library(jsonlite)
library(data.table)
library(ggplot2)
library(dplyr)
library(RPostgreSQL)
library(scales)
library(eia)
library(zoo)
library(lubridate)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#pulling 2018 generator data from database
table="eia860_generator_y2018"
script  <- paste0("select * from ",table," where \"State\"='VA';")
generator_2018 <- data.table(dbGetQuery(db,script))

fetch_time_series_from_db <- function(db_table_name, fuel_code, con){
  # Fetch generation data from a specified db table; return as a data table
  require(RPostgreSQL)
  require(data.table)
  
  sql_script  <- paste0("select value, date from ",db_table_name,";")
  dt = data.table(dbGetQuery(con, sql_script))
  setnames(dt, "value", fuel_code)
  
  return(dt)
}

# Creating a data frame called `series_list` that includes two columns:
#      * `table_name`: the list of db table names with data to be pulled and merged
#      * `fuel_code` : associated fuel type codes, e.g., "coal", "nuc"
#only fuel types for which we want to calculate capacity factors are included in list
series_list = data.frame(
  table_name=c("eia_elec_gen_was_va_99_m",
               "eia_elec_gen_cow_va_99_m",
               "eia_elec_gen_ng_va_99_m",
               "eia_elec_gen_nuc_va_99_m",
               "eia_elec_gen_pel_va_99_m",
               "eia_elec_gen_hyc_va_99_m",
               "eia_elec_gen_www_va_99_m"),
  fuel_code=c("other_biomass_gen_gwh","coal_gen_gwh","natural_gas_gen_gwh","nuclear_gen_gwh","petroleum_liquids_gen_gwh","conventional_hydroelectric_gen_gwh","wood_gen_gwh")
)

series_list$fuel_code<-as.character(series_list$fuel_code)

# Building data table `all_generation` by merging data on generation by several fuel types
all_generation <- NULL

for(row in 1:nrow(series_list)){
  table <- series_list[row,"table_name"]
  fuel <- series_list[row,"fuel_code"]
  
  dt <- fetch_time_series_from_db(table, fuel, db)
  
  if (is.null(all_generation))
  {all_generation <- dt}
  else
  {all_generation <-  merge(all_generation, dt[], by ="date", all=TRUE)}
}

dbDisconnect(db)

#making column names lowercase
names(generator_2018) <- tolower(names(generator_2018))

#selecting relevant variables
generator_2018 <- generator_2018[,.(utility_id,utility_name,plant_code,plant_name,technology,nameplate_capacity_mw,operating_month,operating_year)]

#creating date-online variable for each generator
generator_2018[,date_online:=paste0(operating_year,"-",operating_month,"-01")]
generator_2018[,date_online:=as.Date(date_online)]

#pulling in 2014-2017 data:
readsheet <- function(file_path,range_dimensions){
  library(readxl)
  library(data.table)
  #Data is from "https://www.eia.gov/electricity/data/eia860/xls/"
  # Read just the column names from the spreadsheet
  nms <- names(read_excel(file_path, n_max = 1, skip=1))
  # Refashion column names into good variable names
  good_names = gsub("[?())]","",nms)
  good_names = gsub("[[:punct:][:blank:]/]","_",good_names)
  good_names <- tolower(good_names)
  # If you don't specify column types, read_excel will guess, often that's OK
  # In this case, it doesn't work so well, so I specify the types
  if(length(nms)==73){
    column_types = c("numeric","text","numeric","text","text",
                     "text","text","text","text","text",
                     "text","text","text","text","text",
                     "numeric","numeric", "numeric", "numeric", "numeric",
                     "text", "numeric", "numeric", "text", "text",
                     "numeric", "numeric","numeric", "numeric", "text",
                     "text", "numeric", "text", "text", "text",
                     "text", "text", "text", "text", "text",
                     "text", "text", "text", "text", "text",
                     "numeric", "text", "text", "text","text",
                     "text", "text", "text", "text","numeric",
                     "numeric", "numeric", "numeric", "numeric", "numeric",
                     "numeric", "numeric", "text", "text", "numeric",
                     "numeric", "numeric","text","numeric", "numeric",
                     "text", "text", "text")
  }
  else{
    column_types = c("numeric","text","numeric","text","text",
                     "text","text","text","text","text",
                     "text","text","text","text","text",
                     "numeric","numeric", "numeric", "numeric", "numeric",
                     "text", "numeric", "numeric", "text", "text",
                     "numeric", "numeric","numeric", "numeric", "text",
                     "text", "numeric", "text", "text", "text",
                     "text", "text", "text", "text", "text",
                     "text", "text", "text", "text", "text",
                     "numeric", "text", "text", "text","text",
                     "text", "text", "text", "text","numeric",
                     "numeric", "numeric", "numeric", "numeric", "numeric",
                     "numeric", "numeric", "text", "text", "numeric",
                     "numeric", "numeric","text","numeric", "numeric",
                     "text", "text")
  }
  # Read the data into a data.table
  thisData = data.table(read_excel(file_path, col_types=column_types, range=range_dimensions))
  #sheet=1,cell_cols(1:11)))
  # Use the good variable names
  setnames(thisData,good_names)
  #only selecting VA data
  thisData <- thisData[state=="VA"]
  #only selecting relevant variables
  thisData <- thisData[,.(utility_id,utility_name,plant_code,plant_name,technology,nameplate_capacity_mw,operating_month,operating_year)]
  #creating date variable
  thisData[,date_online:=paste0(operating_year,"-",operating_month,"-01")]
  thisData[,date_online:=as.Date(date_online)]
  return(thisData)
}

generator_2017 = readsheet(here::here("ggplot2","3_1_Generator_Y2017.xlsx"),"A2:BU21439")

generator_2016 = readsheet(here::here("ggplot2","3_1_Generator_Y2016.xlsx"), "A2:BU20726")

generator_2015 = readsheet(here::here("ggplot2","3_1_Generator_Y2015.xlsx"), "A2:BT20070")

generator_2014 = readsheet(here::here("ggplot2","3_1_Generator_Y2014.xlsx"), "A2:BT19747")

#summing nameplate (mw) additions by fuel type then using these additions to get nameplate over time
nameplate_sums <- function(generator_data,data_year){
  
  library(lubridate)
  
  capacity_by_fuel_type <- dcast(generator_data[,.(date_online,nameplate_capacity_mw,technology)],date_online ~ technology,fun = sum , value.var="nameplate_capacity_mw")
  names(capacity_by_fuel_type) <- gsub(" ","_",names(capacity_by_fuel_type))
  names(capacity_by_fuel_type) <- gsub("/","_",names(capacity_by_fuel_type))
  names(capacity_by_fuel_type) <- tolower(names(capacity_by_fuel_type))
  
  #note: solar photovoltaic & natural_gas_internal_combustion_engine are not technology types until 2016 data
  #turning addtions into cummulative capacity over time with cumsum() function
  if(is.element("natural_gas_internal_combustion_engine",colnames(capacity_by_fuel_type))){
    capacity_by_fuel_type <- capacity_by_fuel_type[,.(date=date_online,
                                                      conventional_hydroelectric=cumsum(conventional_hydroelectric),
                                                      coal=cumsum(conventional_steam_coal),
                                                      hydroelectric_pumped_storage=cumsum(hydroelectric_pumped_storage),
                                                      landfill_gas=cumsum(landfill_gas),
                                                      municipal_solid_waste=cumsum(municipal_solid_waste),
                                                      natural_gas=cumsum(natural_gas_fired_combined_cycle+natural_gas_fired_combustion_turbine+natural_gas_internal_combustion_engine+natural_gas_steam_turbine),
                                                      nuclear=cumsum(nuclear),
                                                      other_waste_biomass=cumsum(other_waste_biomass),
                                                      petroleum_liquids=cumsum(petroleum_liquids),
                                                      solar_photovoltaic=cumsum(solar_photovoltaic),
                                                      wood_wood_waste_biomass=cumsum(wood_wood_waste_biomass))]
  }
  else{
    capacity_by_fuel_type <- capacity_by_fuel_type[,.(date=date_online,
                                                      conventional_hydroelectric=cumsum(conventional_hydroelectric),
                                                      coal=cumsum(conventional_steam_coal),
                                                      hydroelectric_pumped_storage=cumsum(hydroelectric_pumped_storage),
                                                      landfill_gas=cumsum(landfill_gas),
                                                      municipal_solid_waste=cumsum(municipal_solid_waste),
                                                      natural_gas=cumsum(natural_gas_fired_combined_cycle+natural_gas_fired_combustion_turbine+natural_gas_steam_turbine),
                                                      nuclear=cumsum(nuclear),
                                                      other_waste_biomass=cumsum(other_waste_biomass),
                                                      petroleum_liquids=cumsum(petroleum_liquids),
                                                      wood_wood_waste_biomass=cumsum(wood_wood_waste_biomass))]
  }
  capacity_by_fuel_type <- capacity_by_fuel_type[year(date)>data_year-1]
  return(capacity_by_fuel_type)
}

#each year's generator data contains data for only the generators that were still operating in that particular year
#this means we must get capacity by fuel type over time from each annual dataset rather than just the most recent dataset to account for plants which may have been retired from year to year
capacity_by_fuel_type_2014 <- nameplate_sums(generator_2014,2013) #used 2013 to include 2013 data so that capacity could be filled in for beginning months of 2014 when merge happens
capacity_by_fuel_type_2015 <- nameplate_sums(generator_2015,2015)
capacity_by_fuel_type_2016 <- nameplate_sums(generator_2016,2016)
capacity_by_fuel_type_2017 <- nameplate_sums(generator_2017,2017)
capacity_by_fuel_type_2018 <- nameplate_sums(generator_2018,2018)

#adding zero-value solar_photovoltaic column to capacity by fuel type 2014 & 2015 tables so they can be merged
capacity_by_fuel_type_2014[,solar_photovoltaic:=0]
capacity_by_fuel_type_2015[,solar_photovoltaic:=0]

#merging the capacity by fuel type tables for each year to get capacity by fuel type over time
capacity_by_fuel_type <- merge(capacity_by_fuel_type_2017,capacity_by_fuel_type_2018,all=TRUE)
capacity_by_fuel_type <- merge(capacity_by_fuel_type,capacity_by_fuel_type_2016,all=TRUE)
capacity_by_fuel_type <- merge(capacity_by_fuel_type,capacity_by_fuel_type_2015,all=TRUE)
capacity_by_fuel_type <- merge(capacity_by_fuel_type,capacity_by_fuel_type_2014,all=TRUE)

#merging data on capacity with data on generation to calculate capacity factors
#       * only merging data beginning in 2013
#       * only including capacity fuel types to match generation fuel types to declutter data
#NOTE: capacity data is in MW but generationg data is in GWh so we must account for this by dividing capacity by 1000 when calculating capacity factors
capacity_factors <- merge(all_generation[year(date)>=2013],capacity_by_fuel_type[,.(date,other_waste_biomass,coal,natural_gas,nuclear,petroleum_liquids,conventional_hydroelectric,wood_wood_waste_biomass)],by="date",all=TRUE)

#filling in NA capacity gaps with last value before NA to get capacity over time (starting at 2013-09-01 because values are all NA before this)
capacity_factors[date>="2013-09-01",`:=`(other_waste_biomass=na.locf(other_waste_biomass),
                                         coal=na.locf(coal),
                                         natural_gas=na.locf(natural_gas),
                                         nuclear=na.locf(nuclear),
                                         petroleum_liquids=na.locf(petroleum_liquids),
                                         conventional_hydroelectric=na.locf(conventional_hydroelectric),
                                         wood_wood_waste_biomass=na.locf(wood_wood_waste_biomass))]

#making capacity factor information begin in 2014 as this is first full year of data
#making capacity factor information end in 2018 as that is last year of generator data
capacity_factors <- capacity_factors[year(date)>=2014&year(date)<=2018]

#adding days in month variable for capacity factor calculations purposes
capacity_factors[,days_in_month:=days_in_month(date)]

#calculating capacity factors by fuel type (recall generation is in GWh but capacity is in mw)
capacity_factors[,`:=`(biomass_cf=other_biomass_gen_gwh/(other_waste_biomass/1000*24*days_in_month)*100,
                       coal_cf=coal_gen_gwh/(coal/1000*24*days_in_month)*100,
                       natural_gas_cf=natural_gas_gen_gwh/(natural_gas/1000*24*days_in_month)*100,
                       nuclear_cf=nuclear_gen_gwh/(nuclear/1000*24*days_in_month)*100,
                       petroleum_liquids_cf=petroleum_liquids_gen_gwh/(petroleum_liquids/1000*24*days_in_month)*100,
                       hydroelctric_cf=conventional_hydroelectric_gen_gwh/(conventional_hydroelectric/1000*24*days_in_month)*100,
                       wood_cf=wood_gen_gwh/(wood_wood_waste_biomass/1000*24*days_in_month)*100)]

#-------------------------------PLOTTING CAPACITY FACTORS VISUALLY-----------------------------------------------

