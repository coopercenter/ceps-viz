library(RPostgreSQL)
library(data.table)
library(zoo)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#pulling 2018 generator data from database
generator_2018 <- data.table(dbGetQuery(db,"select \"Utility_ID\", \"Utility_Name\", \"Plant_Code\", \"Plant_Name\", \"Technology\", \"Nameplate_Capacity_MW\", \"Operating_Month\", \"Operating_Year\" from eia860_generator_y2018 where \"State\"='VA';"))

#pulling in 2014-2017 data & 2019 early release data:
generator_2014 <- data.table(dbGetQuery(db,"select \"Utility ID\", \"Utility Name\", \"Plant Code\", \"Plant Name\", \"Technology\", \"Nameplate Capacity (MW)\", \"Operating Month\", \"Operating Year\" from generator_2014 where \"State\"='VA';"))
generator_2015 <- data.table(dbGetQuery(db,"select \"Utility ID\", \"Utility Name\", \"Plant Code\", \"Plant Name\", \"Technology\", \"Nameplate Capacity (MW)\", \"Operating Month\", \"Operating Year\" from generator_2015 where \"State\"='VA';"))
generator_2016 <- data.table(dbGetQuery(db,"select \"Utility ID\", \"Utility Name\", \"Plant Code\", \"Plant Name\", \"Technology\", \"Nameplate Capacity (MW)\", \"Operating Month\", \"Operating Year\" from generator_2016 where \"State\"='VA';"))
generator_2017 <- data.table(dbGetQuery(db,"select \"Utility ID\", \"Utility Name\", \"Plant Code\", \"Plant Name\", \"Technology\", \"Nameplate Capacity (MW)\", \"Operating Month\", \"Operating Year\" from generator_2017 where \"State\"='VA';"))
generator_2019 <- data.table(dbGetQuery(db,"select \"Utility ID\", \"Utility Name\", \"Plant Code\", \"Plant Name\", \"Technology\", \"Nameplate Capacity (MW)\", \"Operating Month\", \"Operating Year\" from generator_2019_early_release where \"State\"='VA';"))

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

###load in offshore wind
net_capacity_factor_offshore_wind <- data.table(dbGetQuery(db,"select * from net_capacity_factor_offshore_wind ;"))

dbDisconnect(db)

#reformatting column names in generator data & creating date_online variable for each generator with generator_clean function
generator_clean <- function(generator_data){
  
  #refashion column names into good variable names
  nms <- names(generator_data)
  good_names = gsub("[?())]","",nms)
  good_names = gsub("[[:punct:][:blank:]/]","_",good_names)
  good_names <- tolower(good_names)
  
  #replace column names with better names
  names(generator_data) <- good_names
  
  #creating date-online variable for each generator
  generator_data[,date_online:=paste0(operating_year,"-",operating_month,"-01")]
  generator_data[,date_online:=as.Date(date_online)]
  
  #casting nameplate variable as numeric
  generator_data[,nameplate_capacity_mw:=as.numeric(nameplate_capacity_mw)]
  
  return(generator_data)
}

generator_2014 <- generator_clean(generator_2014)
generator_2015 <- generator_clean(generator_2015)
generator_2016 <- generator_clean(generator_2016)
generator_2017 <- generator_clean(generator_2017)
generator_2018 <- generator_clean(generator_2018)
generator_2019 <- generator_clean(generator_2019)

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
                                                      conventional_hydroelectric_mw=cumsum(conventional_hydroelectric),
                                                      coal_mw=cumsum(conventional_steam_coal),
                                                      hydroelectric_pumped_storage_mw=cumsum(hydroelectric_pumped_storage),
                                                      landfill_gas_mw=cumsum(landfill_gas),
                                                      municipal_solid_waste_mw=cumsum(municipal_solid_waste),
                                                      natural_gas_mw=cumsum(natural_gas_fired_combined_cycle+natural_gas_fired_combustion_turbine+natural_gas_internal_combustion_engine+natural_gas_steam_turbine),
                                                      nuclear_mw=cumsum(nuclear),
                                                      other_waste_biomass_mw=cumsum(other_waste_biomass),
                                                      petroleum_liquids_mw=cumsum(petroleum_liquids),
                                                      solar_photovoltaic_mw=cumsum(solar_photovoltaic),
                                                      wood_wood_waste_biomass_mw=cumsum(wood_wood_waste_biomass))]
  }
  else{
    capacity_by_fuel_type <- capacity_by_fuel_type[,.(date=date_online,
                                                      conventional_hydroelectric_mw=cumsum(conventional_hydroelectric),
                                                      coal_mw=cumsum(conventional_steam_coal),
                                                      hydroelectric_pumped_storage_mw=cumsum(hydroelectric_pumped_storage),
                                                      landfill_gas_mw=cumsum(landfill_gas),
                                                      municipal_solid_waste_mw=cumsum(municipal_solid_waste),
                                                      natural_gas_mw=cumsum(natural_gas_fired_combined_cycle+natural_gas_fired_combustion_turbine+natural_gas_steam_turbine),
                                                      nuclear_mw=cumsum(nuclear),
                                                      other_waste_biomass_mw=cumsum(other_waste_biomass),
                                                      petroleum_liquids_mw=cumsum(petroleum_liquids),
                                                      wood_wood_waste_biomass_mw=cumsum(wood_wood_waste_biomass))]
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
capacity_by_fuel_type_2019 <- nameplate_sums(generator_2019,2019)

#adding zero-value solar_photovoltaic column to capacity by fuel type 2014 & 2015 tables so they can be merged
capacity_by_fuel_type_2014[,solar_photovoltaic_mw:=0]
capacity_by_fuel_type_2015[,solar_photovoltaic_mw:=0]

#merging the capacity by fuel type tables for each year to get capacity by fuel type over time
capacity_by_fuel_type <- merge(capacity_by_fuel_type_2014,capacity_by_fuel_type_2015,all=TRUE)
capacity_by_fuel_type <- merge(capacity_by_fuel_type,capacity_by_fuel_type_2016,all=TRUE)
capacity_by_fuel_type <- merge(capacity_by_fuel_type,capacity_by_fuel_type_2017,all=TRUE)
capacity_by_fuel_type <- merge(capacity_by_fuel_type,capacity_by_fuel_type_2018,all=TRUE)
capacity_by_fuel_type <- merge(capacity_by_fuel_type,capacity_by_fuel_type_2019,all=TRUE)

#merging data on capacity with data on generation to calculate capacity factors
#       * only merging data beginning in 2013
#       * only including capacity fuel types to match generation fuel types to declutter data
#NOTE: capacity data is in MW but generationg data is in GWh so we must account for this by dividing capacity by 1000 when calculating capacity factors
capacity_factors <- merge(all_generation[year(date)>=2013],capacity_by_fuel_type[,.(date,other_waste_biomass_mw,coal_mw,natural_gas_mw,nuclear_mw,petroleum_liquids_mw,conventional_hydroelectric_mw,wood_wood_waste_biomass_mw)],by="date",all=TRUE)

#filling in NA capacity gaps with last value before NA to get capacity over time (starting at 2013-09-01 because values are all NA before this)
capacity_factors[date>="2013-09-01",`:=`(other_waste_biomass_mw=na.locf(other_waste_biomass_mw),
                                         coal_mw=na.locf(coal_mw),
                                         natural_gas_mw=na.locf(natural_gas_mw),
                                         nuclear_mw=na.locf(nuclear_mw),
                                         petroleum_liquids_mw=na.locf(petroleum_liquids_mw),
                                         conventional_hydroelectric_mw=na.locf(conventional_hydroelectric_mw),
                                         wood_wood_waste_biomass_mw=na.locf(wood_wood_waste_biomass_mw))]

#making capacity factor information begin in 2014 as this is first full year of data
#making capacity factor information end in 2019 as that is last year of generator data
capacity_factors <- capacity_factors[year(date)>=2014&year(date)<=2019]

#adding days in month variable for capacity factor calculations purposes
capacity_factors[,days_in_month:=days_in_month(date)]

#calculating capacity factors by fuel type (recall generation is in GWh but capacity is in mw)
capacity_factors[,`:=`(biomass_cf=other_biomass_gen_gwh/(other_waste_biomass_mw/1000*24*days_in_month)*100,
                       coal_cf=coal_gen_gwh/(coal_mw/1000*24*days_in_month)*100,
                       natural_gas_cf=natural_gas_gen_gwh/(natural_gas_mw/1000*24*days_in_month)*100,
                       nuclear_cf=nuclear_gen_gwh/(nuclear_mw/1000*24*days_in_month)*100,
                       petroleum_liquids_cf=petroleum_liquids_gen_gwh/(petroleum_liquids_mw/1000*24*days_in_month)*100,
                       hydroelctric_cf=conventional_hydroelectric_gen_gwh/(conventional_hydroelectric_mw/1000*24*days_in_month)*100,
                       wood_cf=wood_gen_gwh/(wood_wood_waste_biomass_mw/1000*24*days_in_month)*100)]

####mean capacity factor of coal
mean_coal_cf_2019<- capacity_factors[year(date)==2019,mean(coal_cf)]
mean_coal_cf_2014<- capacity_factors[year(date)==2014,mean(coal_cf)]

####data tables for coal
mean_coal_cf_2019_data_table<-data.table(year=2019,variable=c("coal", " "),value=c(mean_coal_cf_2019, 100-mean_coal_cf_2019))
mean_coal_cf_2014_data_table<-data.table(year=2014,variable=c("coal", " "),value=c(mean_coal_cf_2014, 100-mean_coal_cf_2014))

#######Coal and Natural Gas Capacity Factors Over Time
lf_coal_cf_and_gas_cf<- melt(capacity_factors[,.(date,coal=coal_cf,natural_gas=natural_gas_cf)],id="date")

######means of all capacity factors in 2019
lf_all_capacity_factors<-melt(capacity_factors[,.(date,biomass_cf,coal_cf,natural_gas_cf,nuclear_cf,petroleum_liquids_cf,hydroelctric_cf, wood_cf)],id="date")
mean_coal_cf_2019<- capacity_factors[year(date)==2019,mean(coal_cf)]
mean_biomass_cf_2019<- capacity_factors[year(date)==2019,mean(biomass_cf)]
mean_natural_gas_cf_2019<- capacity_factors[year(date)==2019,mean(natural_gas_cf)]
mean_nuclear_cf_2019<- capacity_factors[year(date)==2019,mean(nuclear_cf)]
mean_petroleum_liquids_cf_2019<- capacity_factors[year(date)==2019,mean(petroleum_liquids_cf)]
mean_hydroelctric_cf_2019<- capacity_factors[year(date)==2019,mean(hydroelctric_cf)]
mean_wood_cf_2019<- capacity_factors[year(date)==2019,mean(wood_cf)]
###data table excluding biomass bc value doesn't make sense
mean_all_cf_2019_data_table<-data.table(year=2019,Variable=c("Coal", "Natural Gas","Nuclear", "Petroleum Liquids", "Hydroelectric","Wood","Wind"),
                                        value=c(mean_coal_cf_2019,mean_natural_gas_cf_2019,mean_nuclear_cf_2019,mean_petroleum_liquids_cf_2019,mean_hydroelctric_cf_2019,mean_wood_cf_2019,44.8))

###Offshore wind net capacity factors over time
lf_net_cf_offshore_wind<- melt(net_capacity_factor_offshore_wind[,.(Year,Pilot,Stage_I,Stage_II,Stage_III)],id="Year")

