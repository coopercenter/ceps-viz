library(data.table)
library(RPostgreSQL)
library(scales)
library("maps") #contains state & county data
library(sf)
library(tidyr)
library(dplyr)
library(tools)
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
library(ggplot2)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#load in metadata
metadata <- data.table(dbGetQuery(db,"select * from metadata ;"))

#load in capacity by fuel type data (likely will be replaced if we find better solar data)
whole_electric_industry_capacity <- data.table(dbGetQuery(db,"select * from whole_electric_industry_capacity ;"))

#load in data on multiple types of emission compounds (commented out for now as access problems are worked out)
emissions_co2_by_source_va <- data.table(dbGetQuery(db, "select * from emissions_co2_by_source_va ;")) #units = thousand metric tons
emissions_no_by_source_va <- data.table(dbGetQuery(db, "select * from emissions_no_by_source_va ;")) #units = short tons
emissions_so2_by_source_va <- data.table(dbGetQuery(db, "select * from emissions_so2_by_source_va ;")) #units = short tons

#load in energy equity data
energy_burden_county_percent_income <- data.table(dbGetQuery(db,"select * from energy_burden_county_percent_income ;"))
energy_burden_county_expenditures <- data.table(dbGetQuery(db,"select * from energy_burden_county_expenditures ;"))

#load in offshore wind projections
total_mw_offshore_wind <- data.table(dbGetQuery(db,"select * from total_mw_offshore_wind ;"))
total_production_forecast_offshore_wind <- data.table(dbGetQuery(db,"select * from total_production_forecast_offshore_wind ;"))

#function to fetch data from a specified db table; return as a data table & rename 'value' column with descriptive name
fetch_time_series_from_db <- function(db_table_name, value_description, con){
  library(RPostgreSQL)
  library(data.table)
  
  sql_script  <- paste0("select value, date from ",db_table_name," ;")
  dt <- data.table(dbGetQuery(con, sql_script))
  dt <- dt[,.(year=year(date),value)]
  setnames(dt, "value", value_description)
  
  return(dt)
}

# Creating a data frame called `table_list` that includes two columns:
#      * `table_name`: the list of db table names with data to be pulled from db
#      * `value_name` : associated name of 'value', e.g., "coal"
table_list = data.frame(
  table_name=c("eia_elec_gen_cow_va_99_a",
               "eia_elec_gen_pel_va_99_a",
               "eia_elec_gen_ng_va_99_a",
               "eia_elec_gen_nuc_va_99_a",
               "eia_elec_gen_sun_va_99_a",
               "eia_elec_gen_dpv_va_99_a",
               "eia_elec_gen_hyc_va_99_a",
               "eia_elec_gen_www_va_99_a",
               "eia_elec_gen_was_va_99_a",
               "eia_elec_gen_all_va_99_a",
               "eia_seds_tercb_va_a",
               "eia_seds_teccb_va_a",
               "eia_seds_teicb_va_a",
               "eia_seds_teacb_va_a",
               "eia_emiss_co2_totv_ec_to_va_a",
               "eia_emiss_co2_totv_tt_to_va_a",
               "eia_seds_tetcb_va_a",
               "fred_vangsp"),
  value_name=c("coal",
               "oil",
               "gas",
               "nuclear",
               "utility_solar", 
               "distributed_solar",
               "hydropower",
               "wood",
               "other_biomass",
               "total",
               "residential",
               "commercial",
               "industrial",
               "transportation",
               "electric_sector_CO2_emissions",
               "total_CO2_emissions",
               "total_consumption_billion_btu",
               "GDP_million_usd")
)
table_list$value_name <- as.character(table_list$value_name)
table_list$table_name <- as.character(table_list$table_name)

#loading in data from database that is listed in table_list
for(row in 1:nrow(table_list)){
  table <- table_list[row,"table_name"]
  name_of_value <- table_list[row,"value_name"]
  
  dt <- fetch_time_series_from_db(table, name_of_value, db)
  assign(table,dt)
}

dbDisconnect(db)

eia_elec_gen_sun_va_99_a[utility_solar == 0,utility_solar:=NA] #random fix for visual purposes later on

# Isolating renewable and carbon free generation in it's own table -----------------------------------------------
renewable_and_carbon_free_list <- list(eia_elec_gen_nuc_va_99_a,
                                       eia_elec_gen_sun_va_99_a,
                                       eia_elec_gen_dpv_va_99_a,
                                       eia_elec_gen_hyc_va_99_a,
                                       eia_elec_gen_all_va_99_a)

va_annual_renewable_and_carbon_free_gen <- NULL

for(table in renewable_and_carbon_free_list){
  if (is.null(va_annual_renewable_and_carbon_free_gen))
  {va_annual_renewable_and_carbon_free_gen <- table}
  else
  {va_annual_renewable_and_carbon_free_gen <- merge(va_annual_renewable_and_carbon_free_gen, table[], by = "year", all=TRUE)}
}

va_annual_renewable_and_carbon_free_gen[is.na(va_annual_renewable_and_carbon_free_gen)]=0
va_annual_renewable_and_carbon_free_gen[,all_solar:=distributed_solar+utility_solar]

# Creating 'other' generation measure by combining all by fuel type generation and total generation in a table to caluclate other generation over time
gen_by_fuel_type_list <- list(eia_elec_gen_cow_va_99_a,
                              eia_elec_gen_pel_va_99_a,
                              eia_elec_gen_ng_va_99_a,
                              eia_elec_gen_nuc_va_99_a,
                              eia_elec_gen_sun_va_99_a,
                              eia_elec_gen_dpv_va_99_a,
                              eia_elec_gen_hyc_va_99_a,
                              eia_elec_gen_www_va_99_a,
                              eia_elec_gen_was_va_99_a,
                              eia_elec_gen_all_va_99_a)

va_annual_generation <- NULL

for(table in gen_by_fuel_type_list){
  if (is.null(va_annual_generation))
  {va_annual_generation <- table}
  else
  {va_annual_generation <- merge(va_annual_generation, table[], by = "year", all=TRUE)}
}

va_annual_generation[is.na(va_annual_generation)]=0
va_annual_generation[,other:=total-(coal+oil+gas+nuclear+utility_solar+distributed_solar+hydropower+wood+other_biomass)]
other_annual_generation <- va_annual_generation[,.(year,other)]

# Finding sum of total annual renewable generation----------------------------------------------------------------
va_annual_renewable_and_carbon_free_gen[,renewable:=all_solar+hydropower]

# Finding total annual renewable generation as a percent of total energy generation--------------------------------
va_annual_renewable_and_carbon_free_gen[,percent_renewable:=(renewable/total)*100]

# Finding sum of total annual carbon-free generation--------------------------------------------------------------
va_annual_renewable_and_carbon_free_gen[,carbon_free:=hydropower+all_solar+nuclear]

# Finding total annual carbon-free generation as a percent of total energy generation--------------------------------------
va_annual_renewable_and_carbon_free_gen[,percent_carbon_free:=(carbon_free/total)*100]

# Renewable versus Non-renewable Generation---------------------------------------------------------------------
va_annual_renewable_and_carbon_free_gen[,not_renewable:=total-renewable]

# Carbon versus Carbon Free Generation------------------------------------------------------------------------------
va_annual_renewable_and_carbon_free_gen[,carbon_emitting:=total-carbon_free]

# Electricity Consumption per GDP---------------------------------------------------------------------------------
consumption_per_gdp <- merge(eia_seds_tetcb_va_a,fred_vangsp,id="year")
consumption_per_gdp[,GDP_million_usd:=as.numeric(GDP_million_usd)]
consumption_per_gdp[,consumption_per_GDP:=total_consumption_billion_btu/GDP_million_usd] #calculating consumption per GDP
lf_consumption_per_GDP <- melt(consumption_per_gdp[year>=2000,.(year,consumption_per_GDP)],id="year") #reformatting so ready for input to function

# For energy equity figures------------------------------------------------------------------------------------------------
#getting citation information from metadata table
expenditures_source <- metadata[db_table_name=="energy_burden_county_expenditures",data_source_full_name]
percent_income_source <- metadata[db_table_name=="energy_burden_county_percent_income",data_source_full_name]

counties <- st_as_sf(map("county",plot = FALSE, fill = TRUE)) #loading in county data from maps package
va_counties <- subset(counties, startsWith(as.character(counties$ID),"virginia")) #isolating VA counties

va_counties <- separate(data = va_counties, col = ID, into = c("state", "county"), sep = ",") #isolating county name

#isolating just county energy equity data (as there are some cities listed as well)
energy_burden_county_expenditures_counties <- energy_burden_county_expenditures[county %like% "County"]
energy_burden_county_percent_income_counties <- energy_burden_county_percent_income[county %like% "County"]

#adjusting county names to match format of other datasets
va_counties$county <- paste(va_counties$county,"county")
va_counties$county <- toTitleCase(va_counties$county)

#merging county geospatial data with energy equity data
va_energy_equity_by_county <- merge(va_counties,energy_burden_county_expenditures_counties,id="county",all=TRUE)
va_energy_equity_by_county$avg_annual_energy_cost <- as.numeric(va_energy_equity_by_county$avg_annual_energy_cost)
va_energy_equity_by_county <- merge(va_energy_equity_by_county,energy_burden_county_percent_income_counties,id="county",all=TRUE)
va_energy_equity_by_county$avg_energy_burden_as_percent_income <- as.numeric(va_energy_equity_by_county$avg_energy_burden_as_percent_income) 

world <- ne_countries(scale = "medium", returnclass = "sf") #to get outline outside of VA
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) #to get  state outline

#-----------------------------------------REFORMATTING DATASETS--------------------------------------------------------------------

# reformatting the generation dataset
va_gen_w_commas<-data.frame(format(va_annual_generation[,2:12],big.mark=",",scientific=FALSE,trim=TRUE))
va_gen_w_commas<-cbind(va_annual_generation[,1],va_gen_w_commas)

# reformatting the consumption dataset
consumption_by_sector_list <- list(eia_seds_tercb_va_a,eia_seds_teccb_va_a,eia_seds_teicb_va_a,eia_seds_teacb_va_a)

va_annual_consumption <- NULL

for(table in consumption_by_sector_list){
  if (is.null(va_annual_consumption))
  {va_annual_consumption <- table}
  else
  {va_annual_consumption <- merge(va_annual_consumption, table[], by = "year", all=TRUE)}
}

va_con_w_commas<-data.frame(format(va_annual_consumption[,2:5],big.mark=",",scientific=FALSE,trim=TRUE))
va_con_w_commas<-cbind(va_annual_consumption[,1],va_con_w_commas)

#reformatting carbon emissions from electricity sector
virginia_emissions_electric <- eia_emiss_co2_totv_ec_to_va_a[,.(year,electric_sector_CO2_emissions)]
virginia_emissions_electric_commas <- data.frame(signif(virginia_emissions_electric[,2], digits=4))
virginia_emissions_electric_commas <- cbind(virginia_emissions_electric[,1],virginia_emissions_electric_commas)
colnames(virginia_emissions_electric_commas) <- c('Year','Million Metric Tons of CO2')

#reformatting emissions compounds dataset
va_emissions_compounds <- merge(emissions_co2_by_source_va[,.(year=year,CO2=total/1000)],emissions_no_by_source_va[,.(year=year,NO=total/1102311.31)],id="year")
va_emissions_compounds <- merge(va_emissions_compounds,emissions_so2_by_source_va[,.(year=year,SO2=total/1102311.31)],id="year")
va_emissions_compounds <- va_emissions_compounds[11:29,] #limit data to baseline year of 2000



