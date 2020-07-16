library(data.table)
library(here)
library(ggplot2)
library(scales)
library(dbConnect)
library("RPostgreSQL")

db_driver = dbDriver("PostgreSQL")
#insert your filepath here
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

fetch_time_series_from_db <- function(db_table_name, fuel_code, con, date_unit){
  # Fetch generation data from a specified db table; return as a data table
  require(RPostgreSQL)
  require(data.table)
  
  sql_script  <- paste0("select value,", date_unit, " from ",db_table_name,";")
  dt = data.table(dbGetQuery(con, sql_script))
  setnames(dt, "value", fuel_code)
  
  return(dt)
}

# Creating a data frame called `series_list` that includes two columns:
#      * `table_name`: the list of db table names with data to be pulled and merged
#      *  `fuel_code` : associated fuel type codes, e.g., "coal", "nuc"
series_list_m = data.frame(
  table_name=c("eia_elec_gen_was_va_99_m",
               "eia_elec_gen_cow_va_99_m",
               "eia_elec_gen_hps_va_99_m",
               "eia_elec_gen_ng_va_99_m",
               "eia_elec_gen_nuc_va_99_m",
               "eia_elec_gen_aor_va_99_m",
               "eia_elec_gen_oth_va_99_m",
               "eia_elec_gen_pel_va_99_m",
               "eia_elec_gen_dpv_va_99_m",
               "eia_elec_gen_spv_va_99_m",
               "eia_elec_gen_hyc_va_99_m",
               "eia_elec_gen_www_va_99_m"),
  fuel_code=c("biomass","coal","hydro_pumped_storage","natural_gas","nuclear","other_renewables","other","petroleum_liquids","solar_distributed","solar_utility","hydropower","wood")
)
series_list_m$fuel_code<-as.character(series_list_m$fuel_code)

# Building data table `all_generation` by merging data on generation by several fuel types
all_generation_m <- NULL

for(row in 1:nrow(series_list_m)){
  table <- series_list_m[row,"table_name"]
  fuel <- series_list_m[row,"fuel_code"]
  
  dt <- fetch_time_series_from_db(table, fuel, db, "date")
  
  if (is.null(all_generation_m))
  {all_generation_m <- dt}
  else
  {all_generation_m <-  merge(all_generation_m, dt[], by ="date", all=TRUE)}
}

#doing the same for annual data
series_list_a = data.frame(
  table_name=c("eia_elec_gen_was_va_99_a",
               "eia_elec_gen_cow_va_99_a",
               "eia_elec_gen_hps_va_99_a",
               "eia_elec_gen_ng_va_99_a",
               "eia_elec_gen_nuc_va_99_a",
               "eia_elec_gen_aor_va_99_a",
               "eia_elec_gen_oth_va_99_a",
               "eia_elec_gen_pel_va_99_a",
               "eia_elec_gen_dpv_va_99_a",
               "eia_elec_gen_spv_va_99_a",
               "eia_elec_gen_hyc_va_99_a",
               "eia_elec_gen_www_va_99_a"),
  fuel_code=c("biomass","coal","hydro_pumped_storage","natural_gas","nuclear","other_renewables","other","petroleum_liquids","solar_distributed","solar_utility","hydropower","wood")
)
series_list_a$fuel_code<-as.character(series_list_a$fuel_code)

# Building data table `all_generation` by merging data on generation by several fuel types
all_generation_a <- NULL

for(row in 1:nrow(series_list_a)){
  table <- series_list_a[row,"table_name"]
  fuel <- series_list_a[row,"fuel_code"]
  
  dt <- fetch_time_series_from_db(table, fuel, db, "year")
  
  if (is.null(all_generation_a))
  {all_generation_a <- dt}
  else
  {all_generation_a <-  merge(all_generation_a, dt[], by ="year", all=TRUE)}
}

metadata <- data.table(dbGetQuery(db,"select * from metadata ;"))

dbDisconnect(db)


#to plot generation by fuel type over time
lf_all_generation_m <- melt(all_generation_m, id="date")
lf_all_generation_a <- melt(all_generation_a, id="year")

#cleaning for label name purposes for bar charts which arent functionalized
lf_all_generation_m[,variable:=as.character(variable)]
lf_all_generation_m <- lf_all_generation_m[order(variable)] #alphabetizes variable elements
lf_all_generation_m[,variable:=gsub("solar_utility","Solar (utility)",variable)]
lf_all_generation_m[,variable:=gsub("solar_distributed","Solar (distributed)",variable)]
lf_all_generation_m[,variable:=gsub("_"," ",variable)] #subtitutes "_" from variable name with a space to create legend labels
lf_all_generation_m[,variable:=capitalize(variable)] #capitalizes first word of legend labels

lf_all_generation_a[,variable:=as.character(variable)]
lf_all_generation_a <- lf_all_generation_a[order(variable)] #alphabetizes variable elements
lf_all_generation_a[,variable:=gsub("solar_utility","Solar (utility)",variable)]
lf_all_generation_a[,variable:=gsub("solar_distributed","Solar (distributed)",variable)]
lf_all_generation_a[,variable:=gsub("_"," ",variable)] #subtitutes "_" from variable name with a space to create legend labels
lf_all_generation_a[,variable:=capitalize(variable)] #capitalizes first word of legend labels

