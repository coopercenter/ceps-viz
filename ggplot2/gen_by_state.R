if(!("jsonlite" %in% installed.packages()))  install.packages("jsonlite")
if(!("eia" %in% installed.packages()))       install.packages("eia")
if(!("RPostgres" %in% installed.packages())) install.packages("RPostgres")
library(here)
library(tidyverse)
library(stringr)
library(jsonlite)
library(data.table)
library(ggplot2)
library(eia)
library(RPostgreSQL)
library(scales)

db_driver = dbDriver("PostgreSQL")
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

#load in metadata
metadata <- data.table(dbGetQuery(db,"select * from metadata ;"))

dbDisconnect(db)

source(here::here("my_eia_api_key.R"))
source(here::here("ggplot2","viz_functions.R"))

get_EIA_series <- function(eiaKey,series_id) {
  require(jsonlite)
  require(data.table)
  # This function retrieves one EIA time-series with metadata
  # The function returns a list of parts of the series:
  #     seriesID,name,units,frequency,data (as data table)
  
  # eiaKey is your EIA API key
  eiaBase = paste0("http://api.eia.gov/series/?api_key=",eiaKey,"&series_id=") 
  
  vv = paste0(eiaBase,series_id)
  temp = readLines(vv, warn = "F")
  rd <- fromJSON(temp)
  print(paste0("Retrieving: ",rd$series$series_id))
  print(paste0(rd$series$name))
  
  # Now take the 'data' element from the list and make a data frame
  rd2 = data.frame(rd$series$data,stringsAsFactors = F)
  rd2 = data.table(rd2)
  
  setnames(rd2,1,"year"); setnames(rd2,2,'value')
  rd2[,value:=as.numeric(value)]
  rd2[,year:=as.numeric(year)]
  returnList = list(
    series_id = rd$series$series_id,
    name = rd$series$name,
    units = rd$series$units,
    frequency = rd$series$f,
    data = rd2
  )
  return(rd2) 
}

states = c("AK","AL","AR","AZ","CA",
           "CO","CT","DE","FL","GA",
           "HI","IA","ID","IL","IN",
           "KS","KY","LA","MA","MD",
           "ME","MI","MN","MO","MS",
           "MT","NC","ND","NE","NH",
           "NJ","NM","NV","NY","OH",
           "OK","OR","PA","RI","SC",
           "SD","TN","TX","UT","VA",
           "VT","WA","WI","WV","WY")

#I used just the first two states below to test the code while I was writing it to avoid waiting for all the datasets to be retrieved from EIA
states = c("AK","AL") 

for(state in states){
  #series_list contains the EIA series id for each fuel type that was specificied on Basecamp as well as for total generation
  #each series id is then matched with its fuel type in words
  series_list = data.frame(
    series_id=c(paste0("ELEC.GEN.COW-",state,"-99.A"),
                paste0("ELEC.GEN.PEL-",state,"-99.A"),
                paste0("ELEC.GEN.NG-",state,"-99.A"),
                paste0("ELEC.GEN.NUC-",state,"-99.A"),
                paste0("ELEC.GEN.WND-",state,"-99.A"),
                paste0("ELEC.GEN.SUN-",state,"-99.A"),
                paste0("ELEC.GEN.DPV-",state,"-99.A"),
                paste0("ELEC.GEN.HYC-",state,"-99.A"),
                paste0("ELEC.GEN.WWW-",state,"-99.A"),
                paste0("ELEC.GEN.WAS-",state,"-99.A"),
                paste0("ELEC.GEN.ALL-",state,"-99.A")),
    fuel=c("coal",
           "oil",
           "gas",
           "nuclear",
           "wind",
           "solar_utility", #note utility_solar currently includes all utility-scale solar but this can be changed to just utility-scale PV if needed
           "solar_distributed",
           "hydropower",
           "wood",
           "other_biomass",
           "total"))
  
  series_list$fuel<-as.character(series_list$fuel)
  
  # Building data table `all_generation` by merging data on generation by several fuel types
  all_generation <- NULL
  
  for(row in 1:nrow(series_list)){
    table <- series_list[row,"series_id"]
    fuel <- series_list[row,"fuel"]
    
    possibleError <- tryCatch(eia_series(table,key=eiaKey),error=function(e) e) #not every state has data for each fuel type, which would result in an error
    if(inherits(possibleError, "error")) next #so if data for a particular fuel type is missing it will instead move to the next fuel listed in series_id
    
    dt <- get_EIA_series(eiaKey,table)
    setnames(dt,old="value",new=fuel)
    
    if (is.null(all_generation))
    {all_generation <- dt}
    else
    {all_generation <-  merge(all_generation, dt[], by ="year", all=TRUE)}
    
  }
  #the resulting "all_generation" table is a data.table showing monthly generation in total and by fuel type for each state
  #this combined table for each state could perhaps be written to the database rather than writing each generation by fuel type for each state to the database?
  all_generation[is.na(all_generation)]=0 #there are a few missing values in the data where it makes sense that they would have a value of 0, so changing these to 0 for graphing purposes later on
  col_count=as.numeric(ncol(all_generation))
  #there are many other fuel type categories listed on EIA than the ones specifically retrieved from the series_list
  #account for these other miscellaneous fuel types by combining them into "other" category
  #other is estimated by subracting the sum of generation for all reported fuel types from total generation
  #the sum of generation for all the fuel types is obtained by summing up the second column through the second to last column
  #the first column is year, the last column is total, but different states will have different amounts of columns depending on which fuel types EIA provides data for
  all_generation[,other:=total-rowSums(.SD),.SDcols=2:(col_count-1)] 
  
  lf_all_generation <- melt(all_generation, id="year")
  
  generation_by_type_line <- line_figure(list(lf_all_generation),
                                         "year","Generation (GWh)",paste(state,"Annual Electricity Generation by Fuel Type"),
                                         list("eia_elec_gen_nuc_va_99_m"),modifications = scale_y_continuous(labels = comma)) #same source for all, so choosing random data table that will pull correct source for all
  
  line_name <- str_to_lower(paste(state,"gen_by_fuel_type_line",sep="_"))
  assign(line_name,generation_by_type_line) #could eventually utilize ggsave() here, but wanted to wait until code was finalized
  
  generation_by_type_area <- stacked_area_figure(list(lf_all_generation[variable!="total"]),
                                                 "year","Generation (GWh)",paste(state,"Annual Electricity Generation by Fuel Type"),
                                                 list("eia_elec_gen_nuc_va_99_m"),modifications = scale_y_continuous(labels = comma))
  
  area_name <- str_to_lower(paste(state,"gen_by_fuel_type_area",sep="_"))
  assign(area_name,generation_by_type_area)
  
  dt_name <- str_to_lower(paste("eia_elec_gen",state,"a",sep="_"))
  assign(dt_name,all_generation)
}

al_gen_by_fuel_type_area
al_gen_by_fuel_type_line
ak_gen_by_fuel_type_line
ak_gen_by_fuel_type_area
