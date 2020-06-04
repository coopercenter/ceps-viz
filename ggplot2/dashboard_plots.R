#compilation of all figures relevant to dashboard

library(here)
library(data.table)
library(RPostgreSQL)
library(scales)

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
               "eia_emiss_co2_totv_tt_to_va_a"),
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
               "total_CO2_emissions")
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

source(here::here("ggplot2","viz_functions.R"))

#----------------------------------------------PLOTTING DONUT FIGURES------------------------------------------------------------------------------

#plotting donut figure of progress towards renewable generation goal------------------------------------------------------------------------------

#isolating renewable and carbon free generation in it's own table
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

renewable_percent_gen_2019 = va_annual_renewable_and_carbon_free_gen[year==2019,(all_solar+hydropower)/total]
renewable_percent_gen_2030_goal = .3 #30% of Virginia’s electricity from renewables by 2030

renewable_donut_p <- donut_figure_p(renewable_percent_gen_2019,"2019","2.6%",renewable_percent_gen_2030_goal,"2030","30%","Renewable Generation","skyblue","steelblue")
renewable_donut_p

single_ring_renewable_donut_p <- single_ring_donut_figure_p(renewable_percent_gen_2019,"2019","2.6%",renewable_percent_gen_2030_goal,"2030","30%","Renewable Generation","skyblue","steelblue")
single_ring_renewable_donut_p

#plotting donut figure of progress towards carbon-free generation goal ------------------------------------------------------------------------------------------
carbon_free_percent_gen_2019 = va_annual_renewable_and_carbon_free_gen[year==2019,(all_solar+hydropower+nuclear)/total]
carbon_free_percent_gen_2050_goal = 1 #100% of Virginia’s electricity from carbon-free sources by 2050

carbon_free_donut_p <- donut_figure_p(carbon_free_percent_gen_2019,"2019","32.9%",carbon_free_percent_gen_2050_goal,"2050","100%","Carbon-Free Generation","mediumseagreen","seagreen")
carbon_free_donut_p

single_ring_carbon_free_donut_p <- single_ring_donut_figure_p(carbon_free_percent_gen_2019,"2019","32.9%",carbon_free_percent_gen_2050_goal,"2050","100%","Carbon-Free Generation","mediumseagreen","seagreen")
single_ring_carbon_free_donut_p

#plotting donut figure of progess towards wind and solar capacity goals-----------------------------------------------------------------------------------------
solar_capacity_2018_mw = whole_electric_industry_capacity[Year==2018,as.numeric(Solar)]
sw_capacity_2028_goal_mw = 5500 #5,500 MW of onshore wind and solar energy total [in operation] by 2028
sw_capacity_2030_goal_mw = 13600 #13,600 MW of onshore wind and solar energy total by 2030 (from 'Virginia Clean Economy progress dashboard -- UPDATED DRAFT')

solar_capacity_percent_2018 = solar_capacity_2018_mw/sw_capacity_2030_goal_mw
sw_capacity_percent_goal_2028 = sw_capacity_2028_goal_mw/sw_capacity_2030_goal_mw
sw_capacity_percent_goal_2030 = sw_capacity_2030_goal_mw/sw_capacity_2030_goal_mw

sw_capacity_donut_p = donut_figure_p(solar_capacity_percent_2018,"2018","392.5 MW",sw_capacity_percent_goal_2028,"2028","5,500 MW in Operation","Wind & Solar Energy","lightcoral","indianred",sw_capacity_percent_goal_2030,"2030","13,600 MW Total","maroon")
sw_capacity_donut_p

single_ring_sw_capacity_donut_p <- single_ring_donut_figure_p(solar_capacity_percent_2018,"2018","392.5 MW",sw_capacity_percent_goal_2028,"2028","5,500 MW in Operation","Wind & Solar Energy","lightcoral","indianred",sw_capacity_percent_goal_2030,"2030","13,600 MW Total","maroon")
single_ring_sw_capacity_donut_p

#--------------------------------------------PLOTTING GENERATION/PRODUCTION FIGURES----------------------------------------------------------------

#creating 'other' generation measure by combining all by fuel type generation and total generation in a table to caluclate other generation over time
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

va_annual_production_area <- stacked_area_figure(list(eia_elec_gen_cow_va_99_a,eia_elec_gen_pel_va_99_a,eia_elec_gen_ng_va_99_a,eia_elec_gen_nuc_va_99_a,eia_elec_gen_sun_va_99_a,eia_elec_gen_dpv_va_99_a,eia_elec_gen_hyc_va_99_a,eia_elec_gen_www_va_99_a,eia_elec_gen_was_va_99_a,other_annual_generation),
                                                 "year","GWh","VA Annual Generation by Fuel Type",
                                                 list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                                 lower_limit = -1900,return_static = F)
va_annual_production_area

va_annual_production_area_p <- ggplotly_wrapper(va_annual_production_area)
va_annual_production_area_p

va_annual_production_2019_pie_chart_p <- pie_chart_figure_p(list(eia_elec_gen_cow_va_99_a[year==2019],eia_elec_gen_pel_va_99_a[year==2019],eia_elec_gen_ng_va_99_a[year==2019],eia_elec_gen_nuc_va_99_a[year==2019],eia_elec_gen_sun_va_99_a[year==2019],eia_elec_gen_dpv_va_99_a[year==2019],eia_elec_gen_hyc_va_99_a[year==2019],eia_elec_gen_www_va_99_a[year==2019],eia_elec_gen_was_va_99_a[year==2019],other_annual_generation[year==2019]),
                                                            "year","VA 2019 Generation",
                                                            list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"))
va_annual_production_2019_pie_chart_p

va_annual_production_2019_pie_chart_p_with_legend <- pie_chart_figure_p(list(eia_elec_gen_cow_va_99_a[year==2019],eia_elec_gen_pel_va_99_a[year==2019],eia_elec_gen_ng_va_99_a[year==2019],eia_elec_gen_nuc_va_99_a[year==2019],eia_elec_gen_sun_va_99_a[year==2019],eia_elec_gen_dpv_va_99_a[year==2019],eia_elec_gen_hyc_va_99_a[year==2019],eia_elec_gen_www_va_99_a[year==2019],eia_elec_gen_was_va_99_a[year==2019],other_annual_generation[year==2019]),
                                                                        "year","VA 2019 Generation",
                                                                        list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                                                        legend_shown = T)
va_annual_production_2019_pie_chart_p_with_legend

#--------------------------------------------PLOTTING CONSUMPTION FIGURES---------------------------------------------------------------------
va_annual_consumption_area <- stacked_area_figure(list(eia_seds_tercb_va_a,eia_seds_teccb_va_a,eia_seds_teicb_va_a,eia_seds_teacb_va_a),
                                                  "year","Billion Btu","VA Annual Consumption by Sector",
                                                  list("eia_seds_tercb_va_a","eia_seds_teccb_va_a","eia_seds_teicb_va_a","eia_seds_teacb_va_a"),
                                                  return_static = F, modifications = scale_y_continuous(labels = comma))
va_annual_consumption_area

va_annual_consumption_area_p <- ggplotly_wrapper(va_annual_consumption_area)
va_annual_consumption_area_p

va_annual_consumption_2017_pie_chart_p <- pie_chart_figure_p(list(eia_seds_tercb_va_a[year==2017],eia_seds_teccb_va_a[year==2017],eia_seds_teicb_va_a[year==2017],eia_seds_teacb_va_a[year==2017]),
                                                             "year","VA 2017 Consumption",
                                                             list("eia_seds_tercb_va_a","eia_seds_teccb_va_a","eia_seds_teicb_va_a","eia_seds_teacb_va_a"))
va_annual_consumption_2017_pie_chart_p

va_annual_consumption_2017_pie_chart_p_with_legend <-  pie_chart_figure_p(list(eia_seds_tercb_va_a[year==2017],eia_seds_teccb_va_a[year==2017],eia_seds_teicb_va_a[year==2017],eia_seds_teacb_va_a[year==2017]),
                                                                          "year","VA 2017 Consumption",
                                                                          list("eia_seds_tercb_va_a","eia_seds_teccb_va_a","eia_seds_teicb_va_a","eia_seds_teacb_va_a"),
                                                                          legend_shown = T)
va_annual_consumption_2017_pie_chart_p_with_legend

#--------------------------------PLOTTING RENEWABLE & CARBON-FREE GENERATION IN PARTICULAR-----------------------------------------------------

# Finding sum of total annual renewable generation
va_annual_renewable_and_carbon_free_gen[,renewable:=all_solar+hydropower]

# Finding total annual renewable generation as a percent of total energy generation
va_annual_renewable_and_carbon_free_gen[,percent_renewable:=(renewable/total)*100]

# Finding sum of total annual carbon-free generation
va_annual_renewable_and_carbon_free_gen[,carbon_free:=hydropower+all_solar+nuclear]

# Finding total annual carbon-free generation as a percent of total energy generation
va_annual_renewable_and_carbon_free_gen[,percent_carbon_free:=(carbon_free/total)*100]

# Graphing % of VA power generation (in GWh/yr) from renewables & carbon-free sources 
lf_percent_renewable_and_carbon_free<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,percent_renewable,percent_carbon_free)],id="year")

percent_renewable_and_carbon_free_line <- line_figure(list(lf_percent_renewable_and_carbon_free),
                                                      "year","Percentage","Percentage of Total Annual VA Energy Generation",
                                                      list("eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a"),
                                                      return_static = F)
percent_renewable_and_carbon_free_line

percent_renewable_and_carbon_free_line_p <- ggplotly_wrapper(percent_renewable_and_carbon_free_line,line_figure = T)
percent_renewable_and_carbon_free_line_p

# Solar, Hydro, and Nuclear Generation over Time
annual_carbon_free_generation_by_type_line <- line_figure(list(eia_elec_gen_nuc_va_99_a,eia_elec_gen_sun_va_99_a,eia_elec_gen_dpv_va_99_a,eia_elec_gen_hyc_va_99_a,eia_elec_gen_all_va_99_a),
                                                          "year","GWh","Annual VA Generation by Carbon-Free Sources",
                                                          list("eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_all_va_99_a"),
                                                          return_static = F)
annual_carbon_free_generation_by_type_line

annual_carbon_free_generation_by_type_line_p <- ggplotly_wrapper(annual_carbon_free_generation_by_type_line,line_figure = T)
annual_carbon_free_generation_by_type_line_p

# Solar (broken into distributed and utility) over time
solar_generation_time_series_line <- line_figure(list(eia_elec_gen_sun_va_99_a[utility_solar!=0],eia_elec_gen_dpv_va_99_a),
                                                 "year","GWh","Annual VA Generation of Solar Energy",
                                                 list("eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a"),
                                                 return_static = F)
solar_generation_time_series_line

solar_generation_time_series_line_p <- ggplotly_wrapper(solar_generation_time_series_line,line_figure = T)
solar_generation_time_series_line_p

# Wood generation over time
wood_generation_time_series_line <- line_figure(list(melt(eia_elec_gen_www_va_99_a,id="year")),
                                                "year","GWh","Annual VA Energy Generation from Wood",
                                                list("eia_elec_gen_www_va_99_a"),
                                                return_static = F, modifications = theme(legend.position = "none"))
wood_generation_time_series_line

wood_generation_time_series_line_p <- ggplotly_wrapper(wood_generation_time_series_line,line_figure = T)
wood_generation_time_series_line_p

#Stacked Annual Carbon Free Generation Broken Out by Type
carbon_free_generation_by_type_stacked <- stacked_area_figure(list(eia_elec_gen_nuc_va_99_a,eia_elec_gen_sun_va_99_a,eia_elec_gen_dpv_va_99_a,eia_elec_gen_hyc_va_99_a),
                                                              "year","GWh","Annual VA Generation by Carbon-Free Sources",
                                                              list("eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a"),
                                                              return_static = F)
carbon_free_generation_by_type_stacked

carbon_free_generation_by_type_stacked_p <- ggplotly_wrapper(carbon_free_generation_by_type_stacked)
carbon_free_generation_by_type_stacked_p

# Stacked Annual Renewable Generation Broken Out by Type (hydro, utility solar, distributed solar)
renewable_generation_by_type_stacked <- stacked_area_figure(list(eia_elec_gen_sun_va_99_a,eia_elec_gen_dpv_va_99_a,eia_elec_gen_hyc_va_99_a),
                                                            "year","GWh","Annual VA Generation by Renewable Sources",
                                                            list("eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a"),
                                                            return_static = F)
renewable_generation_by_type_stacked

renewable_generation_by_type_stacked_p <- ggplotly_wrapper(renewable_generation_by_type_stacked)
renewable_generation_by_type_stacked_p

# Stacked Renewable versus Non-renewable Generation
va_annual_renewable_and_carbon_free_gen[,not_renewable:=total-renewable]
renewable_vs_non_renewable<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,renewable,not_renewable)],id="year")

renewable_versus_non_renewable_stacked <- stacked_area_figure(list(renewable_vs_non_renewable),
                                                              "year","GWh","Annual VA Renewable and Non-Renewable Generation",
                                                              list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                                              return_static = F)
renewable_versus_non_renewable_stacked

renewable_versus_non_renewable_stacked_p <- ggplotly_wrapper(renewable_versus_non_renewable_stacked)
renewable_versus_non_renewable_stacked_p

# Stacked Carbon versus Carbon Free Generation
va_annual_renewable_and_carbon_free_gen[,carbon_emitting:=total-carbon_free]
carbon_vs_carbon_free<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,carbon_free,carbon_emitting)],id="year")

carbon_versus_carbon_free_stacked <- stacked_area_figure(list(carbon_vs_carbon_free),
                                                         "year","GWh","Annual VA Carbon Emitting and Carbon-Free Generation",
                                                         list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                                         return_static = F)
carbon_versus_carbon_free_stacked

carbon_versus_carbon_free_stacked_p <- ggplotly_wrapper(carbon_versus_carbon_free_stacked)
carbon_versus_carbon_free_stacked_p

# Total Renewable and Total Carbon Free Generation Over Time
renewable_and_carbon_free<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,carbon_free,renewable,total)],id="year")

renewable_and_carbon_free_line <- line_figure(list(renewable_and_carbon_free),
                                              "year","GWh","Annual VA Renewable and Carbon-Free Generation",
                                              list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                              return_static = F)
renewable_and_carbon_free_line

renewable_and_carbon_free_line_p <- ggplotly_wrapper(renewable_and_carbon_free_line,line_figure = T)
renewable_and_carbon_free_line_p

#--------------------------------PLOTTING EMISSIONS FIGURES--------------------------------------------------------

# CO2 total emissions & CO2 emissions from electric sector on same figure
co2_combined_emissions_line <- line_figure(list(eia_emiss_co2_totv_ec_to_va_a,eia_emiss_co2_totv_tt_to_va_a),
                                           "year","Emissions (million metric tons CO2)","Virginia Combined Annual CO2 Emissions",
                                           list("eia_emiss_co2_totv_ec_to_va_a","eia_emiss_co2_totv_tt_to_va_a"),
                                           return_static = F)
co2_combined_emissions_line

co2_combined_emissions_line_p <- ggplotly_wrapper(co2_combined_emissions_line,line_figure = T)
co2_combined_emissions_line_p

# Emissions by compound
# note: to convert from short tons to million metric tons, divide short tons by 1102311.31 & to convert from thousand metric tons to million metric tons, divide by 1000
emissions_line <- line_figure(list(emissions_co2_by_source_va[,.(year=year,CO2=total/1000)],emissions_no_by_source_va[,.(year=year,NO=total/1102311.31)],emissions_so2_by_source_va[,.(year=year,SO2=total/1102311.31)]),
                              "year","Emissions (million metric tons)","Virginia Annual Emissions",
                              list("emissions_co2_by_source_va","emissions_no_by_source_va","emissions_so2_by_source_va"),
                              return_static = F)
emissions_line

emissions_line_p <- ggplotly_wrapper(emissions_line,line_figure = T)
emissions_line_p

# CO2 emissions by fuel type
carbon_by_fuel_emissions_stacked <- stacked_area_figure(list(melt(emissions_co2_by_source_va[,.(year,coal=coal/1000,natural_gas=natural_gas/1000,petroleum=petroleum/1000,other=other/1000)],id="year")),
                                                        "year","Emissions (million metric tons)","Virginia CO2 Emissions By Fuel Type",
                                                        list("emissions_co2_by_source_va"),
                                                        return_static = F)
carbon_by_fuel_emissions_stacked

carbon_by_fuel_emissions_stacked_p <- ggplotly_wrapper(carbon_by_fuel_emissions_stacked)
carbon_by_fuel_emissions_stacked_p 

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
