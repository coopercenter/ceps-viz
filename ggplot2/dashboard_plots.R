#compilation of all figures relevant to dashboard

library(here)

source(here::here("derived_values","dashboard_calculations.R")) #sourcing in data and reformatted data tables & calculations ready to serve as input to viz functions
source(here::here("ggplot2","viz_functions.R")) #sourcing in viz functions

#----------------------------------------------PLOTTING DONUT FIGURES------------------------------------------------------------------------------

#plotting donut figure of progress towards renewable generation goal------------------------------------------------------------------------------

renewable_percent_gen_2019 = va_annual_renewable_and_carbon_free_gen[year==2019,(all_solar+hydropower)/total]
renewable_percent_gen_2030_goal = .3 #30% of Virginia’s electricity from renewables by 2030

renewable_ring = data.frame(category=c(" ","2019 renewable generation","goal"),
                            value=c(1-renewable_percent_gen_2030_goal,renewable_percent_gen_2019,renewable_percent_gen_2030_goal-renewable_percent_gen_2019))

single_ring_renewable_donut_p <- single_ring_donut_figure_p(renewable_ring,"Renewable Generation","2.6% in 2019","Goal: 30% by 2030","label",c("whitesmoke","steelblue","skyblue"),list("eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_all_va_99_a"))
single_ring_renewable_donut_p

#plotting donut figure of progress towards carbon-free generation goal ------------------------------------------------------------------------------------------
carbon_free_percent_gen_2019 = va_annual_renewable_and_carbon_free_gen[year==2019,(all_solar+hydropower+nuclear)/total]
carbon_free_percent_gen_2050_goal = 1 #100% of Virginia’s electricity from carbon-free sources by 2050

carbon_free_ring = data.frame(category=c("goal","2019 carbon free generation"),
                              value=c(carbon_free_percent_gen_2050_goal-carbon_free_percent_gen_2019,carbon_free_percent_gen_2019))

single_ring_carbon_free_donut_p <- single_ring_donut_figure_p(carbon_free_ring,"Carbon-Free Generation","32.9% in 2019","Goal: 100% by 2050","label",c("mediumseagreen","seagreen"),list("eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_all_va_99_a"))
single_ring_carbon_free_donut_p

#plotting donut figure of progess towards wind and solar capacity goals-----------------------------------------------------------------------------------------
#below is a placeholder for solar and wind progress donut while we wait to hopefully get more data on solar nameplate from DMME
solar_capacity_2019_mw = pjm_solar[status=="In Service",sum(mfo)] #currently only solar in service, no wind
sw_capacity_2024_mw_goal = 16100 #16,100 MW of solar and onshore wind by January 1, 2024 (from VCEA Summary 3.0)

sw_ring = data.frame(category=c("additional capacity necessary to reach goal","2019 capacity"),
                     value=c(sw_capacity_2024_mw_goal-solar_capacity_2019_mw,solar_capacity_2019_mw))

single_ring_sw_capacity_donut_p <- single_ring_donut_figure_p(sw_ring,"Onshore Wind & Solar Capacity","701.4 MW of Solar Capacity as of 2019","Goal: 16,100 MW of Onshore Wind & Solar Capacity by 2024","label+value",c("indianred","maroon"),list("pjm_solar","pjm_wind"))
single_ring_sw_capacity_donut_p

#--------------------------------------------PLOTTING GENERATION/PRODUCTION FIGURES----------------------------------------------------------------

va_annual_production_area <- stacked_area_figure(list(eia_elec_gen_cow_va_99_a,eia_elec_gen_pel_va_99_a,eia_elec_gen_ng_va_99_a,eia_elec_gen_nuc_va_99_a,eia_elec_gen_sun_va_99_a,eia_elec_gen_dpv_va_99_a,eia_elec_gen_hyc_va_99_a,eia_elec_gen_www_va_99_a,eia_elec_gen_was_va_99_a,other_annual_generation),
                                                 "year","Generation (GWh)","Virginia Annual Energy Generation by Fuel Type",
                                                 list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                                 lower_limit = -1900,return_static = F)
va_annual_production_area

va_annual_production_area_p <- ggplotly_wrapper(va_annual_production_area) 
va_annual_production_area_p

va_annual_production_2019_pie_chart_p <- pie_chart_figure_p(list(eia_elec_gen_cow_va_99_a[year==2019],eia_elec_gen_pel_va_99_a[year==2019],eia_elec_gen_ng_va_99_a[year==2019],eia_elec_gen_nuc_va_99_a[year==2019],eia_elec_gen_sun_va_99_a[year==2019],eia_elec_gen_dpv_va_99_a[year==2019],eia_elec_gen_hyc_va_99_a[year==2019],eia_elec_gen_www_va_99_a[year==2019],eia_elec_gen_was_va_99_a[year==2019],other_annual_generation[year==2019]),
                                                            "year","Virginia 2019 Energy Generation",
                                                            list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"))
va_annual_production_2019_pie_chart_p

va_annual_production_2019_pie_chart_p_with_legend <- pie_chart_figure_p(list(eia_elec_gen_cow_va_99_a[year==2019],eia_elec_gen_pel_va_99_a[year==2019],eia_elec_gen_ng_va_99_a[year==2019],eia_elec_gen_nuc_va_99_a[year==2019],eia_elec_gen_sun_va_99_a[year==2019],eia_elec_gen_dpv_va_99_a[year==2019],eia_elec_gen_hyc_va_99_a[year==2019],eia_elec_gen_www_va_99_a[year==2019],eia_elec_gen_was_va_99_a[year==2019],other_annual_generation[year==2019]),
                                                                        "year","Virginia 2019 Energy Generation",
                                                                        list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                                                        legend_shown = T)
va_annual_production_2019_pie_chart_p_with_legend

#--------------------------------------------PLOTTING CONSUMPTION FIGURES---------------------------------------------------------------------
va_annual_consumption_area <- stacked_area_figure(list(eia_seds_tercb_va_a,eia_seds_teccb_va_a,eia_seds_teicb_va_a,eia_seds_teacb_va_a),
                                                  "year","Consumption (Billion Btu)","Virginia Annual Energy Consumption by Sector",
                                                  list("eia_seds_tercb_va_a","eia_seds_teccb_va_a","eia_seds_teicb_va_a","eia_seds_teacb_va_a"),
                                                  return_static = F, modifications = scale_y_continuous(labels = comma))
va_annual_consumption_area

va_annual_consumption_area_p <- ggplotly_wrapper(va_annual_consumption_area)
va_annual_consumption_area_p

va_annual_consumption_2018_pie_chart_p <- pie_chart_figure_p(list(eia_seds_tercb_va_a[year==2018],eia_seds_teccb_va_a[year==2018],eia_seds_teicb_va_a[year==2018],eia_seds_teacb_va_a[year==2018]),
                                                             "year","Virginia 2018 Energy Consumption",
                                                             list("eia_seds_tercb_va_a","eia_seds_teccb_va_a","eia_seds_teicb_va_a","eia_seds_teacb_va_a"))
va_annual_consumption_2018_pie_chart_p

va_annual_consumption_2018_pie_chart_p_with_legend <-  pie_chart_figure_p(list(eia_seds_tercb_va_a[year==2018],eia_seds_teccb_va_a[year==2018],eia_seds_teicb_va_a[year==2018],eia_seds_teacb_va_a[year==2018]),
                                                                          "year","Virginia 2018 Energy Consumption",
                                                                          list("eia_seds_tercb_va_a","eia_seds_teccb_va_a","eia_seds_teicb_va_a","eia_seds_teacb_va_a"),
                                                                          legend_shown = T)
va_annual_consumption_2018_pie_chart_p_with_legend

#--------------------------------PLOTTING RENEWABLE & CARBON-FREE GENERATION IN PARTICULAR-----------------------------------------------------

# Graphing % of VA power generation (in GWh/yr) from renewables & carbon-free sources 
lf_percent_renewable_and_carbon_free<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,percent_renewable,percent_carbon_free)],id="year")

percent_renewable_and_carbon_free_line <- line_figure(list(lf_percent_renewable_and_carbon_free),
                                                      "year","Percentage of Total Generation","Percentage of Virginia Annual Energy Generation",
                                                      list("eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a"),
                                                      return_static = F)
percent_renewable_and_carbon_free_line

percent_renewable_and_carbon_free_line_p <- ggplotly_wrapper(percent_renewable_and_carbon_free_line,line_figure = T)
percent_renewable_and_carbon_free_line_p

# Solar, Hydro, and Nuclear Generation over Time
annual_carbon_free_generation_by_type_line <- line_figure(list(eia_elec_gen_nuc_va_99_a,eia_elec_gen_sun_va_99_a,eia_elec_gen_dpv_va_99_a,eia_elec_gen_hyc_va_99_a,eia_elec_gen_all_va_99_a),
                                                          "year","Generation (GWh)","Virginia Annual Energy Generation by Carbon-Free Sources",
                                                          list("eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_all_va_99_a"),
                                                          return_static = F)
annual_carbon_free_generation_by_type_line

annual_carbon_free_generation_by_type_line_p <- ggplotly_wrapper(annual_carbon_free_generation_by_type_line,line_figure = T)
annual_carbon_free_generation_by_type_line_p

# Solar (broken into distributed and utility) over time
solar_generation_time_series_line <- line_figure(list(eia_elec_gen_sun_va_99_a[utility_solar!=0],eia_elec_gen_dpv_va_99_a),
                                                 "year","Generation (GWh)","Virginia Annual Energy Generation of Solar Energy",
                                                 list("eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a"),
                                                 return_static = F)
solar_generation_time_series_line

solar_generation_time_series_line_p <- ggplotly_wrapper(solar_generation_time_series_line,line_figure = T)
solar_generation_time_series_line_p

# Wood generation over time
wood_generation_time_series_line <- line_figure(list(melt(eia_elec_gen_www_va_99_a,id="year")),
                                                "year","Generation (GWh)","Virginia Annual Energy Generation from Wood",
                                                list("eia_elec_gen_www_va_99_a"),
                                                return_static = F, modifications = theme(legend.position = "none"))
wood_generation_time_series_line

wood_generation_time_series_line_p <- ggplotly_wrapper(wood_generation_time_series_line,line_figure = T)
wood_generation_time_series_line_p

# Projected wind generation overtime
wind_projected_generation_time_series_line <- line_figure(list(melt(total_production_forecast_offshore_wind,id="Year")),
                                                          "Year","Projected Generation (GWh)","Virginia Projected Annual Offshore Wind Energy Generation",
                                                          list("total_production_forecast_offshore_wind"),
                                                          return_static = F, modifications = theme(legend.position = "none"), subtitle_description = "Forecast")
wind_projected_generation_time_series_line

wind_projected_generation_time_series_line_p <- ggplotly_wrapper(wind_projected_generation_time_series_line,line_figure = T)
wind_projected_generation_time_series_line_p

# Projected wind capacity
wind_projected_capacity_line <- line_figure(list(melt(total_mw_offshore_wind,id="Year")),
                                            "Year","Projected Capacity (MW)","Virginia Projected Offshore Wind Capacity",
                                            list("total_mw_offshore_wind"),
                                            return_static = F, subtitle_description = "Forecast")
wind_projected_capacity_line

wind_projected_capacity_line_p <- ggplotly_wrapper(wind_projected_capacity_line, line_figure = T)
wind_projected_capacity_line_p

#Stacked Annual Carbon Free Generation Broken Out by Type
carbon_free_generation_by_type_stacked <- stacked_area_figure(list(eia_elec_gen_nuc_va_99_a,eia_elec_gen_sun_va_99_a,eia_elec_gen_dpv_va_99_a,eia_elec_gen_hyc_va_99_a),
                                                              "year","Generation (GWh)","Virginia Annual Energy Generation by Carbon-Free Sources",
                                                              list("eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a"),
                                                              return_static = F)
carbon_free_generation_by_type_stacked

carbon_free_generation_by_type_stacked_p <- ggplotly_wrapper(carbon_free_generation_by_type_stacked)
carbon_free_generation_by_type_stacked_p

# Stacked Annual Renewable Generation Broken Out by Type (hydro, utility solar, distributed solar)
renewable_generation_by_type_stacked <- stacked_area_figure(list(eia_elec_gen_sun_va_99_a,eia_elec_gen_dpv_va_99_a,eia_elec_gen_hyc_va_99_a),
                                                            "year","Generation (GWh)","Virginia Annual Energy Generation by Renewable Sources",
                                                            list("eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a"),
                                                            return_static = F)
renewable_generation_by_type_stacked

renewable_generation_by_type_stacked_p <- ggplotly_wrapper(renewable_generation_by_type_stacked)
renewable_generation_by_type_stacked_p

# Stacked Renewable versus Non-renewable Generation
renewable_vs_non_renewable<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,renewable,not_renewable)],id="year")

renewable_versus_non_renewable_stacked <- stacked_area_figure(list(renewable_vs_non_renewable),
                                                              "year","Generation (GWh)","Virginia Annual Renewable and Non-Renewable Energy Generation",
                                                              list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                                              return_static = F)
renewable_versus_non_renewable_stacked

renewable_versus_non_renewable_stacked_p <- ggplotly_wrapper(renewable_versus_non_renewable_stacked)
renewable_versus_non_renewable_stacked_p

# Stacked Carbon versus Carbon Free Generation
carbon_vs_carbon_free<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,carbon_free,carbon_emitting)],id="year")

carbon_versus_carbon_free_stacked <- stacked_area_figure(list(carbon_vs_carbon_free),
                                                         "year","Generation (GWh)","Virginia Annual Carbon Emitting and Carbon-Free Energy Generation",
                                                         list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                                         return_static = F)
carbon_versus_carbon_free_stacked

carbon_versus_carbon_free_stacked_p <- ggplotly_wrapper(carbon_versus_carbon_free_stacked)
carbon_versus_carbon_free_stacked_p

# Total Renewable and Total Carbon Free Generation Over Time
renewable_and_carbon_free<-melt(va_annual_renewable_and_carbon_free_gen[,.(year,carbon_free,renewable,total)],id="year")

renewable_and_carbon_free_line <- line_figure(list(renewable_and_carbon_free),
                                              "year","Generation (GWh)","Virginia Annual Renewable and Carbon-Free Energy Generation",
                                              list("eia_elec_gen_cow_va_99_a","eia_elec_gen_pel_va_99_a","eia_elec_gen_ng_va_99_a","eia_elec_gen_nuc_va_99_a","eia_elec_gen_sun_va_99_a","eia_elec_gen_dpv_va_99_a","eia_elec_gen_hyc_va_99_a","eia_elec_gen_www_va_99_a","eia_elec_gen_was_va_99_a"),
                                              return_static = F)
renewable_and_carbon_free_line

renewable_and_carbon_free_line_p <- ggplotly_wrapper(renewable_and_carbon_free_line,line_figure = T)
renewable_and_carbon_free_line_p

#--------------------------PLOTTING WIND AND SOLAR PROJECTED CAPACITY ADDITIONS----------------------------------

apco_dom_target_vs_projected_capacity <- line_figure(list(lf_apco_dom_onwind_and_solar[date<='2040-01-01']),
                                                     "date","Capacity (MW)", "Appalachian Power & Dominion Onshore Wind and Solar",
                                                     list("pjm_solar","pjm_wind","VCEA_onshore_wind_solar"),
                                                     return_static = F, subtitle_description = "Current and Projected Capacity vs VCEA Target Capacity")
apco_dom_target_vs_projected_capacity

apco_dom_target_vs_projected_capacity_p <- ggplotly_wrapper(apco_dom_target_vs_projected_capacity,line_figure = T)
apco_dom_target_vs_projected_capacity_p

apco_dom_projected_capacity <- line_figure(list(lf_apco_dom_onwind_and_solar[variable!="target_apco_onshore_wind_and_solar"&variable!="target_dom_onshore_wind_and_solar"&date<"2024-01-01"]),
                                        "date","Capacity (MW)", "Appalachian Power & Dominion Onshore Wind and Solar",
                                        list("pjm_solar","pjm_wind"),
                                        return_static = F,subtitle_description = "Current and Projected Capacity")
apco_dom_projected_capacity

apco_dom_projected_capacity_p <- ggplotly_wrapper(apco_dom_projected_capacity, line_figure = T)
apco_dom_projected_capacity_p

apco_dom_target_capacity <- line_figure(list(melt(VCEA_onshore_wind_solar[,.(year,target_apco_onshore_wind_and_solar,target_dom_onshore_wind_and_solar)],id="year")),
                                        "year","Capacity (MW)", "Appalachian Power & Dominion Onshore Wind and Solar",
                                        list("VCEA_onshore_wind_solar"),
                                        return_static = F,subtitle_description = "VCEA Target Capacity")
apco_dom_target_capacity

apco_dom_target_capacity_p <- ggplotly_wrapper(apco_dom_target_capacity, line_figure = T)
apco_dom_target_capacity_p

on_off_wind_solar_line <- line_figure(list(lf_wind_and_solar_capacity_projections),
                                      "date","Capacity (MW)","Virginia Current and Projected Wind and Solar Capacity",
                                      list("pjm_wind","pjm_solar"),
                                      return_static = F)
on_off_wind_solar_line

on_off_wind_solar_line_p <- ggplotly_wrapper(on_off_wind_solar_line,line_figure = T)
on_off_wind_solar_line_p

on_off_wind_solar_area <- stacked_area_figure(list(lf_wind_and_solar_capacity_projections),
                                              "date","Capacity (MW)","Virginia Current and Projected Wind and Solar Capacity",
                                              list("pjm_wind","pjm_solar"),
                                              return_static = F)
on_off_wind_solar_area

on_off_wind_solar_area_p <- ggplotly_wrapper(on_off_wind_solar_area)
on_off_wind_solar_area_p

#--------------------------------PLOTTING EMISSIONS FIGURES--------------------------------------------------------

# CO2 total emissions & CO2 emissions from electric sector on same figure
co2_combined_emissions_line <- line_figure(list(eia_emiss_co2_totv_ec_to_va_a,eia_emiss_co2_totv_tt_to_va_a),
                                           "year","Emissions (million metric tons CO2)","Virginia Annual CO2 Emissions",
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
                                                        "year","Emissions (million metric tons)","Virginia Annual CO2 Emissions By Fuel Type",
                                                        list("emissions_co2_by_source_va"),
                                                        return_static = F)
carbon_by_fuel_emissions_stacked

carbon_by_fuel_emissions_stacked_p <- ggplotly_wrapper(carbon_by_fuel_emissions_stacked)
carbon_by_fuel_emissions_stacked_p 

#-------------------------------------PLOTTING ENERGY EFFICIENCY FIGURES--------------------------------------

consumption_per_gdp_line <- line_figure(list(melt(energy_consumption_per_unit_gdp_va,id="year")),
                                        "year","Consumption per GDP (Btu/$)","VA Electricity Consumption per unit GDP",
                                        list("fred_vangsp","eia_seds_tetcb_va_a"), #for now, may change to derived values table name at some point 
                                        return_static = F, modifications = theme(legend.position = "none"))
consumption_per_gdp_line

consumption_per_gdp_line_p <- ggplotly_wrapper(consumption_per_gdp_line,line_figure = T)
consumption_per_gdp_line_p

consumption_per_capita_line <- line_figure(list(melt(energy_consumption_per_capita_va,id="year")),
                                           "year","Consumption per Capita (Thousand Btu/Person)","VA Electricity Consumption per Capita",
                                           list("residential_population_va","eia_seds_tetcb_va_a"), #for now, may change to derived values table names at some point 
                                           return_static = F, modifications = theme(legend.position = "none"))
consumption_per_capita_line

consumption_per_capita_line_p <- ggplotly_wrapper(consumption_per_capita_line,line_figure = T)
consumption_per_capita_line_p

#----------------------------------------PLOTTING GEOSPATIAL DATA----------------------------------------------------------
#energy equity figures

#energy burden map showing average energy expenditures by county
va_avg_annual_energy_cost <- ggplot() +
  geom_sf(data = world, fill = "antiquewhite1") +
  geom_sf(data = va_energy_equity_by_county, aes(fill = avg_annual_energy_cost)) +
  geom_sf(data = states, fill = NA) +
  scale_fill_viridis_c(alpha = .6,name="Average Annual Energy Cost \nin Dollars\n") + #setting alpha adds some transparency
  coord_sf(xlim = c(-84, -75), ylim = c(36, 40), expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") +
  labs(title = "Virginia Energy Burden by County", subtitle = "For Households Below the Federal Poverty Level",caption = paste0("Source: ",expenditures_source)) +
  theme(panel.background = element_rect(fill = "aliceblue"),
        plot.caption=element_text(hjust = 0.5,face="italic"),
        legend.title=element_text(size=7),
        legend.text=element_text(size=7))
va_avg_annual_energy_cost 

#energy burden map showing average energy expenditures as percent of income by county
va_avg_annual_energy_percent_exp <-  ggplot() +
  geom_sf(data = world,fill = "antiquewhite1") +
  geom_sf(data = va_energy_equity_by_county, aes(fill = avg_energy_burden_as_percent_income)) +
  geom_sf(data = states, fill = NA) +
  scale_fill_viridis_c(alpha = .6,name="Average Annual Energy Cost \nas Percentage of Income\n") + #setting alpha adds some transparency
  coord_sf(xlim = c(-84, -75), ylim = c(36, 40), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  labs(title = "Virginia Energy Burden by County", subtitle = "For Households Below the Federal Poverty Level", caption = paste0("Source: ",percent_income_source)) + 
  theme(panel.background = element_rect(fill = "aliceblue"),
        plot.caption=element_text(hjust = 0.5,face="italic"),
        legend.title=element_text(size=7),
        legend.text=element_text(size=7))
va_avg_annual_energy_percent_exp

va_avg_annual_energy_cost_p <- ggplotly(va_avg_annual_energy_cost,tooltip = NULL) %>%
  layout(title = list(text=paste0("Virginia Energy Burden by County","<br>","<sup>","For Households Below the Federal Poverty Level","</sup>")),
         xaxis=list(title = paste0("Longitude","<br>","<i>","<sub>",paste0("Source: ",expenditures_source),"<sub>","<i>"),titlefont=list(size=15)),
         yaxis=list(titlefont=list(size=15)))%>%
  config(displaylogo = FALSE)
va_avg_annual_energy_cost_p

va_avg_annual_energy_percent_exp_p <- ggplotly(va_avg_annual_energy_percent_exp,tooltip = NULL) %>%
  layout(title = list(text=paste0("Virginia Energy Burden by County","<br>","<sup>","For Households Below the Federal Poverty Level","</sup>")),
         xaxis=list(title = paste0("Longitude","<br>","<i>","<sub>",paste0("Source: ",expenditures_source),"<sub>","<i>"),titlefont=list(size=15)),
         yaxis=list(titlefont=list(size=15)))%>%
  config(displaylogo = FALSE)
va_avg_annual_energy_percent_exp_p

