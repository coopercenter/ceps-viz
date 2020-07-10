library(here)
library(ggplot2)

source(here::here("derived_values","almanac_gen_figures_calculations.R"))
source(here::here("ggplot2","viz_functions.R")) #sourcing in viz functions

#manually pulling source for bar charts because they are not functionalized
source <- metadata[db_table_name=="eia_elec_gen_was_va_99_a",data_source_full_name] #doesn't matter which table is used-it's all EIA data
source_full <- paste("Source:",source)

generation_by_type <- line_figure(list(lf_all_generation_m), 
                                  "date","Generation (GWh)","Virginia Monthly Electricity Generation by Fuel Type",
                                  list("eia_elec_gen_was_va_99_m"),x_label = "Date",return_static = F) 
generation_by_type

path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="generation_by_type.png")

generation_by_type_p <- ggplotly_wrapper(generation_by_type)
generation_by_type_p

generation_bar <- ggplot(lf_all_generation_a, aes(fill=variable,x=year, y=value)) +
  geom_bar(position = "dodge", stat="identity",aes(group=variable,text=paste0("Year: ",year,"\n","Value: ",value,"\n","Variable: ",variable))) + 
  ylab("Generation (GWh)") + xlab("Year") +
  labs(title ="Virginia Annual Electricity Generation by Fuel Type",caption=source_full)+
  scale_fill_manual(name=NULL,values=ceps_pal[1:12])+
  theme_ceps()
generation_bar

ggsave(path=path2graphics, filename="generation_bar.png")

generation_bar_p <- ggplotly_wrapper(list(figure=generation_bar,x_label="Year",source_description=source_full,title_name="Virginia Annual Electricity Generation by Fuel Type",subtitle_description=NULL,y_label=NULL))
generation_bar_p

generation_bar_stacked <- ggplot(lf_all_generation_a, aes(fill=variable,x=year,y=value)) +
  geom_bar(position = "stack", stat="identity",aes(group=variable,text=paste0("Year: ",year,"\n","Value: ",value,"\n","Variable: ",variable))) + 
  ylab("Generation (GWh)") + xlab(NULL) +
  labs(title ="Virginia Annual Electricity Generation by Fuel Type",caption=source_full) +
  scale_fill_manual(name=NULL,values=ceps_pal[1:12])+
  theme_ceps()
generation_bar_stacked

ggsave(path=path2graphics, filename="generation_bar_stacked.png")

generation_bar_stacked_p <- ggplotly_wrapper(list(figure=generation_bar_stacked,x_label="Year",source_description=source_full,title_name="Virginia Annual Electricity Generation by Fuel Type",subtitle_description=NULL,y_label=NULL))
generation_bar_stacked_p

generation_area_stacked <- line_figure(list(lf_all_generation_a), 
                                       "year","Generation (GWh)","Virginia Annual Electricity Generation by Fuel Type",
                                       list("eia_elec_gen_cow_va_99_a"),return_static = F) 
generation_area_stacked

ggsave(path=path2graphics, filename="generation_area_stacked.png")

generation_area_stacked_p <- ggplotly_wrapper(generation_area_stacked)
generation_area_stacked_p
