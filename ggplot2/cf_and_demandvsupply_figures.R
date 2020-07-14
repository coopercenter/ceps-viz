library(here)

source(here::here("derived_values","cf_and_demandvsupply_figures_calculations.R")) #sourcing in data and reformatted data tables & calculations ready to serve as input to viz functions
source(here::here("ggplot2","viz_functions.R")) #sourcing in viz functions

ma3_graph <- ggplot(daily_generation[!is.na(solar_cf_ma3)],aes(x=date,y=solar_cf_ma3)) + 
  geom_boxplot(aes(y=solar_cf_ma3,x=as.factor(month))) + ylim(0,.40) +
  labs(title="PJM Solar Capacity Factor: 3-day Moving Average",
       x="Month", y="Capacity Factor")+
  scale_x_discrete(labels=month.abb)
ma3_graph

path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="ma3.png")

#looking at mothly cfs visually
monthly_cf_graph<-line_figure(list(cf_monthly_melted),
                             merge_variable = "month",
                             value_unit="Capacity Factor",
                             title_name = "PJM Average Solar Capacity Factors",
                            x_label="Month",
                             return_static = F,
                            modifications = scale_x_discrete(limits=month.abb))
monthly_cf_graph

#now daily
daily_cf_graph <-ggplot() +
  geom_line(data=cf_daily,mapping=aes(date,daily_cf)) + 
  geom_point(data=cf_daily,mapping=aes(date,daily_cf)) + 
  ylab("Capacity Factor") + xlab("Date")+
  scale_x_date(date_labels = "%m-%d")+
  labs(title="PJM Average Solar Capacity Factors",subtitle="By Day")
daily_cf_graph

ggsave(path=path2graphics, filename="daily_cf.png")

#looking at monthly renewable generation 
#vs 30% of monthly demand, which should ideally be covered by renewables
#assuming we have naemplate in place to supply 30% of annual demand on avg with renewables
month_s_v_d_graph<-line_figure(list(cf_monthly_melted2),
                               merge_variable = "month",
                               value_unit="GWhrs",
                               title_name = "Monthly Electricty Supply vs Demand",
                               x_label = "Month",
                               return_static = F,
                               subtitle_description="2030 Forecast",
                               modifications = scale_x_discrete(limits=month.abb)) 
month_s_v_d_graph

#comparing renewable generation by week
#vs target of 30% 2030 weekly demand
#ignoring week 53 as it is only 1 day of the year
week_s_v_d_graph<-line_figure(list(week_melted),
                               merge_variable = "week",
                               value_unit="GWhrs",
                               title_name = "Weekly Electricty Supply vs Demand",
                               x_label = "Week",
                               return_static = F,
                               subtitle_description="2030 Forecast") 
week_s_v_d_graph

#note: this code is cut from some code I'm currently working on for a renewables simulator project
#Here I have only included the pieces of that code which were relevant to the figures 
#just to make things less cluttered and more clear



