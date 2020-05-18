library(data.table)
library(lubridate)
library(here)
library(ggplot2)
library(scales)
library(dbConnect)
library("RPostgreSQL")

db_driver = dbDriver("PostgreSQL")
#insert your filepath here
source(here::here("my_postgres_credentials.R"))
db <- dbConnect(db_driver,user=db_user, password=ra_pwd,dbname="postgres", host=db_host)

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
#      *  `fuel_code` : associated fuel type codes, e.g., "coal", "nuc"
series_list = data.frame(
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
  fuel_code=c("bio","coal","hps","ng","nuc","or","other","pel","solar_d","solar_u","wat","wood")
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

#to plot generation by fuel type over time
lf_all_generation <- melt(all_generation, id="date")
setnames(lf_all_generation, old=c("variable", "value"), new=c("fuel_type","generation"))

generation_by_type <- ggplot(lf_all_generation, aes(fill=fuel_type, x=date, y=generation)) +
  geom_line(aes(color=fuel_type)) + ylab("Generation (thousand MWhrs)") + 
  xlab(NULL) +
  labs(title ="VA Monthly Electricity Generation by Fuel Type") 
generation_by_type

path2graphics <- here::here("graphics")
ggsave(path=path2graphics, filename="generation_by_type.png")


#bar chart breaking up generation by year
all_generation[is.na(solar_d),solar_d:=0]
all_generation[is.na(solar_u),solar_u:=0]

all_generation[,`:=`(cum_coal=cumsum(coal),
                     cum_ng=cumsum(ng),
                     cum_nuc=cumsum(nuc),
                     cum_pel=cumsum(pel),
                     cum_solar_d=cumsum(solar_d),
                     cum_solar_u=cumsum(solar_u),
                     cum_wat=cumsum(wat),
                     cum_wood=cumsum(wood),
                     cum_or=cumsum(or),
                     cum_bio=cumsum(bio),
                     cum_other=cumsum(other)
),year(date)]


lf_all_generation[, cum_generation:=cumsum(generation), by = .(year(date), fuel_type)]

generation_bar <- ggplot(lf_all_generation[month(date)==12], aes(fill=fuel_type,x=date, y=cum_generation)) +
  geom_bar(position = "dodge", stat="identity") + 
  ylab("Generation (thousand MWhrs)") + xlab(NULL) +
  labs(title ="VA Annual Electricity Generation by Fuel Type") +
  scale_x_date(labels = date_format("%Y"), breaks='1 year')
generation_bar

ggsave(path=path2graphics, filename="generation_bar.png")

generation_bar_stacked <- ggplot(lf_all_generation[month(date)==12], aes(fill=fuel_type,x=date, y=cum_generation)) +
  geom_bar(position = "stack", stat="identity") + 
  ylab("Generation (thousand MWhrs)") + xlab(NULL) +
  labs(title ="VA Annual Electricity Generation by Fuel Type") +
  scale_x_date(labels = date_format("%Y"), breaks='1 year')
generation_bar_stacked

ggsave(path=path2graphics, filename="generation_bar_stacked.png")

generation_area_stacked <- ggplot(lf_all_generation[month(date)==12], aes(x=date, y=cum_generation)) +
  geom_area(aes(fill=fuel_type)) + 
  ylab("Generation (thousand MWhrs)") + xlab(NULL) +
  labs(title ="VA Annual Electricity Generation by Fuel Type") +
  scale_x_date(labels = date_format("%Y"), breaks='1 year')
generation_area_stacked

ggsave(path=path2graphics, filename="generation_area_stacked.png")



