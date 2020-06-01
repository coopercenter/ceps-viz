#for plotly donut figures: 
donut_figure_p <- function(data_value,data_year,data_with_true_units,goal_value,goal_year,goal_with_true_units,description_of_goal,light_color,dark_color,end_goal_value=NULL,end_goal_year=NULL,end_goal_with_true_units=NULL,darkest_color=NULL){
  #data_value = numerical value of historical data (as decimal between 0 & 1)
  #note: if the data is not a percent, such as percent renewable generation in 2019, make it a decimal by dividing the data value by the goal value
  #data_year = year data is from as character
  #data_with_true_units = character of description of data in natural units: %, MW, etc
  #goal_value = numerical value of goal (as decimal between 0 & 1)
  #note: if the goal is not a percent: goal_value should = 1 if it is the end goal, goal_value should equal intermediate goal value divided by end goal value if goal featured is not end goal 
  #goal_year = year goal is to be reached as character
  #goal_with_true_units = character of description of goal in natural units: %, MW, etc
  #description_of_goal = character of description of goal category (ex:Renewable Generation)
  #light_color = choice of lighter color, as a character
  #dark_color = choice of darker color, as a character (note: light_color and dark_color should be light and dark versions of same color) 
  #end_goal variations default as NULL, but can used if there is a need for three rings: data, and intermediate goal, and an end goal
  
  library(plotly)
  
  ring1 = data.frame(category=c("zfiller",paste0(description_of_goal,data_year)),
                     value=c(1-data_value,data_value)) #ring1 data.frame consists of elements that will make up outer ring
  ring2 = data.frame(category=c("zfiller",paste0(description_of_goal,goal_year)),
                     value=c(1-goal_value,goal_value)) #ring2 data.frame consists of elements that will make up inner ring
  
  if (is.null(end_goal_value)){
    figure <- plot_ly(textinfo="none",hoverinfo="name") %>%
      add_pie(data = ring1, values = ~value, labels = ~category, hole = 0.8,
              name = "currently", domain = list(x = c(0, 1), y = c(0, 1)),
              marker=list(colors=c("whitesmoke",dark_color),
                          line=list(color="white",width=1))) %>%
      add_pie(data = ring2, values = ~value, labels = ~category, hole = 0.76,
              name = "goal", domain = list(x = c(0, 1), y = c(.1, .9)),
              marker=list(colors=c("whitesmoke",light_color),
                          line=list(color="white",width=1))) %>%
      layout(title=list(text=paste(data_with_true_units,"in",data_year),font = list(color = dark_color,size = 16),x=0.55),showlegend = F) %>%
      add_annotations(x=0.5,y=0.5,text=description_of_goal,showarrow=F,font = list(color = "lightslategrey",size = 14)) %>%
      add_annotations(x=0.5,y=-0.1,text=paste(goal_with_true_units,"by",goal_year),showarrow=F,font = list(color = light_color,size = 16))
  }
  else{
    ring3 = data.frame(category=c("zfiller",paste0(description_of_goal,end_goal_year)),
                       value=c(1-end_goal_value,end_goal_value)) #ring3 data.frame consists of elements that will make up innermost ring
    
    figure <- plot_ly(textinfo="none",hoverinfo="name") %>%
      add_pie(data = ring1, values = ~value, labels = ~category, hole = 0.8,
              name = "currently", domain = list(x = c(0, 1), y = c(0, 1)),
              marker=list(colors=c("whitesmoke",darkest_color),
                          line=list(color="white",width=1))) %>%
      add_pie(data = ring2, values = ~value, labels = ~category, hole = 0.76,
              name = "goal", domain = list(x = c(0, 1), y = c(.1, .9)),
              marker=list(colors=c("whitesmoke",dark_color),
                          line=list(color="white",width=1))) %>%
      add_pie(data = ring3, values = ~value, labels = ~category, hole = 0.7,
              name = "end goal", domain = list(x = c(0, 1), y = c(.2, .8)),
              marker=list(colors=c("whitesmoke",light_color),
                          line=list(color="white",width=1))) %>%
      layout(title=list(text=paste(data_with_true_units,"in",data_year),font = list(color = darkest_color,size = 15),x=0.55),showlegend = F) %>%
      add_annotations(x=0.5,y=0.5,text=description_of_goal,showarrow=F,font = list(color = "lightslategrey",size = 14)) %>%
      add_annotations(x=0.5,y=-0.05,text=paste(goal_with_true_units,"by",goal_year),showarrow=F,font = list(color = dark_color,size = 15))%>%
      add_annotations(x=0.5,y=-0.1,text=paste(end_goal_with_true_units,"by",end_goal_year),showarrow=F,font = list(color = light_color,size = 15))
  }
  return(figure)
}

#for plotly donut figures with a single ring:
single_ring_donut_figure_p <- function(data_value,data_year,data_with_true_units,goal_value,goal_year,goal_with_true_units,description_of_goal,light_color,dark_color,end_goal_value=NULL,end_goal_year=NULL,end_goal_with_true_units=NULL,darkest_color=NULL){
  #see above donut_figure_p function for input descriptions 
  
  library(plotly)
  
  if (is.null(end_goal_value)){
    ring = data.frame(category=c(" ","currently","goal"),
                      value=c(1-goal_value,data_value,goal_value-data_value))
    
    figure <- plot_ly(textinfo="none",hoverinfo="label") %>%
      add_pie(data = ring, values = ~value, labels = ~category, sort = F, hole = 0.7,
              domain = list(x = c(0, 1), y = c(0, 1)),
              marker=list(colors=c("whitesmoke",dark_color,light_color),
                          line=list(color="white",width=1))) %>%
      layout(title=list(text=paste(data_with_true_units,"in",data_year),font = list(color = dark_color,size = 16),x=0.55),showlegend = F) %>%
      add_annotations(x=0.5,y=0.5,text=description_of_goal,showarrow=F,font = list(color = "black",size = 14)) %>%
      add_annotations(x=0.5,y=-0.1,text=paste(goal_with_true_units,"by",goal_year),showarrow=F,font = list(color = light_color,size = 16))
  }
  else{
    ring = data.frame(category=c(" ","currently","intermediate goal","end goal"),
                      value=c(1-end_goal_value,data_value,goal_value-data_value,end_goal_value-goal_value))
    
    figure <- plot_ly(textinfo="none",hoverinfo="label") %>%
      add_pie(data = ring, values = ~value, labels = ~category, sort = F,hole = 0.7,
              domain = list(x = c(0, 1), y = c(0, 1)),
              marker=list(colors=c("whitesmoke",darkest_color,dark_color,light_color),
                          line=list(color="white",width=1))) %>%
      layout(title=list(text=paste(data_with_true_units,"in",data_year),font = list(color = darkest_color,size = 15),x=0.55),showlegend = F) %>%
      add_annotations(x=0.5,y=0.5,text=description_of_goal,showarrow=F,font = list(color = "black",size = 14)) %>%
      add_annotations(x=0.5,y=-0.05,text=paste(goal_with_true_units,"by",goal_year),showarrow=F,font = list(color = dark_color,size = 15))%>%
      add_annotations(x=0.5,y=-0.1,text=paste(end_goal_with_true_units,"by",end_goal_year),showarrow=F,font = list(color = light_color,size = 15))
  }
  return(figure)
}

#another version of plotly donut figures with a single ring, but the data table must be built outside the function:
single_ring_donut_figure_p2 <- function(data_table,description_of_goal,top_description,bottom_description,hover_info,colors_list){
  #data_table must contain 2 columns: category & value where category is the appropriate label to appear on the figure and value is the appropriate value to fil the donut
  #       *must be listed in a particular order to be displayed correctly (displayed counterclockwise)
  #       *order must be: category you want to be last followed by the order you want the categories to be in (except last category which has already been listed)
  #description_of_goal = character of description of goal category (ex:Renewable Generation)
  #top_description = character of description of text you want displayed at top of donut
  #bottom_description = character of description of text you want displayed at bottom of donut
  #hover_info = character description of what features you want the hover info to display (ex: label+value)
  #colors_list = character vector of colors in order corresponding to assignment to categories
  
  library(plotly)
  
  figure <- plot_ly(textinfo="none",hoverinfo=hover_info) %>%
    add_pie(data = data_table, values = ~value, labels = ~category, sort = F,hole = 0.7,
            domain = list(x = c(0, 1), y = c(0, 1)),
            marker=list(colors=colors_list,
                        line=list(color="white",width=1))) %>%
    layout(title=list(text=top_description,font = list(color="black",size = 15),x=0.55),showlegend = F) %>%
    add_annotations(x=0.5,y=0.5,text=description_of_goal,showarrow=F,font = list(color = "black",size = 14)) %>%
    add_annotations(x=0.5,y=-0.1,text=bottom_description,showarrow=F,font = list(color = "black",size = 15))
  figure
  
  return(figure)
}

#for timeseries stacked area figures by particular category:
stacked_area_figure <- function(data_table,value_unit,title_name,annual=TRUE,x_label="Year",lower_limit=0,upper_limit=NA){
  #data_table must have three columns: year (or date if monthly data is being plotted where date must be of form "1990-01-01" for example), variable, and value
  #value_unit = character description of units of value being plotted
  #title_name = character description of what title of figure should be
  #annual is a logical variable, which defualts to TRUE, if non-annual data is being plotted, annual should be FALSE
  #x_label defaults to "Year" but can be substituted with another character if Year is not appropriate xlabel
  #subtitle_name defaults to NULL, but can be set equal to a character if a subtitle is desired
  #lower_limit defaults to 0, but can be changed to another numeric value appropriate for the data
  #upper_limit defaults to NA, but can be adjusted if needed
  
  library(ggplot2)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  library("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels
  
  working_table <- data_table[,1:3]
  working_table[,variable:=as.character(variable)]
  working_table <- working_table[order(variable)] #alphabetizes variable elements
  working_table[,variable:=gsub("_"," ",variable)] #subtitutes "_" from variable name with a space to create legend labels
  working_table[,variable:=gsub("apco","APCO",variable)] #deals with specific case if "apco" is included in a variable name, APCO will be used in the legend label
  working_table[,variable:=gsub("dom","Dominion",variable)]
  working_table[,variable:=gsub("ros","Rest of State",variable)]
  working_table[,variable:=capitalize(variable)] #capitalizes first word of legend labels
  
  if (annual==TRUE){
    figure <- ggplot(working_table, aes(x=year,y=value,fill=variable)) +
      geom_area() + 
      ylab(value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
      labs(title=title_name) +
      scale_fill_discrete(name=NULL)
    figure
  }
  else{
    figure <- ggplot(working_table, aes(x=date,y=value,fill=variable)) +
      geom_area() + 
      ylab(value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
      labs(title=title_name) +
      scale_fill_discrete(name=NULL)
    figure
  }
  return(figure)
}

#for timeseries line figures by particular category:
line_figure <- function(data_table,value_unit,title_name,annual=TRUE,x_label="Year",lower_limit=0,upper_limit=NA){
  #data_table must have three columns: year (or date if monthly/daily data is being plotted where date must be of form "1990-01-01" for example), variable, and value
  #value_unit = character description of units of value being plotted
  #title_name = character description of what title of figure should be
  #annual is a logical variable, which defualts to TRUE, if monthly or daily data is being plotted, annual should be FALSE
  #x_label defaults to "Year" but can be substituted with another character if Year is not appropriate xlabel
  #subtitle_name defaults to NULL, but can be set equal to a character if a subtitle is desired
  #lower_limit defaults to 0, but can be changed to another numeric value appropriate for the data
  #upper_limit defaults to NA, but can be adjusted if needed
  
  library(ggplot2)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  library("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels
  
  working_table <- data_table[,1:3]
  working_table[,variable:=as.character(variable)]
  working_table <- working_table[order(variable)] #alphabetizes variable elements
  working_table[,variable:=gsub("_"," ",variable)] #subtitutes "_" from variable name with a space to create legend labels
  working_table[,variable:=gsub("apco","APCO",variable)] #deals with specific case if "apco" is included in a variable name, APCO will be used in the legend label
  working_table[,variable:=gsub("dom","Dominion",variable)]
  working_table[,variable:=gsub("ros","Rest of State",variable)]
  working_table[,variable:=capitalize(variable)] #capitalizes first word of legend labels
  
  if (annual==TRUE){
    figure <- ggplot(working_table, aes(x=year,y=value,color=variable,shape=variable)) +
      geom_line() + 
      geom_point() +
      ylab(value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
      labs(title=title_name) +
      scale_color_discrete(name=NULL)+
      scale_shape_discrete(name=NULL)
    figure
  }
  else{
    figure <- ggplot(working_table, aes(x=date,y=value,color=variable,shape=variable)) +
      geom_line() + 
      geom_point() +
      ylab(value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
      labs(title=title_name) +
      scale_color_discrete(name=NULL)+
      scale_shape_discrete(name=NULL)
    figure
  }
  return(figure)
}

#for pie charts:
pie_chart_figure <- function(data_table,title_name=NULL,percent_label_size=4){
  #data_table must have a "variable" column containing variable names of different categories and a "value" column containing the associated value of each variable that is to be plotted
  #value may be in GWh or whatever is the unit of what is being plotted, the values need not add to 100% or 1 they can be actual values
  #title_name defaults to NULL but can be set as a character if a title is desired
  #percent_label_size defaults to 4, it can be changed to a smaller size if the pie chart has small slivers for example or set equal to 0 if the percent label is not desired
  
  library(ggplot2)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  library("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels
  
  #to compute percentages of each category(prop) and the position of labels(ypos):
  data_table <- data_table %>% 
    arrange(desc(variable)) %>%
    mutate(prop = value / sum(data_table$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  variable_elements <- as.vector(data_table$variable) #selects the elements of the variable column as a vector
  
  good_names = gsub("_"," ",variable_elements) #subtitutes "_" from variable name with a space to create legend labels
  good_names = gsub("apco","APCO",good_names) #deals with specific case if "apco" is included in a variable name, APCO will be used in the legend label
  good_names = gsub("dom", "Dominion", good_names)
  good_names = gsub("ros", "Rest of state", good_names)
  good_names = capitalize(good_names) #capitalizes first word of legend labels
  
  figure <- ggplot(data_table,aes(x="",y=prop,fill=variable))+
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    geom_text(aes(y=ypos,label=paste0(as.character(round(prop,1)),"%")),color="white",size=percent_label_size) +
    scale_fill_discrete(name=NULL,breaks=variable_elements,labels=good_names)+
    labs(title=title_name)+
    theme(plot.title=element_text(hjust=0.5))
  
  return(figure)
}

#for plotly piecharts with or without legend: 
pie_chart_figure_p <- function(data_table,title_name=NULL,legend_shown=FALSE){
  #data_table must have a "variable" column containing variable names of different categories and a "value" column containing the associated value of each variable that is to be plotted
  #value may be in GWh or whatever is the unit of what is being plotted, the values need not add to 100% or 1 they can be actual values
  #title_name defaults to NULL but can be set as a character if a title is desired
  #legend_shown defaults to FALSE
  #       *if FALSE, no legend is shown and the name of each category and associated percent is displayed on the pie slice
  #       *if TRUE, legend is shown and only the percent is displaye on the pie slice, this may be a better optio if some slices are very small
  #eventually when a custom theme is set, we can store the colors from that theme in a character vector called "theme_colors" then include "marker=list(colors=theme_colors)" as argument in plotly function
  
  library(plotly)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  library("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels
  
  category_count <- length(data_table$variable)
  theme_colors <- hue_pal()(category_count)
  
  working_table <- data_table[,1:3]
  working_table[,variable:=as.character(variable)]
  working_table <- working_table[order(variable)] #alphabetizes variable elements
  working_table[,variable:=gsub("_"," ",variable)] #subtitutes "_" from variable name with a space to create legend labels
  working_table[,variable:=gsub("apco","APCO",variable)] #deals with specific case if "apco" is included in a variable name, APCO will be used in the legend label
  working_table[,variable:=gsub("dom","Dominion",variable)]
  working_table[,variable:=gsub("ros","Rest of State",variable)]
  working_table[,variable:=capitalize(variable)] #capitalizes first word of legend labels
  
  if (legend_shown==FALSE){
    figure <- plot_ly(working_table,labels=~variable,values=~value,type='pie',textinfo="percent+label",hoverinfo="percent+label",marker=list(colors=theme_colors),sort=F) %>%
      layout(title=list(text=title_name,x=0.55),showlegend=F) 
  }
  else{
    figure <- plot_ly(working_table,labels=~variable,values=~value,type='pie',textinfo="percent",hoverinfo="percent+label",marker=list(colors=theme_colors),sort=F) %>%
      layout(title=list(text=title_name,x=0.55)) 
  }
  return(figure)
}

#note: for the functions which return ggplot2 objects, additional different desired plot elements can be added in conjunction with the use of the functions

#function to wrap ggplot objects produced from functions with ggplotly & add data citations:
ggplotly_wrapper <- function(list){
  #list shoule be list output from ggplot functions, which includes ggplot figure (figure), x label name (x_label), and data citation (source_description)
  
  library(plotly)
  
  figure_p <- ggplotly(list$figure) %>%
    layout(xaxis=list(
      title = paste0(list$x_label,"<br>","<i>","<sub>",list$source_description,"<sub>","<i>")))
  #citation is built into x-axis label rather than as an annotation so that it does not move as plot margins change, which happens with plotly annotations
  
  return(figure_p)
}

#-----------------------------------------------------------------------------------------------
#trial stacked area function which takes in a list of pre-loaded datasets and creates a ggplot output which can be used as an input in above ggplotly_wrapper function:
#note: metadata table must also be loaded globally in code before use of this function
stacked_area_trial_figure <- function(data_table_list,merge_variable,value_unit,title_name,character_list=NULL,x_label="Year",lower_limit=0,upper_limit=NA,return_static=TRUE,source_citation=NULL,modifications=NULL){
  #data_table_list is a list of data tables which should be ready to be merged into one table
  #       *if only one table is included in input list (note that it still must be in list form), this table should be ready to be plotted i.e it should include a variable and value column and an x-value (usually date or year) column
  #merge_variable is a character description of which variable the merge should be performed on (ex:"date","year)
  #value_unit = character description of units of value being plotted
  #title_name = character description of what title of figure should be
  #character_list defaults to NULL, but if applicable should be a list of the relevant database data table names as strings, so that their source info can be extracted from metadata table
  #x_label defaults to "Year" but can be substituted with another character if Year is not appropriate xlabel
  #lower_limit defaults to 0, but can be changed to another numeric value appropriate for the data
  #upper_limit defaults to NA, but can be adjusted if needed
  #return_static defaults to TRUE, in which case a ggplot opbject will be returned
  #       *if FALSE, a list containing the ggplot figure, the x axis label name, and the citation will be returned
  #source_citation defaults to NULL, however if a source can't be pulled from the metadata table, the source should be set to be the citation description
  #       *if needed to be set, should be of form: "Source: U.S. Energy Information Administration" for example
  #modifications defaults to NULL, in which case nothing would be added to the figure, but can be set if additional modifications are needed
  #       *examples of different modifications which may be necessary are scaling the y-axis or removing the legend
  
  library(ggplot2)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  library("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels
  
  working_table <- NULL
  
  for(table in data_table_list){
    if (is.null(working_table))
    {working_table <- table}
    else
    {working_table <- merge(working_table, table[], by = merge_variable, all=TRUE)}
  }
  
  if(length(data_table_list)==1) #accounts for possibility that it is necessary for data table to be constructed outside function, in which case only one data table will be listed as input
  {lf_working_table <- working_table}
  else #if multiple tables are listed as input, will melt the merged tables into their longform by merge variable
  {lf_working_table <- melt(working_table,id=merge_variable)}
  
  lf_working_table[,variable:=as.character(variable)]
  lf_working_table <- lf_working_table[order(variable)] #alphabetizes variable elements
  lf_working_table[,variable:=gsub("_"," ",variable)] #subtitutes "_" from variable name with a space to create legend labels
  lf_working_table[,variable:=gsub("apco","APCO",variable)] #deals with specific case if "apco" is included in a variable name, APCO will be used in the legend label
  lf_working_table[,variable:=gsub("dom","Dominion",variable)]
  lf_working_table[,variable:=gsub("ros","Rest of State",variable)]
  lf_working_table[,variable:=capitalize(variable)] #capitalizes first word of legend labels
  
  setnames(lf_working_table,merge_variable,"x_unit")
  
  figure <- ggplot(lf_working_table, aes(x=x_unit,y=value,fill=variable)) +
    geom_area() + 
    ylab(value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
    labs(title=title_name) +
    scale_fill_discrete(name=NULL) +
    modifications
  figure
  
  if(is.null(source_citation)){
    source_list <- NULL
    
    for(table in character_list){
      source <- metadata[db_table_name==table,data_source_full_name]
      
      if(is.null(source_list))
      {source_list <- source}
      else
      {source_list <- c(source_list,source)}
    }
    source_list <-as.vector(unique(source_list))
    
    source_description <- NULL
    
    for (source in source_list){
      if(is.null(source_description))
      {source_description <- paste("Source:",source)}
      else
      {source_description <- paste0(source_description,", ",source)}
    }
  }
  else
  {source_description <- source_citation}
  
  return_list <- list(figure=figure,x_label=x_label,source_description=source_description)
  
  if(return_static==TRUE)
  {return(figure)}
  else
  {return(return_list)}
}

#trial line plot function which takes in a list of pre-loaded datasets and creates a ggplot output which can be used as an input in above ggplotly_wrapper function:
#note: metadata table must also be loaded globally in code before use of this function
line_trial_figure <- function(data_table_list,merge_variable,value_unit,title_name,character_list=NULL,x_label="Year",lower_limit=0,upper_limit=NA,return_static=TRUE,source_citation=NULL,modifications=NULL){
  #data_table_list is a list of data tables which should be ready to be merged into one table
  #       *if only one table is included in input list (note that it still must be in list form), this table should be ready to be plotted i.e it should include a variable and value column and an x-value (usually date or year) column
  #merge_variable is a character description of which variable the merge should be performed on (ex:"date","year)
  #value_unit = character description of units of value being plotted
  #title_name = character description of what title of figure should be
  #character_list defaults to NULL, but if applicable should be a list of the relevant database data table names as strings, so that their source info can be extracted from metadata table
  #x_label defaults to "Year" but can be substituted with another character if Year is not appropriate xlabel
  #lower_limit defaults to 0, but can be changed to another numeric value appropriate for the data
  #upper_limit defaults to NA, but can be adjusted if needed
  #return_static defaults to TRUE, in which case a ggplot opbject will be returned
  #       *if FALSE, a list containing the ggplot figure, the x axis label name, and the citation will be returned
  #source_citation defaults to NULL, however if a source can't be pulled from the metadata table, the source should be set to be the citation description
  #       *if needed to be set, should be of form: "Source: U.S. Energy Information Administration" for example
  #modifications defaults to NULL, in which case nothing would be added to the figure, but can be set if additional modifications are needed
  #       *examples of different modifications which may be necessary are scaling the y-axis or removing the legend
  
  library(ggplot2)
  if(!("Hmisc" %in% installed.packages())) install.packages("Hmisc")
  library("Hmisc") #Hmisc package includes a capitilization function which is utilized to get legend labels
  
  working_table <- NULL
  
  for(table in data_table_list){
    if (is.null(working_table))
    {working_table <- table}
    else
    {working_table <- merge(working_table, table[], by = merge_variable, all=TRUE)}
  }
  
  if(length(data_table_list)==1) #accounts for possibility that it is necessary for data table to be constructed outside function, in which case only one data table will be listed as input
  {lf_working_table <- working_table}
  else #if multiple tables are listed as input, will melt the merged tables into their longform by merge variable
  {lf_working_table <- melt(working_table,id=merge_variable)}
  
  lf_working_table[,variable:=as.character(variable)]
  lf_working_table <- lf_working_table[order(variable)] #alphabetizes variable elements
  lf_working_table[,variable:=gsub("_"," ",variable)] #subtitutes "_" from variable name with a space to create legend labels
  lf_working_table[,variable:=gsub("apco","APCO",variable)] #deals with specific case if "apco" is included in a variable name, APCO will be used in the legend label
  lf_working_table[,variable:=gsub("dom","Dominion",variable)]
  lf_working_table[,variable:=gsub("ros","Rest of State",variable)]
  lf_working_table[,variable:=capitalize(variable)] #capitalizes first word of legend labels
  
  setnames(lf_working_table,merge_variable,"x_unit")
  
  figure <- ggplot(lf_working_table, aes(x=x_unit,y=value,color=variable,shape=variable)) +
    geom_line() + 
    geom_point() +
    ylab(value_unit) + xlab(x_label) + ylim(lower_limit,upper_limit) +
    labs(title=title_name) +
    scale_color_discrete(name=NULL)+
    scale_shape_discrete(name=NULL)+
    modifications
  figure
  
  if(is.null(source_citation)){
    source_list <- NULL
    
    for(table in character_list){
      source <- metadata[db_table_name==table,data_source_full_name]
      
      if(is.null(source_list))
      {source_list <- source}
      else
      {source_list <- c(source_list,source)}
    }
    source_list <-as.vector(unique(source_list))
    
    source_description <- NULL
    
    for (source in source_list){
      if(is.null(source_description))
      {source_description <- paste("Source:",source)}
      else
      {source_description <- paste0(source_description,", ",source)}
    }
  }
  else
  {source_description <- source_citation}
  
  return_list <- list(figure=figure,x_label=x_label,source_description=source_description)
  
  if(return_static==TRUE)
  {return(figure)}
  else
  {return(return_list)}
}

