library(RColorBrewer)
library(colorRamps)
library(plotrix)

# I did not know if we could use ggplot2, so I decided to write everything natively in R
# Which does limit some things...

# Yeah, this was an absolute pain to do. For that, no comments for you. 

hate_crime = read.csv("/Users/hasnain1230/Downloads/hate_crime/hate_crime.csv") # Insert file path here.

hc_create_barplot <- function() {
  hc_year_table = table(hate_crime$DATA_YEAR)
  colfunc = colorRampPalette(c("#004e92", "#000428"))
  
  hc_year_plot = barplot(hc_year_table, col = colfunc(30), xlab = "Year of Hate Crimes", 
                      ylab = "Frequency of Hate Crimes", 
                      main = "Year of Hate Crimes vs. Frequency of Hate Crimes", 
                      ylim = range(pretty(c(0, hc_year_table))), border = "white", 
                      col.axis = "dark blue", col.lab = "blue")
  
  return(hc_year_plot)
}


hc_create_pie_chart <- function() {
  hc_state_table = head(sort(table(hate_crime$STATE_NAME), decreasing = TRUE), 10)
  hc_frame = data.frame(rbind(hc_state_table))
  percentages = round((hc_state_table / sum(hc_state_table)) * 100, 2)
  
  percentages = paste("- ", percentages, sep="")
  
  label_data = paste(colnames(hc_frame), percentages)
  label_data = paste(label_data, "%", sep = "")
  
  pie3D(hc_state_table, labels = label_data, explode = 0, radius = 1, border = "white", 
        labelcol = "black", shade = 0.3, 
        main = "Top 10 States With The Most Hate Crimes from 1991 to 2020")
}

hc_create_box_plot <- function() {
  hc_victim_data = data.frame(x = hate_crime$DATA_YEAR, y=hate_crime$VICTIM_COUNT)
  years = unique(hc_victim_data$x)
  colfunc = colorRampPalette(c("#004e92", "#000428"))
  test = plot(hc_victim_data$x, hc_victim_data$y, xlab = "Year", ylab = 
                "Number of Victims for Each Crime That Year", pch = 7, col = "red", 
              xaxp = c(1991, 2020, 29), 
              main = "Scatterplot of Victim Count For Each Crime, Each Year")
}


hc_create_barplot()
hc_create_pie_chart()
hc_create_box_plot()
