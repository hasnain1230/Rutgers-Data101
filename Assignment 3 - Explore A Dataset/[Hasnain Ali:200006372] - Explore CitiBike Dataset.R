library(ggplot2)
citi_bike <- read.csv("~/Downloads/citibike.May20.50K.csv")

citi_bike_customer_vs_subscriber <- function() { # Total amount of time subscribers use vs. customers.
  citi_bike_frame = data.frame(trip_time = citi_bike$tripduration, type = citi_bike$usertype) # Convert data to dataframe. 
  acb = data.frame(aggregate(citi_bike_frame$trip_time ~ citi_bike_frame$type, citi_bike_frame,  # Get average for trip time for each customer type.
                                FUN = mean))
  
  acb$citi_bike_frame.trip_time = acb$citi_bike_frame.trip_time / 60.0 # Seconds to minutes. 
  
  l = round(acb$citi_bike_frame.trip_time, 2)

  ggplot(data = acb, aes(x = citi_bike_frame.type, y = citi_bike_frame.trip_time)) + # Make barplot with ggplot. In all honesty, I think this should be the default for this course. Basic R is far too limited without custom writing your own libraries. This was so much easier to work with. 
    geom_col(fill=c("#1a476f", "#690202")) + coord_flip() + geom_text(aes(label=l), hjust=1.6, color="white", size=4.5) + 
    xlab("Citi Bike Customer Type") + ylab("Average Length of Ride (Minutes)") + labs(title = "Citi Bike - Customer Type vs. Average Length of Ride") + 
    theme(plot.title = element_text(hjust = 0.5))
  
}

citi_bike_box_plot <- function() {
  citi_bike_frame = data.frame(citi_bike$birth.year)
  boxplot(citi_bike_frame, main="Average Birth Years of CitiBike Riders", horizontal = TRUE, # Boxplot of the birth years. Basic R here. 
          xlab = "Birth Year", col = "steelblue", notch = TRUE, border = "black")
}

citi_bike_table <- function() {
  cbd = data.frame(Start = citi_bike$start.station.name, End = citi_bike$end.station.name)
  
  cbd = table(cbd) # Simply just making a table of the Citi Bikes start and end station. 
  print(typeof(cbd))
  
  View(cbd)
}


citi_bike_customer_vs_subscriber()
citi_bike_box_plot()
citi_bike_table()