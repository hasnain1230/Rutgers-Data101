library(plyr)
library(ggplot2)
library(colorRamps)
hate_crimes = read.csv("~/Downloads/hate_crime/[Hasnain Ali - 200006372] hate_crime.csv")

lying_bias <- function() {
  offender_bias = data.frame(x = hate_crimes$DATA_YEAR, y = hate_crimes$BIAS_DESC)
  offender_1996 = subset(offender_bias, offender_bias$x == 1996 & offender_bias$y == "Anti-Black or African American")
  offender_2014 = subset(offender_bias, offender_bias$x == 2014 & offender_bias$y == "Anti-Black or African American")
  total = rbind(offender_1996, offender_2014)
  counts <- ddply(total, .(total$x), nrow)
  
  df = as.data.frame(matrix(unlist(counts),nrow=length(counts),byrow=FALSE))
  
  ggplot(df, aes(x = V1, y = V2)) + geom_bar(stat = "identity", fill = c("dark red", "steelblue"), color = "white") + 
    scale_x_continuous(breaks = c(1996, 2014)) + geom_text(aes(label=V2), vjust = 2, size=10) + 
    xlab("Year") + ylab("Amount of Hate-Crimes Against African Americans") + labs(title = "Hate Crimes Against African Americans") + 
    theme_dark() + theme(plot.title = element_text(hjust = 0.5))
  
}

true_bias <- function() {
  offender_bias = data.frame(x = hate_crimes$DATA_YEAR, y = hate_crimes$BIAS_DESC)
  offender_bias = subset(offender_bias, y == "Anti-Black or African American")
  df = data.frame(ddply(offender_bias, .(offender_bias$x), nrow))
  
  colnames(df) <- c("Year", "Frequency")
  
  colors_func = colorRampPalette(c("#0f0c29", "#302b63", "#24243e"))
  
  
  ggplot(df, aes(x = Year, y = Frequency)) + geom_bar(stat = "identity", fill = colors_func(30), color = "white") + 
    scale_x_continuous(breaks = seq(1991, 2020, by = 1)) + theme_dark() + ylab("Frequency of Crimes") + 
    labs(title = "Amount of Hate-Crimes Against African Americans from 1991 to 2020") + theme(plot.title = element_text(hjust = 0.5))
}

lying_bias()
true_bias()