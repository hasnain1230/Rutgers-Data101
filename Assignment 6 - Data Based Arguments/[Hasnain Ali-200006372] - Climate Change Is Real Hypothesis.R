temperature_df = data.frame(year = historic_temp$Year, temperature = historic_temp$MaxCelsiusTemp)

p_Test_temp <- function() { # This function will determine whether or not global warming is real using the NH Theory. 
  df1 = subset.data.frame(temperature_df, year == 2004 & temperature != -99999.0)
  df2 = subset.data.frame(temperature_df, temperature != -99999.0)
  df1_rows = (nrow(df1))
  df2_rows = (nrow(df2))
  
  sample_mean1 = mean(df1$temperature, na.rm = TRUE)
  population_mean = mean(df2$temperature, na.rm = TRUE)
  pop_sd = sd(df2$temperature, na.rm = TRUE)
  # n = sqrt(df1_rows)
  
  z_score = ((sample_mean1 - population_mean)) / ((pop_sd))
  p = 1 - pnorm(abs(z_score))
  
  
  cat("The sample mean is:", sample_mean1, "degrees celsius\n")
  cat("The population mean is:", population_mean, "degrees celsius\n")
  cat("The standard deviation is:", pop_sd, "\n")
  cat("The Z-score is:", abs(z_score), "\n")
  
  cat("Using this, we can calculate the P value to be", p * 100, "%")
  
}

p_Test_temp()