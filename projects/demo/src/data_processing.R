### Hao Ye
### file started: 2017-04-12

# process co2 and temperature data

process_co2_temperature_data <- function(co2_data_file = file.path("data", "co2_mm_mlo.txt"), 
                                         temperature_data_file = file.path("data", "GLB.Ts+dSST.csv"), 
                                         out_file = file.path("data", "co2_temperature.csv"))
{
    co2_data <- read.table(co2_data_file, skip = 72)
    co2_data <- co2_data[, -NCOL(co2_data)]
    names(co2_data) <- c("year", "month", "decimal_date", "co2_average", "co2_interpolated", "co2_trend")
    
    temp_data <- read.csv(temperature_data_file, skip = 1)[, 1:13]
    temp_data <- temp_data[temp_data$Year != 2017,]
    temp_data <- tidyr::gather(temp_data, month, temperature, Jan:Dec)
    temp_data$month <- as.numeric(factor(temp_data$month, 
                                         levels = c("Jan", "Feb", "Mar", "Apr", 
                                                    "May", "Jun", "Jul", "Aug", 
                                                    "Sep", "Oct", "Nov", "Dec")))
    temp_data$temperature <- as.numeric(temp_data$temperature)
    
    co2_data$temp_anomaly <- temp_data$temperature[match(paste(co2_data$year, co2_data$month), paste(temp_data$Year, temp_data$month))]
    
    write.csv(co2_data, out_file, row.names = FALSE)
}