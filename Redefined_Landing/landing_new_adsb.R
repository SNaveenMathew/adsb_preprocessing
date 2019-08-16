library(data.table)

landed_altitude <- 400
ac_data <- fread("Desktop/Coding/ads-b_with_id.csv")
setorder(ac_data, V3, V13)
ac_data <- ac_data[, .SD[1], by = list(V3, V13)]
ac_data$ts <- as.POSIXct.numeric(as.numeric(ac_data$V13), origin="1970-01-01")
