library(data.table)

setwd("~/Desktop/Coding/IBM/notebooks")
jfk_data <- fread("../samples_with_jfk_landing_flag.csv")
df <- jfk_data[, .("max_altitude" = max(altitude)), by = "id"]
df <- df[df$max_altitude <= 45000, ]
df$max_altitude <- NULL
jfk_data <- merge(jfk_data, df)
setorder(jfk_data, id, ts)
jfk_data <- jfk_data[jfk_data$ts != 1933 & jfk_data$ts != 1560605807807 &
                       jfk_data$ts != 156070924240 & jfk_data$ts != 10609897 &
                       jfk_data$ts != 15550817 & jfk_data$ts != 156050779 &
                       jfk_data$ts != 156061187 & jfk_data$lon != 30]
conversion_x <- 9.26/(cos(pi * jfk_data$lat/180) * 111.699)
jfk_data[, c("x", "y", "z", "id_start_ts", "id_end_ts", "ts_readable") :=
           list(ceiling((lon + 78)/conversion_x),
                ceiling((lat - 35)/(9.26/111.699)),
                ceiling(altitude/500),
                ts[1], ts[.N],
                as.POSIXct.numeric(as.numeric(ts), origin = '1970-01-01')), ]
jfk_data[, "date" := as.Date(ts_readable), ]
jfk_data <- jfk_data[, c("date", setdiff(colnames(jfk_data),
                                         "date")), with = F]
# jfk_data[, "ts_readable" := NULL]
fwrite(jfk_data[jfk_data$jfk_landing_flag, ],
       "../landing_samples.csv", quote = F, row.names = F)
fwrite(jfk_data,
       "../samples_with_jfk_landing_flag.csv", quote = F, row.names = F)
