library(plyr)
library(dplyr)
library(data.table)
library(splines)
library(parallel)

fillna <- function(na_series) {
  na_idx <- which(is.na(na_series))
  while(length(na_idx) > 0) {
    na_series[na_idx] <- na_series[na_idx - 1]
    na_idx <- which(is.na(na_series))
  }
  return(na_series)
}

expand_summary_df <- function(summary_df) {
  df <- data.frame()
  for(i in 1:nrow(summary_df)) {
    df <- rbind(df, data.frame(ts_readable = summary_df$start_ts[i],
                               altitude = summary_df$start_altitude[i],
                               climb_rate_sign = summary_df$climb_rate_sign[i]))
    df <- rbind(df, data.frame(ts_readable = summary_df$start_ts[i] + summary_df$duration[i],
                               altitude = summary_df$end_altitude[i],
                               climb_rate_sign = summary_df$climb_rate_sign[i]))
  }
  return(df)
}

get_alt_change_df <- function(ac_data) {
  ac_data[, "last_altitude" := c(NA, altitude[-.N]), by = "id"]
  # ac_data$est_climb_rate <- NULL
  alt_change_idx <- which(ac_data$altitude != ac_data$last_altitude | is.na(ac_data$last_altitude))
  alt_change_idx <- unique(unlist(lapply(alt_change_idx, function(idx) c(idx - 1, idx, idx + 1))))
  alt_change_idx <- alt_change_idx[alt_change_idx != 0]
  alt_change_df <- ac_data[alt_change_idx, ]
  # alt_change_df[, "last_ts_alt_change" := c(NA, ts[-.N]), by = "id"]
  # alt_change_df$last_ts_diff_alt_change <- alt_change_df$ts - alt_change_df$last_ts_alt_change
  # alt_change_df$est_climb_rate <- 60 * (alt_change_df$altitude - alt_change_df$last_altitude)/alt_change_df$last_ts_diff_alt_change
  alt_change_df <- alt_change_df[, c("id", "ts", "altitude", "ts_readable"#, "climb_rate"#,
                                     #"est_climb_rate"
                                     )]
  # alt_change_df$est_climb_rate[is.na(alt_change_df$est_climb_rate)] <- 0
  return(alt_change_df)
}

identify_flight_phase <- function(uniq_id, plot_phases = T, return_dataframe = T) {
  return_df <- data.frame()
  for(id1 in uniq_id) {
    tryCatch({
      mdl <- smooth.spline(x = alt_change_df$ts[alt_change_df$id == id1],
                           y = alt_change_df$altitude[alt_change_df$id == id1])
      est_climb_rate <- diff(predict(mdl, alt_change_df$ts[alt_change_df$id == id1])$y)/
        diff(alt_change_df$ts[alt_change_df$id == id1])
      est_climb_rate[abs(est_climb_rate) < 3] <- 0
      climb_rate_sign <- sign(est_climb_rate)
      climb_rate_sign_change <- c(NA, climb_rate_sign[-1] != (climb_rate_sign[-length(climb_rate_sign)]))
      climb_rate_sign_change[is.na(climb_rate_sign_change)] <- T
      df1 <- data.frame(ts_readable = alt_change_df$ts_readable[alt_change_df$id == id1],
                        altitude = alt_change_df$altitude[alt_change_df$id == id1],
                        climb_rate_sign = c(0, climb_rate_sign),
                        climb_rate_sign_change = c(climb_rate_sign_change, F),
                        duration = alt_change_df$alt_duration[alt_change_df$id == id1])
      df1$alt_diff_sign_change <- cumsum(df1$climb_rate_sign_change)
      setDT(df1)
      summary_df <- df1[, .(start_ts = ts_readable[1], start_altitude = altitude[1], end_altitude = altitude[.N],
                            duration = sum(duration, na.rm = T), alt_diff = altitude[.N] - altitude[1]),
                        by = alt_diff_sign_change]
      
      summary_df$climb_rate_sign_change <- T
      df2 <- merge(alt_change_df[alt_change_df$id == id1, c("ts_readable", "id", "ts", "altitude")],
                   summary_df[, c("start_ts", "start_altitude", "end_altitude", "duration", "alt_diff",
                                  "climb_rate_sign_change")],
                   by.x = c("ts_readable"), by.y = "start_ts", all.x = T)
      df2$climb_rate_sign <- sign(df2$alt_diff)
      df2$climb_rate_sign[abs(df2$alt_diff) < 500] <- 0
      df2$climb_rate_sign <- fillna(df2$climb_rate_sign)
      df2$climb_rate_sign_change <- c(T, df2$climb_rate_sign[-1] != df2$climb_rate_sign[-nrow(df2)])
      df2$total_climb_rate_sign_change <- cumsum(df2$climb_rate_sign_change)
      summary_df <- df2[, .(start_ts = ts_readable[1], start_altitude = altitude[1],
                            end_altitude = altitude[.N], duration = sum(duration, na.rm = T),
                            alt_diff = altitude[.N] - altitude[1]),
                        by = "total_climb_rate_sign_change"]
      
      summary_df$climb_rate_sign_change <- T
      summary_df$climb_rate_sign <- sign(summary_df$alt_diff)
      summary_df$climb_rate_sign[abs(summary_df$alt_diff) <= 500] <- 0
      summary_df1 <- expand_summary_df(summary_df)
      summary_df2 <- summary_df1[c(seq(1, nrow(summary_df1), by = 2), nrow(summary_df1)), ]
      summary_df2$decision_point <- TRUE
      summary_df2$decision_point[nrow(summary_df2)] <- FALSE
      climb_height <- (summary_df2$altitude[-1] - summary_df2$altitude[-nrow(summary_df2)])
      delta_time <- (as.numeric(summary_df2$ts_readable[-1]) -
                       as.numeric(summary_df2$ts_readable[-nrow(summary_df2)]))
      climb_rate <- climb_height/delta_time
      summary_df2$climb_rate <- c(climb_rate, climb_rate[length(climb_rate)])
      final_df <- merge(ac_data[ac_data$id == id1, ], summary_df2, all.x = T, by = c("ts_readable", "altitude"))
      final_df$climb_rate_sign <- fillna(final_df$climb_rate_sign)
      final_df$climb_rate <- fillna(final_df$climb_rate)
      if(plot_phases) {
        png(paste0("flight_phase/", id1, ".png"), width = 800, height = 600)
        plot(final_df$ts_readable, final_df$altitude, col = final_df$climb_rate_sign + 4, cex = 0.5, xlab = "Time", ylab = "Altitude")
        points(final_df$ts_readable[final_df$decision_point], final_df$altitude[final_df$decision_point],
               col = "red", cex = 1, pch = 20)
        legend("topright", legend = unique(final_df$climb_rate_sign), fill = unique(final_df$climb_rate_sign) + 4)
        dev.off()
      }
      if(return_dataframe) {
        return_df <- rbind(return_df, final_df)
      }
    }, error = function(e) print(id1))
  }
  return(return_df)
}

identify_flight_phase_plot <- function(uniq_id, return_dataframe = T) {
  return(identify_flight_phase(uniq_id = uniq_id, plot_phases = T, return_dataframe = return_dataframe))
}




# setwd("/Users/naveensathiyanathan/Desktop/Coding/Old/ATC_Analysis_2019_04_01/Flight_Phase_Identification/")
# ac_data <- readRDS("high_ac_data.Rds")
ac_data <- fread("../../../2019-05-24.csv", header = F)
temp_dat <- fread("../../../samples_with_jfk_landing_flag.csv", nrows = 2)
colnames(ac_data) <- colnames(temp_dat)
setDT(ac_data)
setorder(ac_data, id, ts)
ac_data[, "ts_readable" := as.POSIXct.numeric(as.numeric(ts), origin="1970-01-01"), ]
# ac_data$ts_readable <- as.POSIXct.numeric(as.numeric(ac_data$ts), origin="1970-01-01")

alt_change_df <- get_alt_change_df(ac_data)
alt_change_df$alt_change <- TRUE
ac_data <- merge(ac_data, alt_change_df, all = T)
ac_data <- ac_data[!is.na(ac_data$id), ]
ac_data$alt_change[is.na(ac_data$alt_change)] <- F
ac_data[, "total_alt_change" := cumsum(alt_change), by = "id"]
ac_data[, "next_ts" := c(ts[-1], NA), by = "id"]
ac_data$next_ts_diff <- ac_data$next_ts - ac_data$ts
ac_data[, "alt_duration" := sum(next_ts_diff, na.rm = T), by = list(id, total_alt_change)]

alt_change_df <- ac_data[ac_data$alt_change,
                         c("id", "ts", "ts_readable",
                           "altitude", "total_alt_change", "alt_duration"#,
                           #"climb_rate", "est_climb_rate"
                         )]
alt_change_df <- alt_change_df[!is.na(alt_change_df$id), ]

alt_change_df1 <- merge(alt_change_df, ac_data[, c("id", "ts", "lon", "lat", "altitude", "jfk_landing_flag")], by = c("id", "ts"))
alt_change_df1 <- alt_change_df1[alt_change_df1$jfk_landing_flag, ]
jfk_pos <- c(40.6413, -73.7781)
alt_change_df1$distance_from_airport <- apply(alt_change_df1[, c("lon", "lat")], 1,
                                              function(row)
                                                geosphere::distm(row, rev(jfk_pos)))
plot(alt_change_df1$distance_from_airport, alt_change_df1$altitude)

# id1 <- "0C6015_0"
cores <- detectCores() - 1
cl <- makeCluster(cores)
ac_data <- ac_data[, c("id", "ts_readable", "altitude")]
clusterExport(cl, list("smooth.spline", "alt_change_df", "diff", "c", "sign", "merge", "abs", "fillna",
                       "data.frame", "cumsum", "%>%", "group_by", "summarize", "first", "last", "sum",
                       "expand_summary_df", "seq", "plot", "points", "legend", "dev.off", "tryCatch",
                       "setDT", "which", "is.na", "ac_data", "identify_flight_phase"))
uniq_id <- unique(alt_change_df$id)
num <- ceiling(length(uniq_id)/cores)
idx <- ceiling(1:length(uniq_id)/num)
uniq_id_splits <- split(uniq_id, idx)
out_df <- parLapply(cl = cl, X = uniq_id_splits, fun = identify_flight_phase_plot)
stopCluster(cl)
out_df <- do.call(rbind, out_df)
# print(nrow(out_df))
# print(sum(sapply(ac_data$id, function(id1) id1 %in% uniq_id)))
out_df$decision_point[is.na(out_df$decision_point)] <- F
# alt_change_df <- out_df[out_df$decision_point, ]
# alt_change_df[, c("next_ts", "next_altitude") :=
#                 list(c(ts_readable[-1], NA), c(altitude[-1], NA)),
#               by = "id"]
# alt_change_df$next_altitude_diff <- (alt_change_df$next_altitude - alt_change_df$altitude)
# alt_change_df$next_ts_diff <- (as.numeric(alt_change_df$next_ts) - as.numeric(alt_change_df$ts_readable))
# alt_change_df$est_climb_rate <- alt_change_df$next_altitude_diff/alt_change_df$next_ts_diff
# alt_change_df$est_climb_rate[alt_change_df$climb_rate_sign == 0] <- 0
saveRDS(out_df[, c("id", "ts_readable", "altitude", "climb_rate_sign", "decision_point", "climb_rate")],
        "estimated_flight_phase.Rds")


id1 <- "0C6015_0"
plot(out_df$ts_readable[out_df$id == id1], out_df$altitude[out_df$id == id1], col = out_df$climb_rate_sign[out_df$id == id1] + 4, cex = 0.5)
points(out_df$ts_readable[out_df$id == id1 & out_df$decision_point], out_df$altitude[out_df$id == id1 & out_df$decision_point],
       col = "red", cex = 1, pch = 20)
legend("topright", legend = unique(out_df$climb_rate_sign[out_df$id == id1]), fill = unique(out_df$climb_rate_sign[out_df$id == id1]) + 4)


# data.frame(df1 %>% group_by(alt_diff_sign_change) %>%
#                          summarize(start_ts = first(ts_readable),
#                                    start_altitude = first(altitude), end_altitude = last(altitude),
#                                    duration = sum(duration, na.rm = T),
#                                    alt_diff = last(altitude) - first(altitude)))


# summary_df <- data.frame(df2 %>% group_by(total_climb_rate_sign_change) %>%
#                            summarize(start_ts = first(ts_readable),
#                                      start_altitude = first(altitude), end_altitude = last(altitude),
#                                      duration = sum(duration, na.rm = T),
#                                      alt_diff = last(altitude) - first(altitude)))



# plot(ac_data$ts_readable[ac_data$id == id1], ac_data$altitude[ac_data$id == id1], cex = 0.5)
# 
# for(i in seq(from = 1, to = nrow(summary_df1), by = 2)) {
#   lines(summary_df1$ts_readable[i:(i+1)],
#         summary_df1$altitude[i:(i+1)],
#         col = summary_df1$climb_rate_sign[i] + 4, lwd = 3)
# }
# legend("topright", legend = unique(summary_df1$climb_rate_sign),
#        fill = unique(summary_df1$climb_rate_sign) + 4)



# alt_change_df[, "next_altitude" := c(altitude[-1], NA), by = "id"]
# alt_change_df$alt_diff <- (alt_change_df$next_altitude - alt_change_df$altitude)
# alt_change_df$alt_diff_sign <- sign(alt_change_df$alt_diff)
# idx <- which(is.na(alt_change_df$alt_diff_sign))
# # Assume aircraft continues to do what it was doing
# alt_change_df$alt_diff_sign[idx] <- alt_change_df$alt_diff_sign[idx - 1]
# alt_change_df[, "last_alt_diff_sign" := c(1, alt_diff_sign[-.N]), by = 'id']
# alt_change_df$alt_diff_sign_change <- (alt_change_df$alt_diff_sign != alt_change_df$last_alt_diff_sign)
# alt_change_df[, "total_alt_diff_sign_change" := cumsum(alt_diff_sign_change), by = "id"]
# alt_change_df[alt_change_df$id == "0C6015_0"]
# 
# df <- alt_change_df[, .(start_altitude = altitude[1], end_altitude = next_altitude[.N],
#                         duration = sum(alt_duration, na.rm = T)),
#                     by = list(id, total_alt_diff_sign_change)]
# df[df$id == "0C6015_0"]


# get_climb_rate_duration_df <- function(ac_data, alt_change_df) {
#   climb_rate_duration <- ac_data[, c("id", "climb_rate", "next_ts_diff",# "est_climb_rate",
#                                      "altitude", "ts_readable", "ts"#, "nearest_500ft_mark"
#   )]
#   climb_rate_duration <- merge(climb_rate_duration, alt_change_df, all = T)
#   climb_rate_duration <- climb_rate_duration[order(climb_rate_duration$id, climb_rate_duration$ts), ]
#   climb_rate_duration$est_climb_rate <- fillna(climb_rate_duration$est_climb_rate)
#   climb_rate_duration$climb_rate_sign <- sign(climb_rate_duration$est_climb_rate)
#   climb_rate_duration[, "last_climb_rate_sign" := c(NA, climb_rate_sign[-.N]), by = "id"]
#   climb_rate_duration$last_climb_rate_sign[is.na(climb_rate_duration$last_climb_rate_sign)] <- 0
#   climb_rate_duration$climb_rate_sign_change <- (climb_rate_duration$last_climb_rate_sign != climb_rate_duration$climb_rate_sign)
#   climb_rate_duration[, "total_climb_rate_sign_change" := cumsum(climb_rate_sign_change), by = "id"]
#   return(climb_rate_duration)
# }


# climb_rate_duration <- get_climb_rate_duration_df(ac_data, alt_change_df)
# duration_df <- climb_rate_duration[, .(ts_readable = ts_readable[1],
#   start_altitude = altitude[.N]),
#   by = list(id, total_climb_rate_sign_change)]
# duration_df[, c("duration", "alt_diff") :=
#               list(c(diff(as.numeric(ts_readable)), NA),
#                    c(diff(start_altitude), NA)), by = list(id)]
# duration_df[duration_df$id == "0C6015_0"]
# 
# 
# prev_rows <- nrow(duration_df)
# rows <- prev_rows - 1
# 
# while(prev_rows != rows) {
#   prev_rows <- nrow(duration_df)
#   print(prev_rows)
#   climb_rate_duration$alt_diff <- climb_rate_duration$start_altitude <- climb_rate_duration$duration <- NULL
#   climb_rate_duration <- merge(climb_rate_duration, duration_df,
#                                by = c("id", "total_climb_rate_sign_change", "ts_readable"),
#                                all.x = T)
#   
#   climb_rate_duration$climb_rate_sign_change[is.na(climb_rate_duration$climb_rate_sign_change)] <- F
#   climb_rate_duration$alt_diff[is.na(climb_rate_duration$alt_diff)] <- 0
#   climb_rate_duration$climb_rate_sign[abs(climb_rate_duration$alt_diff) < 500] <- 0
#   discrepancy <- sign(climb_rate_duration$alt_diff) != climb_rate_duration$climb_rate_sign
#   while(sum(is.na(discrepancy)) > 0) {
#     climb_rate_duration$alt_diff[is.na(climb_rate_duration$alt_diff)] <- 0
#     discrepancy <- sign(climb_rate_duration$alt_diff) != climb_rate_duration$climb_rate_sign
#   }
#   climb_rate_duration$climb_rate_sign[discrepancy] <- sign(climb_rate_duration$alt_diff)[discrepancy]
#   climb_rate_duration[, "last_climb_rate_sign" := c(climb_rate_sign[1], climb_rate_sign[-.N]), by = "id"]
#   climb_rate_duration$climb_rate_sign_change <- (climb_rate_duration$last_climb_rate_sign != climb_rate_duration$climb_rate_sign)
#   # while(sum(is.na(climb_rate_duration$duration)) > 0) {
#   #   climb_rate_duration$duration[is.na(climb_rate_duration$duration)] <- 0
#   # }
#   # climb_rate_duration$climb_rate_sign_change[climb_rate_duration$duration < 10] <- F
#   climb_rate_duration[, "total_climb_rate_sign_change" := cumsum(climb_rate_sign_change), by = "id"]
#   duration_df <- climb_rate_duration[, .(ts_readable = ts_readable[1],
#                                          start_altitude = altitude[1]),
#                                      by = list(id, total_climb_rate_sign_change)]
#   duration_df[, c("duration", "alt_diff") :=
#                 list(c(diff(as.numeric(ts_readable)), NA),
#                      c(diff(start_altitude), NA)), by = list(id)]
#   rows <- nrow(duration_df)
# }
# duration_df[duration_df$id == "0C6015_0"]
# 
# id1 <- "0C6015_0"
# dir.create("flight_phase", showWarnings = F)
# for(id1 in unique(ac_data$id)) {
#   png(paste0("flight_phase/", id1, ".png"), width = 800, height = 600)
#   plot(ac_data$ts_readable[ac_data$id == id1], ac_data$altitude[ac_data$id == id1])
#   lines(alt_change_df$ts_readable[alt_change_df$id == id1],
#         alt_change_df$altitude[alt_change_df$id == id1], col = "blue", lwd = 2)
#   dev.off()
# }
# 
# 
# 
# 
# plot(ac_data$ts_readable[ac_data$id == "0C6015_0"], ac_data$altitude[ac_data$id == "0C6015_0"])
# lines(alt_change_df$ts_readable[alt_change_df$id == "0C6015_0"], alt_change_df$altitude[alt_change_df$id == "0C6015_0"], col = "red")
# lines(duration_df$ts_readable[duration_df$id == "0C6015_0"], duration_df$start_altitude[duration_df$id == "0C6015_0"], col = "blue")










#------------------------


# get_alt_change_df <- function(ac_data) {
#   ac_data[, 'last_altitude' := c(NA, altitude[-.N]), by = "id"]
#   ac_data$altitude_change <- (ac_data$altitude != ac_data$last_altitude)
#   ac_data$altitude_change[is.na(ac_data$altitude_change)] <- T
#   ac_data$nearest_500ft_mark <- 500 * round(ac_data$altitude/500)
#   alt_change_idx <- which(ac_data$altitude_change)
#   alt_change_idx <- unique(unlist(lapply(alt_change_idx, function(idx) c(idx - 1, idx))))
#   alt_change_idx <- alt_change_idx[alt_change_idx != 0]
#   alt_change_df <- ac_data[alt_change_idx, ]
#   alt_change_df[, c("last_ts_alt_change", "next_ts_alt_change") :=
#                   list(c(ts_readable[1], ts_readable[-.N]),
#                        c(ts_readable[-1], NA)), by = "id"]
#   # alt_change_df$next_ts_diff_alt_change <- as.numeric(alt_change_df$next_ts_alt_change - alt_change_df$ts_readable)
#   return(alt_change_df)
# }




# ac_data[, "last_altitude" := c(NA, altitude[-.N]), by = "id"]
# alt_change <- (ac_data$altitude != ac_data$last_altitude)
# alt_change_idx <- which(alt_change | is.na(alt_change))
# alt_change_idx <- unlist(lapply(alt_change_idx, function(idx) c(idx-1, idx)))
# alt_change_idx <- unique(alt_change_idx)
# alt_change_idx <- alt_change_idx[alt_change_idx != 0]
# alt_change_df <- ac_data[alt_change_idx, ]
# alt_change_df[, c("last_ts_alt_change", "last_altitude") := list(c(ts_readable[1], ts_readable[-.N]),
#                                                                  c(altitude[1], altitude[-.N])), by = "id"]
# alt_change_df$last_ts_diff_alt_change <- as.numeric(alt_change_df$ts_readable - alt_change_df$last_ts_alt_change)
# alt_change_df$smoothed_est_climb_rate <- 60 * (alt_change_df$altitude - alt_change_df$last_altitude)/alt_change_df$last_ts_diff_alt_change
# # For the first row of each aircraft
# alt_change_df$smoothed_est_climb_rate[is.na(alt_change_df$smoothed_est_climb_rate)] <- 0
# ac_data$smoothed_est_climb_rate <- ac_data$last_ts_diff_alt_change <-
#   ac_data$last_ts_alt_change <- ac_data$last_altitude <- NULL
# ac_data <- merge(ac_data, alt_change_df, all = T)
# ac_data$smoothed_est_climb_rate[is.na(ac_data$smoothed_est_climb_rate)] <- 0












# df_00B0EF_0 <- climb_rate_duration[climb_rate_duration$id == climb_rate_duration$id[1], ]





# plot(x = alt_change_df$ts_readable[alt_change_df$id == "A075DF_13"],
#      y = alt_change_df$altitude[alt_change_df$id == "A075DF_13"], type = 'l')
# lines(x = ac_data$ts_readable[ac_data$id == "A075DF_13"],
#       y = ac_data$altitude[ac_data$id == "A075DF_13"], col = 'red')
# 



# alt_change_df$last_ts_diff_alt_change <- as.numeric(alt_change_df$ts_readable - alt_change_df$last_ts_alt_change)
# Estimate the smoothed climb rate in ft/min
# alt_change_df$smoothed_est_climb_rate <- 60 * (alt_change_df$altitude - alt_change_df$last_altitude)/alt_change_df$last_ts_diff_alt_change



# climb_rate_duration$duration[is.na(climb_rate_duration$duration)] <- 0


# climb_rate_duration$climb_rate_sign_change[climb_rate_duration$duration < 60] <- F

# alt_change_df1 <- get_alt_change_df(climb_rate_duration)
# duration_df1 <- climb_rate_duration[climb_rate_duration$climb_rate_sign_change, ]


# climb_rate_duration[climb_rate_duration$climb_rate_sign_change &
#                       climb_rate_duration$id == "00B0EF_0",
#                     c("ts_readable", "climb_rate_sign", "last_climb_rate_sign", "altitude", "duration", "alt_diff")]


# ac_data[, c("min_ts", "max_ts") := list(min(ts_readable), max(ts_readable)), by = "id"]
# ac_data$hr <- hour(ac_data$ts_readable)
# ac_data$day <- weekdays(ac_data$ts_readable)
# friday_hour_18_data <- ac_data[ac_data$hr == 18 & ac_data$day == "Friday", ]
# ac_data$hour_day <- paste0(ac_data$hr, "_", ac_data$day)

# library(animation)
# setorder(friday_hour_18_data, ts)
# uniq_ts <- unique(friday_hour_18_data$ts)
# uniq_ts <- uniq_ts[order(uniq_ts)]
# max_time_df <- friday_hour_18_data[, list(max_ts = max(ts)), by = id]
# friday_hour_18_data <- merge(friday_hour_18_data, max_time_df)
# func <- function() {
#   for(i_time in uniq_ts) {
#     subset_df <- friday_hour_18_data[friday_hour_18_data$ts <= i_time & i_time <= friday_hour_18_data$max_ts, ]
#     subset_df[, "nrows" := .N, by = "id"]
#     subset_df <- subset_df[subset_df$nrows >= 2, ]
#     uniq_id <- unique(subset_df$id)
#     if(nrow(subset_df) > 0) {
#       lines3D(x = subset_df$lon[subset_df$id == uniq_id[1]],
#               y = subset_df$lat[subset_df$id == uniq_id[1]],
#               z = subset_df$altitude[subset_df$id == uniq_id[1]],
#               xlim = range(ac_data$lon), ylim = range(ac_data$lat),
#               zlim = range(ac_data$altitude), colkey = F)
#       for(id1 in uniq_id[-1]) {
#         lines3D(x = subset_df$lon[subset_df$id == id1],
#                 y = subset_df$lat[subset_df$id == id1],
#                 z = subset_df$altitude[subset_df$id == id1],
#                 xlim = range(ac_data$lon), ylim = range(ac_data$lat),
#                 zlim = range(ac_data$altitude), add = T, colkey = F)
#       }
#     }
#   }
# }
# 
# dir.create("hour18_new", showWarnings = F)
# setwd("hour18_new")
# oopt <- ani.options(interval = 0.05, nmax = nrow(max_ac_df))  
# saveHTML(func(), interval = 0.05, width = 870, height = 600)
# setwd("../")




# ac_data$nearest_500ft_mark <- 500 * round(ac_data$altitude/500)
# ac_data[, "last_nearest_500ft_mark" := c(NA, nearest_500ft_mark[-.N]), by = "id"]
# ac_data$altitude_change <- (ac_data$nearest_500ft_mark != ac_data$last_nearest_500ft_mark)
# alt_change_df$nearest_500ft_mark <- 500 * round(alt_change_df$altitude/500)


# climb_rate_duration[, "last_nearest_500ft_mark" := c(NA, nearest_500ft_mark[-.N]), by = "id"]
# climb_rate_duration$nearest_500ft_mark_change <- (climb_rate_duration$last_nearest_500ft_mark != climb_rate_duration$nearest_500ft_mark)
# nearest_500ft_change_idx <- which(climb_rate_duration$nearest_500ft_mark_change)
# nearest_500ft_change_idx <- unique(unlist(lapply(nearest_500ft_change_idx, function(idx) c(idx - 1, idx))))
# nearest_500ft_change_idx <- nearest_500ft_change_idx[nearest_500ft_change_idx != 0]
# climb_rate_duration <- climb_rate_duration[nearest_500ft_change_idx, ]


# df_00B0EF_0[df_00B0EF_0$last_nearest_500ft_mark != df_00B0EF_0$nearest_500ft_mark, ]
# df_00B0EF_0$cutpoints <- paste0(df_00B0EF_0$last_nearest_500ft_mark, "_", df_00B0EF_0$nearest_500ft_mark)


# alt_change_df$nearest_500ft_mark <- 500 * round(alt_change_df$altitude/500)
# alt_change_df[, "last_nearest_500ft_mark" := c(nearest_500ft_mark[1], nearest_500ft_mark[-.N]), by = "id"]

# alt_change_df <- alt_change_df[, c("id", "ts_readable", "smoothed_est_climb_rate")]
