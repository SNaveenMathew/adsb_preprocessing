# Notes on azimuth calculation:
# The azimuth is equal to the current bearing:
# azimuth = bearing(c(lon, lat), c(last_lon, last_lat))
# This means forecast for next location depends on forecast for bearing

# Forecast for next point:
# destPoint(c(lon, lat), next_azimuth, estimated_ground_distance)

# For ground speed = 0 most frequent values of azimuth are: 180, 89, 271, 240, 120, 59, 301, 239, 58

library(plyr)
library(dplyr)
library(data.table)
library(splines)
library(parallel)
library(ggplot2)

forward_fillna <- function(na_series) {
  na_idx <- which(is.na(na_series))
  while(length(na_idx) > 0) {
    na_series[na_idx] <- na_series[na_idx - 1]
    na_idx <- which(is.na(na_series))
  }
  return(na_series)
}

back_fillna <- function(na_series) {
  na_idx <- which(is.na(na_series))
  while(length(na_idx) > 0) {
    na_series[na_idx] <- na_series[na_idx + 1]
    na_idx <- which(is.na(na_series))
  }
  return(na_series)
}

get_azi_change_df <- function(ac_data) {
  ac_data[, "last_azimuth" := c(NA, azimuth[-.N]), by = "id"]
  azi_change_idx <- which(ac_data$azimuth != ac_data$last_azimuth | is.na(ac_data$last_azimuth))
  azi_change_idx <- unique(unlist(lapply(azi_change_idx, function(idx) c(idx - 1, idx))))
  azi_change_idx <- azi_change_idx[azi_change_idx != 0]
  azi_change_df <- ac_data[azi_change_idx, ]
  azi_change_df <- azi_change_df[azi_change_df$altitude != 0, ]
  azi_change_df <- azi_change_df[, c("id", "ts", "azimuth", "ts_readable"#, "climb_rate"#,
                                     #"est_climb_rate"
  )]
  return(azi_change_df)
}

get_azi_diff <- function(azimuths) {
  diffs <- diff(azimuths)
  diffs[diffs < (-180)] <- diffs[diffs < (-180)] + 360
  diffs[diffs > 180] <- diffs[diffs > 180] - 360
  return(diffs)
}

identify_bank_phase <- function(uniq_id, plot_turns = T, return_dataframe = T, plot_point = F) {
  return_df <- data.frame()
  for(id1 in uniq_id) {
    tryCatch({
      id1_df <- azi_change_df[azi_change_df$id == id1, ]
      est_bank_rate <- get_azi_diff(id1_df$azimuth)/diff(id1_df$ts)
      bank_rate_sign <- c(0, sign(est_bank_rate))
      bank_rate_sign_change <- c(F, bank_rate_sign[-1] != bank_rate_sign[-length(bank_rate_sign)])
      id1_df$bank_rate_sign <- bank_rate_sign
      id1_df$bank_rate_sign_change <- bank_rate_sign_change
      id1_df$last_ts_diff <- c(0, diff(id1_df$ts))
      id1_df$last_azimuth_diff <- c(0, get_azi_diff(id1_df$azimuth))
      # id1_df$est_bank_rate <- c(0, est_bank_rate)
      id1_df$total_bank_rate_sign_change <- cumsum(id1_df$bank_rate_sign_change)
      setDT(id1_df)
      # id1_df[, c("total_bank_angle", "total_time") := list(sum(last_azimuth_diff), sum(last_ts_diff)), by = total_bank_rate_sign_change]
      df1 <- id1_df[, .("ts_readable" = ts_readable[1], "total_bank_angle" = sum(last_azimuth_diff), "total_time" = sum(last_ts_diff)), by = "total_bank_rate_sign_change"]
      df1$avg_bank_rate <- df1$total_bank_angle/df1$total_time
      angle_correction_sum <- (df1$total_bank_angle + c(NA, df1$total_bank_angle[-nrow(df1)]))[-1]
      time_correction_sum <- (df1$total_time + c(NA, df1$total_time[-nrow(df1)]))[-1]
      df1$turn <- F
      df1$turn[(abs(df1$avg_bank_rate) > 0.5 & df1$total_time > 12) |
                 abs(df1$total_bank_angle) > 12
               ] <- T
      df1$turn[which(df1$turn) + 1] <- T
      df2 <- df1[df1$turn, ]
      correction_total_bank_angle <- df2$total_bank_angle[-1] + df2$total_bank_angle[-nrow(df2)]
      correction_total_time <- df2$total_time[-1] + df2$total_time[-nrow(df2)]
      idxs <- which(abs(correction_total_bank_angle) < 10 & correction_total_time < 60 &
                      (abs(df2$total_bank_angle[-1]) > 10 | abs(df2$total_bank_angle[-nrow(df2)]) > 10))
      if(length(idxs) > 0) {
        for(idx in idxs) {
          df1$turn[which(df1$turn)[idx:(idx + 1)]] <- F
        }
      }
      id1_df1 <- merge(id1_df, df1, all = T, by = c("total_bank_rate_sign_change", "ts_readable"))
      id1_df1$turn[is.na(id1_df1$turn)] <- F
      id1_df1$total_azi_change <- id1_df1$ts_readable <- NULL
      
      id1_df2 <- merge(id1_df1, ac_data[ac_data$id == id1], all = T, by = c("id", "ts", "azimuth"))
      id1_df2$turn[is.na(id1_df2$turn)] <- F
      turns_df <- id1_df2[id1_df2$turn, ]
      
      if(plot_turns) {
        if(!is.na(turns_df$jfk_landing_flag[1])) {
          png(paste0("bank/jfk_landing/", id1, ".png"), width = 800, height = 600)
        } else {
          png(paste0("bank/", id1, ".png"), width = 800, height = 600)
        }
        # plot(id1_df2$lon, id1_df2$lat)
        # points(id1_df2$lon[id1_df2$turn], id1_df2$lat[id1_df2$turn], pch = 20, col = "red")
        if(plot_point) {
          p <- ggplot() + geom_point(data = id1_df2, aes(x = lon, y = lat, colour = altitude)) +
            geom_point(data = turns_df, aes(x = lon, y = lat))
        } else {
          p <- ggplot() + geom_path(data = id1_df2, aes(x = lon, y = lat),
                                    arrow = arrow(type = "closed", angle = 18,
                                                  length = unit(0.05, "inches"))) +
            geom_point(data = turns_df, aes(x = lon, y = lat, colour = altitude)) +
            scale_colour_gradient(low = "red", high = "green")
        }
        print(p)
        dev.off()
      }
      if(return_dataframe) {
        return_df <- rbind(return_df, id1_df2)
      }
    }, error = function(e) print(e))
  }
  return(return_df)
}

setwd("/Users/naveensathiyanathan/Desktop/Coding/Old/ATC_Analysis_2019_04_01/Banking_Analysis/")
dir.create("bank", showWarnings = F)
dir.create("bank/jfk_landing", showWarnings = F)
ac_data <- fread("../../../samples_with_jfk_landing_flag.csv")
ac_data <- ac_data[ac_data$ground_flag != 1, ]
setDT(ac_data)
setorder(ac_data, id, ts)
ac_data$ts_readable <- as.POSIXct.numeric(as.numeric(ac_data$ts), origin="1970-01-01")

azi_change_df <- get_azi_change_df(ac_data)
azi_change_df$azi_change <- TRUE
ac_data <- merge(ac_data, azi_change_df, all = T)
ac_data$azi_change[is.na(ac_data$azi_change)] <- F
ac_data[, "total_azi_change" := cumsum(azi_change), by = "id"]
ac_data[, "next_ts" := c(ts[-1], NA), by = "id"]
ac_data$next_ts_diff <- ac_data$next_ts - ac_data$ts
# ac_data[, "azi_duration" := sum(next_ts_diff, na.rm = T), by = list(id, total_azi_change)]
azi_change_df <- ac_data[ac_data$azi_change,
                         c("id", "ts", "ts_readable",
                           "azimuth", "total_azi_change"#, "azi_duration"#,
                           #"climb_rate", "est_climb_rate"
                         )]
ac_data$azi_change <- NULL

df <- ac_data[, .(nrows = .N, max_alt = max(altitude)), by = "id"]
df <- df[df$max_alt >= 30000]
id1 <- df$id[which.max(df$nrows)]
ggplot(ac_data[ac_data$id == id1]) + geom_point(aes(x = lon, y = lat, col = altitude))

uniq_id <- unique(ac_data$id)

cores <- detectCores() - 1
num <- ceiling(length(uniq_id)/cores)
idx <- ceiling(1:length(uniq_id)/num)
uniq_id_splits <- split(uniq_id, idx)

cl <- makeCluster(cores)
clusterExport(cl, list("azi_change_df", "ac_data", "tryCatch", "diff", "c", "sign", "cumsum",
                       "list", "sum", "merge", "is.na", "abs", "setDT", "get_azi_diff", "ggplot",
                       "geom_point", "aes", "geom_path", "arrow", "unit", "scale_colour_gradient"))
ret <- parLapply(cl = cl, X = uniq_id_splits, fun = identify_bank_phase)
ret <- do.call(rbind, ret)
ret$turn[ret$ground_speed == 0] <- F
ret$avg_bank_rate[!ret$turn] <- 0
stopCluster(cl)


jfk_data <- ac_data[ac_data$jfk_landing_flag, ]
final_azimuth_df <- jfk_data[, .(final_azimuth = azimuth[.N],
                                 destination = destination[.N],
                                 max_altitude = max(altitude)), by = id]
mean(final_azimuth_df$destination == "JFK")
hist(final_azimuth_df$final_azimuth, breaks = 80)
# Landing azimuths ranges: 25-35 and 205-215
# Most frequent: 30, 201, 31, 211, 29, 300


saveRDS(ret[, c("id", "ts", "azimuth", "azi_change", "turn", "avg_bank_rate")],
        "estimated_banking.Rds")
saveRDS(ret, "full_estimated_banking.Rds")







########## Loop
# return_df <- rbind(return_df, id1_df2)
# non_turn_check <- function() {
#   angle_corrections <- angle_correction_sum[df1$turn]
#   time_corrections <- time_correction_sum[df1$turn]
# }
# mdl <- smooth.spline(x = azi_change_df$ts[azi_change_df$id == id1],
#                      y = azi_change_df$azimuth[azi_change_df$id == id1])
# est_bank_rate <- diff(predict(mdl, azi_change_df$ts[azi_change_df$id == id1])$y)/
#   diff(azi_change_df$ts[azi_change_df$id == id1])
# est_azi_rate[abs(est_azi_rate) < 3] <- 0
# idx <- which(bank_rate_sign_change)
# idx <- unique(unlist(lapply(idx, function(i) c(i - 1, i))))
# bank_rate_sign <- sign(est_bank_rate)
# bank_rate_sign_change <- c(NA, bank_rate_sign[-1] != (bank_rate_sign[-length(bank_rate_sign)]))
# bank_rate_sign_change[is.na(bank_rate_sign_change)] <- T
# df1 <- data.frame(ts_readable = id1_df$ts_readable[idx],
#                   azimuth = id1_df$azimuth[idx],
#                   bank_rate_sign = bank_rate_sign[idx],
#                   bank_rate_sign_change = bank_rate_sign_change[idx])
# id1_df1 <- merge(id1_df, df1, all = T, by = c("ts_readable", "azimuth"))
# id1_df1$bank_rate_sign_change <- back_fillna(id1_df1$bank_rate_sign_change)
# id1_df1$bank_rate_sign <- back_fillna(id1_df1$bank_rate_sign)
# id1_df1$last_ts <- c(NA, id1_df1$ts_readable[-nrow(id1_df1)])
# id1_df1$last_ts_diff <- as.numeric(id1_df1$ts_readable) - id1_df1$last_ts
# id1_df1$last_ts_diff[1] <- 0
# id1_df1$bank_rate_sign_change[1] <- F
# id1_df1$total_bank_rate_sign_change <- cumsum(id1_df1$bank_rate_sign_change)
# id1_df1[, "bank_time" := sum(last_ts_diff), by = "total_bank_rate_sign_change"]
# id1_df2 <- id1_df1[idx, ]
# df1$azi_sign_change <- cumsum(df1$climb_rate_sign_change)
# setDT(df1)
# summary_df <- df1[, .(start_ts = ts_readable[1], start_altitude = altitude[1], end_altitude = altitude[.N],
#                       duration = sum(duration, na.rm = T), alt_diff = altitude[.N] - altitude[1]),
#                   by = alt_diff_sign_change]
# 
# summary_df$climb_rate_sign_change <- T
# df2 <- merge(azi_change_df[azi_change_df$id == id1, c("ts_readable", "id", "ts", "altitude")],
#              summary_df[, c("start_ts", "start_altitude", "end_altitude", "duration", "alt_diff",
#                             "climb_rate_sign_change")],
#              by.x = c("ts_readable"), by.y = "start_ts", all.x = T)
# df2$climb_rate_sign <- sign(df2$alt_diff)
# df2$climb_rate_sign[abs(df2$alt_diff) < 500] <- 0
# df2$climb_rate_sign <- forward_fillna(df2$climb_rate_sign)
# df2$climb_rate_sign_change <- c(T, df2$climb_rate_sign[-1] != df2$climb_rate_sign[-nrow(df2)])
# df2$total_climb_rate_sign_change <- cumsum(df2$climb_rate_sign_change)
# summary_df <- df2[, .(start_ts = ts_readable[1], start_altitude = altitude[1],
#                       end_altitude = altitude[.N], duration = sum(duration, na.rm = T),
#                       alt_diff = altitude[.N] - altitude[1]),
#                   by = "total_climb_rate_sign_change"]
# 
# summary_df$climb_rate_sign_change <- T
# summary_df$climb_rate_sign <- sign(summary_df$alt_diff)
# summary_df$climb_rate_sign[abs(summary_df$alt_diff) <= 500] <- 0
# summary_df1 <- expand_summary_df(summary_df)
# summary_df2 <- summary_df1[seq(1, nrow(summary_df1), by = 2), ]
# summary_df2$decision_point <- TRUE
# final_df <- merge(ac_data[ac_data$id == id1, ], summary_df2, all.x = T, by = c("ts_readable", "altitude"))
# final_df$climb_rate_sign <- fillna(final_df$climb_rate_sign)
# if(plot_phases) {
#   png(paste0("flight_phase/", id1, ".png"), width = 800, height = 600)
#   plot(final_df$ts_readable, final_df$altitude, col = final_df$climb_rate_sign + 4, cex = 0.5)
#   points(final_df$ts_readable[final_df$decision_point], final_df$altitude[final_df$decision_point],
#          col = "red", cex = 1, pch = 20)
#   legend("topright", legend = unique(final_df$climb_rate_sign), fill = unique(final_df$climb_rate_sign) + 4)
#   dev.off()
# }