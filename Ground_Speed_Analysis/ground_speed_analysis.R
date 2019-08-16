library(plyr)
library(dplyr)
library(data.table)
library(splines)
library(parallel)
library(plotrix)

fillna <- function(na_series) {
  na_idx <- which(is.na(na_series))
  while(length(na_idx) > 0) {
    na_series[na_idx] <- na_series[na_idx - 1]
    na_idx <- which(is.na(na_series))
  }
  return(na_series)
}


get_speed_change_df <- function(ac_data) {
  ac_data[, "last_ground_speed" := c(NA, ground_speed[-.N]), by = "id"]
  ground_speed_change <- (ac_data$last_ground_speed != ac_data$ground_speed)
  ground_speed_change[is.na(ground_speed_change)] <- T
  idx <- which(ground_speed_change)
  idx <- unique(unlist(lapply(idx, function(id1) c(id1 - 1, id1))))
  idx <- idx[idx != 0]
  return(ac_data[idx, ])
}

plot_speed_altitude_decision_points <- function(uniq_id) {
  for(id1 in uniq_id) {
    tryCatch({
      speed_change_df1 <- speed_change_df[speed_change_df$id == id1, ]
      out_df1 <- out_df[out_df$id == id1, ]
      mdl <- smooth.spline(x = speed_change_df1$ts,
                           y = speed_change_df1$ground_speed)
      est_acceleration <- diff(predict(mdl, speed_change_df1$ts)$y)/
        diff(speed_change_df1$ts)
      # est_acceleration[abs(est_acceleration) < 3] <- 0 # decide the number here
      acceleration_sign <- sign(est_acceleration)
      
      acceleration_sign_change <- c(NA, acceleration_sign[-1] != (acceleration_sign[-length(acceleration_sign)]))
      acceleration_sign_change[is.na(acceleration_sign_change)] <- T
      df1 <- data.frame(ts_readable = speed_change_df1$ts_readable,
                        ground_speed = speed_change_df1$ground_speed,
                        acceleration_sign = c(0, acceleration_sign),
                        acceleration_sign_change = c(acceleration_sign_change, F),
                        duration = speed_change_df1$speed_duration)
      png(paste0("smooth_ground_speed/", id1, ".png"), width = 800, height = 600)
      if(nrow(out_df1) > 0) {
        twoord.plot(lx = speed_change_df1$ts_readable,
                    ly = predict(mdl, speed_change_df1$ts)$y,
                    rx = out_df1$ts_readable[out_df1$decision_point],
                    ry = out_df1$altitude[out_df1$decision_point])
      } else {
        if(sd(ac_data$altitude[ac_data$id == id1]) > 0) {
          twoord.plot(lx = speed_change_df1$ts_readable,
                      ly = predict(mdl, speed_change_df1$ts)$y,
                      rx = ac_data$ts_readable[ac_data$id == id1],
                      ry = ac_data$altitude[ac_data$id == id1])
        } else {
          plot(x = speed_change_df1$ts_readable,
               y = predict(mdl, speed_change_df1$ts)$y)
          legend("topright", legend = paste0("Altitude: ", as.character(ac_data$altitude[ac_data$id == id1][1])))
        }
      }
      dev.off()
    }, error = function(e) NULL)
  }
}

setwd("/Users/naveensathiyanathan/Desktop/Coding/Old/ATC_Analysis_2019_04_01/Ground_Speed_Analysis/")
ac_data <- fread("../../../samples_with_jfk_landing_flag.csv")
ac_data$ts_readable <- as.POSIXct.numeric(as.numeric(ac_data$ts), origin="1970-01-01")
ac_data$est_climb_rate <- NULL
setDT(ac_data)
setorder(ac_data, id, ts)

cores <- detectCores() - 1
uniq_id <- unique(ac_data$id)
ac_data <- ac_data[, c("id", "ts", "lon", "lat", "ground_speed", "altitude")]
ac_data$ts_readable <- as.POSIXct.numeric(as.numeric(ac_data$ts), origin="1970-01-01")
cl <- makeCluster(cores)
num <- ceiling(length(uniq_id)/cores)
idx <- ceiling(1:length(uniq_id)/num)
uniq_id_splits <- split(uniq_id, idx)


speed_change_df <- get_speed_change_df(ac_data)
speed_change_df <- speed_change_df[!is.na(speed_change_df$id), ]
speed_change_df$speed_change <- TRUE
ac_data <- merge(ac_data, speed_change_df, all = T)
ac_data$speed_change[is.na(ac_data$speed_change)] <- F
ac_data[, "total_speed_change" := cumsum(speed_change), by = "id"]
ac_data[, "next_ts" := c(ts[-1], NA), by = "id"]
ac_data$next_ts_diff <- ac_data$next_ts - ac_data$ts
ac_data <- ac_data[!is.na(ac_data$id), ]
ac_data[, "speed_duration" := sum(next_ts_diff, na.rm = T),
        by = list(id, total_speed_change)]
speed_change_df <- ac_data[ac_data$speed_change,
                           c("id", "ts", "ts_readable",
                             "ground_speed", "total_speed_change", "speed_duration")]

h_thr <- 5

refine_id1_df <- function(id1_df) {
  mdl <- smooth.spline(x = id1_df$ts,
                       y = id1_df$ground_speed)
  est_acceleration <- diff(predict(mdl, id1_df$ts)$y)/diff(id1_df$ts)
  est_acceleration[abs(est_acceleration) < 0.3] <- 0
  acceleration_sign <- sign(est_acceleration)
  acceleration_sign_change <- (acceleration_sign[-1] != (acceleration_sign[-length(acceleration_sign)]))
  idx <- lapply(2:(nrow(id1_df) - 1), function(i) c(i, i+1))
  idx <- idx[acceleration_sign_change]
  idx <- c(1, unique(unlist(idx)))
  id1_df <- id1_df[idx, ]
  return(list(mdl = mdl, id1_df = id1_df))
}

expand_id1_df <- function(id1_df) {
  
}

for(id1 in unique(speed_change_df$id)) {
  tryCatch({
    id1_df <- speed_change_df[speed_change_df$id == id1, ]
    last_row <- id1_df[nrow(id1_df), ]
    id1_df$total_speed_change <- NULL
    prev_rows <- nrow(id1_df)
    rows <- prev_rows - 1
    while(rows != prev_rows) {
      prev_rows <- nrow(id1_df)
      id1_df <- refine_id1_df(id1_df)
      mdl <- id1_df[["mdl"]]
      id1_df <- id1_df[["id1_df"]]
      rows <- nrow(id1_df)
    }
    # id1_df$ts_end <- id1_df$ts_readable + id1_df$speed_duration
    # id1_df$last_ts_end <- c(NA, id1_df$ts_end[-nrow(id1_df)])
    # id1_df$ts_diff <- (id1_df$ts - id1_df$last_ts_end)
    # id1_df$ts_diff[is.na(id1_df$ts_diff)] <- id1_df$speed_duration[1]
    # id1_df$ts_diff[id1_df$ts_diff == 0] <- id1_df$speed_duration[which(id1_df$ts_diff == 0) - 1]
    # id1_df$speed_diff <- c(0, diff(id1_df$ground_speed))
    # # removes <- c()
    # i <- 2
    # while(i <= nrow(id1_df)) {
    #   if(id1_df$ts_diff[i] < 10) {
    #     acc_0_est <- id1_df$speed_diff[i - 1]/id1_df$ts_diff[i - 1]
    #     acc_1_est <- id1_df$speed_diff[i]/id1_df$ts_diff[i]
    #     if(abs(acc_1_est - acc_0_est) < 0.5) {
    #       id1_df$ts_diff[i - 1] <- id1_df$ts_diff[i] + id1_df$ts_diff[i - 1]
    #       id1_df$speed_diff[i - 1] <- id1_df$speed_diff[i - 1] + id1_df$speed_diff[i]
    #       id1_df <- id1_df[-i, ]
    #       i <- i - 1
    #     }
    #   }
    #   i <- i + 1
    # }
    # id1_df$est_acceleration <- id1_df$speed_diff/id1_df$ts_diff
    # id1_df$acceleration_sign <- sign(id1_df$est_acceleration)
    # id1_df$acceleration_sign_change <- c(T, id1_df$acceleration_sign[-1] != id1_df$acceleration_sign[-nrow(id1_df)])
    # id1_df <- id1_df[id1_df$acceleration_sign_change, ]
    # id1_df <- id1_df[, c("ts", "ts_readable", "ground_speed", "est_acceleration")]
    # id1_df$est_acceleration[abs(id1_df$est_acceleration) < 0.05] <- 0
    # id1_df$speed_diff <- c(0, diff(id1_df$ground_speed))
    # id1_df$acceleration_sign <- sign(id1_df$est_acceleration)
    # id1_df$acceleration_sign[abs(id1_df$speed_diff) < 10] <- 0
    # id1_df <- id1_df[c(15, diff(id1_df$ts)) >= 15, ]
    # last_row$total_speed_change <- last_row$speed_duration <- last_row$id <- NULL
    # id1_df$est_acceleration <- id1_df$speed_diff <- id1_df$acceleration_sign <- NULL
    # id1_df <- rbind(id1_df, last_row)
    # id1_df$est_acceleration <- c(diff(id1_df$ground_speed)/diff(id1_df$ts), 0)
    # id1_df$est_acceleration[abs(id1_df$est_acceleration) < 0.05] <- 0
    # id1_df$acceleration_sign <- sign(id1_df$est_acceleration)
    # id1_df$acceleration_sign_change <- c(T, id1_df$acceleration_sign[-1] != id1_df$acceleration_sign[-nrow(id1_df)])
    # id1_df <- id1_df[id1_df$acceleration_sign_change, ]
    # id1_df$decision_point <- T
    # 
    # final_df <- merge(ac_data[ac_data$id == id1, ], id1_df, all = T, by = c("ts_readable", "ground_speed"))
    # final_df$acceleration_sign <- fillna(final_df$acceleration_sign)
    png(paste0("cusum/", id1, ".png"), width = 800, height = 600)
    plot(ac_data$ts_readable[ac_data$id == id1], ac_data$ground_speed[ac_data$id == id1],
         # col = final_df$acceleration_sign + 4,
         cex = 0.5)
    # final_df$decision_point[is.na(final_df$decision_point)] <- F
    # points(final_df$ts_readable[final_df$decision_point], final_df$ground_speed[final_df$decision_point],
    #        col = "red", cex = 1, pch = 20)
    points(id1_df$ts_readable, id1_df$ground_speed,
           col = "red", cex = 1, pch = 20)
    # legend("topright", legend = unique(final_df$acceleration_sign), fill = unique(final_df$acceleration_sign) + 4)
    dev.off()
  }, error = function(e) NULL)
}


expand_summary_df <- function(summary_df) {
  df <- data.frame()
  for(i in 1:nrow(summary_df)) {
    df <- rbind(df, data.frame(ts_readable = summary_df$start_ts[i],
                               ground_speed = summary_df$start_speed[i],
                               acceleration_sign = summary_df$acceleration_sign[i]))
    df <- rbind(df, data.frame(ts_readable = summary_df$start_ts[i] + summary_df$duration[i],
                               ground_speed = summary_df$end_speed[i],
                               acceleration_sign = summary_df$acceleration_sign[i]))
  }
  return(df)
}

out_df <- readRDS("../Flight_Phase_Identification/estimated_flight_phase.Rds")

clusterExport(cl, list("smooth.spline", "speed_change_df", "png", "paste0", "ac_data", "plot", "twoord.plot", "predict", "dev.off", "tryCatch", "out_df"))

# for(id1 in unique(speed_change_df$id)) {
#   subset_df <- speed_change_df[speed_change_df$id == id1]
#   if(nrow(subset_df) > 1) {
#   i_CUSUM <- istar_CUSUM(subset_df$ground_speed, h = 5)
#   png(paste0("cusum/", id1, ".png"))
#   plot(subset_df$ts_readable, subset_df$ground_speed)
#   abline(v = subset_df$ts_readable[i_CUSUM])
#   dev.off()
#   }
# }


parLapply(cl = cl, X = uniq_id_splits, fun = plot_speed_altitude_decision_points)
stopCluster(cl)

















# -----------------------------------
# id1_df$total_speed_change <- NULL
# mdl <- smooth.spline(x = id1_df$ts,
#                      y = id1_df$ground_speed)
# est_acceleration <- diff(predict(mdl, id1_df$ts)$y)/diff(id1_df$ts)
# est_acceleration[abs(est_acceleration) < 0.2] <- 0
# acceleration_sign <- sign(est_acceleration)
# acceleration_sign_change <- (acceleration_sign[-1] != (acceleration_sign[-length(acceleration_sign)]))
# # id1_df$acceleration_sign_change[is.na(id1_df$acceleration_sign_change)] <- T
# idx <- lapply(2:(nrow(id1_df) - 1), function(i) c(i, i+1))
# idx <- idx[acceleration_sign_change]
# idx <- c(1, unique(unlist(idx)))
# idx_df <- id1_df[idx, ]


# id1_df$acceleration_sign_change[is.na(id1_df$acceleration_sign_change)] <- T
# df1 <- data.frame(ts_readable = id1_df$ts_readable,
#                   ground_speed = id1_df$ground_speed,
#                   acceleration_sign = c(id1_df$acceleration_sign),
#                   acceleration_sign_change = c(id1_df$acceleration_sign_change),
#                   duration = id1_df$speed_duration)
# df1$last_acceleration_sign_change <- c(F, df1$acceleration_sign_change[-nrow(df1)])

# bar_tick <- function(dat, nTic) {
#   n <- dim(dat)[1]
#   dat$winIdx <- as.factor(floor((1:n)/nTic))
#   df <- dat[, .(H = max(Price), L = min(Price), O = Price[1], C = Price[.N]), by = winIdx]
#   time_cols <- c("h", "m", "s", "ms")
#   o_c_time_df <- dat[, .(o_Date = Date[1], o_h = h[1], o_m = m[1], o_s = s[1],
#                          o_ms = ms[1], c_Date = Date[.N], c_h = h[.N],
#                          c_m = m[.N], c_s = s[.N], c_ms = ms[.N]), by = winIdx]
#   return(list(H = df$H, L = df$L, O = df$O, C = df$C, o_c_time_df = o_c_time_df))
# }
# 
# istar_CUSUM <- function(x, h){
#   nx <- length(x)
#   xminusEx <- diff(x)
#   S_pos <- S_neg <- 0.0;
#   istar <- c()
#   
#   for(i in 1:(nx-1)) {
#     S_pos <- max(0.0, S_pos + xminusEx[i]);
#     S_neg <- min(0.0, S_neg + xminusEx[i]);
#     if(max(S_pos, -S_neg) >= h) {
#       istar <- c(istar, i);
#       S_pos <- 0.0;
#       S_neg <- 0.0;
#     }
#   }
#   return(istar)
# }


# plot(ac_data$ts_readable[ac_data$id == id1],
#      ac_data$ground_speed[ac_data$id == id1], cex = 0.5)
# lines(ac_data$ts_readable[ac_data$id == id1], predict(mdl, ac_data$ts[ac_data$id == id1])$y)
# abline(v = out_df$ts_readable[out_df$id == id1 & out_df$decision_point])

# ____ Loop ____

# id1_df$ts_diff[is.na(id1_df$ts_diff)] <- 0
# id1_df$total_acceleration_sign_change <- cumsum(id1_df$acceleration_sign_change)
# setDT(df1)
# summary_df <- id1_df[, .(start_ts = ts_readable[1], start_speed = ground_speed[.N] - sum(speed_diff, na.rm = T),
#                          end_speed = ground_speed[.N], end_ts = ts_readable[1] + sum(ts_diff, na.rm = T),
#                       acceleration_sign = acceleration_sign[1]),# speed_diff = ground_speed[.N] - ground_speed[1]),
#                   by = total_acceleration_sign_change]
# end_idx <- which(df1$acceleration_sign_change) - 1
# end_idx <- idx[idx != 0]
# end_idx <- c(idx, nrow(df1))
# summary_df$acceleration_sign_change <- T
# df2 <- merge(id1_df[, c("ts_readable", "id", "ts", "ground_speed")],
#              summary_df[, c("start_ts", "start_speed", "end_speed", "duration", "speed_diff",
#                             "acceleration_sign_change")],
#              by.x = c("ts_readable"), by.y = "start_ts", all.x = T)
# df2$acceleration_sign <- sign(df2$speed_diff)
# df2$acceleration_sign[abs(df2$speed_diff) < 10] <- 0
# df2$acceleration_sign <- fillna(df2$acceleration_sign)
# df2$acceleration_sign_change <- c(T, df2$acceleration_sign[-1] != df2$acceleration_sign[-nrow(df2)])
# df2$total_acceleration_sign_change <- cumsum(df2$acceleration_sign_change)
# summary_df <- df2[, .(start_ts = ts_readable[1], start_speed = ground_speed[1],
#                       end_speed = ground_speed[.N], duration = sum(duration, na.rm = T),
#                       speed_diff = ground_speed[.N] - ground_speed[1]),
#                   by = "total_acceleration_sign_change"]
# 
# summary_df$acceleration_sign_change <- T
# summary_df$acceleration_sign <- sign(summary_df$speed_diff)
# summary_df$acceleration_sign[abs(summary_df$speed_diff) <= 10] <- 0
# summary_df1 <- expand_summary_df(summary_df)
# summary_df2 <- summary_df1[seq(1, nrow(summary_df1), by = 2), ]
# summary_df2$est_acceleration <- c(diff(summary_df2$ground_speed)/diff(as.numeric(summary_df2$ts_readable)),
#                                   summary_df2$acceleration_sign[nrow(summary_df2)])
# summary_df2$acceleration_sign[abs(summary_df2$est_acceleration) <= 0.1] <- 0
# # summary_df2$acceleration_sign <- sign(summary_df2$est_acceleration)
# summary_df2$last_acceleration_sign <- c(1, summary_df2$acceleration_sign[-nrow(summary_df2)])
# summary_df2 <- summary_df2[summary_df2$acceleration_sign != summary_df2$last_acceleration_sign, ]
# summary_df2$decision_point <- TRUE
# final_df <- merge(ac_data[ac_data$id == id1, ], summary_df2, all.x = T, by = c("ts_readable", "ground_speed"),
#                   all = T)