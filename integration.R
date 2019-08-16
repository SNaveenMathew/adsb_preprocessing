library(dplyr)
library(data.table)
library(ggplot2)
library(animation)

setwd("~/Desktop/Coding/Old/ATC_Analysis_2019_04_01")
banking <- readRDS("Banking_Analysis/estimated_banking.Rds")
climb <- readRDS("Flight_Phase_Identification/estimated_flight_phase.Rds")
ac_data <- fread("../../samples_with_jfk_landing_flag.csv")
ac_data$ts_readable <- as.POSIXct.numeric(as.numeric(ac_data$ts), origin="1970-01-01")
ac_data$jfk_landing_flag[is.na(ac_data$jfk_landing_flag)] <- F

ac_data <- merge(ac_data, climb, by = c("id", "ts_readable", "altitude"), all = T)
ac_data <- merge(ac_data, banking, by = c("id", "ts", "azimuth"), all = T)
nms <- colnames(ac_data)
nms[which(nms == "decision_point")] <- "altitude_decision_point"
nms[which(nms == "turn")] <- "turn_point"
names(ac_data) <- nms
ac_data$altitude_decision_point[is.na(ac_data$altitude_decision_point)] <- F
ac_data$turn_point[is.na(ac_data$turn_point)] <- F
setorder(ac_data, id, ts)
ac_data[, c("id_start_ts", "id_end_ts") := list(ts[1], ts[.N]), by = "id"]
uniq_id <- unique(ac_data$id)
chosen_id <- uniq_id[sample(1:length(uniq_id), 1)]
ac_data[, c("total_turns", "max_id_altitude") := list(sum(turn_point), max(altitude)), by = "id"]

jfk_data <- ac_data[ac_data$jfk_landing_flag, ]
jfk_data <- jfk_data[, .SD[-1], by = "id"]
plot(jfk_data$lon[jfk_data$altitude_decision_point], jfk_data$lat[jfk_data$altitude_decision_point])
plot(jfk_data$lon[jfk_data$turn_point], jfk_data$lat[jfk_data$turn_point])
plot(jfk_data$lon[jfk_data$altitude_decision_point & jfk_data$turn_point],
     jfk_data$lat[jfk_data$altitude_decision_point & jfk_data$turn_point])

no_turns <- ac_data[ac_data$total_turns == 0 & ac_data$max_id_altitude >= 20000]
ggplot(no_turns[no_turns$id == no_turns$id[1]], aes(x = lon, y = lat)) +
  geom_path(aes(color = ts), arrow = arrow(angle = 18, length = unit(0.065, "inches"), type = 'closed')) +
  scale_color_gradient(low = "red", high = "green")
# plot(no_turns$lon[no_turns$id == no_turns$id[1]], no_turns$lat[no_turns$id == no_turns$id[1]])
plot(no_turns$ts_readable[no_turns$id == no_turns$id[1]], no_turns$azimuth[no_turns$id == no_turns$id[1]])

dir.create("animation", showWarnings = F)
dir.create(paste0("animation/", chosen_id), showWarnings = F)
start_time <- ac_data$id_start_ts[ac_data$id == chosen_id][1]
end_time <- ac_data$id_end_ts[ac_data$id == chosen_id][1]
# ac_in_time <- sapply(uniq_id, function(id1) {
#   id1_df <- ac_data[ac_data$id == id1, ]
#   return(any(data.table::between(x = id1_df$ts, lower = start_time, upper = end_time)))
# })
# max_ac_df <- data.frame(id = uniq_id[ac_in_time])
max_ac_df <- ac_data[, .(in_time = any(data.table::between(ts, lower = start_time, upper = end_time))), by = "id"]
max_ac_df <- max_ac_df[max_ac_df$in_time, "id"]
max_ac_df <- merge(max_ac_df, ac_data, all = F, by = "id")
max_ac_df$colr <- 1
max_ac_df$colr[max_ac_df$id == chosen_id] <- 2
# max_ac_df$colr[max_ac_df$id == chosen_id] <- max_ac_df$colr[max_ac_df$id == chosen_id] +
#   cumsum(max_ac_df$turn[max_ac_df$id == chosen_id])
max_ac_df$colr <- factor(max_ac_df$colr)
uniq_ts <- unique(max_ac_df$ts)
uniq_ts <- sort(uniq_ts, decreasing = F)
uniq_ts <- uniq_ts[data.table::between(uniq_ts, lower = start_time - 60, upper = end_time + 60)]
i_time <- end_time

func <- function() {
  for(i_time in uniq_ts) {
    subset_df <- max_ac_df[ts <= i_time & i_time <= id_end_ts, ]
    p <- ggplot() +
      geom_path(data = subset_df, arrow = arrow(angle = 18, length = unit(0.065, "inches"), type = 'closed'),
                aes(x = lon, y = lat, group = id, color = colr, frame = ts)) +
      xlim(c(min(max_ac_df$lon), max(max_ac_df$lon))) +
      ylim(c(min(max_ac_df$lat), max(max_ac_df$lat))) +
      theme(legend.position = "none")
    print(p)
  }
}

saveHTML(func(), interval = 0.05, width = 580, height = 400)


# detect_collision <- function(ac_data, continue_motion = TRUE) {
#   trace_previous_path <- !(continue_motion)
#   if(trace_previous_path) {
#     
#   } else {
#     
#   }
# }