library(plyr)
library(dplyr)
library(data.table)
# V6 is heading in degrees
# V2 seems to change for each aircraft during exit-entry
# V14 is 'from' airport
# V7 is altitude in ft
# V8 is ground speed in kts
# V15 is 'to' airport
# V3 looks like a proxy for aircraft
# V13 is timestamp
# V18 is instantaneous climb rate (jittery)
# V16_V19 looks like a good proxy for aircraft id: more relevant than V3 (aircraft)
# V16_V19 can refine V3
# V17 looks like 'aircraft at airport' flag

lower_lat <- 35
upper_lat <- 45
lower_lon <- -78
upper_lon <- -68
lat_range <- 0.4
lon_range <- 0.5
ts_diff_ground <- 1080
ts_diff_exit_enter <- 1080
ts_diff_expired <- 7200
airport_lat_half_range <- airport_lon_half_range <- 0.1
setwd("~/Desktop/Coding/Old/ATC_Analysis_2019_04_01/Refedined_Landing")


########################################################################
############## Reading ADS-B data and preprocessing ####################
########################################################################

landed_altitude <- 1000 # may not need this because of V17
v1_ground <- 50
surely_grounded_speed <- 10
ac_data <- fread("../../../ads-b_with_id.csv")
ac_data <- ac_data %>% rename(azimuth = V6, source = V14, destination = V15, altitude = V7,
                              ground_speed = V8, aircraft = V3, ts = V13, climb_rate = V18,
                              ground_flag = V17, lat = V4, lon = V5, aircraft_type = V11)
ac_data$altitude <- 100 * ceiling(ac_data$altitude/100)
ac_data <- ac_data[ac_data$V10 != "F-EST"]
# ac_data$ground_flag[ac_data$ground_speed < v1_ground & ac_data$altitude < landed_altitude] <- 1
setorder(ac_data, aircraft, ts)
ac_data <- ac_data[, .SD[1], by = list(aircraft, ts)]

ac_data$ts_readable <- as.POSIXct.numeric(as.numeric(ac_data$ts), origin="1970-01-01")
# ac_data$azimuth[ac_data$ground_speed == 0] <- NA
ac_data$quadrant <- ceiling(ac_data$azimuth/90)
ac_data <- ac_data[!grepl(ac_data$aircraft, pattern = "^FAA"), ]
ac_to_remove <- data.frame(aircraft = unique(ac_data$aircraft[ac_data$altitude >= 30000 & ac_data$ground_speed < 100]))


ac_data$V16_V19 <- paste0(ac_data$V16, "_", ac_data$V19)
ac_data[, c("last_lat", "last_lon", "last_azimuth", "last_altitude",
            "last_ts", "last_source", "last_destination", "last_V9",
            "last_ground_flag", "last_V16_V19", "last_quadrant",
            "last_ground_speed", "last_V2",
            "next_lat", "next_lon", "next_azimuth", "next_altitude",
            "next_ts", "next_source", "next_destination", "next_V9",
            "next_ground_flag", "next_V16_V19", "next_quadrant",
            "next_ground_speed", "next_V2",
            "uniq_source",
            "uniq_destination",
            "max_ac_alt") :=
                list(c(lat[1], lat[-.N]), c(lon[1], lon[-.N]), c(azimuth[1], azimuth[-.N]), c(altitude[1], altitude[-.N]),
                     c(ts[1], ts[-.N]), c(NA, source[-.N]), c(NA, destination[-.N]), c(NA, V9[-.N]),
                     c(ground_flag[1], ground_flag[-.N]), c(V16_V19[1], V16_V19[-.N]), c(quadrant[1], quadrant[-.N]),
                     c(ground_speed[1], ground_speed[-.N]), c(V2[1], V2[-.N]),
                     c(lat[-.1], NA), c(lon[-1], NA), c(azimuth[-.1], NA), c(altitude[-1], NA),
                     c(ts[-1], NA), c(source[-1], NA), c(destination[-1], NA), c(V9[-1], NA),
                     c(ground_flag[-1], NA), c(V16_V19[-1], NA), c(quadrant[-1], NA),
                     c(ground_speed[-1], NA), c(V2[-1], NA),
                     paste0(sort(unique(source)), collapse = " "),
                     paste0(sort(unique(destination)), collapse = " "),
                     max(altitude)), by = "aircraft"]
ac_data$last_ts_diff <- ac_data$ts - ac_data$last_ts
ac_data$next_ts_diff <- ac_data$next_ts - ac_data$ts
ac_data$destination <- toupper(ac_data$destination)
ac_data$est_climb_rate <- 60 * (ac_data$altitude - ac_data$last_altitude)/ac_data$last_ts_diff # ft/min
ac_data$climb_rate_ratio <- 1
condition <- ac_data$est_climb_rate != 0 & !is.na(ac_data$est_climb_rate) & ac_data$last_ts_diff < 1200
ac_data$climb_rate_ratio[condition] <- ac_data$climb_rate[condition]/ac_data$est_climb_rate[condition]
ac_data$quadrant_change <- ac_data$quadrant != ac_data$last_quadrant








########################################################################
################# Reading airports data and checking ###################
########################################################################

# This file seems to be more reliable
airports <- read.csv("../../../GlobalAirportDatabase.txt", header = F, stringsAsFactors = F)
airports <- airports %>% dplyr::rename(airport = V5, lat = V7, lon = V8)
airports$airport <- toupper(airports$airport)
airports <- airports[airports$airport != "", ]

# 511 of the 563 destinations can be matched

airports <- airports[data.table::between(airports$lat, lower = lower_lat, upper = upper_lat) &
                       data.table::between(airports$lon, lower = lower_lon, upper = upper_lon), ]
rownames(airports) <- airports$V1 <- NULL





########################################################################
############### Defining landing and entry into sector #################
########################################################################

ac_data$touchdown <- ac_data$last_ground_flag == 0 &
  ac_data$ground_flag == 1
ac_data[, "touchdown_id" := cumsum(touchdown), by = aircraft]
ac_data[, "max_touchdown" := max(touchdown_id), by = aircraft]
ac_data[, max_altitude_touchdown_id := max(altitude), by = list(aircraft, touchdown_id)]
ac_data$touchdown[ac_data$touchdown & (ac_data$max_altitude_touchdown_id < 0.1 * ac_data$max_ac_alt)] <- F
ac_data[, "touchdown_id" := cumsum(touchdown), by = aircraft]
ac_data[, "max_touchdown" := max(touchdown_id), by = aircraft]
ac_data[, max_altitude_touchdown_id := max(altitude), by = list(aircraft, touchdown_id)]

# Consecutive landings within 10 mins is not possible
# These can be considered as missed approaches, therefore only the final landing is considered
false_idx <- c()
for(ac in unique(ac_data$aircraft[ac_data$touchdown & ac_data$max_touchdown > 1])) {
  ac_df <- ac_data[ac_data$aircraft == ac & ac_data$touchdown, ]
  time_between_touchdowns <- as.numeric(difftime(ac_df$ts_readable[-1], ac_df$ts_readable[-nrow(ac_df)], units = "s"))
  if(any(time_between_touchdowns < 600)) {
    idx1 <- which(time_between_touchdowns < 600)
    ac_data_idx <- which(ac_data$aircraft == ac & ac_data$touchdown)
    false_idx <- c(false_idx, ac_data_idx[idx1])
  }
}
if(length(false_idx) > 0) {
  ac_data$touchdown[false_idx] <- F
}


########################################################################
################## Testing definition of touchdown #####################
########################################################################

identify_airport <- function(lat, lon, lat_half_range = airport_lat_half_range, lon_half_range = airport_lon_half_range) {
  df <- airports[data.table::between(airports$lat, lower = lat - lat_half_range,
                                     upper = lat + lat_half_range) &
                   data.table::between(airports$lon, lower = lon - lon_half_range,
                                       upper = lon + lon_half_range), ]
  if(nrow(df) > 1) {
    dists <- apply(df, 1, function(row) {
      row["lat"] <- as.numeric(row["lat"])
      row["lon"] <- as.numeric(row["lon"])
      return(distm(x = c(row["lon"], row["lat"]), y = c(lon, lat)))
    })
    df <- df[which.min(dists), ]
    df$dist <- min(dists)
  } else if(nrow(df) == 1) {
    df$dist <- 0
  } else {
    dists <- apply(airports, 1, function(row) {
      row["lat"] <- as.numeric(row["lat"])
      row["lon"] <- as.numeric(row["lon"])
      return(distm(x = c(lon, lat), y = c(row["lon"], row["lat"])))
    })
    df <- airports[which.min(dists), ]
    df$dist <- min(dists)
  }
  return(df[, c("airport", "dist")])
}


touchdown <- ac_data[ac_data$touchdown, ]
landed_airport <- list()
library(geosphere)
for(i in 1:nrow(touchdown)) {
  landed_airport[[i]] <- identify_airport(touchdown$lat[i], touchdown$lon[i])
}
mean(sapply(landed_airport, nrow))
condition <- sapply(landed_airport, function(row) row$dist) <= 20000
mean(condition) # landed location is within 20 km of airport
touchdown_idx <- which(ac_data$touchdown)
non_touchdown <- touchdown_idx[!condition]
ac_data$touchdown[non_touchdown] <- F
ac_data$airport <- ""
ac_data$airport[ac_data$touchdown] <- sapply(landed_airport, function(row) row$airport)[condition]

precision_recall <- F
if(precision_recall) {
  dests <- ac_data$last_destination[ac_data$touchdown & ac_data$last_destination != ""]
  print(mean(dests == ac_data$airport[ac_data$touchdown & ac_data$last_destination != ""]))
  # Accuracy of 'given' destination: 0.9584
  # Actually the precision is 0.9635 (from azimuth analysis)
  
  # JFK recall (assuming 'airport' is true value)
  tbl <- table(ac_data$destination[ac_data$touchdown & ac_data$airport == "JFK"])
  print(sort(tbl, decreasing = T)/sum(tbl))
  # JFK recall is 0.8927, "" is the main issue
}


lat_lon_boundary_flag <- function(lt, ln, last_lt, last_ln,
                                  lower_lt = lower_lat, lower_ln = lower_lon, upper_lt = upper_lat,
                                  upper_ln = upper_lon, lt_range = lat_range, ln_range = lon_range) {
  ans <- (ln < ((lower_ln + ln_range) | last_ln < (lower_ln + ln_range))) |
    ((ln > (upper_ln - ln_range) | last_ln > (upper_ln - ln_range))) |
    ((lt < (lower_lt + lt_range) | last_ln < (lower_lt + lt_range))) |
    ((lt > (upper_lt - lt_range) | last_ln > (upper_lt - lt_range)))
  return(ans)
}


ac_data$landed <- ac_data$touchdown
ac_data$entered <- (ac_data$V2 != ac_data$last_V2) |
  (lat_lon_boundary_flag(ac_data$lat, ac_data$lon, ac_data$last_lat, ac_data$last_lon) &
          ac_data$last_ts_diff > 1080)
# ac_data$entered <-
#   (lat_lon_boundary_flag(ac_data$lat, ac_data$lon, ac_data$last_lat, ac_data$last_lon) &
#      ac_data$last_ts_diff > 1080)

ac_data$entered[is.na(ac_data$entered)] <- F
ac_data$expired <- (ac_data$last_ts_diff > ts_diff_expired)
ac_data$landed_or_entered_or_expired <- ac_data$landed | ac_data$entered | ac_data$expired
ac_data[, "num_id" := cumsum(landed_or_entered_or_expired), by = "aircraft"]
table(ac_data$num_id)
ac_data$id <- paste0(ac_data$aircraft, "_", ac_data$num_id)






########################################################################
############### Plotting trajectory of resulting IDs ###################
########################################################################

plotting <- F

if(plotting) {
  dir.create("jfk_landing", showWarnings = F)
  setwd("jfk_landing")
  visualize_map <- function(ac_df, color_var = "id", sampl = T,
                            min_lon = NULL, max_lon = NULL, min_lat = NULL, max_lat = NULL) {
    ac_df$num_id <- as.integer(as.factor(ac_df$id))
    ac_df <- split(ac_df, ac_df$num_id)
    if(sampl)
      ac_df <- ac_df[sample(x = 1:length(ac_df), size = n_aircraft, replace = F)]
    ac_df <- do.call(rbind, ac_df)
    ac_df <- ac_df[order(ac_df$ts), ]
    p <- ggplot() +
      geom_sf(data = world) +
      coord_sf(xlim = c(min_lon, max_lon), ylim = c(min_lat, max_lat), expand = F) +
      geom_path(data = ac_df, arrow = arrow(type = "closed", angle = 18,
                                            length = unit(0.1, "inches")),
                aes(x = lon, y = lat, frame = ts, color = id)) +
      xlim(c(min_lon, max_lon)) +
      ylim(c(min_lat, max_lat))
    return(p)
  }
  
  ac_data[, c("last_entered", "next_entered",
              "last_airport", "next_airport",
              "last_touchdown", "next_touchdown") :=
            list(c(F, entered[-.N]), c(entered[-1], F),
                 c(airport[1], airport[-.N]), c(airport[-1], NA),
                 c(F, touchdown[-.N]), c(touchdown[-1], F)), by = "aircraft"]
  jfk_data <- data.frame(id = ac_data$id[ac_data$next_airport == "JFK" & ac_data$next_touchdown
                                         & ac_data$source != "JFK"], jfk_landing_flag = T)
  jfk_data <- merge(jfk_data, ac_data, all.x = T, all.y = T)
  jfk_data$jfk_landing_flag[is.na(jfk_data$jfk_landing_flag)] <- F
  
  min_lon <- min(jfk_data$lon)
  max_lon <- max(jfk_data$lon)
  min_lat <- min(jfk_data$lat)
  max_lat <- max(jfk_data$lat)
  uniq_id <- unique(jfk_data$aircraft)
  
  
  library(parallel)
  library(ggplot2)
  library(rnaturalearth)
  cores <- detectCores() - 1
  cl <- parallel::makeCluster(cores)
  split_num <- ceiling(length(uniq_id)/cores)
  num_id <- ceiling((1:length(uniq_id))/split_num)
  id_splits <- split(uniq_id, num_id)
  jfk_data1 <- jfk_data[, c("id", "ts", "lon", "lat", "aircraft")]
  world <- ne_countries(scale = "medium", returnclass = "sf")
  clusterExport(cl, list("jfk_data1", "coord_sf", "geom_sf", "world", "visualize_map",
                         "min_lon", "max_lon", "min_lat", "max_lat", "png", "paste0",
                         "print", "dev.off", "ggplot", "geom_path", "arrow", "aes",
                         "unit", "xlim", "ylim"))
  
  plot_ids <- function(uniq_id) {
    setwd("~/Desktop/Coding/Old/ATC_Analysis_2019_04_01/Refedined_Landing/jfk_landing/")
    for(id1 in uniq_id) {
      ac_df <- jfk_data1[jfk_data1$aircraft == id1, ]
      p <- visualize_map(ac_df, sampl = F, min_lon = min_lon,
                         max_lon = max_lon, min_lat = min_lat,
                         max_lat = max_lat)
      png(paste0(id1, ".png"), width = 1366, height = 768)
      print(p)
      dev.off()
    }
    setwd("../")
  }
  
  parLapply(cl, id_splits, plot_ids)
  stopCluster(cl)
  
  setwd("../")
}


########################################################################
################## Saving final prepropcessed data #####################
########################################################################

saveRDS(ac_data[, c("aircraft", "id", "ts", "ts_readable", "lon", "lat", "altitude", "ground_speed", "azimuth", "source", "destination")], "ac_data_processed.Rds")
setorder(ac_data, aircraft, ts)
ac_data[, c("last_touchdown", "next_touchdown", "last_entered", "next_entered", "last_airport", "next_airport") :=
          list(c(F, touchdown[-.N]), c(touchdown[-1], F), c(F, entered[-.N]), c(entered[-1], F), c("", airport[-.N]), c(airport[-1], "")), by = "aircraft"]
jfk_data <- data.frame(id = ac_data$id[ac_data$next_airport == "JFK" & ac_data$next_touchdown
                                       & ac_data$source != "JFK"], jfk_landing_flag = T)
jfk_data <- merge(jfk_data, ac_data, all.x = T, all.y = T)
setDT(jfk_data)
# jfk_data[, "max_id_alt" := max(altitude), by = "id"]
setorder(jfk_data, id, ts)
jfk_data[, "jfk_landing_flag" := all(jfk_landing_flag & destination=="JFK"), by = 'id']
conversion_x <- 9.26/(cos(jfk_data$lat * pi/180) * 111.699)
jfk_data[, c("x", "y", "z") := list(as.integer((lon + 78)/conversion_x),
                                as.integer((lat - 35)/0.0829),
                                as.integer(altitude/500)), ]
jfk_data <- jfk_data[jfk_data$x < 200, ]
df <- jfk_data[, .(max_altitude = max(altitude)), by = id]
df <- df[df$max_altitude <= 50000, ]
df$max_altitude <- NULL
jfk_data <- merge(jfk_data, df)
jfk_data[, c("x", "y") := list(ifelse(x >= 0, x, 0),
                           ifelse(y >= 0, y, 0)), ]

write.csv(jfk_data[, c("id", "aircraft", "lat", "lon", "ts", "altitude", "climb_rate",
                       "azimuth", "ground_speed", "jfk_landing_flag", "source",
                       "destination", "ground_flag")],
          "../../../samples_with_jfk_landing_flag.csv", row.names = F)
write.csv(jfk_data[jfk_data$jfk_landing_flag, ],
          "~/Desktop/Coding/IBM/landing_samples.csv", quote = F, row.names = F)
saveRDS(ac_data, "full_ac_data.Rds")
# saveRDS(jfk_data[jfk_data$max_id_alt >= 20000, ], "high_ac_data.Rds")
saveRDS(jfk_data, "full_jfk_data.Rds")



# ac_data[, c("last_last_lon", "last_last_lat") :=
#           list(c(last_lon[1], last_lon[-.N]), c(last_lat[1], last_lat[-.N])),
#         by = "aircraft"]

# (
#   lat_lon_boundary_flag(ac_data$lat, ac_data$lon, ac_data$last_lat, ac_data$last_lon) &
#     (ac_data$quadrant_change |
#        ((ac_data$lon >= (upper_lon - lon_range)) & (ac_data$next_lon <= ac_data$lon)) |
#        ((ac_data$lon <= (lower_lon + lon_range)) & (ac_data$next_lon >= ac_data$lon)) |
#        ((ac_data$lat <= (lower_lat + lat_range)) & (ac_data$next_lat >= ac_data$lat)) |
#        ((ac_data$lat >= (upper_lat - lat_range)) & (ac_data$next_lat <= ac_data$lat))) &
#     ac_data$last_ts_diff > 1080
# )

# ac_data$quadrant_change &
# (((ac_data$lon < (lower_lon + lon_range) | ac_data$last_lon < (lower_lon + lon_range)) &
#     (ac_data$next_lon >= ac_data$lon | ac_data$quadrant_change)) |
#    ((ac_data$lon > (upper_lon - lon_range) | ac_data$last_lon > (upper_lon - lon_range)) &
#       (ac_data$next_lon <= ac_data$lon | ac_data$quadrant_change)) |
#    ((ac_data$lat < (lower_lat + lat_range) | ac_data$last_lon < (lower_lat + lat_range)) &
#       (ac_data$next_lat >= ac_data$lat | ac_data$quadrant_change)) |
#    ((ac_data$lat > (upper_lat - lat_range) | ac_data$last_lon > (upper_lat - lat_range)) &
#       (ac_data$next_lat <= ac_data$lat | ac_data$quadrant_change))) & ac_data$last_ts_diff > 1080

# ac_data$hr <- hour(ac_data$ts_readable)
# for(hr1 in unique(ac_data$hr)) {
#   hour_data <- ac_data[ac_data$hr == hr1, ]
#   saveRDS(hour_data, paste0("hour", hr1, "_data.Rds"))
# }
# 
# 
# # Testing precision and recall of 'destination'
# jfk_ac <- unique(ac_data$aircraft[ac_data$destination == "JFK"])
# df <- c()
# for(ac in jfk_ac) {
#   ac_df <- ac_data[ac_data$aircraft == ac, ]
#   landed_df <- ac_df[ac_df$touchdown, c("lon", "lat", "destination")]
#   df <- rbind(df, landed_df)
# }


# Analyzing whether all destinations are in airports data set
# uniq_dest <- unique(ac_data$destination)
# length(uniq_dest)
# sum(sapply(uniq_dest, function(dest) dest %in% airports$airport))
# unclear_dest <- uniq_dest[sapply(uniq_dest, function(dest) !(dest %in% airports$airport))]

# exits <- ac_data[ac_data$quadrant_change & ac_data$last_ts_diff > 1200, ]
# plot(exits$lon, exits$lat, xlim = range(ac_data$lon), ylim = range(ac_data$lat))
# 
# 
# 
# exits1 <- ac_data[(ac_data$lon < (-77.8) | ac_data$lon > -68.2 | ac_data$lat < 35.2 | ac_data$lat > 44.8 |
#                      ac_data$last_lon < (-77.8) | ac_data$last_lon > -68.2 | ac_data$last_lat < 35.2 | ac_data$last_lat > 44.8) & ac_data$last_ts_diff > 1200, ]
# plot(exits1$lon, exits1$lat, xlim = range(ac_data$lon), ylim = range(ac_data$lat))
# plot(exits1$last_lon, exits1$last_lat, xlim = range(ac_data$lon), ylim = range(ac_data$lat))




# at_airport <- ac_data$near_destination & (ac_data$ground_flag == 1)
# idx <- which(ac_data$destination == "" & at_airport)
# ac_data$destination[idx] <- ac_data$destination[idx - 1] # Backfilling destination
# ac_data$aircraft[at_airport] <- ac_data$destination[at_airport]

# table(ac_data$touchdown_id)
# 


# # df_3C4AD7 <- ac_data[ac_data$aircraft == "3C4AD7", ]
# A03F84_df <- ac_data[ac_data$aircraft == "A03F84", ]
# A0480B
# A05940
# A05BDE
# A075DF
# A07BEF
# A08317
# A263F9
# # A2768C_df <- ac_data[ac_data$aircraft == "A2768C", ]
# # A27A43 ?
# A2E813
# A3904C
# A4034F
# A4077D
# A50CB7
# A517DC
# # A52587 ?
# A52CC8
# A657AD
# A65B64
# A675C0
# A79EA0
# A828D5
# A82C8C
# A857C2
# # A88CBF
# # A89F52
# # A8A309
# A8F9A3
# # A91986 ?
# AB8917
# AC2A33
# # AC6284
# # AC96BD ?
# # ACC5DF
# ACFE09
# C013FB
# C029DC

# AA0343_data <- ac_data[ac_data$aircraft == "AA0343", ]

# ac_data %>% group_by(aircraft, touchdown_tot) %>% summarise()

# df <- ac_data[ac_data$last_destination == ac_data$next_source &
#                 +     ac_data$last_ground_flag == 0 & ac_data$ground_flag == 1 & ac_data$altitude < landed_altitude, ]
# df[1,]

# identify_airport <- function(ground_ac_data, airports, lat_half_range = 0.1, lon_half_range = 0.1) {
#   identified <- c()
#   for(i in 1:nrow(ground_ac_data)) {
#     print(i)
#     identified <- c(identified, nrow(
#       airports[data.table::between(airports$lat, lower = ground_ac_data$lat.x[i] - lat_half_range,
#                                    upper = ground_ac_data$lat.x[i] + lat_half_range) &
#                  data.table::between(airports$lon, lower = ground_ac_data$lon.x[i] - lon_half_range,
#                                      upper = ground_ac_data$lon.x[i] + lon_half_range), ]))
#   }
# }

# setorder(ac_data, aircraft, ts)
# ac_data$near_destination <- abs(ac_data$lat.x - ac_data$lat.y) < 0.4 &
#         abs(ac_data$lon.x - ac_data$lon.y) < 0.4
# ac_data$near_destination[is.na(ac_data$near_destination)] <- F
# condition <- ac_data$near_destination & (ac_data$ground_flag == 1) &
#   (ac_data$last_ground_flag == 0)
# condition[is.na(condition)] <- F
# png("landed.png", width = 1366, height = 768)
# plot(x = ac_data$lon.x[condition],
#      y = ac_data$lat.x[condition])
# # points(airports$lon, airports$lat, col = "red")
# points(ac_data$lon.y[condition],
#        ac_data$lat.y[condition], col = "blue")
# dev.off()
# 
# plot(x = ac_data$last_lon[ac_data$last_ts_diff > ts_diff_ground],
#      y = ac_data$last_lat[ac_data$last_ts_diff > ts_diff_ground])


# ac_data$entered <- ((ac_data$lon < (-77.8) | ac_data$last_lon < (-77.8)) |
#    (ac_data$lon > (-68.2) | ac_data$last_lon > (-68.2)) |
#    (ac_data$lat < 35.2 | ac_data$last_lon < 35.2) |
#    (ac_data$lat > 44.8 | ac_data$last_lon > 44.8)) &
# (((ac_data$last_last_lon >= ac_data$last_lon) & ac_data$last_lon < (-77.8)) |
#    ((ac_data$last_last_lon <= ac_data$last_lon) & ac_data$last_lon > (-68.2)) |
#    ((ac_data$last_last_lat >= ac_data$last_lat) & ac_data$last_lat < 35.2) |
#    ((ac_data$last_last_lat <= ac_data$last_lat) & ac_data$last_lat > 44.8)) |
# (((ac_data$next_lon >= ac_data$lon) & ac_data$lon < (-77.8)) |
#    ((ac_data$next_lon <= ac_data$lon) & ac_data$lon > (-68.2)) |
#    ((ac_data$next_lat >= ac_data$lat) & ac_data$lat < 35.2) |
#    ((ac_data$next_lat <= ac_data$lat) & ac_data$lat > 44.8)) & ac_data$last_ts_diff > 1080



# which(ac_data$last_ground_flag == 0 & ac_data$ground_flag == 1 & ac_data$next_ground_flag == 1 & ac_data$next_ts_diff > ts_diff_ground & ac_data$destination == ac_data$next_destination & ac_data$destination != "")
# df <- ac_data[, ]


# ac_data <- merge(ac_data, airports, all.x = T, by.x = "last_destination", by.y = "airport")
# ground_ac_data <- ac_data[ac_data$last_ground_flag == 0 & ac_data$ground_flag == 1 &
#                             ac_data$altitude < landed_altitude, ]
# source_diff_data <- ac_data[ac_data$source != ac_data$last_source, ]

# airport_near_lat_lon <- function(lat, lon, lat_half_range, lon_half_range) {
#   nrow(airports[data.table::between(airports$lat, lower = lat - lat_half_range, upper = lat + lat_half_range) &
#                   data.table::between(airports$lon, lower = lon - lon_half_range, upper = lon + lon_half_range), ]) > 0
# }
# 
# near_airport <- function(lat, lon, lat_half_range = 0.1, lon_half_range = 0.1) {
#   sapply(1:length(lat), function(i) airport_near_lat_lon(lat[i], lon[i], lat_half_range, lon_half_range))
# }

# ac_data$touchdown <- ac_data$last_ground_flag == 0 &
#   ac_data$ground_flag == 1# &
# ac_data$altitude < landed_altitude# &
# near_airport(ac_data$lat, ac_data$lon)# &
#ac_data$last_destination == ac_data$next_source &
# ac_data$max_ac_alt >= 5000 &
# (ac_data$uniq_source != "" | ac_data$uniq_destination != "") &
# ac_data$V9 == ac_data$last_V9 & ac_data$V9 == ac_data$next_V9



# hist(ac_data$next_ts_diff[ac_data$landed & ac_data$next_ts_diff > 100], breaks = 2000)


# heading0 <- ac_data[ac_data$quadrant == 0, ]


# jfk_data <- ac_data[ac_data$destination == "JFK", ]


# p <- ggplot(data = ac_df,
#             aes_string(x = "lon", y = "lat", frame = "ts", color = color_var)) +
#   geom_path(arrow = arrow(type = "closed", angle = 18,
#                           length = unit(0.1, "inches")))






# Other definitions of landing that failed

# ac_data$V16_V19_change <-  (ac_data$V16_V19 != ac_data$last_V16_V19) | (ac_data$V16_V19 != ac_data$next_V16_V19)
# plot(x = ac_data$lon[ac_data$V16_V19_change], y = ac_data$lat[ac_data$V16_V19_change])
# points(airports$lon, airports$lat, col = "red")



# plot(x = ac_data$lon[ac_data$destination == "" & ac_data$last_destination != ""],
#      y = ac_data$lat[ac_data$destination == "" & ac_data$last_destination != ""])
# points(airports$lon, airports$lat, col = "red")




# ac_data$ground_flag_change <- (ac_data$ground_flag != ac_data$last_ground_flag)
# ac_data$destination_change <- (ac_data$destination != ac_data$last_destination)
# plot(x = ac_data$lon[(ac_data$ground_flag == 1) & ac_data$destination_change],
#      y = ac_data$lat[(ac_data$ground_flag == 1) & ac_data$destination_change])
# points(airports$lon, airports$lat, col = "red")


# points(airports$V16, airports$destination, col = "red")
# plot(x = ac_data$lon[(ac_data$ground_flag == 1) & ac_data$V16_V19_change],
#      y = ac_data$lat[(ac_data$ground_flag == 1) & ac_data$V16_V19_change])
# points(airports$V16, airports$destination, col = "red")
# plot(x = ac_data$lon[(ac_data$destination == "")],
#      y = ac_data$lat[(ac_data$destination == "")])
# points(airports$V16, airports$destination, col = "red")
# plot(x = ac_data$lon[ac_data$ground_flag_change & ac_data$V16_V19_change],
#      y = ac_data$lat[ac_data$ground_flag_change & ac_data$V16_V19_change])
# points(airports$V16, airports$destination, col = "red")
# ac_data$ground_flag_01 <- ((ac_data$last_ground_flag == 0) & (ac_data$ground_flag == 1))
# ac_data$ground_flag_011 <- (ac_data$ground_flag_01 & (ac_data$next_ground_flag == 1))
# plot(x = ac_data$lon[ac_data$ground_flag_011 & ac_data$V16_V19_change],
#      y = ac_data$lat[ac_data$ground_flag_011 & ac_data$V16_V19_change])
# points(airports$V16, airports$destination, col = "red")
# plot(x = ac_data$lon[ac_data$ground_flag_01 & ac_data$V16_V19_change],
#      y = ac_data$lat[ac_data$ground_flag_01 & ac_data$V16_V19_change])
# points(airports$V16, airports$destination, col = "red")
# 
# ac_data$ground_flag_10 <- ((ac_data$last_ground_flag == 1) & (ac_data$ground_flag == 0))
# ac_data$ground_flag_100 <- (ac_data$ground_flag_10 & (ac_data$next_ground_flag == 0))
# 
# plot(x = ac_data$lon[which(ac_data$ground_flag_100) - 1], y = ac_data$lat[which(ac_data$ground_flag_100) - 1])
# points(airports$V16, airports$destination, col = "red")
# 
# plot(x = ac_data$lon[ac_data$ground_flag_01 & ac_data$next_ts_diff >= ts_diff_ground & ac_data$next_ground_flag == 1],
#      y = ac_data$lat[ac_data$ground_flag_01 & ac_data$next_ts_diff >= ts_diff_ground & ac_data$next_ground_flag == 1])
# 
# jfk_data <- ac_data[ac_data$destination == "JFK", ]
# ac_data[, "V17_cumsum" := cumsum(V17_change), by = "V3"]

# Should ideally pick up exiting and entering aircraft only: source changed and altitude > landed
# idxs <- which((ac_data$last_V14 != ac_data$V14) & ac_data$V7 > landed_altitude)
# idxs <- unlist(lapply(idxs, function(idx) c(idx - 1, idx)))
# diffs <- ac_data[idxs, ]
# diffs[1:2, ] # Close to sector boundary, but previous altitude < 1000 ft. Examine what's special at 38.9x, -77.4x
# diffs[3:4, ] # Exit and enter
# diffs[5:6, ] # Exit and enter
# diffs[7:8, ] # Exit and enter

# Among the entering and exiting aircraft: rows with altitude < landed and V17 (on land?) = 1
# idxs <- which(diffs$V7 < landed_altitude & diffs$V17 == 1)
# diffs1 <- diffs[idxs, ]
# diffs1$mid_V5 <- (diffs1$V5 + diffs1$next_V5)/2
# diffs1$mid_V4 <- (diffs1$V4 + diffs1$next_V4)/2
# library(RColorBrewer)
# cols = brewer.pal(4, "Blues")
# pal = colorRampPalette(c("blue", "red"))
# pal = colorRampPalette(cols)
# diffs1$order = findInterval(diffs1$V7, sort(diffs1$V7))
# ggplot(data = diffs1) +
#   geom_point(aes(x = mid_V5, y = mid_V4)) +
#   geom_segment(aes(x = V5, y = V5, xend = next_V5, yend = next_V4)) +
#   xlim(range(ac_data$V5)) +
#   ylim(range(ac_data$V4)) +
#   theme(legend.position = "none")
# 
# plot(x = diffs1$V5, y = diffs1$V4, xlim = range(ac_data$V5), ylim = range(ac_data$V4), col = pal(nrow(diffs1))[diffs1$order])
# hist(diffs1$V7)
# legend("topright", col=pal(2), pch=19,
#        legend=c(round(range(diffs1$V7), 1)))
# 
# idxs <- which(diffs1$V7 < landed_altitude & diffs1$next_V7 > 2000)
# idxs <- unlist(lapply(idxs, function(idx) c(idx, idx + 1)))
# diffs2 <- diffs1[idxs, ]
# diffs2[1:2,]
# hist(diffs2$V7[seq(2, nrow(diffs2), 2)], breaks = 100)
# hist(diffs2$V7[seq(1, nrow(diffs2), 2)], breaks = 100)
# mean(diffs2$V17[seq(2, nrow(diffs2), 2)])
# mean(diffs2$V17[seq(1, nrow(diffs2), 2)])
# 
# 
# idxs <- which((ac_data$last_V16_V19 != ac_data$V16_V19) & (ac_data$V14 == ac_data$last_V14))
# idxs <- unlist(lapply(idxs, function(idx) (idx - 2):(idx + 2)))
# diffs <- ac_data[idxs, ]
# diffs[1:5, ]
# 
# 
# 
# idxs <- which(ac_data$V17 == 1 & ac_data$V6 > 300)
# idxs <- unlist(lapply(idxs, function(idx) c(idx-1, idx, idx+1)))
# diffs <- ac_data[idxs, ]
# diffs[1:3, ]
# diffs[4:6, ]
# diffs[7:9, ]
# 
# 
# # Aircraft that flew at 60000 ft
# ids <- unique(ac_data$V3[ac_data$V7 == 60000])
# high_ac <- ac_data[sapply(ac_data$V3, function(id) id %in% ids), ]
# unique(high_ac$V14)
# hist(high_ac$V6)
# 
# non_high_ac <- ac_data[sapply(ac_data$V3, function(id) !(id %in% ids)), ]
# max(non_high_ac$V7)
