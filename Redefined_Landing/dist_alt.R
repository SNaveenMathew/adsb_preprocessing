library(data.table)

ac_data <- fread("../../../landing_samples.csv")
if(!("distances.Rds" %in% list.files())) {
  dists <- apply(ac_data, 1, function(row)
    geosphere::distm(c(row["lon"], row["lat"]), c(-73.7781, 40.6413)))
  saveRDS(dists, "distances.Rds")
} else {
  dists <- readRDS("distances.Rds")
}

diff_ac <- unique(ac_data$id[(dists < 4000 & ac_data$altitude > 5000) |
                               (dists > 2000 & ac_data$altitude < 1000)])
ac_data$distance <- dists
ac_data <- ac_data[sapply(ac_data$id, function(id1) !(id1 %in% diff_ac)), ]
png("dist_alt.png")
plot(ac_data$distance, ac_data$altitude, xlim = c(0, 250000), col = "grey",
     xlab = "Distance from JFK airport", ylab = "Altitude")
lines(ac_data$distance[ac_data$id == "0D0418_3"],
      ac_data$altitude[ac_data$id == "0D0418_3"], col = "dark green")
lines(ac_data$distance[ac_data$id == "A22702_14"],
      ac_data$altitude[ac_data$id == "A22702_14"], col = "red")
dev.off()
