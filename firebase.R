library(httr)
library(ggplot2)

download <- function(projectURL) {
  urlPath = paste0(projectURL, ".json")
  data = httr::GET(urlPath)
  return(jsonlite::fromJSON(httr::content(data, "text")))
}

data = download('https://neurostride-80ede-default-rtdb.firebaseio.com/')
df <- do.call(rbind, lapply(data, as.data.frame))
rownames(df) <- seq_len(nrow(df)) - 1

df_pos <- data.frame(x = 0:(length(df$roll)-1),  
                          
                          y = c(df$roll, df$pitch, df$yaw),
                          
                          group = c(rep("roll", nrow(df)),
                                    rep("pitch", nrow(df)),
                                    rep("yaw", nrow(df)))
)

df_accel <- data.frame(x = 0:(length(df$roll)-1),  
                          
                          y = c(df$accel_x, df$accel_y, df$accel_z),
                          
                          group = c(rep("accel_x", nrow(df)),
                                    rep("accel_y", nrow(df)),
                                    rep("accel_z", nrow(df)))
)

df_gyro <- data.frame(x = 0:(length(df$roll)-1),  
                          
                          y = c(df$gyro_x, df$gyro_y, df$gyro_z),
                          
                          group = c(rep("gyro_x", nrow(df)),
                                    rep("gyro_y", nrow(df)),
                                    rep("gyro_z", nrow(df)))
)

df_mag <- data.frame(x = 0:(length(df$roll)-1),  
                          
                          y = c(df$mag_x, df$mag_y, df$mag_z),
                          
                          group = c(rep("mag_x", nrow(df)),
                                    rep("mag_y", nrow(df)),
                                    rep("mag_z", nrow(df)))
)

ggplot(df_pos, aes(x, y, col = group)) + 
  geom_line() + facet_grid(group ~ ., scales = "free_y")

ggplot(df_accel, aes(x, y, col = group)) + 
  geom_line() + facet_grid(group ~ ., scales = "free_y")

ggplot(df_gyro, aes(x, y, col = group)) + 
  geom_line() + geom_hline(yintercept=0, linetype="dashed", color="red") + 
  facet_grid(group ~ ., scales = "free_y")

ggplot(df_mag, aes(x, y, col = group)) + 
  geom_line() + facet_grid(group ~ ., scales = "free_y")

# df_reshaped <- data.frame(x = 0:(length(df$roll)-1),  
#                           
#                           y = c(df$roll, df$pitch, df$yaw, df$accel_x,
#                                 df$accel_y, df$accel_z, df$gyro_x, df$gyro_y,
#                                 df$gyro_z, df$mag_x, df$mag_y, df$mag_z),
#                           
#                           group = c(rep("roll", nrow(df)),
#                                     rep("pitch", nrow(df)),
#                                     rep("yaw", nrow(df)),
#                                     rep("accel_x", nrow(df)),
#                                     rep("accel_y", nrow(df)),
#                                     rep("accel_z", nrow(df)),
#                                     rep("gyro_x", nrow(df)),
#                                     rep("gyro_y", nrow(df)),
#                                     rep("gyro_z", nrow(df)),
#                                     rep("mag_x", nrow(df)),
#                                     rep("mag_y", nrow(df)),
#                                     rep("mag_z", nrow(df)))
# )

# ggplot(df_reshaped, aes(x, y, col = group)) + 
#         geom_line() + facet_grid(group ~ ., scales = "free_y")