library(ggplot2)
library(dplyr)
PATH <- "https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/Computers.csv"
df <- read.csv(PATH) %>% select(-c(X, cd, multi, premium))
glimpse(df)

summary(df)

rescale_df <- df %>% mutate(price_scal = scale(price),
         hd_scal = scale(hd),
         ram_scal = scale(ram),
         screen_scal = scale(screen),
         ads_scal = scale(ads),
         trend_scal = scale(trend)) %>% select(-c(price, speed, hd, ram, screen, ads, trend))


kmeans(df, 3)

install.packages("animation")
set.seed(2345)
library(animation)
kmeans.ani(rescale_df[2:3], 3)


pc_cluster <-kmeans(rescale_df, 5)
kmean_withinss <- function(k) {
  cluster <- kmeans(rescale_df, k)
  return (cluster$tot.withinss)
}
kmean_withinss(2)

# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)

# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))


pc_cluster_2 <-kmeans(rescale_df, 7)

pc_cluster_2$cluster
pc_cluster_2$centers
pc_cluster_2$size


pc_cluster_2$size


center <-pc_cluster_2$centers
center
