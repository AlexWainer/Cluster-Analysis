library(ggplot2)
library(GGally)
library(corrplot)
library(cluster)
library(stats)
library(parallel)
library(doParallel)
library(foreach)
library(factoextra)
library(dplyr)
library(gridExtra)

data <- read.csv("assignment data.txt", sep = "")

# Exploratory Data Analysis

data = data %>%
  na.omit()
data = data %>%
  rename(X = X624474 , Y= X837604)
summary(data) 
boxplot(data)


pair = ggpairs(data)
pair

distance_matrix <- dist(data, method = "euclidean")
distance_vector <- as.vector(distance_matrix)

ggplot(data.frame(Distance = distance_vector), aes(x = Distance)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs( x = "Distance", y = "Frequency") +
  theme_minimal()

boxplot(distance_matrix)
dist_df <- as.data.frame(as.matrix(distance_matrix))

#  mean distance of each observation to all others
mean_distances <- apply(dist_df, 1, mean)
# observations with the highest mean distances (outliers)
outlier_indices <- order(mean_distances, decreasing = TRUE)[1:10]
outliers <- data[outlier_indices, ]
outliers_data <- data.frame(
  Observation = c(460, 339, 2539, 4721, 441, 485, 2316, 500, 534, 4722),
  X = c(129640, 215687, 557874, 567067, 89604, 112396, 460464, 115401, 268889, 493270),
  Y = c(884633, 902015, 9597, 977215, 711268, 741755, 35412, 744137, 886754, 951924)
)


knitr::kable(outliers_data, caption = "Table of Identified Outliers")

# normalize data for clustering
data <- scale(data)
data <- as.data.frame(data)

# find optimal k for k-means
set.seed(2024)
k.max <- 20
sil <- rep(0, k.max)
for(i in 2:k.max){
  km.res <- kmeans(data, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  sil[i] <- mean(ss[, 3])
}

# find optimal k for k-medoids
avg_sil_clara <- c()
for (k in 2:20) {
  
  clara_res <- clara(data, k, samples = 10)
  sil_clara <- silhouette(clara_res$clustering, dist(data))
  avg_sil_clara[k] <- mean(sil_clara[, 3])
  
}

results <- data.frame(K = integer(), Sil_Width = numeric(), Method = character())

par(mfrow = c(1,2))

# average silhouette width for k-means
plot(1:k.max, sil, type = "b", pch = 19,
     frame = FALSE, xlab = "Number of clusters k", ylab = "Average silhouette score")
abline(v = which.max(sil), lty = 2)

# average silhouette width for k-medoid 
plot(1:20, avg_sil_clara, type = "b", pch = 19,
     frame = FALSE, xlab = "Number of clusters k", ylab = "Average silhouette score")
abline(v = which.max(avg_sil_clara), lty = 2)


# Parameters
k.max <- 20
num.initializations <- 30  
optimal_k <- numeric(num.initializations)  

for (j in 1:num.initializations) {
  set.seed(j)  
  sil <- numeric(k.max)  
  
  
  for (i in 2:k.max) {
    # k-means clustering
    km.res <- kmeans(data, centers = i, nstart = 1) 
    #  silhouette scores
    ss <- silhouette(km.res$cluster, dist(data))  
    sil[i] <- mean(ss[, 3])  #
  }
  
  optimal_k[j] <- which.max(sil)
}

# sensitivity analysis for K-means

optimal_k_clara <- numeric(num.initializations)  
for (j in 1:num.initializations) {
  set.seed(j)  
  avg_sil_clara <- numeric(k.max)  
  
  
  for (k in 2:k.max) {
    # P CLARA clustering
    clara_res <- clara(data, k, samples = 5)  
    sil_clara <- silhouette(clara_res$clustering, dist(data))  
    avg_sil_clara[k] <- mean(sil_clara[, 3]) 
  }
  
  optimal_k_clara[j] <- which.max(avg_sil_clara)
}

par(mfrow = c(1,2))
hist(optimal_k, breaks = seq(1.5, k.max + 0.5, by = 1), main = " ",
     xlab = "Optimal Number of Clusters K*", col = "lightblue")

hist(optimal_k_clara, breaks = seq(1.5, k.max + 0.5, by = 1), main = " ",
     xlab = "Optimal Number of Clusters K*", col = "coral")

# set-up parrallel processing
no_cores <- detectCores() - 1 

# Register the parallel back-end
cl <- makeCluster(no_cores)
registerDoParallel(cl)

k <- 15  
n_runs <- 500 

#k-means clustering
results <- foreach(i = 1:n_runs, .packages = 'stats') %dopar% {
  set.seed(i)
  km <- kmeans(data, centers = k, nstart = 1)
  return(km$tot.withinss) 
}

results_df <- data.frame(Initialization = 1:n_runs, TotalWCSS = unlist(results))
stopCluster(cl)

#Gap algorithm
gap_stat <- clusGap(data, FUN = kmeans, K.max = 20, B = 50)

plot(gap_stat, frame = FALSE, xlab = "Number of clusters k", main = " ")
abline(v = 17, lty = 2)

# Cluster Analysis
# pick the two optimal k for each algorithm
K1 <- 15
K2 <- 17
K3 <- 15
K4 <- 14

# Compute clusters
km1 <- kmeans(data, centers = K1, nstart = 25)
km2 <- kmeans(data, centers = K2, nstart = 25)
clara1 <- clara(data, k = K3)
clara2 <- clara(data, k = K4)

# Compute silhouette scores
sil_km1 <- silhouette(km1$cluster, dist(data))
sil_km2 <- silhouette(km2$cluster, dist(data))
sil_clara1 <- silhouette(clara1$clustering, dist(data))
sil_clara2 <- silhouette(clara2$clustering, dist(data))

# Plotting function for silhouette scores
plot_silhouette <- function(sil, main) {
  plot(sil, border = NA, main = main)
}

# Plot silhouette scores
par(mfrow = c(1, 2))  
plot_silhouette(sil_km1, " ")
plot_silhouette(sil_km2, " ")

par(mfrow = c(1, 2)) 
plot_silhouette(sil_clara1, " ")
plot_silhouette(sil_clara2, " ")

# Visualizing clusters
plot_k1 = fviz_cluster(list(data = data, cluster = km1$cluster), geom = "point", 
                       main = " ") + theme(legend.position = "none")

plot_k2 = fviz_cluster(list(data = data, cluster = km2$cluster), geom = "point", 
                       main = " ") + theme(legend.position = "none")


plot_k3 = fviz_cluster(list(data = data, cluster = clara1$clustering), geom = "point", main = " ") + theme(legend.position = "none")

plot_k4 = fviz_cluster(list(data = data, cluster = clara2$clustering), geom = "point", main = " ") + theme(legend.position = "none")


grid.arrange(plot_k1,plot_k2,plot_k3,plot_k4, ncol= 2, nrow = 2)

# choose best cluster
# visualize sulhouette score
data$cluster <- km1$cluster

sil_info <- silhouette(km1$cluster, dist(data))

sil_df <- data.frame(cluster = sil_info[, 1], neighbour = sil_info[,2],
                     width = sil_info[, 3], index = 1:nrow(data))

ggplot(sil_df, aes(x = index, y = width, fill = as.factor(cluster))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_hline(yintercept = mean(sil_df$width), linetype = "dashed", color = "red") +
  geom_point(data = sil_df[sil_df$index %in% outlier_indices, ], aes(x = index, y = width), color = "red", size = 3, show.legend = FALSE) +
  labs(title = "", x = "Data Index", y = "Silhouette Width") +
  theme_minimal() +
  theme(legend.position = "none")

# reassign observations with negative silhoutte scores
negative_sil <- sil_df[sil_df$width < 0,]

for (i in seq_along(negative_sil$index)) {
  point_index <- negative_sil$index[i]
  new_cluster <- negative_sil$neighbour[i]
  km1$cluster[point_index] <- new_cluster
  if ("cluster" %in% names(data)) {
    data$cluster[point_index] <- new_cluster
  }
}

# Recalculate the silhouette scores with updated clusters
new_sil_scores <- silhouette(km1$cluster, dist(data))
plot_silhouette(new_sil_scores, " ")
