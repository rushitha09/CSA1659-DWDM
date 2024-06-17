sales_prices <- c(5, 10, 11, 13, 15, 35, 50, 55, 72, 92, 204, 215)
n_bins <- 3
equal_frequency_partitioning <- function(prices, n_bins) {
  bin_size <- length(prices) / n_bins
  bins <- split(prices, ceiling(seq_along(prices) / bin_size))
  return(bins)
}
equal_width_partitioning <- function(prices, n_bins) {
  range <- max(prices) - min(prices)
  bin_width <- range / n_bins
  bin_edges <- seq(min(prices), max(prices), length.out = n_bins + 1)
  bin_indices <- cut(prices, breaks = bin_edges, include.lowest = TRUE, labels = FALSE)
  bins <- split(prices, bin_indices)
  return(bins)
}
clustering_partitioning <- function(prices, n_clusters) {
  set.seed(123) 
  kmeans_result <- kmeans(prices, centers = n_clusters)
  bins <- split(prices, kmeans_result$cluster)
  return(bins)
}
equi_depth_bins <- equal_frequency_partitioning(sales_prices, n_bins)
equi_width_bins <- equal_width_partitioning(sales_prices, n_bins)
clustering_bins <- clustering_partitioning(sales_prices, n_bins)
print("Equal-Frequency (Equi-Depth) Partitioning:")
print(equi_depth_bins)
print("Equal-Width Partitioning:")
print(equi_width_bins)
print("Clustering Partitioning:")
print(clustering_bins)