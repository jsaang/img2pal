#' Image to hex-code
#'
#' This function is ~~~~ image to hex-code ~~
#'
img2hex <- function(image, k=4){

  image_df <- image %>%
    as.data.frame(wide = 'c') %>%
    tbl_df()

  # k-means clustering
  image_cluster <- image_df %>%
    select(-x, -y) %>%
    kmeans(k)

  cluster_colors <- image_cluster$centers %>%
    tbl_df() %>%
    mutate(label = as.character(1:nrow(image_cluster$centers)))

  # as data frame
  cluster_colors_df <- as.data.frame(cluster_colors[, 1:3])

  # sorting
  cluster_colors_df_order <- cluster_colors_df[order(cluster_colors_df[ ,1]), ]

  # cluster r, g, b
  r <- cluster_colors_df_order[, 1] * 255
  g <- cluster_colors_df_order[, 2] * 255
  b <- cluster_colors_df_order[, 3] * 255

  # rgb to hex
  rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
  hex <- rgb2hex(r, g, b)

  return(hex)

}
