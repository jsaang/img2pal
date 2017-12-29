img_quantize <- function(img, level){

  img_df <- img %>%
    as.data.frame(wide = 'c') %>%
    tbl_df()

  img_cluster <- img_df %>%
    select(-x, -y) %>%
    kmeans(level, algorithm = 'Lloyd', iter.max = 100)

  cluster_colors <- img_cluster$centers %>%
    tbl_df() %>%
    mutate(label = as.character(1:nrow(img_cluster$centers)))

  img_df_cluster <- img_df %>%
    mutate(label = as.character(img_cluster$cluster)) %>%
    left_join(cluster_colors, by = 'label')

  img_PCA <- img_df_cluster %>%
    select(3:5) %>%
    prcomp(center = TRUE, scale = TRUE)

  img_df_cluster <- img_df_cluster %>%
    mutate(
      u = img_PCA$x[,1],
      v = img_PCA$x[,2]
    )

  return(img_df_cluster)
}
