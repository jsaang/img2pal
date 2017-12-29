converted_img <- function(img_df_cluster, img){

  back_to_img <- img_df_cluster %>%
    select(x, y, c.1.y, c.2.y, c.3.y) %>%
    gather(key = 'cc', value = 'value', starts_with('c.')) %>%
    mutate(cc = gsub('c\\.', '', cc)) %>%
    mutate(cc = as.numeric(gsub('\\.y', '', cc)))

  img_cluster_result <- back_to_img %>%
    as.cimg(dim = dim(img))

  rg <- rasterGrob(img_cluster_result, interpolate = TRUE)

  qplot(1:10, 1:10, geom = 'blank') +
    annotation_custom(rg, xmin = -Inf, xmax = Inf,
                      ymin = -Inf, ymax = Inf) +
    geom_point(alpha = 0) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0),
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank()
    )
}
