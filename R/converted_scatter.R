converted_scatter <- function(img_df_cluster){
  img_df_cluster %>%
    sample_n(size = 0.1 * nrow(img_df_cluster)) %>%
    ggplot(aes(x = u,
               y = v,
               col = rgb(c.1.y,
                         c.2.y,
                         c.3.y))) +
    geom_point(size = 1.5) +
    scale_color_identity() +
    theme_light() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank()
    )
}
