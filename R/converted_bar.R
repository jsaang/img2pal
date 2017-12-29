converted_bar <- function(img_df_cluster) {
  img_df_cluster %>%
    ggplot(aes(reorder(label, c.1.y),
               fill = rgb(c.1.y,
                          c.2.y,
                          c.3.y))) +
    geom_bar() +
    scale_fill_identity() +
    theme_light() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank()
    )
}
