vis_compare <- function(img, levels){

  plot_list <- lapply(levels, function(x) vis(img, x))

  col <- length(levels)

  grid.arrange(grobs = lapply(plot_list, grobTree), ncol = col)
}

