vis <- function(img, level){
  img_quantized <- img %>% img_quantize(level)

  vis1 <- converted_img(img_quantized, img)
  vis2 <- converted_bar(img_quantized)
  vis3 <- converted_scatter(img_quantized)

  grid.arrange(vis1, vis2, vis3, ncol = 1, nrow = 3)
}
