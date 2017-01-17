shape2rda <- function(shape,outdir){
  NUTS1=readShapePoly(shape)
  fn <- paste0(outdir,"/NUTS1.rda")
  save(NUTS1, ascii=FALSE, file=fn)
}
