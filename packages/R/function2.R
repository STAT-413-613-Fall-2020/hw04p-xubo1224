#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
myseq_graph <- function(x){
  # Error reminder
  if (is.data.frame(x) != "TRUE"){
    stop("x should be data frame")
  }
  else if (ncol(x) != 4){
    stop("x must have 4 values")
  }
  # create function
  num <- vector(mode = "double", length = nrow(x))
  for (i in seq_along(num)){
    r <- x[i,]
    num[[i]] <- myseq_n(as.double(c(r[1],r[2],r[3])), as.integer(r[4]))
  }
  xlab <- x[,4]
  ylab <- data.frame(num)
  df <- cbind(xlab, ylab)
  colnames(df) <- c("xlab", "ylab")
  df <- data.frame(df)
  ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = xlab, y = ylab))+
    ggplot2::geom_line()+
    ggplot2::xlab("n")+
    ggplot2::ylab("output")+
    ggplot2::ggtitle("My Sequence")
}
