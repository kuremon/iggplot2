# Found on http://lamages.blogspot.com.au/2013/04/how-to-change-alpha-value-of-colours-in.html
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha=alpha))
}

#' @title Identify Points in a scatter plots
#' @param x, y coordinates of points in a scatter plot.
#' @param n the maximum number of points to be identified.
#' @param col points color
#' @param pch points size
#' @param alpha alpha level of the selected points
#' @param remove.points Should the selected points be removed from the plots?
#' @param ... additional arguments to \code{\link{identify}}
#' @export
identifyPch <- function(x, y = NULL, n = length(x),  col="black", pch = 19, alpha, remove.points=FALSE, ...){
  if(remove.points){
    col="white"
    pch=21
  }
  if(length(col)==1) col=rep(col,length(x))
  if(!missing(alpha)) col=add.alpha(col,alpha)
  xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
  sel <- rep(FALSE, length(x)); res <- integer(0)
  while(sum(sel) < n) {
    ans <- identify(x[!sel], y[!sel], n = 1, plot = FALSE, ...)
    if(!length(ans)) break
    ans <- which(!sel)[ans]
    points(x[ans], y[ans], col = col[ans], pch = pch)
    sel[ans] <- TRUE
    res <- c(res, ans)
  }
  res
}

#' @title Plot points and identify them
#' @param x, y coordinates of points in a scatter plot.
#' @param group a grouping factor to determine colors on the plot.
#' @param ... additional arguments to \code{\link{identifyPch}}
#' @return An index vector of selected points.
#' @export
#' @examples
#' \donttest{
#' idx=with(iris,plot.and.identify(Sepal.Length,Sepal.Width,Species))
#' }
plot.and.identify=function(x,y,group,...){
  group=as.factor(group)
  n.group=length(levels(group))
  if(n.group==1){
    colors="black"
  }else{
    colors=gg_color_hue(n.group)
  }
  col=colors[group]
  plot(x,y,col=col,xlab="",ylab="")
  identifyPch(x,y,col=col,...)
}

#' @title Identify points based on a ggplot2 object
#' @param g a ggplot2 object
#' @param ... additional arguments to \code{\link{plot.and.identify}}
#' @details The identification phase is not done on the ggplot2 figure but on a basic plot ressembing the original ggplot2 object.
#' @return An index vector of selected points.
#' @export
#' @examples
#' \donttest{
#' g=ggplot(data=mtcars,aes(x=wt,y=disp))+
#'  geom_point(aes(color=as.factor(cyl)))
#' idx=identify.ggplot2(g)
#' }
identify.ggplot2=function(g,...){
  data=ggplot_build(g)$data[[1]]
  plot.and.identify(data$x,data$y,data$group,...)
}

#' @title Add names to ggplot2 scatter plot points
#' @param g a ggplot2 object
#' @param idx index of the names to add. If \code{idx} is not specified, \code{\link{identify.ggplot2}} is called.
#' @param label the name of the variable to be used for the names. If \code{label} is not specified, the indexes \code{idx} are used instead.
#' @param ... additional arguments to \code{\link{geom_text}}
#' @return The ggplot2 object \code{g} with names added. 
#' @export
#' @examples
#' \donttest{
#' g=ggplot(data=mtcars,aes(x=wt,y=disp))+
#'  geom_point(aes(color=as.factor(cyl)))
#' add_names.ggplot2(g)
#' }
add_names.ggplot2=function(g,idx,label,...){
  if(missing(idx)) idx=identify.ggplot2(g)
  data=g$data
  
  if(missing(label)){
    data$label.=seq(1,nrow(data))
    label="label."
  }
  g+geom_text(data=data[idx,],aes_string(label=label),...)
}