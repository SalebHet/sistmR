#' @export
#' @import ggplot2
#' @import ggpubr

barplot <-  function(datadf,x_var,y_var,group = "NONE",color = "RdGy",xscale = "none",yscale = "none"){
  if(group == "NONE"){
  plot <- ggpubr::ggbarplot(datadf,x = x_var,y = y_var,fill = x_var)
  }else{
    datadf[,group] <- as.factor(datadf[,group])
    plot <- ggpubr::ggbarplot(datadf,x = x_var,y = y_var,fill = group,sort.by.groups = FALSE, combine = TRUE)
  }
  plot <- ggpubr::ggpar(plot, palette = color,xscale = xscale,yscale = yscale)
  return(plot)
}
