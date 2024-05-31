#' @export
#' @import ggplot2
#' @import ggpubr

lineplot <-  function(datadf,x_var,y_var,group = "NONE",color = "RdGy"){
  lplot <- ggplot(datadf)
  if(group == "NONE"){
    cat("Groupe = NONE")
    lplot <<- lplot + geom_line(aes(x=x_var, y=y_var, group=group, color=group))
  }else{
    cat("Groupe = ",group)
    lplot <<- lplot + geom_line(aes(x=x_var, y=y_var))
  }
  browser()
  cat(str(lplot))
  return(lplot)
}
