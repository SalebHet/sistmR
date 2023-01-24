#' @export
#' @import ggplot2
#' @import ggpubr
#' @import graphics

piePlot <- function(dataDF,vec,groupe){
  grp <- unique(dataDF[,groupe])
  datas <- c()
  for (i in 1:length(grp)) {
    collec <- c()
    for (j in 1:length(dataDF[,vec])) {
      #browser()
      if(dataDF[j,groupe] == grp[i]){
        #browser()
        collec <- append(collec,dataDF[j,vec])#c(dataDF[j,vec],collec)
      }
    }
    cat("grp: ")
    cat(grp[i],"\n")
    cat("collec: ")
    cat(collec,"\n")
    #browser()
    datas <- append(datas,sum(collec))#c(sum(collec),datas)
  }
  return(pie(datas,grp))
}
