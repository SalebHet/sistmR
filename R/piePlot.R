#' @export
#' @import ggplot2
#' @import ggpubr
#' @import graphics
#' @import ggbeeswarm
#' @import dplyr

piePlot <- function(dataDF,vec,groupe,treatment="sum",color = "RdGy"){
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
    if(treatment=="sum"){
      datas <- append(datas,sum(collec))#c(sum(collec),datas)
    }
    if(treatment=="mean"){
      datas <- append(datas,mean(collec))#c(sum(collec),datas)
    }
    if(treatment=="median"){
      datas <- append(datas,median(collec))#c(sum(collec),datas)
    }
  }



  data <- data.frame(
    group=grp,
    value=datas
  )

  # Compute the position of labels
  data <- data %>%
    arrange(desc(group)) %>%
    mutate(prop = value / sum(data$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )

  # Basic piechart
  plotPie <- ggplot(data, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() +
    theme(legend.position="none") +

    geom_text(aes(y = ypos, label = group), color = "white", size=6) +
    scale_fill_brewer(palette=color)
  #return(pie(datas,grp,col = scale_color_brewer(palette = color)))
  return(plotPie)
}
