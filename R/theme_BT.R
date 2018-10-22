#' @title applies British Telecoms font and colour schemes to ggplot2 plots
#'
#' @description This colour scheme and font make the graphs specific to British Telecoms brand
#' @param axis.text.size axis text size for both x and y axis
#' @param legend.title.text.size plot's legend text size
#' @param axis.title.size axis title text size
#' @param plot.title.text.size plot title text size
#' @param plot.legend.position plot legend position i.e. top/bottom etc.
#' @export
#' @name theme_BT
#' @rdname theme_BT
#' @seealso \code{\link[ggplot2]{theme}}
#' @seealso \code{\link[ggplot2]{labs}}
#' @examples \dontrun{ggplot2() +
#' theme_BT()}
#'
theme_BT <- function(axis.text.size = 12,
                     legend.title.text.size = 17,
                     axis.title.size = 14,
                     plot.title.text.size = 20,
                     plot.legend.position = "top"){
  extrafont::loadfonts()
  options(warn = -1)
  ggplot2::theme(plot.background = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 legend.position = plot.legend.position,
                 axis.title.y = ggplot2::element_text(family = "BT Font", face = "bold", size = axis.title.size),
                 axis.title.x = ggplot2::element_text(family = "BT Font", face = "bold", size = axis.title.size),
                 axis.text.y = ggplot2::element_text(family = "BT Font", face = "bold", size = axis.text.size),
                 axis.text.x = ggplot2::element_text(family = "BT Font", face = "bold", size = axis.text.size),
                 axis.ticks.y = ggplot2::element_blank(),
                 axis.ticks.x = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(fill = "#666666"),
                 strip.text.y = ggplot2::element_text(family = "BT Font",face = "bold",size = axis.title.size,colour = "white"),
                 strip.text.x = ggplot2::element_text(family = "BT Font",face = "bold",size = axis.title.size,colour = "white",lineheight = 3),
                 legend.text = ggplot2::element_text(family = "BT Font",face = "bold",size = axis.title.size),
                 legend.title = ggplot2::element_text(family = "BT Font",face = "bold",size = legend.title.text.size),
                 plot.title = ggplot2::element_text(family = "BT Font",face = "bold",size = plot.title.text.size,hjust=0)
                )
}
