#' My theme
#'
#' @description
#' Modfiy the theme_minimal(). Refer to: https://ggplot2.tidyverse.org/reference/theme.html.
#'
#' @export
#' @importFrom stats chisq.test
#' @import ggplot2
#' @import ggtext

#' @param base_family Specify the font family name
#' @returns Pwetty plot.
theme_suzR <- function(base_family = "IBM Plex Sans") {

  # use and modify the minimal theme:
  theme_minimal() +
    theme(

      # change grid elements:
      panel.grid = element_blank(),

      # move plot title to align with plot rather than panel:
      plot.title.position = "plot",

      # add more space below subtitle
      #plot.margin = margin(),

      ## not sure if I want to keep those:
      # by default, put a legend on top:
      legend.position = "top",
      legend.margin = margin(t = 0),
      #axis.line = element_line(color = "grey50"),
      #panel.grid = element_blank(),
      text = element_text(family = base_family),
      #plot.title = element_text(size = 18),
      #plot.subtitle = element_text(size = 12, vjust = -0.3),


      # axis titles:

      # add markdown
      plot.title = ggtext::element_markdown(size = 16, face = "bold"),
      plot.subtitle = ggtext::element_markdown(size = 12, vjust = -0.3),
      plot.caption = ggtext::element_markdown(),

      axis.title = ggtext::element_markdown(margin = margin(0,0,0,10)),
      axis.title.y = ggtext::element_markdown(margin = margin(0,10,0,0)),
      axis.title.x = ggtext::element_markdown(margin = margin(10,0,0,0)),

    )
}
