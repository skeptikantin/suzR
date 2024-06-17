# Theme specifications ----------------------------------------------------

theme_suzR <- function(base_family = "IBM Plex Sans") {
  theme_minimal() +
    theme(
      legend.position = "top",
      #axis.line = element_line(color = "grey50"),
      #panel.grid = element_blank(),
      text = element_text(family = base_family),
      #plot.title = element_text(size = 18),
      #plot.subtitle = element_text(size = 12, vjust = -0.3),

      # creating space
      legend.margin = margin(t = 20),

      # axis titles:

      # add markdown
      axis.title = ggtext::element_markdown(margin = margin(0,0,0,10)),
      axis.title.y = ggtext::element_markdown(margin = margin(0,10,0,0)),
      axis.title.x = ggtext::element_markdown(margin = margin(10,0,0,0)),
      plot.title = ggtext::element_markdown(size = 15),
      plot.subtitle = ggtext::element_markdown(size = 10, vjust = -0.3)

    )
}
