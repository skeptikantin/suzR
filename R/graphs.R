
# define theme function
theme_plex <- function() {

  # assign font family
  font <- "IBM Plex Sans"

  # replace elements of another theme:
  theme_minimal() %+replace%

    theme(

      # grid elements
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),

      # since theme_minimal already strips axis lines, no need to do again

      # text elements
      plot.title = element_text(
        family = font,
        size = 20,
        face = 'bold',
        hjust = 0,
        vjust = 2,
        color = "#3b3b3b"),

      plot.subtitle = element_text(
        family = font,
        size = 14,
        hjust = 0,
        margin = margin(0, 0, 20, 0)),

      plot.caption = element_text(
        family = font,
        size = 9,
        hjust = 1),

      axis.title = element_text(
        family = font,
        face = "bold",
        size = 10),

      axis.text = element_text(
        family = font,
        size = 9),

      axis.text.x = element_text(
        margin = margin(5, b = 10))

    )


}
