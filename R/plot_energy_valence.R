#' @export
my_energy_valence <- function (data = spotifywRapped::saved_tracks,
                               vibe = "neon", name = "untitled.png",
                               saveto = getwd()) {

  file_name <- paste0(saveto, "/" ,name)

  labels <- c("Sad Boy Hours", "Chilling", "Hype Mood", "Energetic Despair")

  sad_count <- sum(data$energy <= 0.5 & data$valence <= 0.5)
  chill_count <- sum(data$energy <= 0.5 & data$valence > 0.5)
  hype_count <- sum(data$energy > 0.5 & data$valence > 0.5)
  energetic_count <- sum(data$energy > 0.5 & data$valence <= 0.5)

  counts <- c(sad_count, chill_count, hype_count, energetic_count)

  vibe_label <- labels[which.max(counts)]

  if (vibe == "soft") {
    dot_color <- "#66545e"
    background_color <- "white"
    text_line_color <- "black"
    background_image <- system.file("vibes", "enval_soft.png",
                                    package = "spotifywRapped")
  } else if(vibe == "neutral") {
    dot_color <- "#664228"
    background_color <- "white"
    text_line_color <- "black"
    background_image <- system.file("vibes", "enval_neutral.png",
                                    package = "spotifywRapped")
  } else if (vibe == "neon") {
    dot_color <- "#e5ff04"
    background_color <- "black"
    text_line_color <- "white"
    background_image <- system.file("vibes", "enval_neon.png",
                                    package = "spotifywRapped")
  } else if (vibe == "bright") {
    dot_color <- "#42a593"
    background_color <- "#1e1d1d"
    text_line_color <- "white"
    background_image <- system.file("vibes", "enval_soft.png",
                                    package = "spotifywRapped")
  }

  enval_plot <- ggplot(data, aes(x = energy, y = valence)) +
    geom_point(size = 5, color = dot_color) +
    theme(plot.margin = margin(345, 84, 663, 84, "points"),
          plot.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = background_color),
          panel.grid.major = element_line(color = text_line_color, linewidth = 1),
          axis.text = element_blank(),
          axis.title = element_blank()) +
    scale_x_continuous(breaks = 0.5) +
    scale_y_continuous(breaks = 0.5) +
    annotate('text', x = .19, y = .95,label="Energetic Despair", size = 12,
             col = text_line_color) +
    annotate('text', x = .87, y = .95, label = "Hype Mood", size = 12,
             col = text_line_color) +
    annotate('text', x = .91, y = .05, label = "Chilling", size = 12,
             col = text_line_color) +
    annotate('text', x = .18, y = .05, label = "Sad Boy Hours", size = 12,
             col = text_line_color) +
    annotate('text', x = 0.99, y = .55, label = "Energy", size = 9,
             col = text_line_color, angle = 90) +
    annotate('text', x = 0.95, y = .48, label = "Valence", size = 9,
             col = text_line_color) +
    coord_cartesian(clip = "off") +
    annotation_custom(grid::textGrob(vibe_label, gp = grid::gpar(fontsize = 100, col = text_line_color)),
                      xmin = 0.5, xmax = 0.5, ymax = -0.6)

  png(filename = file_name,
      width = 1080, height = 1920, units = "px")

  ggdraw() +
    draw_image(background_image) +
    draw_plot(enval_plot)

  dev.off()
}
