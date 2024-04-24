artist_frequency <- function(dataset = data.frame(),
                             category = "saved",
                             vibe = "neon",
                             name = "untitled",
                             saveto = getwd()) {

  # check inputs
  stopifnot(category == "saved" || category == "top")
  stopifnot(vibe == "soft" ||
              vibe == "neutral" ||
              vibe == "neon" ||
              vibe == "bright")

  # file set-up
  file_name <- file.path(saveto, paste0(name, ".png"))

  # choose dataset
  if (length(dataset) == 0) {
    if (category == "top") {
      dataset <- spotifywRapped::top_tracks_longterm
    } else {
      dataset <- spotifywRapped::saved_tracks
    }
  }

  # arranging data
  occurences <- table(unlist(dataset$artist))
  df <- as.data.frame(occurences)
  data_sorted <- df[order(df$Freq,
                          decreasing = TRUE), ]
  new_df <- head(data_sorted, 10)

  top_artist <- new_df$Var1[new_df$Freq == max(new_df$Freq)]
  number_song <- new_df$Freq[new_df$Var1 == top_artist]


  # color palette
  if (vibe == "soft") {
    bar_color <- "#66545e"
    background_color <- "white"
    text_line_color <- "black"
    if (category == "saved") {
      background_image <- system.file("vibes", "frequency_saved_soft.png",
                                      package = "spotifywRapped")
    } else if (category == "top") {
      background_image <- system.file("vibes", "frequency_top_soft.png",
                                      package = "spotifywRapped")
    }
  } else if (vibe == "neutral") {
    bar_color <- "#664228"
    background_color <- "white"
    text_line_color <- "black"
    if (category == "saved") {
      background_image <- system.file("vibes", "frequency_saved_neutral.png",
                                      package = "spotifywRapped")
    } else if (category == "top") {
      background_image <- system.file("vibes", "frequency_top_neutral.png",
                                      package = "spotifywRapped")
    }
  } else if (vibe == "neon") {
    bar_color <- "#00fb35"
    background_color <- "black"
    text_line_color <- "white"
    if (category == "saved") {
      background_image <- system.file("vibes", "frequency_saved_neon.png",
                                      package = "spotifywRapped")
    } else if (category == "top") {
      background_image <- system.file("vibes", "frequency_top_neon.png",
                                      package = "spotifywRapped")
    }
  } else if (vibe == "bright") {
    bar_color <- "#42a593"
    background_color <- "#1e1d1d"
    text_line_color <- "white"
    if (category == "saved") {
      background_image <- system.file("vibes", "frequency_saved_bright.png",
                                      package = "spotifywRapped")
    } else if (category == "top") {
      background_image <- system.file("vibes", "frequency_top_bright.png",
                                      package = "spotifywRapped")
    }
  }

  # plot set-up
  y_max <- ceiling(max(new_df$Freq) / 5) * 5
  new_df$Var1 <- as.character(new_df$Var1)
  for (i in seq_along(new_df$Var1)) {
    if (!is.na(new_df$Var1[i]) && nchar(new_df$Var1[i]) > 20) {
      new_label <- paste0(substr(new_df$Var1[i], 1, 20), "...")
      new_df$Var1[i] <- new_label
    }
  }

  #plot
  artist_frequency_plot <- ggplot2::ggplot(data = new_df,
                                           ggplot2::aes(x = reorder(Var1, -new_df$Freq), y = new_df$Freq)) +
    ggplot2::geom_bar(stat = "identity", fill = bar_color) +
    ggplot2::geom_text(aes(label = Var1),
                       vjust = -1,
                       hjust = 0,
                       color = text_line_color,
                       size = 8,
                       angle = 30,
                       position = position_nudge(x = -0.25)) +
    ggplot2::scale_x_discrete(labels = NULL,
                              breaks = NULL,
                              expand = expansion(add = c(0.75, 3))) +
    ggplot2::scale_y_continuous(breaks = seq(0, y_max, 5),
                                limits = c(0, y_max),
                                expand = expansion(add = c(0, 7))) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(388, 128, 705, 125, "points"),
                   plot.background = ggplot2::element_rect(fill = fill = background_color,
                     color = NA)) +
    ggplot2::theme(axis.line = element_line(color = bar_color),
          axis.line.x = element_blank(),
          axis.text = element_text(color = bar_color),
          panel.grid.major = element_line(color = bar_color,
                                          linetype = "dotted"),
          panel.grid.minor = element_line(color = bar_color,
                                          linetype = "dotted")) +
    ggplot2::coord_cartesian(ylim = c(0, y_max), xlim = c(1, 11)) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::annotation_custom(grid::textGrob(top_artist,
                                              gp = grid::gpar(
                                                fontsize = 60,
                                                col = text_line_color)),
                               xmin = 6.5, xmax = 6.5, ymax = -27.5) +
    ggplot2::annotation_custom(grid::textGrob(number_song,
                                              gp = grid::gpar(
                                                fontsize = 60,
                                                col = text_line_color)),
                               xmin = 6.5, xmax = 6.5, ymax = -48)

  # arrange file
  png(filename = file_name, width = 1080, height = 1920, units = "px")
  print(
    cowplot::ggdraw() +
      cowplot::draw_image(background_image) +
      cowplot::draw_plot(artist_frequency_plot)
  )
  dev.off()
  return(file_name)
}
