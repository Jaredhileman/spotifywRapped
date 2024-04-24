# artist_frequency <- function(saved_tracks = spotifywRapped::saved_tracks,
#                              category = "saved",
#                              vibe = "neon") {
#   stopifnot(category == "saved" || category == "top")
#
#
#   # graphing
#   occurences <- table(unlist(saved_tracks$artist))
#   df <- as.data.frame(occurences)
#   data_sorted <- df[order(df$Freq,
#                           decreasing = TRUE), ]
#   new_df = head(data_sorted, 10)
#
#
#   # color palette
#   if (vibe == "soft") {
#     bar_color <- "#66545e"
#     background_color <- "white"
#     text_line_color <- "black"
#     background_image <- system.file("vibes", "enval_soft.png",
#                                     package = "spotifywRapped")
#   } else if(vibe == "neutral") {
#     bar_color <- "#664228"
#     background_color <- "white"
#     text_line_color <- "black"
#     background_image <- system.file("vibes", "enval_neutral.png",
#                                     package = "spotifywRapped")
#   } else if (vibe == "neon") {
#     bar_color <- "#00fb35"
#     background_color <- "black"
#     text_line_color <- "white"
#     background_image <- system.file("vibes", "enval_neon.png",
#                                     package = "spotifywRapped")
#   } else if (vibe == "bright") {
#     bar_color <- "#42a593"
#     background_color <- "#1e1d1d"
#     text_line_color <- "white"
#     background_image <- system.file("vibes", "enval_soft.png",
#                                     package = "spotifywRapped")
#   }
#
#   #our data
#   soft_p = c('#66545e', '#eda990', '#aa6f73', '#a39193', '#f6e0b5')
#   y_max <- ceiling(max(new_df$Freq) / 5) * 5
#   for (Var1 in new_df$Var1) {
#     if (!is.na(Var1) && nchar(Var1) > 20) {
#       new_label <- paste0(substr(Var1, 1, 20), "...")
#       new_df$Var1[new_df$Var1 == Var1] <- new_label
#     }
#   }
#   graph <- ggplot(data = new_df,
#                   aes(x = reorder(Var1, -Freq), y = Freq)) +
#     geom_bar(stat = "identity", fill = bar_color) +
#     geom_text(aes(label = Var1),
#               vjust = -1,
#               hjust = 0,
#               color = text_line_color,
#               size = 3,
#               angle = 30,
#               position = position_nudge(x = -0.25)) +
#     scale_x_discrete(labels = NULL, breaks = NULL) +
#     scale_y_continuous(breaks = seq(0, y_max, 5), limits = c(0, y_max)) +
#     labs(x = "", y = "") +
#     theme_minimal() +
#     theme(plot.background = element_rect(fill = background_color)) +
#     theme(axis.line = element_line(color = bar_color),
#           axis.line.x = element_blank(),
#           axis.text = element_text(color = bar_color),
#           panel.grid.major = element_line(color = bar_color,
#                                           linetype = "dotted"),
#           panel.grid.minor = element_line(color = bar_color,
#                                           linetype = "dotted")) +
#     coord_cartesian(ylim = c(0, y_max + 1), xlim = c(1, 11))
#
#   graph
#   #return(file_name)
# }
# artist_frequency(vibe = "bright")
