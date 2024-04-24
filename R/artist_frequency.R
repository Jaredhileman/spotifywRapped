NumberArtists <- function(category = "saved") {
  stopifnot(category == "saved" || category == "top")
}


saved_tracks <- spotifywRapped::saved_tracks

# graphing
occurences <- table(unlist(saved_tracks$artist))
df <- as.data.frame(occurences)
data_sorted <- df[order(df$Freq,
                        decreasing = TRUE), ]
new_df = head(data_sorted, 10)


#our data
soft_p = c('#66545e', '#eda990', '#aa6f73', '#a39193', '#f6e0b5')
y_max <- ceiling(max(new_df$Freq) / 5) * 5
new_df$Var1 <- as.character(new_df$Var1)
for (Var1 in new_df$Var1) {
  if (!is.na(Var1) && nchar(Var1) > 10) {
    new_label <- as.factor(paste0(substr(Var1, 1, 30), "..."))
    new_df$Var1[new_df$Var1 == Var1] <- new_label
  }
}
graph <- ggplot(data = new_df,
                aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = soft_p[3]) +
  geom_text(aes(label = Var1),
            vjust = -1,
            hjust = 0,
            color = soft_p[3],
            size = 3,
            angle = 30,
            position = position_nudge(x = -0.25)) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  scale_y_continuous(breaks = seq(0, y_max, 5), limits = c(0, y_max)) +
  labs(x = "", y = "") +
  scale_color_manual(values = soft_p) +
  #scale_color_manual(palette = c()) +
  theme_minimal() +
  theme(axis.line = element_line(color = soft_p[1]),
        axis.line.x = element_blank(),
        axis.text = element_text(color = soft_p[1]),
        panel.grid.major = element_line(color = soft_p[1],
                                        linetype = "dotted"),
        panel.grid.minor = element_line(color = soft_p[1],
                                        linetype = "dotted")) +
  coord_cartesian(ylim = c(0, y_max + 5))

graph
