library(spotifyr)
library(tidyverse)

NumberArtists <- function(category = "saved") {
  stopifnot(category == "saved" || category == "top")
}


#set-up
id <- '1c68aad85abf44c09d1345783463851b'
secret <- '9e7fc07f6d0e4fa0b5d34122905a4766'

access_token <- get_spotify_access_token(id, secret)
authorization <- get_spotify_authorization_code(client_id = id,
                                                client_secret = secret,
                                                scope = c("user-top-read",
                                                          "user-library-read"))

#saved tracks data frame
saved_tracks <- get_my_saved_tracks(authorization = authorization)
saved_tracks <- rbind(saved_tracks,
                      get_my_saved_tracks(authorization = authorization))
reduce(saved_tracks$track.artists)

# top tracks data frame
top_tracks <- get_my_top_artists_or_tracks(type = "tracks",
                                           time_range = "short_term",
                                           authorization = authorization)
top_tracks <- rbind(top_tracks,
                    get_my_top_artists_or_tracks(type = "tracks",
                                                 offset = 20,
                                                 time_range = "short_term",
                                                 authorization = authorization))
top_tracks <- rbind(top_tracks,
                    get_my_top_artists_or_tracks(type = "tracks",
                                                 offset = 40,
                                                 time_range = "short_term",
                                                 authorization = authorization))

# graphing
occurences <- table(unlist(saved_tracks$artist))
df <- as.data.frame(occurences)
data_sorted <- df[order(df$Freq,
                        decreasing = TRUE), ]
new_df = head(data_sorted, 10)


#our data
soft_p = c('#66545e', '#eda990', '#aa6f73', '#a39193', '#f6e0b5')
y_max <- ceiling(max(new_df$Freq) / 5) * 5
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
  coord_cartesian(ylim = c(0, max(new_df$Freq) +
                             5 * length(new_df$Var1[1])),
                  xlim = c(1, nrow(new_df) +
                             length(new_df$Var1[length(new_df$Var1)])))

graph
