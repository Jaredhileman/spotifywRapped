


#' @export
my_top_five <- function(name, time, vibe, kind = "artists", saveto = getwd()) {

  if (!is.character(time) || !is.character(vibe) || !is.character(kind) ||
      !is.character(name)) {
    stop("time, vibe, and kind must be character strings")
  }

  background_relative <- paste0(kind, "_", time, "_", vibe, ".png")
  background_path <- system.file("vibes", background_relative,
                                 package = "spotifywRapped")

  print(background_path)
  background <- magick::image_read(background_path)
  if (kind == "artists") {
    if (time == "long") {
      graphdata <- spotifywRapped::top_artists_longterm
    } else if (time == "medium") {
      graphdata <- spotifywRapped::top_artists_mediumterm
    } else if (time == "short") {
      graphdata <- spotifywRapped::top_artists_shortterm
    }
    image_url <- graphdata$images[1][[1]]$url[[1]]

  } else {
    if (time == "long") {
      graphdata <- spotifywRapped::top_tracks_longterm
    } else if (time == "medium") {
      graphdata <- spotifywRapped::top_tracks_mediumterm
    } else if (time == "short") {
      graphdata <- spotifywRapped::top_tracks_shortterm
    }
    image_url <- graphdata$album.images[1][[1]]$url[[1]]
  }

  mini_image <- magick::image_read(image_url)
  mini_image <- magick::image_resize(mini_image, "530x530")

  box_coordinates <- data.frame(
    x = rep(400, times = 4) / 1080,
    y = (1920 - c(1140, 1270, 1400, 1530) - 42) / 1920,
    label = graphdata$name[2:5]
  )

  file_name <- paste0(name, ".png")
  print(file_name)
  postables_path <- file.path(saveto, file_name)
  print(postables_path)

  png(postables_path, width = 1080, height = 1920)

  par(mar = c(0, 0, 0, 0))  # Set all margins to zero
  plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xaxt = 'n', yaxt = 'n',
       xlab = '', ylab = '', main = '', bty = 'n', ann = FALSE)

  for (label in box_coordinates$label) {
    if (nchar(label) > 30) {
      new_label <- paste0(substr(label, 1, 30), "...")
      box_coordinates$label[box_coordinates$label == label] <- new_label
    }
  }

  # Overlay the raster image
  rasterImage(background, 0, 0, 1, 1) # Adjust these coordinates based on where you want the image within the plot area
  rasterImage(mini_image, 275 / 1080, (1920 - 530 - 500)/1920, (275 + 530)/1080,
              (1920 - 500)/1920)
  textcolor <- ifelse(vibe == "bright" || vibe == "neon", "white", "black")
  text(x = (190 + 350)/1080, y = (1920 - 350)/1920,
       labels = graphdata$name[1], col = textcolor, cex = 3, adj = 0.5)
  text(x = box_coordinates$x, y = box_coordinates$y,
       labels = box_coordinates$label, col = textcolor, cex = 3, adj = 0)

  # Close the graphics device
  dev.off()
}

# image_path <- "C:/Users/jared/Downloads/songs_long_soft.png"
# image <- magick::image_read(image_path)
#labels = box_coordinates$label
# url1 <- top_artists$images[1][[1]]$url[[1]]
# artist_image <- magick::image_read(url1)
# artist_image <- magick::image_resize(artist_image, "530x530")
#
# box_coordinates <- data.frame(
#   x = rep(400, times = 4) / 1080,
#   y = (1920 - c(1140, 1270, 1400, 1530) - 42) / 1920,
#   label = top_artists$name[2:5]
# )
#
#
# png("my_plot_with_image.png", width = 1080, height = 1920)
#
#
# par(mar = c(0, 0, 0, 0))  # Set all margins to zero
# plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xaxt = 'n', yaxt = 'n',
#      xlab = '', ylab = '', main = '', bty = 'n', ann = FALSE)
#
#
# # Overlay the raster image
# rasterImage(image, 0, 0, 1, 1) # Adjust these coordinates based on where you want the image within the plot area
# rasterImage(artist_image, 275 / 1080, (1920 - 530 - 500)/1920, (275 + 530)/1080, (1920 - 500)/1920)
# text(x = box_coordinates$x, y = box_coordinates$y,
#      labels = box_coordinates$label, col = "black", cex = 3, adj = 0)
#
# # Close the graphics device
# dev.off()


