# Figure 2: Megalithic sites in Serra do Laboreiro.
source(here::here("analysis", "scripts", "00_Setup.R"))

# --- Load images --------------------------------------------------------------
img1 <- magick::image_read(file.path(path_images, "OFeM1.JPG"))
img2 <- magick::image_read(file.path(path_images, "OFeM3_Dolmen.JPG"))
img3 <- magick::image_read(file.path(path_images, "Landscape.JPG"))

# --- Resize -------------------------------------------------------------------
target_width <- 800
img1 <- magick::image_resize(img1, paste0(target_width / 2, "x"))
img2 <- magick::image_resize(img2, paste0(target_width / 2, "x"))
img3 <- magick::image_resize(img3, paste0(target_width, "x"))

# --- Annotate labels ----------------------------------------------------------
img1 <- magick::image_annotate(img1, "A",
    gravity = "northwest", location = "+10+10",
    size = 40, color = "black", strokecolor = "black"
)
img2 <- magick::image_annotate(img2, "B",
    gravity = "northeast", location = "+10+10",
    size = 40, color = "black", strokecolor = "black"
)
img3 <- magick::image_annotate(img3, "C",
    gravity = "northwest", location = "+10+10",
    size = 40, color = "black", strokecolor = "black"
)

# --- Compose and display ------------------------------------------------------
top_row <- magick::image_append(c(img1, img2), stack = FALSE)
composition <- magick::image_append(c(top_row, img3), stack = TRUE)
print(composition)

# --- Save figure --------------------------------------------------------------
magick::image_write(composition,
    file.path(path_images, "fig2.png"),
    format = "png"
)

magick::image_write(composition,
    file.path(path_figures, "Figure 2.pdf"),
    format = "pdf"
)
