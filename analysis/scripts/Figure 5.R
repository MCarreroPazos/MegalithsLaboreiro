# Figure 5: Visual networks over the mounds in Laboreiro region
# Created with the visibility analysis plugin from QGIS (Čučković, 2016)
source(here::here("analysis", "scripts", "00_Setup.R"))

pdf_path <- file.path(path_figures, "Figure 5.pdf")
png_path <- file.path(path_images, "fig5.png")

# --- Convert PDF to PNG for Quarto ------------------------------------------
if (file.exists(pdf_path)) {
  fig5 <- magick::image_read_pdf(pdf_path, density = 200)
  magick::image_write(fig5, path = png_path, format = "png")
  message("Figure 5 PNG saved to: ", png_path)
} else if (file.exists(png_path)) {
  message("Figure 5 PNG already exists: ", png_path)
} else {
  warning("Figure 5 source not found. Please add 'Figure 5.pdf' to the figures/ folder or 'fig5.png' to images/.")
}
