# Figure 1: Study area map
source(here::here("analysis", "scripts", "00_Setup.R"))

# --- Load figure from PDF -----------------------------------------------------
fig1 <- magick::image_read_pdf(
  file.path(path_figures, "Figure 1.pdf"),
  density = 150
)

# --- Display ------------------------------------------------------------------
print(fig1)
