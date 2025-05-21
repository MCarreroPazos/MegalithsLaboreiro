# Load library
library(magick)

# Define directory path
dir_path <- "/Users/miguelpazoscarrero/Desktop/Laboreiro/images/"

# Load images
img1 <- image_read(paste0(dir_path, "OFeM1.JPG"))
img2 <- image_read(paste0(dir_path, "OFeM3_Dolmen.JPG"))
img3 <- image_read(paste0(dir_path, "Landscape.JPG"))

# Set the target width for the full composition
target_width <- 800  

# Resize the top images so that they add up to the target width
img1 <- image_resize(img1, paste0(target_width / 2, "x"))  
img2 <- image_resize(img2, paste0(target_width / 2, "x"))

# Resize the bottom image to match the target width
img3 <- image_resize(img3, paste0(target_width, "x"))

# Add labels "A", "B", and "C" correctly
img1 <- image_annotate(img1, "A", 
                       gravity = "northwest", 
                       location = "+10+10",
                       size = 40,
                       color = "black", 
                       strokecolor  = "black")
img2 <- image_annotate(img2, "B", 
                       gravity = "northeast",
                       location = "+10+10",
                       size = 40, 
                       color = "black", 
                       strokecolor = "black")
img3 <- image_annotate(img3, "C", 
                       gravity = "northwest",
                       location = "+10+10",
                       size = 40, 
                       color = "black", 
                       strokecolor = "black")

# Combine the first row (OFeM1 and OFeM3_Dolmen)
top_row <- image_append(c(img1, img2), stack = FALSE)

# Merge both rows vertically (top row + full-width bottom image)
composition <- image_append(c(top_row, img3), stack = TRUE)

# Display the final image
print(composition)

# Save the final image
figures_path <- "/Users/miguelpazoscarrero/Desktop/Laboreiro/figures/"
output_pdf_path <- paste0(figures_path, "Figure 2.pdf")
image_write(composition, output_pdf_path, format = "pdf")
