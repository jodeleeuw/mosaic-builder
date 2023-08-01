library(magick)

folder <- "images"
target_folder <- "images_resized"
SMALLEST_EDGE_SIZE <- 800

all.files <- list.files(folder, pattern = ".jpg", full.names = TRUE)

for(path in all.files){
  
  b.name <- basename(path)
  
  if(file.exists(paste0(target_folder, "/", b.name))){
    next
  }
  
  i <- image_read(path)
  
  i <- i |>
    image_scale(geometry = paste0(SMALLEST_EDGE_SIZE,"x",SMALLEST_EDGE_SIZE,"^")) |>
    image_write(path=paste0(target_folder, "/", b.name))
}
