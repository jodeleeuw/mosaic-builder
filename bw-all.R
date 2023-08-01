library(magick)

folder <- "images_magic_crop"
final.size <- "300x300"

all.files <- list.files(folder, pattern = ".jpg", full.names = TRUE)

pb <- progress::progress_bar$new(total=length(all.files))
for(path in all.files){
  
  b.name <- basename(path)
  
  if(file.exists(paste0("images_final/", b.name))){
    print("image already converted!")
    next
  }
  
  i <- image_read(path)
  
  i <- i |>
    image_convert(colorspace = "gray") |>
    image_scale(geometry = final.size) |>
    image_write(path=paste0("images_final/", b.name))
  
  pb$tick()
}
