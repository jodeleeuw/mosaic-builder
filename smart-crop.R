library(httr)
library(jsonlite)
library(magick)

cloud.path <- "https://storage.googleapis.com/photo-mosaic-project-jrd/"

side.size <- 300

folder <- "images_resized"

paths <- list.files(folder, pattern=".jpg", full.names = TRUE)

imagga_crop <- function(image.path){
  
  base.path <- basename(image.path)
  
  if(file.exists(paste0("images_magic_crop/", base.path))){
    print("image already cropped!")
    return()
  }
  
  image_url <- paste0(cloud.path, base.path)
  
  api <- "https://api.imagga.com/v2/croppings"
  
  req <- httr::GET(
    url = api,
    query = list(image_url=image_url,
                 resolution=paste0(300,"x",300)),
    add_headers(Authorization="Basic YWNjXzIwZTM3ZTc3YzgyMzIwNTo2YThjY2E1ZmE3MWJlMGFjZjc2ODdjNWRmOTdjOTE2Mg==")
  )
  
  result <- jsonlite::fromJSON(content(req, "text"))$result$croppings
  
  crop.w <- result$x2 - result$x1
  crop.h <- result$y2 - result$y1
  crop.s <- max(crop.w, crop.h)
  
  crop.string <- paste0(crop.s,"x",crop.s,"+",result$x1,"+",result$y1)
  
  i <- image_read(image.path) |>
    image_crop(crop.string) |>
    image_write(path=paste0("images_magic_crop/",base.path))
}

imagekit_crop <- function(image.path){
  
  base.path <- basename(image.path)
  
  download.file(
    paste0("https://ik.imagekit.io/cgpwkvcbf/", base.path, "?tr=w-300,h-300,fo-auto"), 
    paste0("images_imagekit_crop/", base.path), 
    method = "libcurl", mode="wb")
}

start <- 1700
stop <- 1896
pb <- progress::progress_bar$new(total=stop-start+1)
for(i in start:stop){
  imagga_crop(paths[i])  
  pb$tick()
}
