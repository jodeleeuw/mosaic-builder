library(progress)
library(magick)

# can print at 36x36 or 30x40
# 600 dpi, so a 300x300 sq would be half inch printed
# 60 x 80 photos?

create.tile.map <- function(image.path, w.tiles, h.tiles) {
  
  image <- image_read(image.path)
  
  attr <- image_info(image)
  
  w <- attr$width
  h <- attr$height
  
  tile.size.w <- floor(w / w.tiles)
  tile.size.h <- floor(h / h.tiles)
  
  tile.size <- min(tile.size.h, tile.size.w)
  
  h_pixels_to_cut <- h - tile.size * h.tiles
  w_pixels_to_cut <- w - tile.size * w.tiles
  
  crop_string <- paste0(w.tiles * tile.size, "x", h.tiles * tile.size, 
                        "+", floor(w_pixels_to_cut/2), "+", floor(h_pixels_to_cut/2))
  
  
  return(
    image |>
    image_crop(crop_string) |>
    image_scale(paste0(w.tiles,"x",h.tiles)) |>
    image_convert(colorspace = "gray") |>
    image_data() |>
    as.integer()
  )
}

get.mean.color <- function(image.path){
  
  image <- image_read(image.path)
  
  i <- image |> 
    image_scale("1x1") |>
    image_data() |>
    as.integer()
  return(i[1,1,1])
}

get.mean.color.2 <- function(image.path){
  image <- image_read(image.path)
  
  d <- image |> image_data() |> as.integer()
}

create.file.to.color.map <- function(files){
  pb <- progress_bar$new(total=length(files))
  out <- data.frame(file=files, color=1)
  for(i in 1:nrow(out)){
    out[i,"color"] <- get.mean.color(out[i,"file"])
    pb$tick()
  }
  return(out)
}

get.closest.match <- function(color.to.match, file.colors){
  matches <- file.colors %>%
    mutate(d = abs(color.to.match - color)) %>%
    filter(d <= min(d) + 20) %>%
    pull(file)
  
  return(sample(matches, 1))
}

batch.convert.bw.and.crop <- function(files){
  pb <- progress_bar$new(total=length(files))
  for(f in files){
    convert.bw.and.crop(f)
    pb$tick()
  }
}

convert.bw.and.crop <- function(image.path){
  
  b.name <- basename(image.path)
  
  image <- image_read(image.path)
  
  attr <- image_info(image)
  
  w <- attr$width
  h <- attr$height
  
  if(w < h){
    crop_string <- paste0(w, "x", w,"+0+",floor((h-w)/2))
  } else {
    crop_string <- paste0(h, "x", h,"+",floor((w-h)/2),"+0")
  }
  
  i <- image |> 
    image_convert(colorspace="gray") |> 
    image_crop(crop_string) |>
    image_scale("300x300") |>
    image_write(path=paste0("images_bw/", b.name))
}

adjust.brightness <- function(image, target_brightness, scaling){
  current_brightness <- (image |> 
    image_scale("1x1") |>
    image_data() |>
    as.integer())[1,1,1]
  
  percent.change <- target_brightness / current_brightness * 100 - 100
  
  i <- image |>
    image_modulate(brightness = 100 + (percent.change * scaling))
  
  return(i)
}

swap.duplicate.neighbors <- function(df){
  
  df$row <- as.numeric(df$row)
  df$col <- as.numeric(df$col)
  # df.duplicates <- df %>% 
  #   mutate(has_duplicate_neighbor = file == (df %>% filter(row %in% (row-1):(row+1), col %in% (col-1):(col+1), ))
  has_duplicate <- rep(FALSE, nrow(df))
  for(i in 1:nrow(df)){
    y <- df[i,]$row
    x <- df[i,]$col
    f <- df[i,]$file

    n <- df %>% filter(row %in% (y-1):(y+1), col %in% (x-1):(x+1), file==f) %>% nrow()
    
    has_duplicate[i] <- n > 1
  }
}
