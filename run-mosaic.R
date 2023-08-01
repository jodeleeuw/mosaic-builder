source("mosaic-library.R")
library(readr)
library(dplyr)
library(tidyr)
library(RcppHungarian)

RUN_FROM_SCRATCH <- FALSE # set to TRUE to avoid using any cached steps.

image.folder <- "images"
tiles.folder <- "images_final"
#original.image <- "0983-L1055348.jpg"
#original.image <- "0810-L1210394.jpg"
#original.image <- "New Braunfels 256.jpg"
#original.image <- "New Braunfels 302.jpg"
original.image <- "base_church.jpg"
original.image.path <- paste0(image.folder, "/", original.image)
number.of.tiles.width <- 80
number.of.tiles.height <- 60



## Create array of image pixel values
tile.map <- create.tile.map(original.image.path, number.of.tiles.width, number.of.tiles.height)

## Create list of images with corresponding average color
color.map.file.name <- paste0("cache/file_color_map_", original.image,".csv")
if(file.exists(color.map.file.name) && !RUN_FROM_SCRATCH){
  file.colors <- read_csv(color.map.file.name)
} else {
  file.colors <- create.file.to.color.map(list.files(tiles.folder, full.names = T))
  write_csv(file.colors, file=color.map.file.name)
}

## Create a tidy-formatted lookup table for row, col -> file
tidy.map.file.name <- paste0("cache/tidy_map_", original.image, ".csv")
if(!file.exists(tidy.map.file.name) || RUN_FROM_SCRATCH){
  mosaic.map <- tile.map %>% as_tibble()
  colnames(mosaic.map) <- 1:80
  
  mosaic.map <- mosaic.map %>%
    mutate(row=1:n()) %>%
    pivot_longer(1:80, names_to="col", values_to = "color") %>%
    mutate(file=" ")
  
  n.per.image <- ceiling(nrow(mosaic.map) / nrow(file.colors))
  
  all.file.images <- file.colors %>% slice(rep(1:n(), each=n.per.image))
  
  cost.matrix <- abs(outer(mosaic.map$color, all.file.images$color, '-'))
  
  solution <- RcppHungarian::HungarianSolver(cost.matrix)
  
  mosaic.map$file <- all.file.images$file[solution$pairs[,2]]
  
  write_csv(mosaic.map, file=tidy.map.file.name)
  
  mosaic.map.with.files <- mosaic.map %>%
    mutate(row = as.numeric(row), col = as.numeric(col))
} else {
  mosaic.map.with.files <- read_csv(tidy.map.file.name) %>%
    mutate(row = as.numeric(row), col = as.numeric(col))
}


## find duplicates, swap with non-duplicates

match.threshold <- 3

check.if.duplicates <- function(item, mm){
  ro <- item$row
  co <- item$col
  fi <- item$file
  dup <- (mm %>% 
            filter(row %in% c(ro-1, ro, ro+1), col %in% c(co-1, co, co+1), file == fi) %>%
            nrow()) >= 2
  return(dup)
}

duplicates <- rep(FALSE, nrow(mosaic.map.with.files))
for(i in 1:nrow(mosaic.map.with.files)){
  duplicates[i] <- check.if.duplicates(mosaic.map.with.files[i,], mosaic.map.with.files)
}

mosaic.map.add.duplicate.check <- mosaic.map.with.files
mosaic.map.add.duplicate.check$duplicate <- duplicates

pb <- progress_bar$new(total=nrow(mosaic.map.add.duplicate.check))
for(i in 1:nrow(mosaic.map.add.duplicate.check)){
  r <- mosaic.map.add.duplicate.check[i,]
  if(r$duplicate){
    possible.swaps <- mosaic.map.add.duplicate.check %>%
      filter(abs(color - r$color) <= match.threshold, r$file != file)
    
    swapped <- FALSE
    while(!swapped){
      rs <- possible.swaps %>% slice_sample(n=1)
      r.copy <- r
      rs.copy <- rs
      
      j <- which(mosaic.map.add.duplicate.check$row == rs$row & mosaic.map.add.duplicate.check$col == rs$col)
      
      
      r.copy$file <- rs$file
      #r.copy$color <- rs$color #this was the wrong call, changes tile position color
      
      rs.copy$file <- r$file
      
      mosaic.map.add.duplicate.check[i,] <- r.copy
      mosaic.map.add.duplicate.check[j,] <- rs.copy
      
      # this doesn't work because it doesn't count the current item?
      safe <- !check.if.duplicates(r.copy, mosaic.map.add.duplicate.check) &&
        !check.if.duplicates(rs.copy, mosaic.map.add.duplicate.check)
      
      if(safe){
        r.copy$duplicate <- FALSE
        rs.copy$duplicate <- FALSE
        mosaic.map.add.duplicate.check[i,] <- r.copy
        mosaic.map.add.duplicate.check[j,] <- rs.copy
        swapped <- TRUE
      } else {
        # undo the swap
        mosaic.map.add.duplicate.check[i,] <- r
        mosaic.map.add.duplicate.check[j,] <- rs
        
      }
    }
  }
  pb$tick()
}

## iterate through each row of the mosaic and build a row image
mm.final <- mosaic.map.add.duplicate.check

pb <- progress_bar$new(total=max(mm.final$row))
for(r in 1:max(mm.final$row)){
  mosaic.row <- mm.final %>% filter(row == r)  
  imgs <- lapply(mosaic.row$file, image_read)
  target.colors <- mosaic.row$color
  
  #row.stack <- adjust.brightness(imgs[[1]], target.colors[1])
  row.stack <- imgs[[1]]
  for(x in 2:length(imgs)){
    row.stack <- c(row.stack, adjust.brightness(imgs[[x]], target.colors[x], 0.25))
    #row.stack <- c(row.stack, imgs[[x]])
  }
  
  row.image <- image_append(row.stack)
  
  if(r < 10){
    n <- paste0("0", r)
  } else {
    n <- r
  }
  
  image_write(row.image, paste0("rows/", n, ".jpg"))
  
  pb$tick()
}


## Join all the row images together for the final image!

row_image_paths <- list.files("rows", pattern = ".jpg", full.names = TRUE)

row_images <- lapply(row_image_paths, image_read)

final_image_layers <- row_images[[1]]
 
pb <- progress_bar$new(total=length(row_images))
for(x in 2:length(row_images)){
  final_image_layers <- c(final_image_layers, row_images[[x]])
  pb$tick()
}

final_image <- image_append(final_image_layers, stack=TRUE)

image_write(final_image, paste0("final_test_",original.image))
