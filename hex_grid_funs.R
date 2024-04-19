library(sf) #to make hexgrid
library(dplyr)
library(magic)
library(tidyr)

source("no_adjacency_lsw_funs.R")

# wrapper
make_lsw <- function(blocks, treatments, heights, img,
                     layout = "random_no_adjacency",
                     stopping = 100){
  if(layout=="random_block"){
    l <- make_layout_random_block(blocks = blocks, 
                                  heights = heights, 
                                  img = img)
  }
  
  if(layout == "random_latin"){
    l <- make_layout_random_latin(blocks = blocks, 
                                  heights = heights, 
                                  img = img)
  }
  
  if(layout == "random_no_adjacency"){
    l <- make_layout_no_adjacency(blocks = blocks, 
                                  heights = heights, 
                                  img = img,
                                  stopping = stopping)
  }
  
  g <- make_hexgrid(blocks = blocks, 
                    treatments = treatments, 
                    heights = heights)
  
  #return as sf object
  left_join(l, g) |>
    st_as_sf() |>
    select(-panel)
}


# function to make latin-squares-esque design
make_layout_random_latin <-
  function(blocks = blocks,
           heights = heights,
           img = img) {
    
    treatments <- length(img)
    
    if(heights <= treatments){
      d <-  rlatin(treatments, n = blocks)
    }else{
      d <- rlatin(treatments, n = blocks*(heights))
    }
    
    d |>
      matrix(ncol = treatments, byrow=TRUE) |>
      as_tibble() |>
      mutate(block = rep(1:blocks, n()/blocks),
             height = ceiling(1:n()/blocks)) |>
      #filter step
      filter(height <= heights) |>
      pivot_longer(-c(height, block)) |>
      mutate(
        name = gsub("V", "", name) |> as.numeric(),
        img = img[value]
      )  |>
      arrange(block, height) |>
      mutate(panel = 1:n())
  }



# function to sample from images
# for a randomization within a block
make_section <- function(j = 5) sample(img, j, replace = FALSE) 



make_layout_random_block <- function(blocks, 
                        heights, j = 5, 
                        offset = 0.3,
                        img){
  setup_layout <- tidyr::crossing(
    block = 1:blocks, # 4 blocks n=4 per height
    height = 1:heights #3 tide heights
  ) 
  
  setup_layout |>
    group_by(block, height) |>
    reframe(img = make_section(j = j)) |>
    # add column ID
    group_by(height) |>
    mutate(column = 1:n()) |>
    ungroup() |>
    arrange(block, height) |>
    mutate(panel = 1:n())
}

### width and height of a hexagon
w <- function(x) sqrt(3)*x
h <- function(x) x/sqrt(3)


### function to make hex grid
make_hexgrid <- function(blocks, treatments, heights){
  g <- make_hex(blocks*treatments/2, heights)[1:(blocks*treatments*heights),]
  block_row_frame <- get_row_block(g, treatments, blocks)
  
  left_join(g, block_row_frame) |>
    arrange(block, height)|>
    rename(panel = name)
  
}

make_hex <- function(wi, hi, flat_topped = TRUE){
  wi <- wi-1 #for grid width to get it right
  hi <- 2*(hi-2)+1 # for grid height
  
  x <- st_sfc(st_polygon(list(rbind(c(0,0), 
                                    c(w(wi),0), 
                                    c(w(wi),h(hi)), 
                                    c(0,0)))))
  g <- st_make_grid(x,
                    #n = c(10,3),
                    cellsize = 1, 
                    offset = c(sqrt(3), 0.5),
                    square = FALSE, flat_topped = flat_topped)
  g <- st_as_sf(g)
  g$id <- 1:nrow(g)
  g
}

# #test
# g <- make_hex(5, 3)
# ggplot(g[,]) + geom_sf()  + geom_sf_text(aes(label = id))
# g <- make_hex(10, 3)
get_row_block <- function(g, treatments, blocks){
  st_centroid(g) |> 
    st_coordinates() |>
    as_tibble() |>
    mutate(id = g$id,
           height = safe_floor(Y)+1) |>
    arrange(height, X) |>
    mutate(block = rep(1:blocks, treatments) |> 
             sort() |> rep(max(height)), 
           name = 1:n()) 
  
}


get_block <- function(g, treatments, height){
  st_coordinates(g) |> 
    as_tibble() |> 
    group_by(L2) |> 
    summarize(x = max(X)) |>
    arrange(x)
        mutate(x = ceiling(x)) |>
    rename(id = L2,
           block = x)
}


