#library(ggimage)
library(dplyr)
library(ggplot2)
library(sf) #to make hexgrid
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

source("hex_grid_funs.R")

img <- list.files("images",
                  pattern="png", full.names=TRUE)

# wrapper
make_lsw <- function(blocks, treatments, heights, img){
  l <- make_layout(blocks = blocks, heights = heights, img = img)
  g <- make_hexgrid(blocks = blocks, 
                    treatments = treatments, 
                    heights = heights)
  
  #return as sf object
  left_join(l, g) |>
    st_as_sf()
}


# function to sample from images
# for a randomization within a block
make_section <- function(j = 5) sample(img, j, replace = FALSE) 



make_layout <- function(blocks, 
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
    # # add 0.3 to even cols, as they will be spaced up a bit
    # mutate(row = height,
    #        row = row + ifelse(column/2 - column %/% 2 == 0.5,
    #                     offset,
    #                     0))  |>
    arrange(block, height) |>
    mutate(panel = 1:n())
}

# # test
# make_layout(blocks = 1, heights = 2, offset = 0.3) |>
# ggplot(aes(x = column, y = row)) +
#   geom_image(aes(image=img), size = 0.22) +
#   scale_x_continuous(limits = c(0.5, 20.5), expand = c(0.1,0.2))+
#   scale_y_continuous(limits = c(0, 4), expand = c(0,0)) 

# Urban Wild	2 panel sites, 3 rows, 20 wide, n = 4 per panel type per row with 5 types
# random
uw_1 <- make_layout(blocks = 4, heights = 3)
grid_1 <- make_hexgrid(blocks = 4, treatments = 5, heights = 3)
d_1 <- left_join(uw_1, grid_1) |>
  st_as_sf()


ggplot(d_1) + geom_sf()  + geom_sf_text(aes(label = id))
ggplot(d_1) + geom_sf()  + geom_sf_text(aes(label = height))
ggplot(d_1) + geom_sf()  + geom_sf_text(aes(label = block))
ggplot(d_1) + geom_sf_pattern( pattern= 'image',
                                pattern_type = "expand",
                                pattern_filename = d_1$img,
                                linewidth=0)  

ggsave(filename = "plans/uw_1a.jpg", width = 2024, height = 768, units = "px")          


for(i in 1:10){
d <- make_lsw(blocks = 4, treatments = 5, height = 3, img = img)
ggplot(d)+ geom_sf_pattern( mapping = aes(pattern_filename =img),
                             pattern= 'image',
                             pattern_type = "expand",
                             pattern_filename = d$img,
                             linewidth=0) +
  theme_void()

ggsave(filename = paste0("plans/uw_", i, ".jpg"), 
       width = 2024, height = 768, units = "px")          

}

#merge grid and treatments


uw_2 <- make_layout(blocks = 4, heights = 6, offset = 0.3)

ggplot(uw_1, aes(x = column, y = row)) +
  geom_image(aes(image=img), size = 0.22) +
   scale_x_continuous(limits = c(0.5, 20.5), expand = c(0.1,0.2))+
   scale_y_continuous(limits = c(0, 4), expand = c(0,0)) 

ggsave(filename = "plans/uw_1.jpg", width = 2024, height = 768, units = "px")          
