#library(ggimage)
library(dplyr)
library(ggplot2)
library(sf) #to make hexgrid
library(ggpattern)

source("hex_grid_funs.R")

# get images of treatments
img <- list.files("images",
                  pattern="png", full.names=TRUE)


plot_save_lsw <- function(obj, filename="out.jpg", title=""){
  ggplot(obj) + geom_sf_pattern( mapping = aes(pattern_filename =img),
                                 pattern= 'image',
                                 pattern_type = "expand",
                                 pattern_filename = obj$img,
                                 linewidth=0) +
    theme_void()+
    ggtitle(title)
  
  ggsave(filename = paste0("plans/", filename), 
         width = 2024, height = 768, units = "px")  
}

# Urban Wild	2 panel sites, 3 rows, 4 blocks, 5 treatments
# n = 4 per panel type per row, 20 wide total
# stratified random
set.seed(31415)
uw_1 <- make_lsw(blocks = 4, treatments = 5, height = 3, img = img)
uw_2 <- make_lsw(blocks = 4, treatments = 5, height = 3, img = img)

plot_save_lsw(uw_1, "urban_wild_west.jpg", "Urban Wild West Living Seawall")
plot_save_lsw(uw_2, "urban_wild_east.jpg", "Urban Wild East Living Seawall")


# Seaport	1 panel site, 6 rows, 4 blocks, 5 treatments
# n = 4 per panel type per row, 20 wide total
# stratified random
set.seed(31415)
sp_1 <- make_lsw(blocks = 4, treatments = 5, height = 6, img = img, stopping = 300)
plot_save_lsw(sp_1, "seaport_fan_pier.jpg", "Seaport Fan Pier Living Seawall")


