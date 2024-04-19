

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

for(i in 1:10){
  make_lsw(blocks = 4, treatments = 5, height = 3, img = img) |>
    plot_save_lsw(paste0("test_4_3/test_4_3_", i, ".jpg"))
}

for(i in 1:10){
  make_lsw(blocks = 4, treatments = 5, height = 6, img = img) |>
    plot_save_lsw(paste0("test_4_6/test_4_6_", i, ".jpg"))
}
