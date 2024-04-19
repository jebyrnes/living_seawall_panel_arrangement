library(shiny)
library(dplyr)
library(ggplot2)
library(sf) #to make hexgrid
library(ggpattern)
library(shinythemes)

source("hex_grid_funs.R")



# get images of treatments
img <- list.files("images",
                  pattern="png", full.names=TRUE)

make_img_names <- function(img = img){
  out <- gsub("images\\/(.*)\\.png$", "\\1", img)

  gsub("([a-z])([A-Z])", "\\1 \\2", x=out)
}

img_html <- paste0(make_img_names(img), "<br> <img src=",img," width=40 height=40>")
img_html <- lapply(img_html, HTML)

ui <- fluidPage(
  theme = shinytheme("united"),
  
  titlePanel("Make a Living Seawall", 
             windowTitle = "Make a Living Seawall"),
  
  #HTML(img_html),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("heights", "Number of Tidal Heights", value = 3, min = 1, max = 15),
      numericInput("blocks", "Number of Blocks", value = 2, min = 2, max = 15),
      
      br(),
      checkboxGroupInput("panels", 
                         HTML("Choose Panels to Use <br>(must use at least 4)"),
                         selected = 1:length(img),
                         choiceNames = img_html,
                         choiceValues = 1:length(img)),
      
      br(),
      hr(),
      
      checkboxInput("has_seed", "Enter wall design number"),
      
      uiOutput("seed_set"),
      
      actionButton("button", "Generate Living Seawall")
      
    ),
    mainPanel(
      plotOutput("wall")
    )
  ),
  
  fluidRow(
    br(),br(),
    #img(src='nasa-logo-web-rgb_small.jpg', align = "left", height = 50),
    img(src='umb_logo.png', align = "left", height = 50),
    HTML("&nbsp; &nbsp; App from the <a href=https://stonelivinglab.org/>Stone Living Lab</a> in collaboration with <a href=https://www.livingseawalls.com.au/>Living Seawalls</a>"),
    img(src='lsw_logo.jpg', align = "left", height = 50)
  )
)

server <- function(input, output){
  
  wall_obj <- 
    eventReactive(input$button,
                  {
                    if(input$has_seed){
                      set.seed(input$seed_set)
                    }
                    print(input$panels)
                    
                    make_lsw(blocks = input$blocks, 
                       treatments = length(input$panels), 
                       height = input$heights, 
                       img = img[as.numeric(input$panels)])
  })
  
  output$seed_set <- renderUI({
    if(input$has_seed){
      textInput("seed", "Design seed number", value = 31415)
    }else{
      NULL
    }
  })
  
  output$wall <- renderPlot({
    ggplot(wall_obj()) + geom_sf_pattern( mapping = aes(pattern_filename =img),
                                   pattern= 'image',
                                   pattern_type = "expand",
                                   pattern_filename = wall_obj()$img,
                                   linewidth=0) +
      theme_void()
  })
  
}

shinyApp(ui, server)
