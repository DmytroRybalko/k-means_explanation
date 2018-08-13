
library(shiny)
library(tidyverse)
library(gganimate)
library(animation)

source("helper_functions.R")

#################
### Get data:
#################
# My dataset as tibble of two variables
set.seed(1234)
x1 <- rnorm(12, mean = rep(1:3, each = 4), sd = 1)
y1 <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 1)
my_df_xy <- tibble(x = x1, y = y1) %>%
  rowid_to_column()

# Dataset from Roger D. Peng's book "Exploratory Data Analysis with R"
x2 <- c(0.75, 1.1, 1.25, 0.55, 2.15, 2.17, 1.85, 1.86, 2.87, 2.75, 2.91, 2.71)
y2 <- c(0.6, 1.0, 1.25, 1.0, 1.9, 1.8, 1.85, 2.5, 1.0, 0.85, 0.86, 1.18)
eda_df_xy <- tibble(x = x2,  y = y2) %>%
  rowid_to_column()
# initial clusters: Cx = c(1, 1.7, 2.5); Cy = c(2, 1.0, 1.5)

# Set list of all datasets for simulations
df_xy <- list()
df_xy$"my_dataset" <- my_df_xy
df_xy$"eda_dataset" <- eda_df_xy

# Set widht and heigth for animation 
ani_height = 480
ani.options(ani.width = 700)

# Set theme for animated plots
theme_ani <- theme(
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  legend.title = element_text(face = "bold", color = "black", size = 12),
  legend.text = element_text(size = 11, color = "black"),
  legend.position = c(1, 1),
  legend.justification = c(1, 1),
  legend.background = element_blank(),
  axis.text = element_text(size = 11, colour = "black"),
  axis.title = element_text(size = 12, face = "bold"))

###############################################################################
### Define UI ########

ui <- fluidPage(

  fluidRow(
    column(3,
           #br(),
           wellPanel(
             h4(strong("How k-means work")),
             
             helpText("Here you can choose predefine, input your own or simulate randome dataset to see k-means in action."),
             
             selectInput("dataset", 
                         label = "Choose dataset",
                         choices = c("my_dataset",
                                     "eda_dataset",
                                     "[input dataset]",
                                     "[generate dataset]"),
                         selected = "eda_dataset"),
             
             # Generate dataset
             conditionalPanel(
               condition = "input.dataset == '[generate dataset]'",
               sliderInput("n_observ", "Number of observations", min = 10, max = 50, value = 12)
               ),
             
             # User input dataset
             conditionalPanel(
               condition = "input.dataset == '[input dataset]'",
               textInput("user_x",
                         "Set x coordinates of your dataset: ",
                         value = "1, 3, 5",
                         placeholder = "1, 1.7, 2.5, ..."),
               textInput("user_y",
                         "Set y coordinates of your dataset: ",
                         value = "2, 4, 6",
                         placeholder = "2, 1.0, 1.5, ...")
             ),
             
             # Choose a number of clusters
             textInput("clust_x",
                       "Set x coordinates of initial clusters :",
                       value = "1, 1.7, 2.5", placeholder = "1, 1.7, 2.5"),
             
             textInput("clust_y",
                       "Set y coordinates of initial clusters :",
                       value = "2, 1.0, 1.5", placeholder = "2, 1.0, 1.5"),
             
             # Choose a number of iteration
             numericInput("n_iter", "Choose a number of iteration:", 5, min = 2, max = 10),
             
             h5(strong("The initial location of the cluster centers")),
             
             imageOutput("start_chart"),
             
             radioButtons("scale", label = NULL,
                          list("Optimal scale" = "opt",
                               "Equal scale" = "eql"),
                          selected = "opt", inline = TRUE),
             
             br(),
             actionButton("run","Run k-means")
           )
      
    ), #column sidebar
    
    column(6,
           imageOutput("kmeans_chart", height = "100%"),
             
           imageOutput("tot_sum_chart", height = "100%")
      ),
    
    column(3,
           htmlOutput("my_kmeans"),
           
           htmlOutput("kmeans")
    )
  )
)
  
server <- function(input, output, session) {
   
   # Make cluster tibble
   df_cl <- reactive({
       # Get vector with x and y coordinates of clusters
       Cx <- as.numeric(unlist(strsplit(input$clust_x, ",")))
       Cy <- as.numeric(unlist(strsplit(input$clust_y, ",")))
       
       # Number of clusters define as length of Cx or Cy vectors
       n_clust <- length(Cx)
       
       # Design dataset with cluster's coordinates and identification numbers
       df_cl <- tibble(Cluster = factor(seq(n_clust), ordered = T), Cx, Cy)
   })
   
   # Make observation tibble
   observ <- reactive({
     if (input$dataset == "[generate dataset]") {
       tibble(x = rnorm(input$n_observ, mean = 1, sd = 3.5),
              y = rnorm(input$n_observ, mean = -2, sd = 2.5)) %>%
         rowid_to_column()
     }  
     else if (input$dataset == "[input dataset]") {
       tibble(x = as.numeric(unlist(strsplit(input$user_x, ","))),
              y = as.numeric(unlist(strsplit(input$user_y, ",")))) %>%
         rowid_to_column()
     }
     else { df_xy[[input$dataset]] }
   })
   
   # Make dataset that connect observations with clusters
   df_kmeans0 <- reactive({
     bind_cols(observ(), map_dfr(df_cl(), rep, length.out = dim(observ())[1]))
        }
   )
   
   # Show initial clusters
   output$start_chart <- renderPlot({

     if (input$scale == "opt") {
       ggplot(df_kmeans0(), aes(x = x, y = y)) +
         geom_point(shape = 1, size = 5, stroke = 1) +
         geom_text(aes(x = x, y = y, label = rownames(df_kmeans0())),
                   vjust = 2.0, size = 4, color = "black") +
         geom_point(aes(x = Cx, y = Cy, color = Cluster),
                    shape = 3, size = 5, stroke = 1) +
         labs(x = NULL, y = NULL) +
         theme_classic() +
         theme(legend.position = "top",
               legend.title = element_text(face = "bold", size = 12),
               legend.text = element_text(size = 12, face = "bold"),
               legend.background = element_rect(fill = "white", colour = "black"))
     } else {
       ggplot(df_kmeans0(), aes(x = x, y = y)) +
         geom_point(shape = 1, size = 5, stroke = 1) +
         geom_text(aes(x = x, y = y, label = rownames(df_kmeans0())),
                   vjust = 2.0, size = 4, color = "black") +
         geom_point(aes(x = Cx, y = Cy, color = Cluster),
                    shape = 3, size = 5, stroke = 1) +
         labs(x = NULL, y = NULL) +
         theme_classic() +
         theme(legend.position = "top",
               legend.title = element_text(face = "bold", size = 12),
               legend.text = element_text(size = 12, face = "bold"),
               legend.background = element_rect(fill = "white", colour = "black")) +
         coord_fixed()
     }
     })
   ##########################################################################
   ## 2. Run k-means algorithm
   
   ## a) Get dataset with k-means iterations 
   
   df_kmeans <- eventReactive(input$run, {
     wraper_kmeans(observ(), df_cl(), input$n_iter)
   })
   
   ## b) View main results of k_means algorithm for chosen iteration

   my_kmeans <- eventReactive(input$run, {
     get_kmeans(input$n_iter, df_kmeans())
   })

   output$my_kmeans <- renderUI(
     HTML(paste("<h5><strong>Result of implemented k-means:</strong></h5>"),
       paste(
         c("<pre>", capture.output(print(my_kmeans())), "</pre>"),
         collapse = "<br>")
     )
   )
   
   ## c) Get result from build-in k-means() function
   
   r_kmeans <- eventReactive(input$run, {
       kmeans(observ() %>% select(-rowid),
              centers = cbind(as.numeric(unlist(strsplit(input$clust_x, ","))),
                              as.numeric(unlist(strsplit(input$clust_y, ",")))),
              iter.max = input$n_iter)
   })
   
   # output as in .Rmd file
   output$kmeans <- renderUI(
     HTML(paste("<h5><strong>Result of buil-in k-means:</strong></h5>"),
       paste(
         c("<pre>", capture.output(print(r_kmeans())), "</pre>"),
         collapse = "<br>")
          )
   )
   
   ############################################################################
   ## 3. Visualize results
   
   ## a) how k-means work
   output$kmeans_chart <- renderImage({

      ggplot(map_dfr(df_kmeans(), ~ .), aes(x, y, color = Cluster, frame = Iteration)) +
        geom_point(aes(group = Cluster), size = 5) +
        geom_point(aes(x = Cx, y = Cy), shape = 3, size = 5, color = "black", stroke = 1) +
        geom_path(aes(x = Cx, y = Cy, cumulative = TRUE, group = Cluster),
                   color = "black") +
        geom_text(aes(x, y, label = rowid), vjust = 1.8, size = 4, color = 'black') +
        geom_polygon(data = df_polygon(df_kmeans()),
                    aes(x, y, group = Cluster, fill = Cluster), alpha = .5) +
        coord_equal() +
        labs(title = "How k-means work:") +
        theme_ani -> gg
     
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      # Make animation responsive then resize window
      width <- session$clientData$output_kmeans_chart_width
      ani.options(ani.width = width, ani.height = ani_height)
      
      gganimate(gg, "outfile.gif")
     
      # Return a list containing the filename
      list(src = "outfile.gif",
          contentType = 'image/gif',
          width = width,
          height = ani_height,
          deleteFile = TRUE)
     })
   
   ## b) Total within-cluster sum of square
   output$tot_sum_chart <- renderImage({
       
       df_long <- map_dfr(df_kmeans(), ~ .) 
       df_long %>% 
         group_by(Iteration) %>% 
         summarise(tot_sum = sum(Eu_dist)) %>% 
         ungroup() %>% 
         ggplot(aes(x = Iteration, y = tot_sum, frame = Iteration, cumulative = TRUE)) +
            geom_bar(stat = 'identity', aes(fill = Iteration)) +
            labs(x = NULL, title = "Total within-cluster sum of square on:") +
            theme_ani -> gg_tot_sum
       
       outfile <- tempfile(fileext='.gif')
       width <- session$clientData$output_tot_sum_chart_width
       gganimate(gg_tot_sum,"outfile2.gif")
       list(src = "outfile2.gif",
            contentType = 'image/gif',
            width = width,
            height = ani_height,
            deleteFile = TRUE)
   })
 } #server
 
# Run the application 
shinyApp(ui = ui, server = server)
# runApp("shiny_app")
