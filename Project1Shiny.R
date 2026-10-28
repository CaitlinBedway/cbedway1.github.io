#5: Shiny App

library(shiny)
library(tidyverse)
library(readxl)
# Import the data into an object called frogs

frogs <- read_excel("CalaverasData.xls")

frogs <-  rename(frogs,
                 day = Day,
                 frog_type = `rent/ind/pro`,
                 distance = `jump distance`,
                 jump_n = `jump #`,
                 distance_rel = `Relative to jump #1`,
                 distance_3 = `3-jump dist`,
                 distance_3_off = `measured 3-jump`,
                 duration = `jump duration`,
                 angle_00 = unknown1,
                 angle_01 = angle,
                 angle_10 = unknown2,
                 velocity_00 = unknown3,
                 velocity_01 = Vel,
                 velocity_10 = unknown4
) %>%
  mutate(
    row = seq_len(nrow(.))
  ) %>%
  select(row, everything())

#Chunk #3
frogs <- frogs %>%
  mutate(jump_n = as.integer(jump_n))

#Chunk #4
## frog_type
frogs <- frogs %>%
  mutate(
    frog_type2 = case_when(
      frog_type == 1 ~ "rental",
      frog_type == 2 ~ "individual",
      frog_type == 3 ~ "pro",
      frog_type < 0 ~ "unknown"
    )
  )

rent_frogs <- frogs |>
  filter(frog_type2 == "rental") |>
  mutate(distance_m = distance/100)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Analyzing How Different Sample Sizes of Frogs Effect Jump Distance Probabilities"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Sample Size of:",
                  min = 1,
                  max = 100,
                  value = 50),
      numericInput("distance_m",
                   "Distance jumped (m):",
                   min = 0.0001,
                   max = 3,
                   value = 1.6)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(textOutput("myText"),
              plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$myText <- renderText({
    set.seed(3470)
    n <- input$n
    distance_m <- input$distance_m
    myrentsim <- replicate(5000, max(sample(rent_frogs$distance_m, n, replace=FALSE)))
    prob <- mean(myrentsim >= distance_m)
    paste(" 1) The sample size of frogs jumping is", n, 
          "2) the distance selected is", distance_m, "m",
          "3) and the probability of frogs hitting/going over that distance is ", prob)})
  
  
  output$distPlot <- renderPlot({
    set.seed(3470)
    n <- input$n
    distance_m <- input$distance_m
    myrentsim <- replicate(5000, max(sample(rent_frogs$distance_m, n, replace=FALSE)))
    prob <- mean(myrentsim >= distance_m)
    
    mytitle <- paste("Histogram Analyzing Sample Size of Frogs and Probability of Making Jumps")
    
    ggplot(data.frame(myrentsim), aes(x = myrentsim)) + geom_histogram() +
      labs(title=mytitle,
           x = "Jump distance (m)")
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)


