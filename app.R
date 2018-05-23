# https://therbootcamp.github.io/_sessions/D3S3_PlottingII/PlottingII_practical_answers.html
#

library(shiny)
library(Rmisc)
library(rsconnect) 

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         
         selectInput(inputId = "bincolor", label = "Color",
                     choices = c("red","yellow","blue","green","black"), selected = "green"),
         
         checkboxInput(inputId = "addmean", label = "Add mean line?", value = FALSE),
         checkboxInput(inputId = "addCI", label = "Add mean and 95% CI?", value = FALSE),
         checkboxInput(inputId = "addIQR", label = "Add median and interquartile range?", value = FALSE)     
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Histogram", plotOutput("distPlot")),  
                        tabPanel("Summary",verbatimTextOutput("summary")),                            
                        tabPanel("Description", 
                                 h3("Source: "),
                                 p(" R datasets from "),
                                 p("Hardle, W. (1991) Smoothing Techniques with Implementation in S. New York: Springer"))                         
            )
            
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

      
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = input$bincolor, border = 'white',
           main = "Old Faithful Geyser Data", xlab = "Time Between Eruptions (mins)") 
      
      if(input$addmean) {  # If the addmean box is checked...
            
            # Add a vertical line at the mean of x
            abline(v = mean(x),
                   lwd = 2,      # Thickness
                   lty = 1)      # Solid line
            
      } # close if statement
      
      if(input$addCI) {  # If the addCI box is checked...
            
            # Add vertical lines for the 95% CI
            abline(v = CI(x, ci = 0.95),
                   lwd = 2,      # Thickness
                   lty = 2)      # Dashed line
            
      } # close if statement     
      
      if(input$addIQR) {  # If the addIQR box is checked...
            
            # Add vertical lines for the 1Q,Median,3Q
            abline(v = summary(x)[c(2,3,5)],
                   lwd = 2,      # Thickness
                   lty = 3)      # Dotted line
            
      } # close if statement     
      
   })
   
# for the summary tab
   output$summary <- renderPrint({
         x <- faithful[, 2]             
         summary(x)
   })    
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

