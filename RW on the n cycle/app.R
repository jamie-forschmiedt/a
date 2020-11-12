#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Random Walk on the N Cycle"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of vertices:",
                        min = 3,
                        max = 10,
                        value = 6),
            
            numericInput("p",
                         "Probability of moving clockwise:",
                         value=0.5),
            
            numericInput("steps",
                      "Number of steps:",
                      value = 50)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot"),
           plotOutput("ncycle")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
     #   x    <- faithful[, 2]
      #  bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
       # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #})
    
    output$ncycle <- renderPlot({
        nvalue <- input$n
        prob <- input$p
        nsteps <- input$steps
        
        ncycle <- function(n, p, s) {
            numSteps <- rep(0, n) #vector of the number of visits to each state (initialized to 0 for all states)
            
            ## starting state:
            x = floor(runif(1)*n + 1)
            
            ## run the walk for s steps
            for (i in 1:s) {
                if (runif(1) < p) { #with probability p
                    if (x != n) { #if the current state is not n
                        x = x + 1 #move up 1
                    } else { #if the current state is n
                        x = 1 #move to 1
                    }
                } else { #with probability 1-p
                    if (x != 1) { #if the current state is not 1
                        x = x - 1 #move down 1
                    } else { #if the current state is 1
                        x = n #move to n
                    }
                }
                numSteps[x] = numSteps[x] + 1 #record the visit to the new state
            } 
            
            # This will create a vector describing the proportion of time the walk spent at each state
            Proportions <- numSteps 
            for (i in 1:length(numSteps)) {
                Proportions[i] = numSteps[i] / s 
            }
            
            # This will create the same vector, but with each entry rounded to 3 decimal places
            ProportionsRounded <- Proportions 
            for (i in 1:n) {
                ProportionsRounded[i] = round(Proportions[i], digits=3)
            }
            
            # Total variation between the proportion of time spent at each state and the stationary distribution:
            # (Stationary distribution will be uniform regardless of p)
            TV <- 0
            for (i in 1:n) {
                TV <- TV + abs(Proportions[i] - (1/n))
            }
            TV = 0.5*TV
            print(paste("Total Variation:", round(TV, digits=3)))
            
            # Bar graph of the proportion of time spent at each state
            cycleplot <- barplot(ProportionsRounded, names=c(1:n), xlab="State", ylab="Proportion of steps",
                                 main=paste("Random walk on the", n, "cycle for", s, "steps, p =", p))
            text(x = cycleplot, y = ProportionsRounded, label = ProportionsRounded, pos=1, cex = 0.8, col = "black")
        }
        ncycle(nvalue, prob, nsteps)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
