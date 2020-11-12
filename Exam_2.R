library(tidyverse)
library(shiny)
library(stringr)
library(rvest)
library(data.table)
library(openintro)

# With more time, I would have liked to add the pictures from the following page, but I was unable to figure out
# the loop to read in the images and the subtleties of the "magick" package
# https://www.kaggle.com/kvpratama/pokemon-images-dataset
# I would have also liked to fill in some of the descriptions I was unable to scrape


# Read in my data, clean it up a bit
poke.data.comb <- read_csv("poke.data.comb.csv")
poke.data.comb$desc.2 <- ifelse(is.na(poke.data.comb$desc.2), "", poke.data.comb$desc.2)
poke.data.comb <- poke.data.comb[,3:25]

ui <- fluidPage(
  # Title/Introduction
  h1("Exam 2 Submission", align = "center"),
  h4("Andrew Kazal", align = "center"),
  HTML("<br/>"),
  h4("Welcome to an interactive webpage on Pokemon! I hope you enjoy exploring the features! To get the most out
     of the app, please make your window as large as possible.", align = "center"),
  
  
  # Segment my app for more clarity
  HTML("<br/>"),
  HTML('<hr style="color: blue;">'),
  
  # Question 1
  h3("Explore Some Pokémon"),
  HTML("<br/>"),
  p("In this section, you can select a Pokemon and see a description taken from the 'Bulbapedia' wesbite.",
    "For an example, check out the page on Charmander:",
    a("https://bulbapedia.bulbagarden.net/wiki/Charmander_(Pokemon)"), # supposed to create a hyperlink
    br(),
    "For some of the Pokemon, there is no description. These Pokemon were troublesome when gathering description data."),
  selectInput("selected.poke", "Please select a Pokemon:", 
              choices = poke.data.comb$Name, 
              selected = "Bulbasaur"),
  textOutput("selected.poke.1"),
  textOutput("selected.poke.2"),
  textOutput("selected.poke.3"),
  
  
  # Segment my app for more clarity
  HTML("<br/>"),
  HTML('<hr style="color: blue;">'),
  
  
  # Question 2
  sidebarLayout(
    
    # Here's the text and general information
    mainPanel(
      h3("Learn About Your Favorite Pokemon"),
      p("In this section, various visuals will tell you about your favorite Pokemon. Enjoy!"),
      # Add a portion telling the user about the different attributes for their pokemon in general
      selectInput("fav.selected.poke", "Please select your favorite Pokemon:", 
                  choices = poke.data.comb$Name,
                  selected = "Bulbasaur"),
      textOutput("fav.poke.1"),
      textOutput("fav.poke.type"),
      HTML("<br/>"),
      p("Here is some other information about your favorite Pokemon:"),
      tableOutput("fav.poke.gen")),
    
    # Here's the table
    sidebarPanel(textOutput("fav.poke.3"),
               HTML("<br/>"),
               tableOutput("random.5.type1"))),

  
  # Create some separation for clarity
  HTML("<br/>"), # does the same thing as:
  br(), # this line, above is the HTML code behind br()
  br(),
  
  
  # Question 3
  
  # Overview
  textOutput("first.plot.desc"),
  HTML("<br/>"),
  br(),
  
  # Graph
  plotOutput("best.graph"),
  
  
  
  
  
  # More page spacing
  br(),
  br(),
  br(),
  br(),
  
  
  
  
  # Question 4
  
  # Introduction
  p("The next graph shows a linear model predicting HP using Speed with all Pokemon plotted as fitted points."),
  br(),
  
  # Graph
  plotOutput("lin.model"),
  
  # Page spacing
  br(),
  br(),
  
  # Description of coefficients and table of coefficients as well as p-value
  sidebarLayout(
    mainPanel(
      HTML("<br/>"),
      p("The linear model shown in the graph above has the coefficients shown in the table to the right.
        In this model, the first coefficient represents the intercept, and the second coefficient 
        repesents the slope. Generally, the intercept can be interpreted as the predicted HP value at
        Speed = 0 for some Pokemon. Additionally, the slope can be interpreted as: for a one unit increase in Speed,
        a Pokemon is predicted to have 'Coefficient 2' more HP, holding all else constant. In the context of this
        data and estimated model, a Pokemon with no speed is predicted to have an HP of approximately 58. And, for
        a one unit increase in speed, a Pokemon is predicted to have around 0.15 more HP, all else equal. While this
        estimate may be precise and highly statistically significant, it is fairly small. To get a sense for the effect,
        for a Pokemon with a somewhat average speed of 75, this Pokemon is predicted to have an HP of about 70. 
        This finding is consistent with the idea that a Pokemon with a higher Speed is probably well rounded in statistics
        for most other variables (e.g. defense, attack, etc.).")),
    sidebarPanel(
      tableOutput("coeff.table")))
  
  
  
  
)

server <- function(input, output, session) {
  
  # Question 1
  
  # I want to make some reactive text
  
  # Which Pokemon selected
  output$selected.poke.1 <- renderText({
    paste("You have selected ", input$selected.poke, ". ", sep = "")
  })
  
  # Output the description(s) associated with that Pokemon
  output$selected.poke.2 <- renderText({
    selected.poke.data <- poke.data.comb %>% 
      filter(Name == input$selected.poke)
    selected.poke.data$desc.1
  })
  output$selected.poke.3 <- renderText({
    selected.poke.data <- poke.data.comb %>% 
      filter(Name == input$selected.poke)
    selected.poke.data$desc.2
  })
  
  
  
  # Question 2
  
  # Again, want some reactive text
  
  # Which Pokemon favorite
   output$fav.poke.1 <- renderText({
    paste("Your favorite Pokemon is ", input$fav.selected.poke, ".", " This Pokemon is the following type:", sep = "")
  })

  # What type of Pokemon
  output$fav.poke.type <- renderText({
    fav.poke.type1 <- poke.data.comb %>% 
      filter(Name == input$fav.selected.poke) %>% 
      select(`Type 1`)
    fav.poke.type1$`Type 1`
  })
  
  # Give some general information about favorite Pokemon
  output$fav.poke.gen <- renderTable({
    gen.poke.tab <- poke.data.comb %>% 
      filter(Name == input$fav.selected.poke)
    gen.poke.tab[,3:13]
  })
  
  # Create a header for table
  output$fav.poke.3 <- renderText({
    paste("Here are some other Pokemon you may also be interested in:", " ", sep = )
  })
  
  # Create the table
  output$random.5.type1 <- renderTable({
    # First I am going to narrow my data table down to the favorite pokemon
    fav.poke.type1 <- poke.data.comb %>% 
      filter(Name == input$fav.selected.poke)
    
    # Now I will use my complete dataset to take all pokemon with that exact type
    poke.data.to.sample <- poke.data.comb %>% 
      filter(`Type 1` == as.character(fav.poke.type1[3]))
    
    # Now I will remove the favorite pokemon from the dataset so that the favorite pokemon is not 
    # accidentaly recommended
    poke.data.to.sample <- poke.data.to.sample[!(poke.data.to.sample$Name == input$fav.selected.poke),]
    
    # And finally, I sample 5 and display the names
    sampled.poke <- poke.data.to.sample[sample(nrow(poke.data.to.sample), 5), 2]
    sampled.poke
  })
  
  
  
  # Question 3
  
  # Introduction
  output$first.plot.desc <- renderText({
    paste("The following graph automatically finds the best statistic for", input$fav.selected.poke,
    "and then shows a dot indicating where", input$fav.selected.poke, "ranks relative to other Pokemon in that statistic. The
    shaded area behind the dot shows the disribution of the other pokemon.")
  })
  
  # Plot
  output$best.graph <- renderPlot({
    
    # Split into two datasets to make graphing and labeling easier
    name.poke <- as.character(input$fav.selected.poke)
    poke.user <- poke.data.comb %>%
      filter(Name == input$fav.selected.poke) %>% 
      mutate(Color = name.poke)

    poke.not.user <- poke.data.comb %>%
      mutate(Color = "Other Pokemon")
    
    poke.both <- full_join(poke.not.user, poke.user)

    
    # Create the correct graph that is conditional on which attribute the Pokemon performs highest
    # Need a series of if, else if statements
    if(poke.user["best.hp"]==1){
      ggplot(data = poke.both, aes(x = HP, fill = Color)) +
        geom_density(data = poke.not.user,
                     aes(x = HP)) +
        geom_dotplot(poke.user,
                     method = "histodot",
                     mapping = aes(x = HP),
                     binwidth = 4,
                     dotsize = 1) +
        labs(title = paste(input$fav.selected.poke, "'s HP Relative to HP for Other Pokemon", sep = "")) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_y_continuous(limits = c(0, 0.02))
    }
    
    else if(poke.user["best.atk"]==1){
      ggplot(data = poke.both, aes(x = Attack, fill = Color)) +
        geom_density(data = poke.not.user,
                     aes(x = Attack)) +
        geom_dotplot(poke.user,
                     method = "histodot",
                     mapping = aes(x = Attack),
                     binwidth = 4,
                     dotsize = 1) +
        labs(title = paste(input$fav.selected.poke, "'s Attack Relative to Attack for Other Pokemon", sep = "")) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_y_continuous(limits = c(0, 0.015))
      
    }
    
    else if(poke.user["best.def"]==1){
      ggplot(data = poke.both, aes(x = Defense, fill = Color)) +
        geom_density(data = poke.not.user,
                     aes(x = Defense)) +
        geom_dotplot(poke.user,
                     method = "histodot",
                     mapping = aes(x = Defense),
                     binwidth = 4,
                     dotsize = 1) +
        labs(title = paste(input$fav.selected.poke, "'s Defense Relative to Defense for Other Pokemon", sep = "")) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_y_continuous(limits = c(0, 0.015))
      
    }
    
    else if(poke.user["best.sp.atk"]==1){
      ggplot(data = poke.both, aes(x = `Sp. Atk`, fill = Color)) +
        geom_density(data = poke.not.user,
                     aes(x = `Sp. Atk`)) +
        geom_dotplot(poke.user,
                     method = "histodot",
                     mapping = aes(x = `Sp. Atk`),
                     binwidth = 4,
                     dotsize = 1) +
        labs(title = paste(input$fav.selected.poke, "'s Special Attack Relative to Special Attack for Other Pokemon", sep = "")) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_y_continuous(limits = c(0, 0.015))
    }
    
    else if(poke.user["best.sp.def"]==1){
      ggplot(data = poke.both, aes(x = `Sp. Def`, fill = Color)) +
        geom_density(data = poke.not.user,
                     aes(x = `Sp. Def`)) +
        geom_dotplot(poke.user,
                     method = "histodot",
                     mapping = aes(x = `Sp. Def`),
                     binwidth = 4,
                     dotsize = 1) +
        labs(title = paste(input$fav.selected.poke, "'s Special Defense Relative to Special Defense for Other Pokemon", sep = "")) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_y_continuous(limits = c(0, 0.015))
    }
    
    else if(poke.user["best.spd"]==1){
      ggplot(data = poke.both, aes(x = Speed, fill = Color)) +
        geom_density(data = poke.not.user,
                     aes(x = Speed)) +
        geom_dotplot(poke.user,
                     method = "histodot",
                     mapping = aes(x = Speed),
                     binwidth = 4,
                     dotsize = 1) +
        labs(title = paste(input$fav.selected.poke, "'s Speed Relative to Speed for Other Pokemon", sep = "")) +
        theme(plot.title = element_text(hjust = 0.5)) + 
        scale_y_continuous(limits = c(0, 0.015))
    }
    
  })
  
  
  
  
  
  
  # Question 4
  
  # Create the linear model, add it to a plot with the scatter of Pokemon
  output$lin.model <- renderPlot({
    
  # Split the data
    name.poke <- as.character(input$fav.selected.poke)
    poke.graph <- poke.data.comb %>%
      filter(Name == input$fav.selected.poke) %>%
      mutate(Color = name.poke)
    poke.other.graph <- poke.data.comb %>%
      filter(Name != input$fav.selected.poke) %>%
      mutate(Color = "Other Pokemon")
  
  # Create and store the model
  speed.hp.model <- lm(HP ~ Speed, data = poke.data.comb)
  
  # Create the graph
  cols <- c("Other Pokemon" = "#3591d1", "Your Pokemon" = "#f04546")
  ggplot(aes(x = Speed, y = HP), data = poke.data.comb) +
    geom_point(data = poke.other.graph, 
               aes(x = Speed, y = HP, color = "Other Pokemon"), size = 0.66) +
    geom_point(data = poke.graph, 
               aes(x = Speed, y = HP, color = "Your Pokemon", label = poke.graph$Name), size = 5) +
    scale_colour_manual(name="Key",values=cols) + 
    geom_abline(intercept = speed.hp.model$coefficients[[1]],
                slope = speed.hp.model$coefficients[[2]],
                col = "black") +
    labs(title = paste("Linear Model Predicting HP Using Speed (", input$fav.selected.poke, " Highlighted)", sep = "")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(data = poke.graph, aes(label = Name), hjust = 0, nudge_x = 2, nudge_y = 2, angle = 30)
  })
  
  # Create the table of coefficients and p-value from the model
  output$coeff.table <- renderTable({
    speed.hp.model <- lm(HP ~ Speed, data = poke.data.comb)
    
    table.of.coeff <- data.table(speed.hp.model$coefficients[[1]],
                        speed.hp.model$coefficients[[2]],
                        summary(speed.hp.model)$coefficients[2,4])
    colnames(table.of.coeff) <- c("Coefficient 1 - Intercept", "Coefficient 2 - Slope", "p-value for Coefficient 2")
    
    table.of.coeff
    
  })
  }

shinyApp(ui, server)