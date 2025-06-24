# libraries required
library(shiny)
library(bslib)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bsicons)
library(reshape2)
library(sf)
library(rvest)
library(viridis)
library(ggrepel)
library(ggthemes)

# Load data
load("../../Data/allStateIndicatorsData.Rdata")
load("../../Data/crimeAgainstWomenDistribution.Rdata")
load("../../Data/combinedCrime.Rdata")
load("../../Data/allStateCrimeData.Rdata")
load("../../Data/rsPercentage.Rdata")
load("../../Data/lastFiveYearElection.Rdata")
load("../../Data/stateWisePoliticalData.Rdata")

# setting up the map of India
shp1 <-  read_sf("../../Data/IND_adm/IND_adm1.shp")
shp1 <- shp1%>%
  dplyr::mutate(NAME_1 = if_else(NAME_1 == 'Uttaranchal', 
                                 'Uttarakhand', 
                                 if_else(NAME_1 == 'Orissa', 
                                         'Odisha', 
                                         NAME_1)))

# setting up the theme of the ui
my_theme <- bs_theme(
  bg = "#2B3E50",     # Dark background color
  fg = "#FFFFFF",     # White text
  primary = "#3399FF", # Primary color 
  secondary = "#D9534F",
  success = "#5CB85C",
  info = "#5BC0DE",
  warning = "#F0AD4E",
  danger = "#D9534F",
  base_font = font_google("Roboto"),
  heading_font = font_google("Roboto Condensed")
)

# setting up the theme for the plots
theme_set(
  theme_classic() +
  theme(plot.title=element_text(size=20,
                                face="bold",
                                family="American Typewriter",
                                color="red",
                                hjust=0.5,
                                lineheight=1.2),
        plot.subtitle=element_text(size=15,
                                   family="American Typewriter",
                                   face="bold",
                                   hjust=0.5),
        plot.caption=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.title.y=element_text(size=15),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))
  )


# UI page 1
# State Comparison 
stateComparison <- fluidPage(
  # titlePanel("Top States in Different Aspects"),

  sidebarLayout(
    sidebarPanel(
      h3("Let's Compare Indian States"),
      selectInput("indicator", "Select an Indicator:",
                  choices = unique(StateIndicators$indicator),
                  selected = "Area (Sq. Km.)"),
      tableOutput("top_states"),
      
      # Select input for choosing a state
      h3("At what cost?"),
      selectInput("state_for_at_what_cost_table", "Select a State:",
                  choices = unique(names(StateIndicators)[-c(1:4)]),  
                  selected = "Rajasthan"),
      tableOutput("top_bottom_achievements")
    ),
    mainPanel(
      h3(textOutput("IndicatorName")),
      plotOutput("india_map_by_rank", width = "650px", height = "600px")
    )
  ),
  
  div(
    style = "margin-bottom: 30px;",  
  )
)


# Crime Page with Accordion layout
crimePage <- fluidPage(
 accordion(
     accordion_panel(
       title = "Is crime incresing these years?",
       icon = bsicons::bs_icon("bar-chart"),
       selectInput("selectedState", "Choose States:", choices = unique(State_crime$State), selected = c("Andhra Pradesh", "Assam", "Goa"), multiple = TRUE ),
       fluidRow(
         # Place each plot in a separate column
         column(6, align = "center", # Half width column for each plot
                plotOutput("lastFourYearCrimeRatePlot", width = "100%", height = "400px")),
         column(6, align = "center", 
                plotOutput("differntRatesAgainstCrime", width = "100%", height = "400px"))
       )
     ),
    accordion_panel(
      title = "Crime Composition Against Women",
      icon = bsicons::bs_icon("bar-chart"),
      div(
        style = "display: flex; justify-content: center;",
        plotOutput("crime_dist_plot", width = 900, height = 500)
      )
    ),
    accordion_panel(
      title = "Which State is not safe?",
      icon = bsicons::bs_icon("sliders"),
      selectInput("crimeType", "Select Crime Type:",
                  choices = colnames(combined_crime)[2:11],
                  multiple = TRUE,
                  selected = 'Rape'
                  ),
      plotOutput("combinedPlot")
    ),
    id = "acc",
  ),
 div(
   style = "margin-bottom: 30px;",  
 )
)

# UI page 3 : election and women

electionPage <- fluidPage(
  fluidRow(
    column(
      width = 12,
      h3("How is Women Participation in Elections in Recent Years?"),
      plotOutput("elections", width = "80%", height = "500px")
    )
  ),
  
  hr(),  # Horizontal line for separation
  
# Second Section: Relation Between Women Participation and Crime Types
  fluidRow(
    column(
      width = 12,
      h3("How Women Participation in Elections and Different Crimes Are Related?")
    )
  ),
  
  fluidRow(
    column(
      width = 2,
      selectInput(
        inputId = "crimeTypePage3",
        label = "Select Crime Type:",
        choices = colnames(combined_crime)[2:11],
        selected = "Rape"
      )
    ),
    column(
      width = 10,
      plotOutput("womenPower", width = "100%", height = "500px")
    )
  ),
  div(
    style = "margin-bottom: 30px;",  
  )
  
)



# Nav bar items
ui <- page_fluid(
  # theme = shinytheme("superhero"),
  theme = my_theme,
  navset_pill(
    nav_panel("India's States", stateComparison),
    nav_panel("Crime In India", crimePage),
    nav_panel("Women & Elections", electionPage)
  ),
  id = "tab"
)

# Server logic
server <- function(input, output) {

  # Helper Functions

  # Function to arrange states in ranking according to selected indicator
  rank_list_states <- function(indicator_name) {
    no_of_states <- 29
    data <- StateIndicators %>%
      filter(indicator == indicator_name) %>%
      select(-indicator, -source, -X_sr_no, -periodicity_latest_available_data)
    name <- array(dim = no_of_states)
    rank <- numeric(length = no_of_states)

    for(i in 1:no_of_states) {
      name[i] <- names(data[i])
      rank[i] <- as.numeric(data[[i]][1])
    }
    data <- data_frame(State = name, Ranking = rank) %>%
      arrange(rank)
    return(data)
  }

  # Crime Distribution graph
  crime_distribution <- function() {
    CrimeAgainstWomen <- arrange(CrimeAgainstWomen, desc(X_2014))
    type <- CrimeAgainstWomen$crime_head[2:9]
    values <- CrimeAgainstWomen$X_2014[2:9]
    type[9] <- "Others"
    values[9] <- sum(CrimeAgainstWomen$X_2014[10:15])
    # Create a data frame for plotting
    plot_data <- data.frame(
      Type = type,
      Values = values
    )
    # Plot using ggplot2
    ggplot(plot_data, aes(x = Values, y = reorder(Type, Values))) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(
        title = "Incidence of Crime Committed Against Women in India During 2014",
        x = "Number of Cases Reported",
        y = "Type of Crime"
      ) 
  }
  
  # india graph ploter
  plot_states_by_rank <- function(shp1, rank_data) {
    
    # Initialize a numeric vector to store ranks
    map.states.numbers <- dim(shp1)[1]
    map.value <- numeric(length = map.states.numbers)
    
    # Map ranks to the states based on names
    for (i in 1:map.states.numbers) {
      state_name <- shp1$NAME_1[i]
      rank_row <- rank_data[rank_data$State == state_name, ]
      if (nrow(rank_row) > 0) {
        map.value[i] <- rank_row$Ranking  # Assigning rank to the state
      } 
      else {
        map.value[i] <- NA
      }
    }
    
    # Adding ranks to the shapefile
    shp1$sales <- map.value

    # color for different ranks
    rank_colors <- c(
      "1" = "yellow",    # Rank 1 - bright yellow
      "2" = "orange",    # Rank 2 - orange
      "3" = "lightgreen", # Rank 3 - light green
      "4" = "lightblue",  # Rank 4 - light blue
      "5" = "blue",      # Rank 5 - blue
      "6" = "purple",     # Rank 6 - purple
      "NA" = "transparent"
    )
    
    # plot by ggplot2
    plot <- ggplot(shp1) +
      geom_sf(aes(fill = factor(sales)), alpha = 0.7) +  # Use factor for discrete colors
      geom_sf_text(aes(label = NAME_1), size = 3, color = "black") +
      ggthemes::theme_map() +
      theme(legend.position = "right",
            legend.title = element_text(size = 14),  
            legend.text = element_text(size = 12)) +
      scale_fill_manual(values = rank_colors, na.value = "transparent") +  # Gray for NA values
      guides(fill = guide_legend(title = "Ranking")) +  # Custom legend title
      
      # legend labels to show both state names and ranks
      scale_fill_manual(values = rank_colors, 
                        labels = paste("#",  rank_data$Ranking, " ", rank_data$State),
                        guide = "legend") 
    
    return(plot)
  }
  
  
  # Function to get the top and bottom achievements of a specific state
  output$top_bottom_achievements <- renderTable({
    achievements <- StateIndicators %>%
      select(Indicator = indicator, Rank = !!sym(input$state_for_at_what_cost_table)) %>%
      filter(!is.na(Rank)) %>%  # Exclude rows with NA values in Rank
      arrange(as.numeric(Rank))  # Sort by Rank (ascending)
    
    # Get the top 3 and bottom 3
    top_3 <- head(achievements, 3)   # Top 3 rows
    bottom_3 <- tail(achievements, 3)  # Bottom 3 rows
    
    # Combine the top 3 and bottom 3
    combined_achievements <- bind_rows(top_3, bottom_3)
    
    return(combined_achievements)
  })
  
  

  
  
  # Crime distribution and Women's Population per Women Police
  output$combinedPlot <- renderPlot({
    selected_cols <- input$crimeType # Get the selected column names
    if (is.null(selected_cols) || length(selected_cols) == 0) {
      ggplot() +
        labs(title = "Please Select Type!!", x = "", y = "")
    } else {
      # Melt the data frame to long format for ggplot
      long_data <- melt(combined_crime, id.vars = "States",
                        measure.vars = selected_cols,
                        variable.name = "Crime Type",
                        value.name = "Value")
      
      # Scale Women Population / Women Police data to match the range of Value
      max_crime_value <- max(long_data$Value, na.rm = TRUE)
      max_women_pop <- max(combined_crime$`Women Population / Women Police`, na.rm = TRUE)
      
      combined_crime <- combined_crime %>%
        mutate(scaled_women_pop = (`Women Population / Women Police` / max_women_pop) * max_crime_value)
      
      ggplot() +
        # Bar plot for scaled Women Population / Women Police with a secondary y-axis
        geom_bar(data = combined_crime, 
                 aes(x = States, y = scaled_women_pop),
                 stat = "identity", fill = "blue", alpha = 0.3) +
        # Line and point plot for Crime Types by State
        geom_line(data = long_data,
                  aes(x = States, y = Value, color = `Crime Type`, group = `Crime Type`),
                  size = 1.5, alpha = 0.5) +
        geom_point(data = long_data,
                   aes(x = States, y = Value, color = `Crime Type`, group = `Crime Type`),
                   size = 2) +
        labs(title = "Crime Types by State and Women's Population per Women Police",
             x = "States", y = "Values Per 100k") +
        scale_y_continuous(
          # Primary y-axis for Crime Types
          name = "Crime cases Per 100k",
          # Secondary y-axis for Women Population per Women Police
          sec.axis = sec_axis(~ . * (max_women_pop / max_crime_value),
                              name = "Women Population per Women Police")
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
    }
  })

  # Plot for Cases of Abortion
  output$casesPlot <- renderPlot({
    ggplot(combined_crime, aes(x = States, y = Cases)) +
      geom_bar(stat = "identity", fill = "orange") +
      labs(title = "Cases of Abortion", x = "States", y = "Number of Cases") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
   
  # line graph
  output$lastFourYearCrimeRatePlot=renderPlot({
    data <- State_crime[State_crime$State %in% input$selectedState, c(1, 8:11)] 
    # Convert to long format
    data_long <- data %>%
      pivot_longer(cols = starts_with("Crime_rate"), 
                   names_to = "year", 
                   values_to = "cases") %>%
      mutate(year = as.numeric(sub("Crime_rate\\.(\\d+)", "20\\1", year)), # Extract numeric year
             cases = as.numeric(cases)) # Convert cases to numeric
    
    ggplot(data_long, aes(year, cases))+
      geom_line(aes(col = State), size = 1)+
      geom_point(aes(col = State), size = 2)+
      labs(title = "Crime rates over past 4 years", subtitle = "comparison of states over the ipc crimes in the last 4 years", 
           x = "Year", y = "Crime Rate (registered cases/1000 people)")
    
    
  })
  # different rates
  output$differntRatesAgainstCrime <- renderPlot({
    data <- State_crime[State_crime$State %in% input$selectedState, c(1, 13:15)]
    
    # Convert to long format
    data_long <- data %>% 
      pivot_longer(cols = c(Investigation_Rate, Charge.Sheeting_rate, Conviction_Rate),
                   names_to = "indicator",
                   values_to = "Rate") %>%
      mutate(Rate = as.numeric(Rate))
    
    # Plotting
    ggplot(data_long, aes(x = State, y = Rate, fill = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ indicator, scales = "fixed", nrow = 1) +
      labs(title = "Crime Indicators by State",
           x = "State",
           y = "Rate") +
      theme(axis.text.x = element_blank())
  })

  # Display selected indicator name
  output$IndicatorName <- renderText({
    paste("Top 3 States In", input$indicator)
  })

  # Display rank list of states for selected indicator
  output$top_states <- renderTable({
    head(rank_list_states(input$indicator))
  })
  
  # india map output
  output$india_map_by_rank <- renderPlot({
    plot_states_by_rank(shp1 ,rank_list_states(input$indicator)[1:3, ])
  })

  # Crime distribution plot
  output$crime_dist_plot <- renderPlot({
    crime_distribution()
  })

  
  output$elections <- renderPlot({
    ggplot() +
      # Blue line for `rsPercentage` (Rajya Sabha)
      geom_line(data = rsPercentage, 
                aes(x = Year, y = `Percentage Women Elected`, color = "Rajya Sabha"), 
                alpha = 0.5, size = 1.5, group = 1) +
      geom_point(data = rsPercentage, 
                 aes(x = Year, y = `Percentage Women Elected`, color = "Rajya Sabha"), 
                 size = 2) +
      
      # Red line for `lastFiveYearElection` (Lok Sabha)
      geom_line(data = lastFiveYearElection, 
                aes(x = Year, y = `Percentage Women Elected`, color = "Lok Sabha"), 
                alpha = 0.5, size = 1.5, group = 1) +
      geom_point(data = lastFiveYearElection, 
                 aes(x = Year, y = `Percentage Women Elected`, color = "Lok Sabha"), 
                 size = 2) +
      
      labs(title = "Women Percentage Over Years", x = "Year", y = "Women Percentage") +
      # Manual color scale for legend
      scale_color_manual(values = c("Rajya Sabha" = "blue", "Lok Sabha" = "red"))
      
  })
    
  output$womenPower <- renderPlot({
    
    crimeT <- input$crimeTypePage3
    
    ggplot(data = stateWisePoliticalData) +
      # Bar graph for percentage of seats won by women
      geom_bar(aes(x = `Name of State / UT`, y = `% Of Seats won by Women`, fill = "% Seats Won by Women"), 
               alpha = 0.4, col = "blue", stat = "identity") +
      
      # Line graph for crime rates in 2019
      geom_line(data = combined_crime,
                aes(x = States, y = !!sym(crimeT), color = "Crime Cases", group = 1),
                size = 1, alpha = 0.5) +
      geom_point(data = combined_crime,
                 aes(x = States, y = !!sym(crimeT), color = "Crime Cases"),
                 size = 2) +
      
      # Adding labels and formatting
      labs(x = "State", title = "Seats Won by Women in Different States and Crime Cases") +
      
      # Customizing axis labels
      theme(axis.text.x = element_text(hjust = 1, angle = 60),
            axis.title.y  = element_text(hjust = 0.7),
            legend.position = "top") +
      
      # Custom color scale and legend
      scale_fill_manual(values = c("% Seats Won by Women" = "blue")) +
      scale_color_manual(values = c("Crime Cases" = "red")) +
      
      # Remove the legend title
      guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL))
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)



