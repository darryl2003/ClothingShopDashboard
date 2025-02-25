#LOAD DATA

#########################################
#Load necessary libraries
library(dplyr)
library(readr)
library(plotly)
library(ggplot2)
library(shiny)
library(tidyr)
library(htmlwidgets)

#Load the dataset
data <- read_csv("shopping_trends.csv")

#View the first few rows of the dataset
head(data)

#########################################

#DATA CLEANING

#Check for missing values in each column
colSums(is.na(data))

#Remove rows with missing critical values
data <- data %>%
  filter(!is.na(`Purchase Amount (USD)`), !is.na(Category))

#Rename columns for simplicity
data <- data %>%
  rename(
    purchase_amount = `Purchase Amount (USD)`,
    purchase_frequency = `Frequency of Purchases` ,
    review_rating = `Review Rating`,
    location = Location,
    season = Season,
    age = Age,
    gender = Gender,
    category = Category,
    color = Color
  )

#Remove duplicate rows
data <- data %>%
  distinct()

#Convert categorical variables to factors
data <- data %>%
  mutate(
    gender = as.factor(gender),
    season = as.factor(season),
    category = as.factor(category),
    color = as.factor(color),
    location = as.factor(location)
  )

#Ensure numeric columns are properly formatted
data$purchase_amount <- as.numeric(data$purchase_amount)
data$review_rating <- as.numeric(data$review_rating)
data$age <- as.numeric(data$age)

#########################################
#DATA TRANSFORMING

#State Mapping for reflective question 2 for intial question 2
state_mapping <- data.frame(
  location = c('Kentucky', 'Maine', 'Massachusetts', 'Rhode Island', 'Oregon', 
               'Wyoming', 'Montana', 'Louisiana', 'West Virginia', 'Missouri',
               'Arkansas', 'Hawaii', 'Delaware', 'New Hampshire', 'New York',
               'Alabama', 'Mississippi', 'North Carolina', 'California',
               'Oklahoma', 'Florida', 'Texas', 'Nevada', 'Kansas', 'Colorado',
               'North Dakota', 'Illinois', 'Indiana', 'Arizona', 'Alaska',
               'Tennessee', 'Ohio', 'New Jersey', 'Maryland', 'Vermont',
               'New Mexico', 'South Carolina', 'Idaho', 'Pennsylvania',
               'Connecticut', 'Utah', 'Virginia', 'Georgia', 'Nebraska', 'Iowa',
               'South Dakota', 'Minnesota', 'Washington', 'Wisconsin', 'Michigan'),
  state_abbreviation = c('KY', 'ME', 'MA', 'RI', 'OR', 'WY', 'MT', 'LA', 'WV', 'MO',
                         'AR', 'HI', 'DE', 'NH', 'NY', 'AL', 'MS', 'NC', 'CA', 'OK',
                         'FL', 'TX', 'NV', 'KS', 'CO', 'ND', 'IL', 'IN', 'AZ', 'AK',
                         'TN', 'OH', 'NJ', 'MD', 'VT', 'NM', 'SC', 'ID', 'PA', 'CT',
                         'UT', 'VA', 'GA', 'NE', 'IA', 'SD', 'MN', 'WA', 'WI', 'MI')
)

#Define the color-tone mapping for intial question 3
color_tone_mapping <- data.frame(
  color = c("Green", "Pink", "Blue", "Yellow", "Orange", "Brown", "Gray", "White", 
            "Maroon", "Red", "Beige", "Turquoise", "Purple", "Black", "Lavender", 
            "Charcoal", "Teal", "Cyan", "Gold", "Silver", "Peach", "Indigo", "Magenta",
            "Olive", "Tan", "Navy", "Crimson", "Amber", "Ivory", "Coral", "Violet"),
  tone = c("Pastel", "Pastel", "Cool", "Bright", "Bright", "Earthy", "Cool", "Neutral",
           "Earthy", "Bright", "Neutral", "Cool", "Bright", "Neutral", "Pastel", 
           "Neutral", "Cool", "Cool", "Bright", "Neutral", "Bright", "Cool", "Bright",
           "Earthy", "Neutral", "Cool", "Bright", "Bright", "Neutral", "Bright", "Pastel")
)
data <- data %>%
  left_join(color_tone_mapping, by = "color") #Join the mapping to the original data

#Define custom colors for each color in the pie chart of initial question 3
custom_colors <- c(
  "Green" = "#00FF00", "Pink" = "#FFC0CB", "Blue" = "#0000FF", "Yellow" = "#FFFF00",
  "Orange" = "#FFA500", "Brown" = "#A52A2A", "Gray" = "#808080", "White" = "#FFFFFF",
  "Maroon" = "#800000", "Red" = "#FF0000", "Beige" = "#F5F5DC", "Turquoise" = "#40E0D0",
  "Purple" = "#800080", "Black" = "#000000", "Lavender" = "#E6E6FA", "Charcoal" = "#36454F",
  "Teal" = "#008080", "Cyan" = "#00FFFF", "Gold" = "#FFD700", "Silver" = "#C0C0C0",
  "Peach" = "#FFE5B4", "Indigo" = "#4B0082", "Magenta" = "#FF00FF", "Olive" = "#808000",
  "Tan" = "#D2B48C", "Navy" = "#000080", "Crimson" = "#DC143C", "Amber" = "#FFBF00",
  "Ivory" = "#FFFFF0", "Coral" = "#FF7F50", "Violet" = "#EE82EE"
)

#Define custom colors for tones based on their representation in initial question 3
tone_colors <- c(
  "Pastel" = "#FFC0CB",  # Soft pink
  "Bright" = "#FFFF00",  # Bright yellow
  "Cool" = "#00FFFF",    # Cool cyan
  "Neutral" = "#808080", # Neutral gray
  "Earthy" = "#A52A2A"   # Earthy brown
)

#Check the structure of data after pre-processing
str(data)

#########################################

#DATA ANALYSIS (UI PART AND SERVER PART)

#########################################

#UI

#########################################

# Define UI
ui <- navbarPage(
  "Shopping Trends Dashboard",
  
  #########################################
  # Home page
  tabPanel(
    "Home",
    fluidPage(
      titlePanel("Welcome to Shopping Trends Analysis"),
      p("This dashboard provides insights from transactional records dataset from a fashion business which has stores in the spread around USA"),
      p("This project aims to uncover trends and insights within the dataset, then utilize the data visualisation techniques to explore these trends and insights to enable data-driven decision-making to further improve the profitability and growth of the retail business"),
      p("The objectives of this visualisations are:"),
      tags$ul(
        tags$li("To analyse the influence of demographic factors such as age and gender on purchasing behavior"),
        tags$li("To identify high-performing locations and product categories that drives revenue"),
        tags$li("To uncover how product color preferences vary and affects sales across different seasons and locations"),
        tags$li("To discover shipping type preference and its impact on the sales performance"),
      ),
      p("Hence, below intial questions were raised:"),
      tags$ul(
        tags$li("1.	What is the average purchase amount across different age groups and genders?"),
        tags$li("2.	Which location contributes the most to overall revenue?"),
        tags$li("3.	How do product color preferences vary across different seasons and locations?"),
        tags$li("4.	Which shipping type is the most popular among the customers?"),
      )
    )
  ),
  
  #########################################
  # Initial Question 1 Page
  navbarMenu(
    "Initial Question 1",
    #########################################
    tabPanel(
      "Analysis",
      fluidPage(
        titlePanel("What is the average purchase amount across different age groups and genders?"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "age_grouping",
              label = "Group Ages By:",
              choices = c("Every Year" = 1, "Every 5 Years" = 5, "Every 10 Years" = 10, "Every 20 Years" = 20),
              selected = 5
            )
          ),
          mainPanel(
            plotlyOutput("linePlot")
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "Justify",
            tags$div(
              style = "margin-top: 20px;",
              p("It can be seen that female consumers in the age range of 18 to 30 and above 55 tend to have higher average purchase amounts than male consumers in the same age groups."),
              p("This observation raises further questions, such as:"),
              tags$ul(
                tags$li("Are higher purchases by female customers in certain age groups due to there being more female customers overall?"),
                tags$li("Which product categories are most popular among different age groups and genders?")
              )
            )
          )
        )
      )
    ),
    #########################################
    tabPanel(
      "Reflective Question 1.1",
      fluidPage(
        titlePanel("Are higher purchases by female customers in certain age groups due to there being more female customers overall?"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "age_grouping_reflective1",
              label = "Group Ages By:",
              choices = c("Every Year" = 1, "Every 5 Years" = 5, "Every 10 Years" = 10, "Every 20 Years" = 20),
              selected = 1
            )
          ),
          mainPanel(
            plotlyOutput("stackedBarChart")
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "Justify",
            tags$div(
              style = "margin-top: 20px;",
              p("This visualization shows that the number of female customers is always lower than the male customers across all age groups"),
              p("Despite this, the average spending of the female customers is still higher at certain age groups, indicating that the female customers tend to spend higher amount of spending than male customers"),
            )
          )
        )
      )
    ),
    #########################################
    tabPanel(
      "Reflective Question 1.2",
      fluidPage(
        titlePanel("Which product categories are most popular among different age groups and genders?"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(
              inputId = "chart_type_1_2",
              label = "Select Chart Type:",
              choices = c("Bar Chart" = "bar", "Pie Chart" = "pie"),
              selected = "bar"
            ),
            conditionalPanel(
              condition = "input.chart_type_1_2 == 'pie'",
              radioButtons(
                inputId = "selected_age_group",
                label = "Select Age Group:",
                choices = c("15 - 19", "20 - 24", "25 - 29", "30 - 34", "35 - 39", 
                            "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64", 
                            "65 - 69", "70 - 74"),
                selected = "15 - 19"
              )
            )
          ),
          mainPanel(
            plotlyOutput("chartOutput_1_2", height = "600px")
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "Justify",
            tags$div(
              style = "margin-top: 20px;",
              p("This visualisation reveals that clothing categories are the most preferred by all ages and genders, followed by accessories categories"),
              p("The pie chart further shows that clothing categories alone contributes to almost half of the total sales"),
            )
          )
        )
      )
    )
  ),
  
  #########################################
  # Initial Question 2 Page
  navbarMenu(
    "Initial Question 2",
    
    #########################################
    tabPanel(
      "Analysis",
      fluidPage(
        titlePanel("Which location contributes the most to overall revenue?"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "top_locations",
              label = "Select Number of Top Locations:",
              choices = c(5, 10, 15, 20),
              selected = 5
            )
          ),
          mainPanel(
            plotlyOutput("revenueBarChart")
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "Justify",
            tags$div(
              style = "margin-top: 20px;",
              p("the rank of the locations based on their revenue contribution is shown, revealing the top revenue contributor locations and indicating the disparities between different locations"),
              p("This observation raises further questions, such as:"),
              tags$ul(
                tags$li("Which product categories generate the highest revenue in the top-performing locations?"),
                tags$li("Are there significant seasonal variations in revenue for the locations?")
              )
            )
          )
        )
      )
    ),
    
    #########################################
    tabPanel(
      "Reflective Question 2.1",
      fluidPage(
        titlePanel("Which product categories generate the highest revenue in the top-performing locations?"),
        sidebarLayout(
          sidebarPanel(
            numericInput(
              inputId = "top_locations_reflective",
              label = "Number of Top-Performing Locations:",
              value = 5,
              min = 1,
              step = 1
            )
          ),
          mainPanel(
            plotlyOutput("categoryRevenueChart")
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "Justify",
            tags$div(
              style = "margin-top: 20px;",
              p("This visualisation reveals that in all of the top-performing location, clothing category contributes the most, followed by accessories"),
              p("This shows that not only these categories are the most preferred by the customers but it also generates the highest revenue in the top-contributing locations"),
            )
          )
        )
      )
    ),
     
    #########################################
    tabPanel(
      "Reflective Question 2.2",
      fluidPage(
        titlePanel("Are there significant seasonal variations in revenue for the locations?"),
        #Maps in 2x2 grid
        fluidRow(
          column(6, h4("Summer Revenue Trends"), plotlyOutput("summerMap", height = "400px")),
          column(6, h4("Winter Revenue Trends"), plotlyOutput("winterMap", height = "400px"))
        ),
        fluidRow(
          column(6, h4("Spring Revenue Trends"), plotlyOutput("springMap", height = "400px")),
          column(6, h4("Fall Revenue Trends"), plotlyOutput("fallMap", height = "400px"))
        ),
        #Pie chart below the maps
        fluidRow(
          column(12, h4("Revenue Distribution Across Seasons"), plotlyOutput("seasonalRevenuePie", height = "400px"))
        ),
        fluidRow(
          column(
            width = 12,
            align = "Justify",
            tags$div(
              style = "margin-top: 20px;",
              p("This visualisation reveals that Southern region of USA has the highest revenue during Fall season compared to another seasons"),
              p("Moreover, the pie chart shows that Fall season has the highest income compared to other seasons"),
            )
          )
        )
      )
    )
  ),
  
  #########################################
  # Initial Question 3 Page
  navbarMenu(
    "Initial Question 3",
    #########################################
    tabPanel(
      "Analysis",
      fluidPage(
        titlePanel("How do product color preferences vary across different seasons ?"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "selected_season",
              label = "Select Season:",
              choices = c("Summer", "Winter", "Spring", "Fall"),
              selected = "Summer"
            ),
            radioButtons(
              inputId = "view_type",
              label = "View By:",
              choices = c("Color" = "color", "Tone" = "tone"),
              selected = "tone"
            )
          ),
          mainPanel(
            plotlyOutput("preferencePieChart", height = "700px", width = "700px")
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "Justify",
            tags$div(
              style = "margin-top: 20px;",
              p("This visualisation shows that bright colors tone is the most popular across all seasons, followed by cool colors tone."),
              p("This indicates that even though, generally specific color tone is preferred during specific season, there is no significant effect of season to the customer color preferences in this dataset"),
              p("However, the popularity of bright and cool colours tones still raise further addition question such as:"),
              tags$ul(
                tags$li("Does color preferences impact the average purchase amount?"),
              )
            )
          )
        )
      )
    ),
    #########################################
    tabPanel(
      "Reflective Question 3.1",
      fluidPage(
        titlePanel("Does color preferences impact the average purchase amount?"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(
              inputId = "selected_tone",
              label = "Select Tone to Highlight:",
              choices = c("All Tones" = "None", "Pastel", "Cool", "Bright", "Earthy","Neutral"),
              selected = "None"
            )
          ),
          mainPanel(
            plotlyOutput("barChart")
          )
        ),
        fluidRow(
          column(
            width = 12,
            align = "Justify",
            tags$div(
              style = "margin-top: 20px;",
              p("This visualisation suggests that while specific colors and tones have higher average purchase amounts, overall the relationship between color and spending behaviour are not strong enough to make any pattern, supported by lack of significant variation (mostly ranging between $55 and $65)"),
              p("Further segmentation are required to possibly reveal more actionable patterns."),
            )
          )
        )
      )
    )
  ),
  
  #########################################
  # Initial Question 4 Page
  navbarMenu(
    "Initial Question 4",
    #########################################
    tabPanel(
      "Analysis",
      fluidPage(
        titlePanel("Which shipping type is the most popular among the customers?"),
        mainPanel(
          plotlyOutput("shippingDoughnutChart")
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "Justify",
          tags$div(
            style = "margin-top: 20px;",
            p("The visualisation reveals that although Free Shipping is the most popular option, there are no noticeable difference among the shipping type preference"),
            p("Hence, further additional question is required to make data-driven decision:"),
            tags$ul(
              tags$li("Does the choice of the shipping type correlate with total purchase value?"),
            )
          )
        )
      )
    ),
    #########################################
    tabPanel(
      "Reflective Question 4.1",
      fluidPage(
        titlePanel("Does the choice of the shipping type correlate with total purchase value?"),
        mainPanel(
          plotlyOutput("shippingBoxPlot")
        )
      ),
      fluidRow(
        column(
          width = 12,
          align = "Justify",
          tags$div(
            style = "margin-top: 20px;",
            p("This visualisation reveals that although most shipping types have similar median and purchase values, faster shipping options, such as Express and 2-Days Shipping, show slightly higher variability in purchase amounts"),
            p("This highlights faster shipping are somewhat more preferred even though it costs more"),
          )
        )
      )
    )
  )
)
#########################################
#SERVER
#########################################

# Define Server
server <- function(input, output, session) {
  
  #########################################
  #Initial Question 1: Average Purchase Amount by Age and Gender
  #########################################
  output$linePlot <- renderPlotly({
    age_interval <- as.numeric(input$age_grouping)
    min_age <- min(data$age, na.rm = TRUE)
    max_age <- max(data$age, na.rm = TRUE)
    age_ranges <- seq(floor(min_age / age_interval) * age_interval, max_age, by = age_interval)
    age_labels <- if (age_interval == 1) {
      as.character(age_ranges)
    } else {
      paste0(age_ranges, "-", age_ranges + age_interval - 1)
    }
    
    grouped_data <- data %>%
      mutate(
        age_group = if (age_interval == 1) {
          as.character(age)
        } else {
          paste0(floor(age / age_interval) * age_interval, "-", 
                 (floor(age / age_interval) + 1) * age_interval - 1)
        }
      ) %>%
      group_by(age_group, gender) %>%
      summarise(average_purchase_amount = mean(purchase_amount, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      mutate(age_group = factor(age_group, levels = age_labels, ordered = TRUE))
    
    plot_ly(
      data = grouped_data,
      x = ~age_group,
      y = ~average_purchase_amount,
      color = ~gender,  #Explicitly map the gender column
      colors = c("Female" = "red", "Male" = "blue"),  #Map colors explicitly
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = 'text',
      text = ~paste(
        "Age Group: ", age_group,
        "<br>Gender: ", gender,
        "<br>Avg Purchase: $", round(average_purchase_amount, 2)
      )
    ) %>%
      layout(
        title = "Average Purchase Amount by Age Group and Gender",
        xaxis = list(title = "Age Group", tickangle = -45),
        yaxis = list(title = "Average Purchase Amount (USD)")
      )
  })
  
  #########################################
  #Reflective Question 1.1: Customer Demographics by Age Group and Gender
  #########################################
  
  output$stackedBarChart <- renderPlotly({
    age_interval <- as.numeric(input$age_grouping_reflective1)
    min_age <- min(data$age, na.rm = TRUE)
    max_age <- max(data$age, na.rm = TRUE)
    age_ranges <- seq(floor(min_age / age_interval) * age_interval, max_age, by = age_interval)
    age_labels <- if (age_interval == 1) {
      as.character(age_ranges)
    } else {
      paste0(age_ranges, "-", age_ranges + age_interval - 1)
    }
    
    grouped_data <- data %>%
      mutate(
        age_group = if (age_interval == 1) {
          as.character(age)
        } else {
          paste0(floor(age / age_interval) * age_interval, "-", 
                 (floor(age / age_interval) + 1) * age_interval - 1)
        }
      ) %>%
      group_by(age_group, gender) %>%
      summarise(customer_count = n(), .groups = "drop") %>%
      ungroup() %>%
      mutate(age_group = factor(age_group, levels = age_labels, ordered = TRUE))
    
    plot_ly(
      data = grouped_data,
      x = ~age_group,
      y = ~customer_count,
      color = ~gender,
      colors = c("Female" = "red", "Male" = "blue"),
      type = 'bar',
      hoverinfo = 'text',
      text = ~paste(
        "Age Group: ", age_group,
        "<br>Gender: ", gender,
        "<br>Count: ", customer_count
      )
    ) %>%
      layout(
        title = "Customer Demographics by Age Group and Gender",
        barmode = "stack",  # Apply `barmode` here
        xaxis = list(title = "Age Group", tickangle = -45),
        yaxis = list(title = "Customer Count")
      )
  })
  
  #########################################
  ### Reflective Question 1.2: Product Popularity by Age Group and Gender
  #########################################
  
  output$chartOutput_1_2 <- renderPlotly({
    #Group age into custom labels
    grouped_data <- data %>%
      mutate(
        age_group = case_when(
          age >= 15 & age <= 19 ~ "15 - 19",
          age >= 20 & age <= 24 ~ "20 - 24",
          age >= 25 & age <= 29 ~ "25 - 29",
          age >= 30 & age <= 34 ~ "30 - 34",
          age >= 35 & age <= 39 ~ "35 - 39",
          age >= 40 & age <= 44 ~ "40 - 44",
          age >= 45 & age <= 49 ~ "45 - 49",
          age >= 50 & age <= 54 ~ "50 - 54",
          age >= 55 & age <= 59 ~ "55 - 59",
          age >= 60 & age <= 64 ~ "60 - 64",
          age >= 65 & age <= 69 ~ "65 - 69",
          age >= 70 & age <= 74 ~ "70 - 74"
        )
      ) %>%
      group_by(age_group, gender, category) %>%
      summarise(total_purchases = n(), .groups = "drop")
    
    if (input$chart_type_1_2 == "bar") {
      #Bar chart with facets
      bar_chart <- ggplot(grouped_data, aes(x = category, y = total_purchases, fill = gender)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~age_group, ncol = 3) +
        scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +
        labs(x = "Product Category", y = "Total Purchases", fill = "Gender") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(bar_chart)
      
    } else if (input$chart_type_1_2 == "pie") {
      #Filter data for the selected age group
      filtered_data <- grouped_data %>%
        filter(age_group == input$selected_age_group) %>%
        mutate(gender_category = paste(category, "-", gender))
      
      #Pie chart
      pie_chart <- plot_ly(
        data = filtered_data,
        labels = ~gender_category,
        values = ~total_purchases,
        type = "pie",
        textinfo = "label+percent",
        hoverinfo = "text",
        text = ~paste("Category: ", category, "<br>Gender: ", gender, "<br>Purchases: ", total_purchases)
      ) %>%
        layout(
          title = paste("Product Category Breakdown for Age Group:", input$selected_age_group),
          showlegend = TRUE
        )
      
      pie_chart
    }
  })
  
  #########################################
  #Initial Question 2: Location Contribution to Revenue
  #########################################
  
  output$revenueBarChart <- renderPlotly({
    top_n <- as.numeric(input$top_locations)
    
    #Summarize the total revenue by location and select top locations
    location_revenue <- data %>%
      group_by(location) %>%
      summarise(total_revenue = sum(purchase_amount, na.rm = TRUE)) %>%
      arrange(desc(total_revenue)) %>%
      slice_head(n = top_n)
    
    #Create the bar chart
    plot_ly(
      data = location_revenue,
      x = ~reorder(location, -total_revenue),  #Sort by descending revenue
      y = ~total_revenue,
      type = 'bar',
      text = ~paste("$", round(total_revenue, 2)),  #Display text inside bars
      textposition = 'inside',  #Position the text inside
      textfont = list(color = 'white'),  #Text color
      hoverinfo = 'text',
      marker = list(color = 'rgba(30, 144, 255, 0.8)',  # Blue color
                    line = list(color = 'rgba(30, 144, 255, 1.0)', width = 1))  #Border
    ) %>%
      layout(
        title = "Top Locations Contributing to Overall Revenue",
        xaxis = list(title = "Location"),
        yaxis = list(title = "Total Revenue (USD)", tickprefix = "$"),
        margin = list(l = 50, r = 50, b = 100, t = 50),  #Adjust margins
        showlegend = FALSE
      )
  })
  
  #########################################
  # Reflective Question 2.1: Product Categories in Top Locations
  #########################################
  
  output$categoryRevenueChart <- renderPlotly({
    top_n <- as.numeric(input$top_locations_reflective)
    location_category_revenue <- data %>%
      group_by(location, category) %>%
      summarise(total_revenue = sum(purchase_amount, na.rm = TRUE), .groups = "drop")
    top_locations <- location_category_revenue %>%
      group_by(location) %>%
      summarise(location_revenue = sum(total_revenue), .groups = "drop") %>%
      arrange(desc(location_revenue)) %>%
      slice_head(n = top_n)
    filtered_data <- location_category_revenue %>%
      filter(location %in% top_locations$location)
    
    plot_ly(
      data = filtered_data,
      x = ~category,
      y = ~total_revenue,
      color = ~location,
      type = 'bar',
      text = ~paste(
        "Location: ", location,
        "<br>Category: ", category,
        "<br>Total Revenue: $", round(total_revenue, 2)
      ),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Revenue by Product Categories in Top Locations",
        barmode = "group",
        xaxis = list(title = "Category"),
        yaxis = list(title = "Total Revenue (USD)")
      )
  })
  
  #########################################
  # Reflective Question 2.2: Seasonal Revenue Maps
  #########################################
  
  generate_seasonal_map <- function(season_name, state_mapping, show_legend = FALSE) {
    season_data <- data %>%
      filter(season == season_name) %>%
      group_by(location) %>%
      summarise(total_revenue = sum(purchase_amount, na.rm = TRUE), .groups = "drop") %>%
      left_join(state_mapping, by = c("location" = "location"))
    
    plot_ly(
      data = season_data,
      type = 'choropleth',
      locations = ~state_abbreviation,
      z = ~total_revenue,
      locationmode = 'USA-states',
      colorscale = 'Reds',
      showscale = show_legend,  #Only show legend for one map
      text = ~paste("Location: ", location, "<br>Total Revenue: $", round(total_revenue, 2)),
      hoverinfo = 'text'
    ) %>%
      layout(
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = toRGB("white")
        )
      )
  }
  
  #Seasonal Maps Outputs with Single Legend
  output$summerMap <- renderPlotly({ generate_seasonal_map("Summer", state_mapping, FALSE) })
  output$winterMap <- renderPlotly({ generate_seasonal_map("Winter", state_mapping, FALSE) })
  output$springMap <- renderPlotly({ generate_seasonal_map("Spring", state_mapping, FALSE) })
  output$fallMap <- renderPlotly({ generate_seasonal_map("Fall", state_mapping, TRUE) })  # Show legend here
  
  #Pie Chart for Revenue Distribution Across Seasons
  output$seasonalRevenuePie <- renderPlotly({
    seasonal_revenue <- data %>%
      group_by(season) %>%
      summarise(total_revenue = sum(purchase_amount, na.rm = TRUE)) %>%
      arrange(desc(total_revenue))
    
    plot_ly(
      data = seasonal_revenue,
      labels = ~season,
      values = ~total_revenue,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~paste("Season: ", season, "<br>Total Revenue: $", round(total_revenue, 2)),
      marker = list(colors = c("brown", "green", "blue", "yellow"))  #Custom color mapping correspond to the season
    ) %>%
      layout(
        title = "Revenue Distribution Across Seasons",
        showlegend = TRUE
      )
  })
  
  #########################################
  # Initial Question 3: Product Preferences Across Seasons
  #########################################
  
  output$preferencePieChart <- renderPlotly({
    filtered_data <- data %>%
      filter(season == input$selected_season)
    
    if (input$view_type == "tone") {
      #Aggregate data by tone
      aggregated_data <- filtered_data %>%
        group_by(tone) %>%
        summarise(total_purchases = n(), .groups = "drop")
      
      #Create the pie chart for tones
      plot_ly(
        data = aggregated_data,
        labels = ~tone,
        values = ~total_purchases,
        type = 'pie',
        textinfo = 'label+percent',
        hoverinfo = 'text',
        text = ~paste("Tone: ", tone, "<br>Total Purchases: ", total_purchases),
        marker = list(colors = tone_colors[aggregated_data$tone])  #Apply tone colors
      ) %>%
        layout(title = paste("Tone Preferences in", input$selected_season))
      
    } else {
      #Aggregate data by color
      aggregated_data <- filtered_data %>%
        group_by(color) %>%
        summarise(total_purchases = n(), .groups = "drop")
      
      #Create the pie chart for colors
      plot_ly(
        data = aggregated_data,
        labels = ~color,
        values = ~total_purchases,
        type = 'pie',
        textinfo = 'label+percent',
        hoverinfo = 'text',
        text = ~paste("Color: ", color, "<br>Total Purchases: ", total_purchases),
        marker = list(colors = custom_colors[aggregated_data$color])  # Apply custom colors
      ) %>%
        layout(title = paste("Color Preferences in", input$selected_season))
    }
  })
  
  #########################################
  # Reflective Question 3.1: Does Color Preference Impact Average Purchase Amount?
  #########################################
  
  output$barChart <- renderPlotly({
    aggregated_data <- data %>%
      group_by(color, tone) %>%
      summarise(avg_purchase_amount = mean(purchase_amount, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        highlight = ifelse(input$selected_tone == "None" | tone == input$selected_tone, 1, 0.3),
        color_marker = ifelse(input$selected_tone == "None" | tone == input$selected_tone, "blue", "lightblue")
      )
    
    max_value <- max(aggregated_data$avg_purchase_amount, na.rm = TRUE)
    y_axis_max <- max_value + (max_value * 0.1)
    
    plot_ly(
      data = aggregated_data,
      x = ~color,
      y = ~avg_purchase_amount,
      type = 'bar',
      marker = list(color = ~color_marker, opacity = ~highlight),
      text = ~paste("$", round(avg_purchase_amount, 2)),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Average Purchase Amount by Color",
        xaxis = list(title = "Colors", tickangle = -45),
        yaxis = list(title = "Average Purchase Amount (USD)", range = c(0, y_axis_max))
      )
  })
  
  #########################################
  # Initial Question 4: Most Popular Shipping Types
  #########################################
  
  output$shippingDoughnutChart <- renderPlotly({
    shipping_data <- data %>%
      group_by(`Shipping Type`) %>%
      summarise(order_count = n(), .groups = "drop")
    
    plot_ly(
      data = shipping_data,
      labels = ~`Shipping Type`,
      values = ~order_count,
      type = 'pie',
      hole = 0.4,
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~paste("Shipping Type: ", `Shipping Type`, "<br>Orders: ", order_count)
    ) %>%
      layout(title = "Most Popular Shipping Types")
  })
  
  #########################################
  # Reflective Question 4.1: Does Shipping Type Correlate with Total Purchase Value?
  #########################################
  
  output$shippingBoxPlot <- renderPlotly({
    plot_ly(
      data = data,
      x = ~`Shipping Type`,
      y = ~purchase_amount,
      type = 'box',
      boxpoints = "all",
      jitter = 0.3,
      pointpos = -1.8,
      marker = list(color = "skyblue"),
      text = ~paste("Shipping Type: ", `Shipping Type`, "<br>Purchase Amount: $", round(purchase_amount, 2)),
      hoverinfo = 'text'
    ) %>%
      layout(
        title = "Purchase Value Distribution by Shipping Type",
        xaxis = list(title = "Shipping Type"),
        yaxis = list(title = "Purchase Amount (USD)")
      )
  })
}


#########################################
#RUN SHINY APP
#########################################
shinyApp(ui, server) # Run the app


