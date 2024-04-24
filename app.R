# Business Case -------------------------------------------------------

  # Kempinski Hotels, known for its unparalleled guest services, currently faces challenges in harnessing the full potential of 
  # its vast data to drive strategic decision-making. The absence of a centralized platform for data analysis has led to 
  # potential gaps in optimizing occupancy rates, managing revenue, and capitalizing on market dynamics. Furthermore, without 
  # predictive analytics, the ability to anticipate and mitigate booking cancellations remains untapped, which can lead to missed 
  # opportunities for maximizing room utilization and revenue.
   
  # Our Shiny application addresses these challenges by introducing a powerful internal tool that consolidates key performance metrics 
  # and predictive insights into a single, intuitive interface. By providing tracking of occupancy and revenue, the hotel management 
  # can swiftly respond to emerging trends and make data-informed decisions. The application’s predictive capabilities for cancellation 
  # probabilities empower the staff to proactively confirm risky bookings or consider strategic overbooking to ensure high occupancy levels.
   
  # Additionally, the pricing recommendation module within the app uses sophisticated algorithms to suggest optimal pricing for new bookings, 
  # balancing competitiveness with profitability. This ensures that Kempinski Hotels can maintain its market-leading position by offering rates 
  # that reflect both guest value and business strategy.
  
  # Our solution transforms data into a strategic asset, positioning Kempinski Hotels at the forefront of innovation in the hospitality industry, 
  # ready to meet the evolving demands of its clientele while enhancing operational efficiency and revenue management.


# Initial Settings -------------------------------------------------------
pdf(file = "/dev/null") # Prevents the creation of a PDF file
setwd("/srv/shiny-server/myapp") # Sets working directory to that of myapp within the shiny server

# Loading libraries ---------------------------------------------------------
library(shiny)
library(relaimpo)
library(shinydashboard)
library(ggplot2)
library(readr) 
library(dplyr)
library(lubridate)
library(zoo)
library(flexdashboard)
library(randomForest)
library(shinyWidgets)
library(tidyverse)
library(caret)


# Preparations / Calculations for use later on ---------------------------------------------------------

# Load the dataset(s)
bookingData <- read_csv("booking (1).csv")
bookingData_ml <- read_csv("booking (1).csv") # same data, but for machine learning usage

# Correctly converting date of reservation to Date type
bookingData <- bookingData %>%
  mutate(`date of reservation` = mdy(`date of reservation`)) # Assuming the date format is Month/Day/Year

# Add columns for start_date, length_of_stay, and end_date for each booking to the dataset
bookingData <- bookingData %>% 
  mutate(
    start_date = `date of reservation`,
    length_of_stay = as.numeric(`number of weekend nights` + `number of week nights`),
    end_date = start_date + length_of_stay
  )

# Delete rows with missing values
bookingData <- bookingData %>% 
  na.omit()

# This function deliverrs the number of occupied rooms per day and the resulting revenue per day
daily_summary <- function(data) {
  if(nrow(data) == 0) {
    return(data.frame(date_seq = as.Date(character()), revenue = numeric(), occupancy_count = numeric()))
  }
  data <- data %>%
    filter(`booking status` == "Not_Canceled") %>%
    rowwise() %>%
    mutate(date_seq = list(seq.Date(start_date, end_date, by = "day"))) %>%
    unnest(date_seq) %>%
    group_by(date_seq) %>%
    summarise(revenue = mean(`average price`) * n(), occupancy_count = n(), .groups = "drop") %>%
    ungroup()
  
  return(data)
} 

# Define the total rooms available in the hotel as the max number of rooms occupied in one day based on all data
totalRoomsEstimate <- max(daily_summary(bookingData)$occupancy_count)

# This function delivers the number of booking entries within a date range (either cancelled or not cancelled)
get_values <- function(data, start_input, end_input, cancelled) {

  # number of days within day range
  num_days <- as.numeric(difftime(end_input, start_input, units = "days")) + 1
  
  # prepare string (by default all 0's)
  values <- c(rep(0,num_days))
  
  # filter the data we are interested in and accumulate the number of entries in values-vector
  full_in_range <- data %>% # bookings that are fully within the filtered date range
    filter(`start_date` <= start_input & `end_date` >= end_input & `booking status` == cancelled) 
  
  if(nrow(full_in_range)) {
    for(i in 1:nrow(full_in_range)) {
      indexes = seq(from = 1, to = (as.numeric(difftime(end_input, start_input, units = "days")) + 1), by = 1)
      values[indexes] <- values[indexes] + 1
    }
  }
  
  not_full_in_range <- data %>%  # bookings that are partially within the filtered date range
    filter(`start_date` <= start_input & `end_date` < end_input& `end_date` > start_input & `booking status` == cancelled) %>%
    rowwise() %>%
    mutate(start_date_here = 1,
           end_date_here = as.numeric(difftime(as.Date(`end_date`), start_input, units = "days")) + 1) %>%
    ungroup()
  
  if(nrow(not_full_in_range) > 0) {
    for(i in 1:nrow(not_full_in_range)) {
      indexes = seq(from = not_full_in_range$start_date_here[i], to = not_full_in_range$end_date_here[i], by = 1)
      values[indexes] <- values[indexes] + 1
    }
  }
  
  not_full_in_range2 <- data %>%  # bookings that are partially within the filtered date range
    filter(`start_date` > start_input & `start_date` < end_input & `end_date` >= end_input & `booking status` == cancelled) %>%
    rowwise() %>%
    mutate(start_date_here = as.numeric(difftime(as.Date(`start_date`), start_input, units = "days")) + 1,
           end_date_here = length(values)) %>%
    ungroup()
  
  if(nrow(not_full_in_range2) > 0) {
    for(i in 1:nrow(not_full_in_range2)) {
      indexes = seq(from = not_full_in_range2$start_date_here[i], to = not_full_in_range2$end_date_here[i], by = 1)
      values[indexes] <- values[indexes] + 1
    }
  }
  
  not_full_in_range3 <- data %>%  # bookings that are partially within the filtered date range
    filter(`start_date` > start_input & 
             `start_date` < end_input & 
             `end_date` < end_input & 
             `end_date` > start_input & `booking status` == cancelled) %>%
    rowwise() %>%
    mutate(start_date_here = as.numeric(difftime(`start_date`, start_input, units = "days")) + 1,
           end_date_here = as.numeric(difftime(`end_date`, start_input, units = "days")) + 1) %>%
    ungroup()
  
  
  if(nrow(not_full_in_range3) > 0) {
    for(i in 1:nrow(not_full_in_range3)) {
      indexes = seq(from = not_full_in_range3$start_date_here[i], to = not_full_in_range3$end_date_here[i], by = 1)
      values[indexes] <- values[indexes] + 1
    }
  }
  return(values)
}





# Create classification model (random forest) for cancellation prediction ----------------------------------------------------------

# Preprocessing
bookingData_ml$Cancelled <- as.factor(ifelse(bookingData_ml$`booking status` == "Canceled", 1, 0)) # Convert 'booking status' to a binary factor for cancellation

bookingData_ml <- bookingData_ml %>% # Drop columns that won't be used in the model
  dplyr::select(-`booking status`, -`date of reservation`, -Booking_ID)

bookingData_ml <- na.omit(bookingData_ml) # Remove any rows with missing values

colnames(bookingData_ml) = c('number_of_adults', 'number_of_children', 'number_of_weekend_nights', 'number_of_week_nights', 'type_of_meal', 'car_parking_space', 'room_type', 'lead_time', 'market_segment_type', 'repeated', 'P_C', 'P_not_C', 'average_price', 'special_requests', 'Cancelled')  # remove spaces in column names

max_price = max(bookingData$`average price`)

# Split the data into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(bookingData_ml$Cancelled, p = 0.8, list = FALSE)
trainingSet <- bookingData_ml[splitIndex, ]
testingSet <- bookingData_ml[-splitIndex, ]

# Train the Random Forest model
rfModel <- randomForest(Cancelled ~ ., data = trainingSet)

# Summarize the model
print(rfModel)

# Predict on the testing set
predictions <- predict(rfModel, newdata = testingSet)

# Evaluate model performance
confusionMatrix(predictions, testingSet$Cancelled, positive = '1')

# check importance of variables, save it as a dataframe, and plot it to investigate feature importance
feat_imp_df <- importance(rfModel) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

plot_cp <- ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: <Model>"
  )
print(plot_cp)
# We extract the top 7 most important variables for prediction: lead_time, special_requests, average_price, market_segment_type, number_of_weekend_nights, number_of_adults, number_of_week_nights
# these are the variables for which we will ask the user to input the actual data to calculate the cancellation probability
# for the remaining variables, we will use a default value, as their impact on the prediction is not that significant





# Create logistic regression model for price recommendation ----------------------------------------------------------

bookingData_ml <- bookingData_ml %>% # Drop columns that won't be used in the model
  dplyr::select(-Cancelled) %>%
  mutate(
    room_type = as.factor(room_type),
    market_segment_type = as.factor(market_segment_type),
    type_of_meal = as.factor(type_of_meal)
  )

# Split the data into training and testing sets for the linear regression model
set.seed(123)
splitIndex_lr <- createDataPartition(bookingData_ml$average_price, p = 0.8, list = FALSE)
trainingSet_lr <- bookingData_ml[splitIndex_lr, ]
testingSet_lr <- bookingData_ml[-splitIndex_lr, ]

# Train the Linear Regression model (lm) to predict average_price
lmModel <- lm(average_price ~ ., data = trainingSet_lr) 

# get top 5 most important variables; we will ask the user for the input values for these variables to give an accurate price recommendation
calc.relimp <- calc.relimp(lmModel, type = c("lmg", "last", "first"), rela = TRUE) # get variable importance
lmg_results <- calc.relimp$lmg # Extract the 'lmg' metric results
sorted_lmg <- sort(lmg_results, decreasing = TRUE) # Sort the variables by their 'lmg' importance in descending order
top_5_variables <- head(sorted_lmg, 5) # Get the top 5 most relevant variables
print(top_5_variables) # Print the top 5 variables and their importance scores

# We extract the top 5 most important variables: room_type, market_segment_type, number_of_children, number_of_adults, type_of_meal 
# these are the variables for which we will ask the user to input the actual data to calculate the cancellation probability
# for the remaining variables, we will use a default value, as their impact on the prediction is not that significant







# Define the UI ---------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader( 
    title = 
      # add link that directs you to the kempsinski hotels website when clicking their logo
      tags$li(
      a(href = 'https://www.kempinski.com/en', 
        tags$img(src = 'https://upload.wikimedia.org/wikipedia/commons/f/f8/Kempinski_Logo_2015.svg', style = 'height:45px; float:left;') 
      ),
      # add that logo
      tags$h1("Hotel Insights Dashboard", style = 'float:left; margin-left: 10px; line-height: 50px;'),
      class = "logo"
    ),
    # add messages dropdown menu
    dropdownMenu(type = "messages",
                 messageItem(from = "Sales Department", message = "New booking received", time = "12:45"),
                 messageItem(from = "Housekeeping", message = "Room 101 is ready for check-in", time = "12:00"),
                 messageItem(from = "Reception", message = "Check-in for Mr. Smith", time = "11:30")
                )
  ),
  dashboardSidebar(
    # add sidebar menu
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Cancellation Prediction", tabName = "cancellationPrediction", icon = icon("times-circle")),
      menuItem("Pricing Recommendation", tabName = "pricingRecommendation", icon = icon("dollar"))
    )
  ),
  dashboardBody(
    tags$head(
      # add different div classes to be attached to different elements in our UI to adapt their designs
      tags$style(HTML(
      '
      .myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
      .content-wrapper, .main-sidebar, .right-side, .left-side {
      overflow-y: auto;
      }

      .box {
      overflow: visible; 
      }
      #predictPriceBtn {
          background-color: #518CB8; 
          color: white;
          border-color: #518CB8;
          border-radius: 4px; 
      }
      #predictPriceBtn:hover {
          background-color: #0000A0; 
      }
      #predictBtn {
          background-color: #518CB8; 
          color: white;
          border-color: #518CB8;
          border-radius: 4px; 
      }
      #predictBtn:hover {
          background-color: #0000A0; 
      }
      box-header.with-border.box-title {
      background-color: #242D31; 
      color: white;
      text-align: center;
      font-size: 18px;
      margin: 0;
      padding: 10px;
      border-radius: 4px;
      }
      .gauge-container {
      width: 100%; 
      height: 250px; 
      display: flex;
      justify-content: center; 
      align-items: center;
      overflow: visible;
      margin-top: 250px;
      }
      .gauge-container .shiny-bound-output {
      transform: scale(2); /* Double the size of the gauge */
      transform-origin: 50% 50%; /* Scale from the center */
      margin-top: 250px; /* Adjust as needed to bring the label into view */
      }
      .warning-sign-container {
      width: 100%;
      height: auto;
      display: flex;
      justify-content: center;
      align-items: center;
      overflow: visible;
      }
      .warning-sign-container .shiny-bound-output {
      transform: scale(2);
      transform-origin: 50% 0%;
      margin-top: 100px;
      }
      .warning-sign-container h4 {
      font-size: 14px;
      color: red;
      }
      .shiny-output-container {
      text-align: center;
      }
      .shiny-plot-output {
      margin: auto;
      display: block;
      }
      #priceTagPlot {
      margin-left: -10px;
      margin-top: -40px;
      }
      
    '
    ))),
    # add heading to our dashboard
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Welcome to Kempinski Hotel Insights Dashboard </span>\');
      })
     ')),
    # change the font of that heading to the same as the font of the logo
    tags$head(
      tags$link(
        rel= "stylesheet",
        href = "https://fonts.googleapis.com/css?family=Allura&display=swap"
      ),
      tags$style(HTML(
        '.myClass { 
        font-size: 30px;
        line-height: 50px;
        text-align: left;
        font-family: "Allura", cursive;
        padding: 0 15px;
        overflow: hidden;
        color: black;
        }
        '
      ))),
    tabItems(
      # add dahsboard
      tabItem(tabName = "dashboard",
              # add filters and their options for the metrics that are being displayed on the dashboard
              # we chose the filter options based on what we considered most relevant / logical to get a good overview over the company metrics, such as bookings and cancellations over time
              box( title = "Adjust Filters", width = 12, status = "primary", solidHeader = TRUE, 
                   collapsible = TRUE, collapsed = FALSE,
                fluidRow(
                  column(width =4,
                         dateRangeInput("dateRange", "Select Date Range:",
                                        start = "2017-07-01", # Start of July 2017
                                        end = "2018-12-31", # Last day of 2018
                                        min = "2017-07-01", # Minimum date available for selection
                                        max = "2018-12-31", # Maximum date available for selection
                                        format = "yyyy-mm-dd"),
                         # Dropdown for number of adults
                         selectInput("numAdults", "Number of Adults:",
                                     choices = sort(unique(bookingData$`number of adults`)),
                                     selected = 1, multiple = TRUE)),
                  column(width = 4,
                         # Dropdown for number of children
                         selectInput("numChildren", "Number of Children:",
                                     choices = sort(unique(bookingData$`number of children`)),
                                     selected = 0, multiple = TRUE),
                         
                         # Dropdown for room type
                         selectInput("marketSegment", "Market Segment Type:",
                                     choices = sort(unique(bookingData$`market segment type`)),
                                     selected = unique(bookingData$`market segment type`), multiple = TRUE)),
                  column(width = 4,
                         # Dropdown for market segment type
                         selectInput("roomType", "Room Type:",
                              choices = sort(unique(bookingData$`room type`)),
                              selected = unique(bookingData$`room type`)[1], multiple = TRUE))
                )
              ),
              # add the three metric boxes to the dashboard
              box(title = "Resulting Metrics", width = 12, status = "primary", solidHeader = TRUE, 
                  collapsible = TRUE, collapsed = FALSE,
                fluidRow( 
                  shinydashboard::valueBoxOutput("occupancyRateBox"),
                  shinydashboard::valueBoxOutput("adrBox"),
                  shinydashboard::valueBoxOutput("revparBox")
                ),
                fluidRow(
                  box(title = "Occupied Rooms per Day", status = "primary", solidHeader = TRUE,
                      plotly::plotlyOutput("bookingsPlot")),
                  box(title = "Cancellations per Day", status = "primary", solidHeader = TRUE,
                      plotly::plotlyOutput("cancellationsPlot"))
                )
              )
      ),
      tabItem(tabName = "cancellationPrediction",
            # create the tab for predicting the cancellation probability of a new booking
            # for that, again, create the filter options
            # the filter options were chosen based on the feature probability of the model used (explained in the code of the model)
            fluidRow(
              box( 
                   title = "Enter Booking Details", width = 12, status = "primary", solidHeader = TRUE, 
                   collapsible = TRUE, collapsed = FALSE,
                column(width =4,
                       numericInput("numWeekendNightsPrediction", "Number of Weekend Nights", value = 2, min = 0, max = 5),
                       numericInput("numAdultsPrediction_cp", "Number of Adults", value = 1,min = 1, max = 5),
                       actionButton("predictBtn", "Predict Cancellation")
                ),
                column( width = 4,
                       numericInput("leadTimePrediction", "Lead Time", value = 0, min = 0, max = 1000),
                       numericInput("averagePricePrediction", "Price", value = 0, min = 0, max = 10000),
                ),
                column(width = 4,
                       numericInput("specialRequestsPrediction", "Special Requests", value = 0, min = 0, max = 10),
                       selectInput("marketSegmentPrediction", "Market Segment Type", choices = sort(unique(bookingData$`market segment type`)))
                       ),
              ),
              # add plot for output
              fluidRow(
                div(class = "gauge-container", 
                    gaugeOutput("cancellationProbability")
                )
              ),
              # add warning sign (this will be displayed only for bookings with high cancellation probability)
              fluidRow(
                div(class = "warning-sign-container",
                    uiOutput("warningSign")
                )
              ),
      )),
      # create tab for pricing recommendation
      tabItem(tabName = "pricingRecommendation",
              fluidRow(
                # create filter options
                # the filter options were chosen based on the feature probability of the model used (explained in the code of the model)
                box( title = "Enter Booking Details", width = 12, status = "primary", solidHeader = TRUE, 
                     collapsible = TRUE, collapsed = FALSE,
                     column(width =4,
                            numericInput("numAdultsPrediction_pr", "Number of Adults", value = 1, min = 1, max = 5),
                            numericInput("numChildrenPrediction_pr", "Number of Children", value = 0, min = 1, max = 5),
                            actionButton("predictPriceBtn", "Price Recommendation"),
                     ),
                     column( width = 4,
                             selectInput("marketSegmentPrediction", "Market Segment Type",
                                         choices = sort(levels(trainingSet_lr$market_segment_type)),
                                         selected = levels(trainingSet_lr$market_segment_type)[1]),
                             selectInput("roomTypePrediction", "Room Type",
                                         choices = sort(levels(trainingSet_lr$room_type)),
                                         selected = levels(trainingSet_lr$room_type)[1])
                     ),
                     column(width = 4,
                            selectInput("typeOfMealPrediction", "Type of Meal",
                                        choices = sort(levels(trainingSet_lr$type_of_meal)),
                                        selected = levels(trainingSet_lr$type_of_meal)[1])
                     ),
                    ),
              ),
              fluidRow(
                column(width = 12, align = "center", uiOutput("dynamic_image"))
              ),
              fluidRow(
                column(width = 12, align = "center", plotOutput("priceTagPlot"))
              )
              )
    )
  ),
  title = "Kempsinki Hotel Insights"  # This sets the browser tab title
)




  
  
# Define Server ----------------------------------------------------------------------------- 
server <- function(input, output, session) {

  # Show a popup message when the app is loaded
  showModal(modalDialog(
    title = "Welcome",
    HTML("Welcome to the Kempinski Hotel's internal Insights dashboard!<br><br>
    Here, you'll find a comprehensive tool for analyzing existing historical hotel bookings. Our dashboard provides intuitive filters to delve into the intricate details of past reservations.<br><br>
    Explore the cancellation predictions tab to assess the probability of a booking being canceled, empowering you to make informed decisions about resource allocation and guest management.<br><br>
    In addition, utilize the pricing recommendation tab to determine an optimal price strategy based on relevant information, ensuring competitive yet profitable rates for our esteemed guests.<br><br>
    We're thrilled to have you on board as we embark on this journey of data-driven hospitality excellence. Enjoy your exploration!<br><br>
    Note: The first run of the Shiny app might take a longer time as it involves model training. This is essential for predictive analytics. Allow the process to complete.
    "),
    easyClose = TRUE
  ))  
  
  # DASHBOARD
  # Reactive expression for filtered data based on date range input
  reactiveData <- reactive({
    req(input$dateRange) 
    
    # Start with the full dataset and then progressively apply filters
    filteredData <- bookingData %>%
      filter((start_date <= as.Date(input$dateRange[1]) & # select all bookings that are fully within the date range (either started / ended exactly on the selected dates or started before / ended after)
               end_date >= as.Date(input$dateRange[2])) |
               (start_date <= as.Date(input$dateRange[1]) & # or that are partly within the date range
               end_date < as.Date(input$dateRange[2]) &
                  end_date > as.Date(input$dateRange[1])) |
               (start_date > as.Date(input$dateRange[1]) & # or that are partly within the date range but the other way around
               end_date >= as.Date(input$dateRange[2]) &
                 start_date < as.Date(input$dateRange[2])) |
               (start_date > as.Date(input$dateRange[1]) & # or that starts and ends within the date range
                  end_date < as.Date(input$dateRange[2]) &
                  start_date < as.Date(input$dateRange[2]) &
                  end_date > as.Date(input$dateRange[1]))) %>%
      filter(`number of adults` %in% input$numAdults) %>%
      filter(`number of children` %in% input$numChildren) %>%
      filter(`room type` %in% input$roomType) %>%
      filter(`market segment type` %in% input$marketSegment)
    
    filteredData
  })
  
  # Calculate and render the occupancy rate
  output$occupancyRateBox <- shinydashboard::renderValueBox({
    data <- reactiveData() # Get the reactive filtered data
    filteredData <- daily_summary(data)
    num_days <- as.numeric(difftime(as.Date(input$dateRange[2]), as.Date(input$dateRange[1]), units = "days")) + 1
    daily_occur <- sum(filteredData$occupancy_count/totalRoomsEstimate) / num_days
    # Handle the situation where the filters result in empty data
    if(is.na(daily_occur) | nrow(data) == 0 | nrow(filteredData) == 0) {
      daily_occur <- 0
    }
    shinydashboard::valueBox(paste(round(daily_occur, 2) * 100, "%"), "Average Occupancy Rate per Day", icon = icon("bed"), color = "aqua")
  })
  
  # Calculate and render the Average Daily Price
  output$adrBox <- shinydashboard::renderValueBox({
    data <- reactiveData() # Get the reactive filtered data
    filteredData <- daily_summary(data)
    adr <- sum(filteredData$revenue) / sum(filteredData$occupancy_count)
    # Handle the situation where the filters result in empty data
    if(nrow(data) == 0 | nrow(filteredData) == 0 | is.na(sum(filteredData$revenue))) {
      adr <- 0
    }
    shinydashboard::valueBox(paste("$", round(adr, 2)), "Average Daily Price", icon = icon("usd"), color = "green")
  })
  
  # Calculate and render the Revenue per Room
  output$revparBox <- shinydashboard::renderValueBox({
    
    data <- reactiveData()
    
    # Here, among all filtered data, we distinguish between bookings that cover the whole date range selected and bookings that cover it only partially
    # so that we can calculate the total revenue per room of those bookings correctly and easily
    full_in_range <- data %>%
      filter(`start_date` <= as.Date(input$dateRange[1]) & `end_date` >= as.Date(input$dateRange[2]) & `booking status` == "Not_Canceled") %>%
      summarise(revenue_full = sum(`average price` * length_of_stay), 
                num = n()) 
    
    not_full_in_range <- data %>%
      filter(`start_date` <= as.Date(input$dateRange[1]) & `end_date` < as.Date(input$dateRange[2]) & `end_date` > as.Date(input$dateRange[1]) & `booking status` == "Not_Canceled") %>%
      summarise(part_revenue = sum(`average price` * as.numeric(difftime(`end_date`, as.Date(input$dateRange[1]), units = "days"))),
                num = n()) 
    
    not_full_in_range2 <- data %>%
      filter(`start_date` > as.Date(input$dateRange[1]) & `start_date` < as.Date(input$dateRange[2]) & `end_date` >= as.Date(input$dateRange[2]) & `booking status` == "Not_Canceled") %>%
      summarise(part_revenue = sum(`average price` * as.numeric(difftime(as.Date(input$dateRange[2]), `start_date`, units = "days"))),
                num = n()) 
    
    not_full_in_range3 <- data %>%
      filter(`start_date` > as.Date(input$dateRange[1]) & `start_date` < as.Date(input$dateRange[2]) & `end_date` > as.Date(input$dateRange[1]) & `end_date` < as.Date(input$dateRange[2]) & `booking status` == "Not_Canceled") %>%
      summarise(part_revenue = sum(`average price` * as.numeric(difftime(`end_date`, `start_date`, units = "days"))),
                num = n()) 
    
    total_Revenue = full_in_range$revenue_full + not_full_in_range$part_revenue + not_full_in_range2$part_revenue + not_full_in_range3$part_revenue
    occupiedRooms = full_in_range$num + not_full_in_range$num + not_full_in_range2$num + not_full_in_range3$num
    
    # Handle the situation where the filters result in empty data
    if(occupiedRooms == 0) { 
      total_Revenue = 0
      occupiedRooms = 1
    }
    shinydashboard::valueBox(paste("$", round(total_Revenue / occupiedRooms, 2)), "Revenue per Booking", icon = icon("usd"), color = "yellow")
  })
  
  
  # Bookings over time
  output$bookingsPlot <- plotly::renderPlotly({
    req(input$dateRange)
    
    data <- reactiveData()
    
    # Calculate the number of days in the selected range
    num_days <- as.numeric(difftime(as.Date(input$dateRange[2]), as.Date(input$dateRange[1]), units = "days")) + 1
    
    # get not-cancelled bookings within the date range
    values <- get_values(data, as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), "Not_Canceled")
    
    # get the sequence of data within the date range selected
    dates <- seq.Date(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), by = "day")
    
    # create data frame with dates and values
    this_data <- data.frame(date = dates, value = values)
    
    # Determine date breaks based on the number of days
    date_breaks <- case_when(
      num_days <= 10 ~ "1 days",
      num_days <= 20 ~ "2 days",
      num_days <= 50 ~ "5 days",
      num_days <= 100 ~ "10 days",
      num_days <= 200 ~ "20 days",
      num_days <= 300 ~ "30 days",
      num_days <= 400 ~ "40 days",
      num_days <= 500 ~ "50 days",
      num_days > 500 ~ "100 days",
    )
    
    # create the plot
    p <- ggplot(this_data, aes(x = date, y = value)) +
      geom_line() + 
      geom_hline(yintercept = totalRoomsEstimate, linetype = "dashed", color = "red") +
      scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y") +
      labs(x = "Date", y = "Number of Occupied Rooms") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # add data points to the plot in case there are <= 20 data points 
    # if we would display data points for the situations where there is way more data displayed, the plot could look messy and packed, and the hotels probably would not be able to learn from the data and derive insights from the plots properly. That's why we set the threshold here
    if(num_days <= 20) {
      p <- p + geom_point()
    }
    
    # print the plot
    print(p)
  })
  
  
  
  # Plot the weekly cancellations over time
  output$cancellationsPlot <- plotly::renderPlotly({
    req(input$dateRange)
    
    data <- reactiveData()
    
    # Calculate the number of days in the selected range
    num_days <- as.numeric(difftime(as.Date(input$dateRange[2]), as.Date(input$dateRange[1]), units = "days")) + 1
    
    # get cancelled bookings within the date range
    values <- get_values(data, as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), "Canceled")
    
    # get the sequence of data within the date range selected
    dates <- seq.Date(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), by = "day")
    
    # create data frame with dates and values
    this_data <- data.frame(date = dates, value = values)
    
    # Determine date breaks based on the number of days
    date_breaks <- case_when(
      num_days <= 10 ~ "1 days",
      num_days <= 20 ~ "2 days",
      num_days <= 50 ~ "5 days",
      num_days <= 100 ~ "10 days",
      num_days <= 200 ~ "20 days",
      num_days <= 300 ~ "30 days",
      num_days <= 400 ~ "40 days",
      num_days <= 500 ~ "50 days",
      num_days > 500 ~ "100 days",
    )
    
    # Create the plot
    p <- ggplot(this_data, aes(x = date, y = value)) +
      geom_line() + 
      scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y") +
      labs(x = "Date", y = "Number of Cancellations") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Add points for plots which only display <= data entries
    # again, we set this threshold to avoid that the plot looks too packed in case it displays more data 
    if(num_days <= 20) {
      p <- p + geom_point()
    }
    
    # print the plot
    print(p)
  })
  

  
  
  # CANCELLATION PREDICTION using rfModel
  
  # Observe event for the predict button
  cancellation_probability <- reactiveVal(0)

  observeEvent(input$predictBtn, {
    
    # Safely read a single value from the input, or assign a default value if it's NULL or has multiple values
    safely_read_input <- function(input_val, default = NA) {
      if(is.null(input_val) || length(input_val) != 1) {
        return(default)
      } else {
        return(input_val)
      }
    }
    
    # Create a new data frame
    # The data frame will include the values that are selected for the provided filters
    # For the remaining variables, we select default values
    new_data <- data.frame(
      number_of_adults = safely_read_input(input$numAdultsPrediction_cp, default = 1),
      number_of_children = safely_read_input(input$numChildrenPrediction_cp, default = 0),
      number_of_weekend_nights = safely_read_input(input$numWeekendNightsPrediction, default = 2),
      number_of_week_nights = safely_read_input(input$numWeekNightsPrediction, default = 0),
      type_of_meal = safely_read_input(input$mealTypePrediction, default = "Not Specified"),
      car_parking_space = safely_read_input(input$carParkingPrediction, default = 0),
      room_type = safely_read_input(input$roomTypePrediction, default = "Standard"),
      lead_time = safely_read_input(input$leadTimePrediction, default = 0),
      market_segment_type = safely_read_input(input$marketSegmentPrediction, default = "Other"),
      repeated = safely_read_input(input$repeatedPrediction, default = 0),
      P_C = safely_read_input(input$PCPrediction, default = 0),
      P_not_C = safely_read_input(input$PnotCPrediction, default = 0),
      average_price = safely_read_input(input$averagePricePrediction, default = 0),
      special_requests = safely_read_input(input$specialRequestsPrediction, default = 0),
      stringsAsFactors = FALSE
    )
    
    
    # Predict the probability of cancellation using the loaded rfModel
    prediction_prob <- predict(rfModel, newdata = new_data, type = "prob")
    
    # Extract the probability of cancellation
    cancellation_probability(prediction_prob[, "1"]) # Update reactive value
    
    # Update the gauge output
    output$cancellationProbability <- renderGauge({
      gauge(value = cancellation_probability() * 100, min = 0, max = 100, symbol = '%',
            gaugeSectors(success = c(0, 10), warning = c(10, 20), danger = c(20, 100)),
            label = "Cancellation probability")
    })
    
    # Create the warning text for those bookings with high cancellation probability to inform the hotels that it might be a good idea to ask the client for confirmation
    output$warningSign <- renderUI({
      if(cancellation_probability() > 0.25) {
        return(tags$div(tags$h4("High cancellation probability! Contact the client to confirm reservation!"),
                        class = "warning-text"))
    
      }
    })
        
    # Extract the probability of cancellation
    cancellation_probability <- prediction_prob[, "1"] # Assuming '1' is the factor level for cancellation
    
    # Output the cancellation probability using a probability progress bar
    output$cancellationProbability <- renderGauge({
      gauge(value = cancellation_probability * 100, min = 0, max = 100, symbol = '%',
            gaugeSectors(success = c(0, 10), warning = c(10, 20), danger = c(20, 100)),
            label = "Cancellation probability")
    })
  })
  
  
  
  # PRICE RECOMMENDATION using lmModel
  
  # Observe event for the predict button
  observeEvent(input$predictPriceBtn, {
    
    # Safely read a single value from the input, or assign a default value if it's NULL or has multiple values
    safely_read_input <- function(input_val, default = NA) {
      if(is.null(input_val) || length(input_val) != 1) {
        return(default)
      } else {
        return(input_val)
      }
    }
    
    # Assuming all the inputs are dropdowns that should have a single selected value    # Create a new data frame
    # The data frame will include the values that are selected for the provided filters
    # For the remaining variables, we select default values    
    new_booking <- data.frame(
      number_of_adults = safely_read_input(input$numAdultsPrediction_pr, default = 1),
      number_of_children = safely_read_input(input$numChildrenPrediction_pr, default = 0),
      number_of_weekend_nights = safely_read_input(input$numWeekendNightsPrediction, default = 2),
      number_of_week_nights = safely_read_input(input$numWeekNightsPrediction, default = 0),
      type_of_meal = factor(input$typeOfMealPrediction, levels = levels(trainingSet_lr$type_of_meal)),
      car_parking_space = safely_read_input(input$carParkingPrediction, default = 0),
      room_type = factor(input$roomTypePrediction, levels = levels(trainingSet_lr$room_type)),
      lead_time = safely_read_input(input$leadTimePrediction, default = 0),
      market_segment_type = factor(input$marketSegmentPrediction, levels = levels(trainingSet_lr$market_segment_type)),
      repeated = safely_read_input(input$repeatedPrediction, default = 0),
      P_C = safely_read_input(input$PCPrediction, default = 0),
      P_not_C = safely_read_input(input$PnotCPrediction, default = 0),
      special_requests = safely_read_input(input$specialRequestsPrediction, default = 0),
      stringsAsFactors = FALSE
    )
    
    # Predict the probability of cancellation using the loaded lmModel
    predicted_price <- predict(lmModel, newdata = new_booking)
    
    # Create price tag visualization
    output$priceTagPlot <- renderPlot({

       par(bg = "transparent")
       plot.new()
       plot.window(xlim = c(0, 10), ylim = c(0, 10))
       
       # Draw the tag with a shadow for depth
       rect(2.05, 5.05, 8.05, 9.05, col = 'grey90', border = NA)  # Shadow
       rect(2, 5, 8, 9, col = 'ivory', border = 'darkgoldenrod', lwd = 2)  # Tag
       
       # Draw the string
       lines(c(5, 5), c(9, 10), lwd = 2)
       
       # Add the price text with a bold font
       text(5, 7, labels = paste("$", format(round(predicted_price, 2), nsmall = 2), sep = ""), cex = 3, col = '#8B0000', font = 2)
       
       # Add dollar signs to the left and right of the tag
       text(5, 5.5, labels = "PRICE RECOMMENDATION", cex = 1, col = "darkgoldenrod", font = 2)
       text(3, 7, labels = "$", cex = 5, col = 'darkgoldenrod', font = 2)
       text(7, 7, labels = "$", cex = 5, col = 'darkgoldenrod', font = 2)
     }, bg = "transparent"
    )
    
    # Add the dynamic pictures for some of the filter options
    # For example, this displays a picture of two adults and one child for that filter selection
    # With this, we want to make the application more user friendly and make the interface look more appealing
    output$dynamic_image <- renderUI({
      
      # Construct the file name based on filter inputs
      file_name <- paste0("image_", input$numAdultsPrediction_pr, "_", input$numChildrenPrediction_pr, ".webp") 
      
      timestamp <- Sys.time()
      
      # Check if the image file exists in the 'www' directory
      if(file.exists(file.path("www", file_name))) {
        tags$img(src = file_name, height = "200px", width = "200px") # Display the image
      } else {
        "" # No image displayed for the remaining filter options
      }
    })
  })
}  


# Run the application -----------------------------------------------------------------------
shinyApp(ui = ui, server = server)
