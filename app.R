library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(maps)
library(ggh4x)
library(gridExtra)
library(caret)
library(reshape2)
library(lpSolve)

data <- read_excel("/Documents and Settings/mrose/Documents/Window_Manufacturing.xlsx")
names(data) <- gsub(" ", "_", names(data))

data <- data %>%
  mutate_at(vars(1:8, 11), as.numeric)

data <- data %>%
  mutate_at(vars(9, 10, 12), as.factor)

source("/Documents and Settings/mrose/Documents/regression_model.R")
source("/Documents and Settings/mrose/Documents/app_function.R")
source("/Documents and Settings/mrose/Documents/app_function_2.R")

ui <- fluidPage(
  titlePanel("Final Project"),
  
  tabsetPanel(
    tabPanel("Overview",
             # Content
             tags$style(HTML("
               h2 {
                 color: #132B43;
                 font-weight: bold;
               }
             ")),
             h2("Business Problem"),
             h4(style = "color: #3366CC;", "ClearVue Windows located in San Francisco, CA is experiencing high rates of window breakage, approximately 1 in every 16, during the manufacturing process. This issue has resulted in increased costs, production delays and decreased customer satisfaction. It is crucial for ClearVue Windows to address this problem to improve operational efficiency, maintain customer satisfaction and reduce financial losses."),
             h4(style = "color: #3366CC;", "We understand that you the manufacturer do not have control over what the supplier provides to you in terms of specifications that are chosen at the time of pouring the glass. We have taken this into account by adding constraints to these outside factors based on the data provided in order to accurately represent the state of the window glass as it is received by you from the supplier. We also understand that the customer comes first and their specifications are a priority, so we have made it possible to include those customer specifications when optimizing the manufacturer details."),
             h4(style = "color: #3366CC;", "Our goal is for our application to identify the issues in the manufacturing process causing the high rates of window breakage and create a decision support system (DSS) that will reduce window breakage during the manufacturing process by at least 20% so that only 1 in 20 windows is breaking."),
             
             h2("Analytics Problem Framing"),
             h4(style = "color: #3366CC;", "Our first approach to achieving our goal was to use descriptive analytics to examine the relationships between the variables in the data we were provided in the hopes of identifying any strong relationships between them and the breakage rate."),
             h4(style = "color: #3366CC;", "Predicting breakage rate is a regression problem so we have created a linear regression model using the caret library. We also want to minimize that breakage rate which is an optimization problem so we employed prescriptive analytics by using the ipSolve library to optimize the linear equation from our regression model."),
             h4(style = "color: #3366CC;", "Our end users can expect to recieve optimized values for the following manufacturer specifications based on what their customers want: edge deletion rate, spacer distance, and cut speed. End users can take these values and implement them during the manufacturing process to reduce breakage rate."),
             h4(style = "color: #3366CC;", "Key Assumptions:"),
             p(style = "color: #56B1F7;","The historical data used for analysis and modeling accurately represents the manufacturing process and its associated factors affecting window breakage."), 
             p(style = "color: #56B1F7;","High rates of window breakage are due to factors within the manufacturing process rather than external factors like transportation or customer mishandling"),
             h4(style = "color: #3366CC;", "Key Metrics for Success:"),
             p(style = "color: #56B1F7;", "Reducing the breakage rate by an average 20% improvement."),
             p(style = "color: #56B1F7;", "Providing end users with manufacturer specifications that are feasible and within tolerance levels."),
             p(style = "color: #56B1F7;", "Demonstrate to end users the relationships between the breakage rate and all relevant variables, manufacturer or otherwise."),
             h2("Data"),
             h3(style = "color: #3366CC;", "Data Dictionary"),
             tableOutput("dataDictionary"),
             h4(style = "color: #56B1F7;", "Data source:"),
             p("https://purdue0-my.sharepoint.com/:x:/g/personal/mart2336_purdue_edu/EQtHw5veA1VPlt0bf_WFSBoBQB2B2ddjQ9bGMUvPjexNrg?e=yU00pm")
    ),
    
    tabPanel("Descriptive Analytics",
             fluidRow(
               column(width = 6,
                      plotOutput("plot1"),
                      verbatimTextOutput("text1")
               ),
               column(width = 6,
                      plotOutput("plot2"),
                      verbatimTextOutput("text2")
               )
             ),
             fluidRow(
               column(width = 6,
                      plotOutput("plot3"),
                      verbatimTextOutput("text3")
               ),
               column(width = 6,
                      plotOutput("plot4"),
                      verbatimTextOutput("text4")
               )
             )
    ),
    
    tabPanel("Predictive Analytics",
             fluidRow(
               column(width = 4,
                      h2("Model Specifications"),
                      tags$ul(
                        tags$li(style = "color: #3366CC;", "The model only considers complete cases in the data (does not consider NA values)"),
                        tags$li(style = "color: #3366CC;", "The model has removed highly correlated variables based on a correlation cutoff of .8 or higher."),
                        tags$li(style = "color: #3366CC;", "Categorical variables (supplier, supplier location, and window type) have been encoded so that they can be used in the model"),
                        tags$li(style = "color: #3366CC;", "Data has been preprocessed and scaled so all values are between 0 and 1."),
                        tags$li(style = "color: #3366CC;", "Data has been seperated 70/30 into training and testing data for the model.")
                      )
               ),
               column(width = 4,
                      h4("Model Fit Evaluation"),
                      plotOutput("modelFitPlot")
               ),
               column(width = 4,
                      h4("Coef Table"),
                      tableOutput("coefTable")
               )
             ),
             fluidRow(
               column(width = 12,
                      h4("Predicted vs. Actual"),
                      plotOutput("predictedVsActualPlot")
               )
             )
    ),
    
    tabPanel("Prescriptive Analytics",
             fluidRow(
               column(width = 4,
                      h4("Customer Specifications"),
                      sliderInput("windowSizeInput", "Window Size:", min = round(min(data$Window_Size, na.rm = TRUE), 2), max = round(max(data$Window_Size, na.rm = TRUE), 2), value = mean(data$Window_Size, na.rm = TRUE)),
                      selectInput("windowTypeInput", "Window Type:", choices = unique(data$Window_Type[!is.na(data$Window_Type)]))
               ),
               column(width = 4,
                      h4("Supplier Specifications"),
                      selectInput("glassSupplierInput", "Glass Supplier:", choices = unique(data$Glass_Supplier[!is.na(data$Glass_Supplier)])),
                      sliderInput("glassThicknessInput", "Glass Thickness:", min = round(min(data$Glass_thickness, na.rm = TRUE), 2), max = round(max(data$Glass_thickness, na.rm = TRUE), 2), value = .5),
                      sliderInput("windowColorInput", "Window Color:", min = round(min(data$Window_color, na.rm = TRUE), 2), max = round(max(data$Window_color, na.rm = TRUE), 2), value = 54)
               ),
               column(width = 4,
                      actionButton("optimizeButton", "Show me optimal manufacturing specifications")
               )
             ),
             fluidRow(
               column(width = 8,
                      h4("Optimal Manufacturing Specifications"),
                      tableOutput("optimizedTable"),
               ),
               column(width = 4,
                      h2("Constraints"),
                      tags$ul(
                        tags$li(style = "color: #3366CC;","User inputs are constrained by the minimum and maximum values for corresponding variables in the dataset."),
                        tags$li(style = "color: #3366CC;","Other supplier specifications such as ambient temperature and silicon viscosity are estimated based on data provided."),
                        tags$li(style = "color: #3366CC;","Optimized values below are constrained similarly by the minimum and maximum values for corresponding variables in the dataset.")
                      )
  )))
))

server <- function(input, output) {
  
  output$dataDictionary <- renderTable({
    variable_names <- gsub("_", " ", names(data))
    r_data_types <- sapply(data, class)
    short_descriptions <- c(
      "Portion of windows that broke out of 1.0","Customer specification - size of window (inches)",
      "Supplier specification (inches)","Supplier specification (degrees Celsius)", "Manufacturer specification - speed at which window glass is cut (m/min)",
      "Manufacturer specification - percentage of windows requiring edge deletion", "Manufacturer specification (mm)",
      "Supplier specification - protective coating, value indicated percent tint", "Customer specification - window frame material",
      "Glass supplier A, B, C, or D", "Supplier specification - viscosity of the glass when poured (poise)", "Michigan, Minnesota, Wisconsin, or Iowa"
    )
    data_dictionary <- data.frame(
      "Variable Name" = variable_names,
      "R Data Type" = r_data_types,
      "Short Variable Description" = short_descriptions,
      stringsAsFactors = FALSE
    )
    data_dictionary
  })
  
  output$plot1 <- renderPlot({
    filtered_data <- data %>% na.omit()
    
    avg_breakage <- filtered_data %>%
      group_by(Glass_Supplier_Location) %>%
      summarise(Avg_Breakage_Rate = mean(Breakage_Rate, na.rm = TRUE))
    
    avg_breakage$Glass_Supplier_Location <- tolower(avg_breakage$Glass_Supplier_Location)
    map_data <- map_data("state")
    map_data2 <- left_join(map_data, avg_breakage, by = c("region" = "Glass_Supplier_Location"))
    
    label_data <- map_data2 %>%
      filter(region %in% c("iowa", "michigan", "minnesota", "wisconsin"))
    
    ggplot(map_data2, aes(map_id = region, fill = Avg_Breakage_Rate)) +
      geom_map(map = map_data,
               color = "black", linewidth = 0.2) +
      stat_midpoint(data = label_data, aes(label = region, x = long, y = lat), geom = "text", size=3, color="white") +
      scale_fill_gradient(low="#56B1F7", high="#132B43") +
      expand_limits(x = map_data$long, y = map_data$lat) +
      labs(fill = "Average Breakage Rate")
  })
  
  output$text1 <- renderPrint({
    cat("Average breakage rate by glass supplier location.")
  })
  
  output$plot2 <- renderPlot({
    filtered_data <- data %>% na.omit()
    
    avg_breakage <- filtered_data %>%
      group_by(Glass_Supplier, Window_Type) %>%
      summarise(Avg_Breakage_Rate = mean(Breakage_Rate, na.rm = TRUE), .groups = "drop")
    
    colors <- c("#56B1F7", "#3366CC", "#132B43")
    
    ggplot(avg_breakage, aes(x = Glass_Supplier, y = Avg_Breakage_Rate, fill = Window_Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = colors) +
      labs(x = "Glass Supplier", y = "Average Breakage Rate") +
      theme_bw()
  })
  
  output$text2 <- renderPrint({
    cat("Average breakage rate by glass supplier and window type.")
  })
  
  output$plot3 <- renderPlot({
    filtered_data <- data %>% na.omit()
    
    ggplot(filtered_data, aes(x = Window_Size, y = Breakage_Rate)) +
      geom_point(color = "#56B1F7") +
      geom_smooth(formula = y ~ x, method = "lm", se = FALSE, color = "#132B43") +
      labs(x = "Window Size", y = "Breakage Rate") +
      theme_bw()
  })
  
  output$text3 <- renderPrint({
    cat("Window Size vs. Breakage Rate with regression line.")
  })
  
  output$plot4 <- renderPlot({
    filtered_data <- data %>% na.omit()
    plots <- list()
    columns <- c("Glass_thickness", "Cut_speed", "Edge_Deletion_rate")
    fill_colors <- c("#56B1F7", "#3366CC", "#132B43")
    
    for (i in seq_along(columns)) {
      col <- columns[i]
      bin_width <- (max(filtered_data[[col]]) - min(filtered_data[[col]])) / 8
      bins <- seq(min(filtered_data[[col]]), max(filtered_data[[col]]), by = bin_width)
      avg_breakage <- filtered_data %>%
        group_by(bin = cut(.data[[col]], breaks = bins, include.lowest = TRUE, right = FALSE)) %>%
        summarise(Avg_Breakage_Rate = mean(Breakage_Rate)) %>%
        filter(!is.na(bin))
      
      plot <- ggplot(avg_breakage, aes(x = bin, y = Avg_Breakage_Rate)) +
        geom_bar(stat = "identity", fill = fill_colors[i]) +
        labs(x = gsub("_", " ", col), y = "Average Breakage Rate") +
        theme_bw()
      plots[[col]] <- plot
    }
    
    p1 <- plots[["Glass_thickness"]]
    p2 <- plots[["Cut_speed"]]
    p3 <- plots[["Edge_Deletion_rate"]]
    grid.arrange(p1, p2, p3, nrow = 3)
  })
  
  output$text4 <- renderPrint({
    cat("Histograms for glass thickness, cut speed, and edge deletion rate vs. average breakage rate.")
  })
  
  output$modelFitPlot <- renderPlot({
    ggplot(melted_stats, aes(x = RowNames, y = value, fill = Dataset)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Statistical Measure", y = "Value") +
      scale_fill_manual(values = c("Train_Stats" = "#56B1F7", "Test_Stats" = "#132B43")) +
      theme_bw()
  })
  
  output$predictedVsActualPlot <- renderPlot({

    predicted <- predict(myModel, newdata = d)
    actual <- d$y
    
    ggplot() +
      geom_point(aes(x = actual, y = predicted), color = "#56B1F7") +
      geom_abline(intercept = 0, slope = 1, color = "#132B43") +
      labs(x = "Actual Values", y = "Predicted Values") +
      theme_bw()
  })
  
  output$coefTable <- renderTable({
    coef_table <- summary(myModel)$coef
    Predictor_Variable <- rownames(coef_table)
    Predictor_Variable <- data.frame(Predictor_Variable)
    coef_table[, "Pr(>|t|)"] <- round(coef_table[, "Pr(>|t|)"], 4)
    coef_table <- data.frame(coef_table)
    coef_table <- cbind(Predictor_Variable,coef_table)
    names(coef_table)[1] <- "Predictor Variable"
    names(coef_table)[3] <- "Std. Error"
    names(coef_table)[4] <- "t value"
    names(coef_table)[5] <- "Pr(>|t|)"
    coef_table
  })
  
  observeEvent(input$optimizeButton, {
    output$optimizedTable <- renderTable({
      optimizeValues()
    })
  })
  
  optimizeValues <- reactive({

    objectiveCoefficients <- coef(myModel$finalModel)[-c(1)] 
    objectiveCoefficients <- c(objectiveCoefficients)

    constraintMatrix <- matrix(0, nrow = 17, ncol = length(objectiveCoefficients))
    colnames(constraintMatrix) <- colnames(d[-1])
    columnNames <- colnames(constraintMatrix)
    windowSizeIndex <- which(columnNames == "Window_Size")
    windowTypeIndex <- which(columnNames == toString(find_window_type(input$windowTypeInput)))
    supplierLocationIndex <- which(columnNames == toString(find_supplier_location(input$glassSupplierInput)))
    glassThicknessIndex <- which(columnNames == "Glass_thickness")
    windowColorIndex <- which(columnNames == "Window_color")
    ambientTempIndex <- which(columnNames == "Ambient_Temp")
    cutSpeedIndex <- which(columnNames == "Cut_speed")
    edgeDeletionIndex <- which(columnNames == "Edge_Deletion_rate")
    spacerDistanceIndex <- which(columnNames == "Spacer_Distance")
    siliconViscosityIndex <- which(columnNames == "Silicon_Viscosity")
    
    values <- c(7, 8)
    windowTypeIndex2 <- values[!values %in% windowTypeIndex]
    values2 <- c(10,11,12)
    supplierLocations <- values2[!values2 %in% supplierLocationIndex]
    supplierLocationIndex2 <- supplierLocations[1]
    supplierLocationIndex3 <- supplierLocations[2]

    constraintMatrix[1, windowSizeIndex] <- 1
    constraintMatrix[2, windowTypeIndex] <- 1
    constraintMatrix[3, supplierLocationIndex] <- 1
    constraintMatrix[4, glassThicknessIndex] <- 1
    
    constraintMatrix[5, windowTypeIndex2] <- 1
    constraintMatrix[6, supplierLocationIndex2] <- 1
    constraintMatrix[7, supplierLocationIndex3] <- 1
    
    constraintMatrix[8, ambientTempIndex] <- 1
    constraintMatrix[9, cutSpeedIndex] <- 1
    constraintMatrix[10, edgeDeletionIndex] <- 1
    constraintMatrix[11, spacerDistanceIndex] <- 1
    constraintMatrix[12, siliconViscosityIndex] <- 1
    
    constraintMatrix[13, cutSpeedIndex] <- 1
    constraintMatrix[14, edgeDeletionIndex] <- 1
    constraintMatrix[15, spacerDistanceIndex] <- 1
    constraintMatrix[16, siliconViscosityIndex] <- 1
    constraintMatrix[17, ambientTempIndex] <- 1
    
    constraintValues <- c(input$windowSizeInput, 1, 1, input$glassThicknessInput, 0, 0, 0, 
                          max = round(max(data$Ambient_Temp, na.rm = TRUE), 2), max = round(max(data$Cut_speed, na.rm = TRUE), 2), 
                          max = round(max(data$Edge_Deletion_rate, na.rm = TRUE), 2), max = round(max(data$Spacer_Distance, na.rm = TRUE), 2), 
                          max = round(max(data$Silicon_Viscosity, na.rm = TRUE), 2), round(min(data$Cut_speed, na.rm = TRUE), 2),
                          round(min(data$Edge_Deletion_rate, na.rm = TRUE), 2), round(min(data$Glass_thickness, na.rm = TRUE), 2),
                          round(min(data$Silicon_Viscosity, na.rm = TRUE), 2), round(min(data$Ambient_Temp, na.rm = TRUE), 2))

    constraintDirections <- rep("=", 7)
    constraintDirections <- c(constraintDirections, rep("<=", 5))
    constraintDirections <- c(constraintDirections, rep(">", 5))

    objectiveSense <- "min"
    
    lpSolution <- lp(direction = "min", 
                     objective.in = objectiveCoefficients, 
                     const.mat = constraintMatrix, 
                     const.dir = constraintDirections, 
                     const.rhs = constraintValues, 
                     compute.sens = TRUE)
    

    optimizedValues <- lpSolution$solution
    optimizedVariables <- names(objectiveCoefficients)

    optimizedTable <- data.frame(Predictor_Variable = optimizedVariables, Optimized_Value = optimizedValues)
    
    optimized_values <- t(optimizedValues)
    colnames(optimized_values) <- optimizedVariables
    optimized_values <- data.frame(optimized_values)
    
    predicted_values <- predict(myModel, newdata = optimized_values)
    newRow <- data.frame(
      Predictor_Variable = "Breakage_Rate",
      Optimized_Value = predicted_values
    )
    
    optimizedTable <- rbind(optimizedTable, newRow)
    optimizedTable <- subset(optimizedTable, Predictor_Variable %in% c("Cut_speed", "Edge_Deletion_rate", "Spacer_Distance", "Breakage_Rate"))

    return(optimizedTable)
  })

}

shinyApp(ui = ui, server = server)