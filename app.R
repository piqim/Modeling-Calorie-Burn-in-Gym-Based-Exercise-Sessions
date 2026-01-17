# ==============================================================================
# Interactive Gym Exercise Analysis Dashboard
# Project: Modeling Calorie Burn in Gym-Based Exercise Sessions
# ==============================================================================

# Load packages ----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(broom)
library(car)
library(emmeans)
library(ggeffects)

# Load and prepare data --------------------------------------------------------
gym_data <- read_csv("dataset/gym_members_exercise_tracking.csv", 
                     show_col_types = FALSE) %>%
  mutate(
    Gender = factor(Gender),
    Workout_Type = factor(Workout_Type),
    Experience_Level_Label = case_when(
      Experience_Level == 1 ~ "Beginner",
      Experience_Level == 2 ~ "Intermediate",
      Experience_Level == 3 ~ "Expert",
      TRUE ~ as.character(Experience_Level)
    ),
    Experience_Level_Label = factor(Experience_Level_Label, 
                                    levels = c("Beginner", "Intermediate", "Expert"))
  )

# ==============================================================================
# UI
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header ---------------------------------------------------------------------
  dashboardHeader(
    title = "Gym Exercise Analytics",
    titleWidth = 300
  ),
  
  # Sidebar --------------------------------------------------------------------
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("📊 Data Explorer", tabName = "data_tab", icon = icon("database")),
      menuItem("📈 Descriptive Stats", tabName = "descriptive_tab", icon = icon("chart-bar")),
      menuItem("🔬 Statistical Inference", tabName = "inference_tab", icon = icon("flask")),
      menuItem("🤖 Modeling & Prediction", tabName = "modeling_tab", icon = icon("brain")),
      menuItem("📥 Download Results", tabName = "download_tab", icon = icon("download")),
      menuItem("ℹ️ About", tabName = "about_tab", icon = icon("info-circle"))
    )
  ),
  
  # Body -----------------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box-title { font-weight: bold; }
        .info-box { cursor: pointer; }
        .small-box { cursor: pointer; }
      "))
    ),
    
    tabItems(
      
      # TAB 1: Data Explorer ---------------------------------------------------
      tabItem(
        tabName = "data_tab",
        
        fluidRow(
          box(
            title = "Dataset Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h4("Gym Members Exercise Tracking Dataset"),
            p("This dataset contains information about gym members' exercise sessions, 
              including demographics, workout details, and calories burned."),
            hr(),
            valueBoxOutput("total_records", width = 3),
            valueBoxOutput("total_variables", width = 3),
            valueBoxOutput("avg_calories", width = 3),
            valueBoxOutput("avg_duration", width = 3)
          )
        ),
        
        fluidRow(
          box(
            title = "Data Preview",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            sliderInput("preview_rows", "Number of rows to display:",
                        min = 5, max = 50, value = 10, step = 5),
            DTOutput("data_preview")
          )
        ),
        
        fluidRow(
          box(
            title = "Variable Summary",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("data_summary")
          )
        )
      ),
      
      # TAB 2: Descriptive Statistics ------------------------------------------
      tabItem(
        tabName = "descriptive_tab",
        
        fluidRow(
          box(
            title = "Variable Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            selectInput("desc_variable",
                        "Select Variable:",
                        choices = c("Age", "Weight (kg)", "Height (m)", "BMI",
                                    "Max_BPM", "Avg_BPM", "Resting_BPM",
                                    "Session_Duration (hours)", "Calories_Burned",
                                    "Fat_Percentage", "Water_Intake (liters)",
                                    "Workout_Frequency (days/week)"),
                        selected = "Calories_Burned"),
            
            selectInput("desc_grouping",
                        "Group By (Optional):",
                        choices = c("None", "Gender", "Workout_Type", "Experience_Level_Label"),
                        selected = "None"),
            
            actionButton("update_desc", "Update Visualization", 
                         class = "btn-primary", icon = icon("refresh"))
          ),
          
          box(
            title = "Summary Statistics",
            status = "info",
            solidHeader = TRUE,
            width = 9,
            DTOutput("summary_stats_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Distribution Plot",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("distribution_plot", height = "400px")
          ),
          
          box(
            title = "Boxplot by Group",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("boxplot_group", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            title = "Categorical Variables Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4, plotlyOutput("gender_plot", height = "300px")),
              column(4, plotlyOutput("workout_plot", height = "300px")),
              column(4, plotlyOutput("experience_plot", height = "300px"))
            )
          )
        )
      ),
      
      # TAB 3: Statistical Inference -------------------------------------------
      tabItem(
        tabName = "inference_tab",
        
        fluidRow(
          box(
            title = "Hypothesis Test Selection",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            selectInput("hypothesis_test",
                        "Select Research Question:",
                        choices = c(
                          "Compare Calories by Gender" = "gender_test",
                          "Compare Calories by Workout Type" = "workout_test",
                          "Compare Calories by Experience Level" = "experience_test",
                          "Gender × Workout Type Interaction" = "interaction_test"
                        ),
                        selected = "gender_test"),
            
            actionButton("run_test", "Run Statistical Test", 
                         class = "btn-success", icon = icon("play")),
            
            hr(),
            h4("Test Information"),
            textOutput("test_description")
          ),
          
          box(
            title = "Test Results",
            status = "info",
            solidHeader = TRUE,
            width = 8,
            verbatimTextOutput("test_results"),
            hr(),
            h4("Plain Language Interpretation"),
            uiOutput("test_interpretation")
          )
        ),
        
        fluidRow(
          box(
            title = "Visualization",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("inference_plot", height = "450px")
          )
        ),
        
        fluidRow(
          box(
            title = "Post-Hoc Comparisons (if applicable)",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("posthoc_table")
          )
        )
      ),
      
      # TAB 4: Modeling & Prediction -------------------------------------------
      tabItem(
        tabName = "modeling_tab",
        
        fluidRow(
          box(
            title = "Select Model",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            radioButtons("model_choice",
                         "Choose Regression Model:",
                         choices = c(
                           "Model 1: Base (Weight + Duration + Avg BPM)" = "model1",
                           "Model 2: With Gender (Weight + Duration + Avg BPM + Gender)" = "model2",
                           "Model 3: ANCOVA (Gender × Workout Type + Duration)" = "model3"
                         ),
                         selected = "model2",
                         inline = TRUE)
          )
        ),
        
        fluidRow(
          box(
            title = "Model Summary",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("model_summary")
          ),
          
          box(
            title = "Model Performance",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("model_performance_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Coefficients",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("coefficients_table")
          )
        ),
        
        fluidRow(
          box(
            title = "🎯 Calorie Burn Predictor",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            h4("Enter Your Details:"),
            
            numericInput("pred_weight", "Weight (kg):", 
                         value = 70, min = 40, max = 130, step = 1),
            
            sliderInput("pred_duration", "Session Duration (hours):", 
                        min = 0.5, max = 2, value = 1.2, step = 0.1),
            
            numericInput("pred_bpm", "Average BPM:", 
                         value = 140, min = 120, max = 170, step = 5),
            
            selectInput("pred_gender", "Gender:", 
                        choices = c("Female", "Male"), selected = "Male"),
            
            selectInput("pred_workout", "Workout Type:", 
                        choices = c("Cardio", "HIIT", "Strength", "Yoga"), 
                        selected = "Cardio"),
            
            actionButton("predict_btn", "Predict Calories", 
                         class = "btn-success btn-lg", icon = icon("calculator"))
          ),
          
          box(
            title = "Prediction Result",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            uiOutput("prediction_output"),
            hr(),
            plotlyOutput("prediction_viz", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "Diagnostic Plots",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6, plotOutput("diagnostic_plot1", height = "350px")),
              column(6, plotOutput("diagnostic_plot2", height = "350px"))
            )
          )
        )
      ),
      
      # TAB 5: Download Results ------------------------------------------------
      tabItem(
        tabName = "download_tab",
        
        fluidRow(
          box(
            title = "Export Options",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h3("Download Analysis Results"),
            p("Select the data you want to export:"),
            hr()
          )
        ),
        
        fluidRow(
          box(
            title = "Descriptive Statistics",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            h4("Summary Tables"),
            downloadButton("download_continuous", "Download Continuous Variables Summary", 
                           class = "btn-primary"),
            br(), br(),
            downloadButton("download_categorical", "Download Categorical Variables Summary", 
                           class = "btn-primary")
          ),
          
          box(
            title = "Model Results",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            h4("Regression Models"),
            downloadButton("download_model1", "Download Model 1 Coefficients", 
                           class = "btn-success"),
            br(), br(),
            downloadButton("download_model2", "Download Model 2 Coefficients", 
                           class = "btn-success"),
            br(), br(),
            downloadButton("download_comparison", "Download Models Comparison", 
                           class = "btn-success")
          )
        ),
        
        fluidRow(
          box(
            title = "ANOVA Results",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            h4("Statistical Tests"),
            downloadButton("download_anova", "Download ANOVA Table", 
                           class = "btn-warning"),
            br(), br(),
            downloadButton("download_posthoc", "Download Post-Hoc Tests", 
                           class = "btn-warning")
          ),
          
          box(
            title = "Complete Dataset",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            h4("Raw Data"),
            downloadButton("download_data", "Download Full Dataset", 
                           class = "btn-danger")
          )
        )
      ),
      
      # TAB 6: About -----------------------------------------------------------
      tabItem(
        tabName = "about_tab",
        
        fluidRow(
          box(
            title = "About This Project",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h3("📐 Modeling Calorie Burn in Gym-Based Exercise Sessions"),
            hr(),
            
            h4("Project Overview"),
            p("This interactive dashboard presents a comprehensive analysis of gym members' 
              exercise data, combining descriptive statistics, statistical inference, and 
              predictive modeling to understand the factors influencing calorie burn during 
              workout sessions."),
            
            h4("Research Questions"),
            tags$ol(
              tags$li(strong("Can we predict calories burned?"), 
                      " Using weight, session duration, and average BPM as predictors."),
              tags$li(strong("Does gender affect calorie burn?"), 
                      " Controlling for physical and workout variables."),
              tags$li(strong("Which workout type is most effective?"), 
                      " Comparing Cardio, HIIT, Strength, and Yoga across genders.")
            ),
            
            h4("Key Findings"),
            tags$ul(
              tags$li("✅ ", strong("Session duration"), " is the strongest predictor of calories burned (R² = 0.94)"),
              tags$li("✅ ", strong("Gender significantly affects"), " calorie burn (~80 calories difference)"),
              tags$li("⚠️ ", strong("Workout type doesn't significantly differ"), 
                      " when duration is controlled"),
              tags$li("💡 ", strong("Duration > Intensity"), " for maximizing calorie burn")
            ),
            
            h4("Methodology"),
            tags$ul(
              tags$li(strong("Part 1:"), " Descriptive Analysis - Explored distributions and relationships"),
              tags$li(strong("Part 2:"), " Statistical Inference - Multiple regression and ANOVA"),
              tags$li(strong("Part 3:"), " Interactive Dashboard - Built with R Shiny")
            ),
            
            h4("Technologies Used"),
            p(tags$code("R"), " • ", tags$code("Shiny"), " • ", tags$code("tidyverse"), " • ",
              tags$code("ggplot2"), " • ", tags$code("plotly"), " • ", tags$code("DT")),
            
            hr(),
            h4("Repository"),
            p("📁 ", a("GitHub Repository", 
                       href = "https://github.com/piqim/Modeling-Calorie-Burn-in-Gym-Based-Exercise-Sessions",
                       target = "_blank")),
            
            h4("Author"),
            p("Created as part of a data analysis portfolio project"),
            p("Dataset: Gym Members Exercise Tracking (973 observations, 15 variables)")
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # TAB 1: Data Explorer -------------------------------------------------------
  
  output$total_records <- renderValueBox({
    valueBox(
      format(nrow(gym_data), big.mark = ","),
      "Total Records",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$total_variables <- renderValueBox({
    valueBox(
      ncol(gym_data),
      "Variables",
      icon = icon("list"),
      color = "green"
    )
  })
  
  output$avg_calories <- renderValueBox({
    valueBox(
      round(mean(gym_data$Calories_Burned), 0),
      "Avg Calories Burned",
      icon = icon("fire"),
      color = "orange"
    )
  })
  
  output$avg_duration <- renderValueBox({
    valueBox(
      paste(round(mean(gym_data$`Session_Duration (hours)`), 2), "hrs"),
      "Avg Session Duration",
      icon = icon("clock"),
      color = "purple"
    )
  })
  
  output$data_preview <- renderDT({
    datatable(
      head(gym_data, input$preview_rows),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'tip'
      ),
      class = 'cell-border stripe'
    )
  })
  
  output$data_summary <- renderPrint({
    summary(gym_data %>% select(-Experience_Level_Label))
  })
  
  # TAB 2: Descriptive Statistics ----------------------------------------------
  
  output$summary_stats_table <- renderDT({
    req(input$desc_variable)
    
    if (input$desc_grouping == "None") {
      summary_data <- gym_data %>%
        summarise(
          Mean = mean(.data[[input$desc_variable]], na.rm = TRUE),
          Median = median(.data[[input$desc_variable]], na.rm = TRUE),
          SD = sd(.data[[input$desc_variable]], na.rm = TRUE),
          Min = min(.data[[input$desc_variable]], na.rm = TRUE),
          Max = max(.data[[input$desc_variable]], na.rm = TRUE),
          Count = n()
        ) %>%
        mutate(across(where(is.numeric), ~round(., 2)))
    } else {
      summary_data <- gym_data %>%
        group_by(Group = .data[[input$desc_grouping]]) %>%
        summarise(
          Mean = mean(.data[[input$desc_variable]], na.rm = TRUE),
          Median = median(.data[[input$desc_variable]], na.rm = TRUE),
          SD = sd(.data[[input$desc_variable]], na.rm = TRUE),
          Min = min(.data[[input$desc_variable]], na.rm = TRUE),
          Max = max(.data[[input$desc_variable]], na.rm = TRUE),
          Count = n(),
          .groups = "drop"
        ) %>%
        mutate(across(where(is.numeric), ~round(., 2)))
    }
    
    datatable(summary_data, 
              options = list(dom = 't'),
              class = 'cell-border stripe')
  })
  
  output$distribution_plot <- renderPlotly({
    req(input$desc_variable)
    
    p <- ggplot(gym_data, aes(x = .data[[input$desc_variable]])) +
      geom_histogram(bins = 30, fill = "#2E86AB", color = "white", alpha = 0.8) +
      geom_vline(aes(xintercept = mean(.data[[input$desc_variable]])), 
                 color = "red", linetype = "dashed", linewidth = 1) +
      labs(title = paste("Distribution of", input$desc_variable),
           x = input$desc_variable,
           y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$boxplot_group <- renderPlotly({
    req(input$desc_variable)
    
    if (input$desc_grouping == "None") {
      p <- ggplot(gym_data, aes(y = .data[[input$desc_variable]])) +
        geom_boxplot(fill = "#A23B72", alpha = 0.7) +
        labs(title = paste("Boxplot of", input$desc_variable),
             y = input$desc_variable) +
        theme_minimal() +
        theme(axis.text.x = element_blank())
    } else {
      p <- ggplot(gym_data, aes(x = .data[[input$desc_grouping]], 
                                y = .data[[input$desc_variable]],
                                fill = .data[[input$desc_grouping]])) +
        geom_boxplot(alpha = 0.7) +
        labs(title = paste(input$desc_variable, "by", input$desc_grouping),
             x = input$desc_grouping,
             y = input$desc_variable) +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p)
  })
  
  output$gender_plot <- renderPlotly({
    p <- ggplot(gym_data, aes(x = Gender, fill = Gender)) +
      geom_bar(alpha = 0.8) +
      scale_fill_manual(values = c("#2E86AB", "#A23B72")) +
      labs(title = "Gender Distribution", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$workout_plot <- renderPlotly({
    p <- ggplot(gym_data, aes(x = Workout_Type, fill = Workout_Type)) +
      geom_bar(alpha = 0.8) +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Workout Type Distribution", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$experience_plot <- renderPlotly({
    p <- ggplot(gym_data, aes(x = Experience_Level_Label, fill = Experience_Level_Label)) +
      geom_bar(alpha = 0.8) +
      scale_fill_manual(values = c("#6A994E", "#F18F01", "#BC4B51")) +
      labs(title = "Experience Level Distribution", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # TAB 3: Statistical Inference -----------------------------------------------
  
  output$test_description <- renderText({
    descriptions <- list(
      gender_test = "Independent samples t-test: Compares mean calories burned between males and females.",
      workout_test = "One-Way ANOVA: Compares mean calories burned across different workout types.",
      experience_test = "One-Way ANOVA: Compares mean calories burned across experience levels.",
      interaction_test = "Two-Way ANOVA: Tests Gender × Workout Type interaction on calories burned."
    )
    
    descriptions[[input$hypothesis_test]]
  })
  
  test_output <- eventReactive(input$run_test, {
    
    if (input$hypothesis_test == "gender_test") {
      test_result <- t.test(Calories_Burned ~ Gender, data = gym_data)
      
      list(
        result = test_result,
        interpretation = if (test_result$p.value < 0.05) {
          paste0("✅ There IS a statistically significant difference in calories burned between genders (p = ",
                 format.pval(test_result$p.value, digits = 3), "). Males burn approximately ",
                 round(abs(diff(test_result$estimate)), 1), " more calories on average.")
        } else {
          paste0("❌ There is NO statistically significant difference in calories burned between genders (p = ",
                 format.pval(test_result$p.value, digits = 3), ").")
        }
      )
      
    } else if (input$hypothesis_test == "workout_test") {
      model <- aov(Calories_Burned ~ Workout_Type, data = gym_data)
      test_result <- summary(model)
      p_value <- test_result[[1]]$`Pr(>F)`[1]
      
      list(
        result = test_result,
        posthoc = TukeyHSD(model),
        interpretation = if (p_value < 0.05) {
          paste0("✅ There IS a statistically significant difference in calories burned across workout types (p = ",
                 format.pval(p_value, digits = 3), "). See post-hoc tests for pairwise comparisons.")
        } else {
          paste0("❌ There is NO statistically significant difference in calories burned across workout types (p = ",
                 format.pval(p_value, digits = 3), ").")
        }
      )
      
    } else if (input$hypothesis_test == "experience_test") {
      model <- aov(Calories_Burned ~ Experience_Level_Label, data = gym_data)
      test_result <- summary(model)
      p_value <- test_result[[1]]$`Pr(>F)`[1]
      
      list(
        result = test_result,
        posthoc = TukeyHSD(model),
        interpretation = if (p_value < 0.05) {
          paste0("✅ There IS a statistically significant difference in calories burned across experience levels (p = ",
                 format.pval(p_value, digits = 3), "). See post-hoc tests for pairwise comparisons.")
        } else {
          paste0("❌ There is NO statistically significant difference in calories burned across experience levels (p = ",
                 format.pval(p_value, digits = 3), ").")
        }
      )
      
    } else if (input$hypothesis_test == "interaction_test") {
      model <- lm(Calories_Burned ~ Gender * Workout_Type + `Session_Duration (hours)`, data = gym_data)
      test_result <- Anova(model, type = "III")
      
      list(
        result = test_result,
        model = model,
        emm = emmeans(model, ~ Gender * Workout_Type),
        interpretation = paste0(
          "Gender effect: p = ", format.pval(test_result$`Pr(>F)`[2], digits = 3),
          if (test_result$`Pr(>F)`[2] < 0.05) " (Significant ✅)" else " (Not significant ❌)",
          "\nWorkout Type effect: p = ", format.pval(test_result$`Pr(>F)`[3], digits = 3),
          if (test_result$`Pr(>F)`[3] < 0.05) " (Significant ✅)" else " (Not significant ❌)",
          "\nInteraction effect: p = ", format.pval(test_result$`Pr(>F)`[5], digits = 3),
          if (test_result$`Pr(>F)`[5] < 0.05) " (Significant ✅)" else " (Not significant ❌)"
        )
      )
    }
  })
  
  output$test_results <- renderPrint({
    test_output()$result
  })
  
  output$test_interpretation <- renderUI({
    tags$div(
      style = "padding: 15px; background-color: #f0f0f0; border-left: 4px solid #2E86AB;",
      HTML(gsub("\n", "<br>", test_output()$interpretation))
    )
  })
  
  output$inference_plot <- renderPlotly({
    req(input$run_test)
    
    if (input$hypothesis_test == "gender_test") {
      p <- ggplot(gym_data, aes(x = Gender, y = Calories_Burned, fill = Gender)) +
        geom_boxplot(alpha = 0.7) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
        scale_fill_manual(values = c("#2E86AB", "#A23B72")) +
        labs(title = "Calories Burned by Gender",
             y = "Calories Burned") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else if (input$hypothesis_test == "workout_test") {
      p <- ggplot(gym_data, aes(x = Workout_Type, y = Calories_Burned, fill = Workout_Type)) +
        geom_boxplot(alpha = 0.7) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Calories Burned by Workout Type",
             y = "Calories Burned") +
        theme_minimal() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else if (input$hypothesis_test == "experience_test") {
      p <- ggplot(gym_data, aes(x = Experience_Level_Label, y = Calories_Burned, 
                                fill = Experience_Level_Label)) +
        geom_boxplot(alpha = 0.7) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
        scale_fill_manual(values = c("#6A994E", "#F18F01", "#BC4B51")) +
        labs(title = "Calories Burned by Experience Level",
             y = "Calories Burned") +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else {
      emm_data <- as.data.frame(test_output()$emm)
      p <- ggplot(emm_data, aes(x = Workout_Type, y = emmean, color = Gender, group = Gender)) +
        geom_point(size = 3) +
        geom_line(linewidth = 1) +
        geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
        scale_color_manual(values = c("#2E86AB", "#A23B72")) +
        labs(title = "Estimated Marginal Means: Gender × Workout Type",
             y = "Estimated Calories Burned") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p)
  })
  
  output$posthoc_table <- renderDT({
    req(input$run_test)
    
    if (!is.null(test_output()$posthoc)) {
      posthoc_df <- as.data.frame(test_output()$posthoc[[1]]) %>%
        rownames_to_column("Comparison") %>%
        mutate(across(where(is.numeric), ~round(., 3)))
      
      datatable(posthoc_df,
                options = list(pageLength = 10, scrollX = TRUE),
                class = 'cell-border stripe')
    } else if (!is.null(test_output()$emm)) {
      pairs_result <- pairs(test_output()$emm, adjust = "tukey")
      pairs_df <- as.data.frame(pairs_result) %>%
        mutate(across(where(is.numeric), ~round(., 3)))
      
      datatable(pairs_df,
                options = list(pageLength = 10, scrollX = TRUE),
                class = 'cell-border stripe')
    } else {
      datatable(data.frame(Message = "No post-hoc tests available for this analysis."),
                options = list(dom = 't'))
    }
  })
  
  # TAB 4: Modeling & Prediction -----------------------------------------------
  
  selected_model <- reactive({
    if (input$model_choice == "model1") {
      lm(Calories_Burned ~ `Weight (kg)` + `Session_Duration (hours)` + Avg_BPM, 
         data = gym_data)
    } else if (input$model_choice == "model2") {
      lm(Calories_Burned ~ `Weight (kg)` + `Session_Duration (hours)` + Avg_BPM + Gender, 
         data = gym_data)
    } else {
      lm(Calories_Burned ~ Gender * Workout_Type + `Session_Duration (hours)`, 
         data = gym_data)
    }
  })
  
  output$model_summary <- renderPrint({
    summary(selected_model())
  })
  
  output$model_performance_table <- renderDT({
    model <- selected_model()
    perf <- glance(model) %>%
      select(r.squared, adj.r.squared, sigma, statistic, p.value) %>%
      mutate(across(everything(), ~round(., 4))) %>%
      rename(
        `R²` = r.squared,
        `Adjusted R²` = adj.r.squared,
        `Residual Std Error` = sigma,
        `F-statistic` = statistic,
        `p-value` = p.value
      ) %>%
      pivot_longer(everything(), names_to = "Metric", values_to = "Value")
    
    datatable(perf,
              options = list(dom = 't'),
              class = 'cell-border stripe')
  })
  
  output$coefficients_table <- renderDT({
    tidy(selected_model(), conf.int = TRUE) %>%
      mutate(across(where(is.numeric), ~round(., 4))) %>%
      datatable(options = list(pageLength = 15, scrollX = TRUE),
                class = 'cell-border stripe')
  })
  
  prediction_result <- eventReactive(input$predict_btn, {
    
    if (input$model_choice == "model1") {
      new_data <- data.frame(
        `Weight (kg)` = input$pred_weight,
        `Session_Duration (hours)` = input$pred_duration,
        Avg_BPM = input$pred_bpm,
        check.names = FALSE
      )
    } else if (input$model_choice == "model2") {
      new_data <- data.frame(
        `Weight (kg)` = input$pred_weight,
        `Session_Duration (hours)` = input$pred_duration,
        Avg_BPM = input$pred_bpm,
        Gender = factor(input$pred_gender, levels = c("Female", "Male")),
        check.names = FALSE
      )
    } else {
      new_data <- data.frame(
        Gender = factor(input$pred_gender, levels = c("Female", "Male")),
        Workout_Type = factor(input$pred_workout, levels = c("Cardio", "HIIT", "Strength", "Yoga")),
        `Session_Duration (hours)` = input$pred_duration,
        check.names = FALSE
      )
    }
    
    pred <- predict(selected_model(), newdata = new_data, interval = "prediction", level = 0.95)
    
    list(
      prediction = pred[1],
      lower = pred[2],
      upper = pred[3]
    )
  })
  
  output$prediction_output <- renderUI({
    req(input$predict_btn)
    
    pred <- prediction_result()
    
    tags$div(
      style = "padding: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
               color: white; border-radius: 10px; text-align: center;",
      h2(style = "margin: 0; font-size: 48px; font-weight: bold;",
         paste0(round(pred$prediction, 0), " calories")),
      h4(style = "margin: 10px 0; opacity: 0.9;", "Predicted Calorie Burn"),
      hr(style = "border-color: rgba(255,255,255,0.3);"),
      p(style = "margin: 5px 0; font-size: 14px;",
        paste0("95% Prediction Interval: ", round(pred$lower, 0), " - ", round(pred$upper, 0), " calories")),
      p(style = "margin: 5px 0; font-size: 12px; opacity: 0.8;",
        "Based on your inputs and selected model")
    )
  })
  
  output$prediction_viz <- renderPlotly({
    req(input$predict_btn)
    
    pred <- prediction_result()
    
    plot_data <- data.frame(
      Category = c("Lower Bound", "Prediction", "Upper Bound"),
      Value = c(pred$lower, pred$prediction, pred$upper),
      Color = c("#FF6B6B", "#4ECDC4", "#FF6B6B")
    )
    
    p <- ggplot(plot_data, aes(x = Category, y = Value, fill = Category)) +
      geom_col(alpha = 0.8, width = 0.6) +
      geom_text(aes(label = round(Value, 0)), vjust = -0.5, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#FF6B6B")) +
      labs(title = "Prediction Interval Visualization",
           y = "Calories Burned") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(size = 12, face = "bold"))
    
    ggplotly(p)
  })
  
  output$diagnostic_plot1 <- renderPlot({
    par(mfrow = c(2, 2))
    plot(selected_model(), which = c(1, 2))
  })
  
  output$diagnostic_plot2 <- renderPlot({
    par(mfrow = c(2, 2))
    plot(selected_model(), which = c(3, 5))
  })
  
  # TAB 5: Download Handlers ---------------------------------------------------
  
  output$download_continuous <- downloadHandler(
    filename = function() {
      paste0("continuous_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      continuous_vars <- c("Age", "Weight (kg)", "Height (m)", "BMI",
                           "Max_BPM", "Avg_BPM", "Resting_BPM",
                           "Session_Duration (hours)", "Calories_Burned",
                           "Fat_Percentage", "Water_Intake (liters)",
                           "Workout_Frequency (days/week)")
      
      summary_data <- gym_data %>%
        select(all_of(continuous_vars)) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
        group_by(Variable) %>%
        summarise(
          Mean = mean(Value, na.rm = TRUE),
          Median = median(Value, na.rm = TRUE),
          SD = sd(Value, na.rm = TRUE),
          Min = min(Value, na.rm = TRUE),
          Max = max(Value, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(across(where(is.numeric), ~round(., 2)))
      
      write_csv(summary_data, file)
    }
  )
  
  output$download_categorical <- downloadHandler(
    filename = function() {
      paste0("categorical_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      categorical_vars <- c("Gender", "Workout_Type", "Experience_Level_Label")
      
      summary_data <- gym_data %>%
        select(all_of(categorical_vars)) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Category") %>%
        group_by(Variable, Category) %>%
        summarise(Count = n(), .groups = "drop") %>%
        group_by(Variable) %>%
        mutate(Percentage = round(Count / sum(Count) * 100, 2))
      
      write_csv(summary_data, file)
    }
  )
  
  output$download_model1 <- downloadHandler(
    filename = function() {
      paste0("model1_coefficients_", Sys.Date(), ".csv")
    },
    content = function(file) {
      model1 <- lm(Calories_Burned ~ `Weight (kg)` + `Session_Duration (hours)` + Avg_BPM,
                   data = gym_data)
      write_csv(tidy(model1, conf.int = TRUE), file)
    }
  )
  
  output$download_model2 <- downloadHandler(
    filename = function() {
      paste0("model2_coefficients_", Sys.Date(), ".csv")
    },
    content = function(file) {
      model2 <- lm(Calories_Burned ~ `Weight (kg)` + `Session_Duration (hours)` + 
                     Avg_BPM + Gender,
                   data = gym_data)
      write_csv(tidy(model2, conf.int = TRUE), file)
    }
  )
  
  output$download_comparison <- downloadHandler(
    filename = function() {
      paste0("models_comparison_", Sys.Date(), ".csv")
    },
    content = function(file) {
      model1 <- lm(Calories_Burned ~ `Weight (kg)` + `Session_Duration (hours)` + Avg_BPM,
                   data = gym_data)
      model2 <- lm(Calories_Burned ~ `Weight (kg)` + `Session_Duration (hours)` + 
                     Avg_BPM + Gender,
                   data = gym_data)
      
      comparison <- bind_rows(
        glance(model1) %>% mutate(Model = "Model 1"),
        glance(model2) %>% mutate(Model = "Model 2")
      ) %>%
        select(Model, r.squared, adj.r.squared, sigma, AIC, BIC)
      
      write_csv(comparison, file)
    }
  )
  
  output$download_anova <- downloadHandler(
    filename = function() {
      paste0("anova_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      model3 <- lm(Calories_Burned ~ Gender * Workout_Type + `Session_Duration (hours)`,
                   data = gym_data)
      anova_result <- Anova(model3, type = "III")
      write_csv(as.data.frame(anova_result) %>% rownames_to_column("Term"), file)
    }
  )
  
  output$download_posthoc <- downloadHandler(
    filename = function() {
      paste0("posthoc_tests_", Sys.Date(), ".csv")
    },
    content = function(file) {
      model3 <- lm(Calories_Burned ~ Gender * Workout_Type + `Session_Duration (hours)`,
                   data = gym_data)
      emm <- emmeans(model3, ~ Gender * Workout_Type)
      pairs_result <- as.data.frame(pairs(emm, adjust = "tukey"))
      write_csv(pairs_result, file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("gym_data_full_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(gym_data, file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)