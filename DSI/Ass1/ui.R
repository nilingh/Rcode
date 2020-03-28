# The UI part of a Shiny app
# Course: DATA423-20S1
# Task: Assignment 1
# Student: Zhen Huang
# ID: 74093323

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Assignment 1 - Zhen Huang"),

    # navbarPage at the top layer
    navbarPage(
        title = ">>",
        tabPanel("General infomation",
                 navlistPanel(
                     tabPanel("Data Summary",
                              tabsetPanel(
                                  tabPanel("Categorical", verbatimTextOutput(outputId = "summary_my_tb1")),
                                  tabPanel("Date", verbatimTextOutput(outputId = "summary_my_tb2")),
                                  tabPanel("Numberic", verbatimTextOutput(outputId = "summary_my_tb3")),
                                  tabPanel("Overall", verbatimTextOutput(outputId = "summary_my_tb4"))
                                  ) # end tabsetPanel
                              ), # end Data Summary
                     tabPanel("Raw Data",
                              verticalLayout(
                                  DT::dataTableOutput(outputId = "my_tb_raw"),
                                  checkboxInput(inputId = "round_cols", label = "Round numeric columns", value = TRUE)
                                  ) # verticalLayout of Raw Data
                              ), # end Raw Data
                     
                     tabPanel("Missing Data",
                              verticalLayout(
                                  plotOutput(outputId = "vis_my_tb"),
                                  # checkboxInput(inputId = "cluster", label = "Cluster missingness", value = TRUE),
                                  checkboxInput(inputId = "sort", label = "Sort columns by type", value = FALSE)
                                  ) # end verticalLayout of Missing Data
                              ), # end Missing Data
                     tabPanel("Novelty variables",
                              plotOutput(outputId = "barplot_novelty")
                              ),
                     widths = c(2, 10) # widths of navlistPanel
                     
                     ) # end navListPanel,
                 
                 ), # end tabPanel Gerneral infomation
        tabPanel("Categorical data",
                 tabsetPanel(
                     tabPanel("Mosaic Plot",
                              plotOutput(outputId = "Mosaic"),
                              selectizeInput(inputId = "var_mosaic", label = "Select variables:", choices = cate_cols, multiple = TRUE, selected = default_cate_cols)
                     )
                     ,
                     tabPanel("Correlation in Ordinal Data",
                              fluidRow(
                                  column(8, plotOutput(outputId = "Corr_cate_plot")),
                                  column(4, wellPanel("Ordinal data mapping to number",
                                                      hr(),
                                                      h5("Priority"),p("Priority == Low  ~ 1"),p("Priority == Medium  ~ 2"),p("Priority == High  ~ 3"),
                                                      h5("Price"),p("Price == Cheap  ~ 1"),p("Price == Costly  ~ 2"),p("Price == Extravagant  ~ 3"),
                                                      h5("Speed"),p("Speed == Slow  ~ 1"),p("Speed == Medium  ~ 2"),p("Speed == Fast  ~ 3"),
                                                      h5("Duration"),p("Duration == Short  ~ 1"),p("Duration == Long  ~ 2"),p("Duration == Very Long  ~ 3"),
                                                      h5("Temp"),p("Temp == Cold  ~ 1"),p("Temp == Warm  ~ 2"),p("Temp == Hot  ~ 3")
                                                      ))
                              )
                     )
                 )
        ), # end tabPanel of Categorical data
        tabPanel("Numeric Data",
                 navlistPanel(
                     tabPanel("Overview",
                              tabsetPanel(
                                  tabPanel('Data Overview',
                                           fluidRow(
                                               column(9,plotOutput(outputId = "Boxplot")),
                                               column(3,
                                                      hr(),
                                                      wellPanel(
                                                          sliderInput(inputId = "iqr_range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                                                          checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                                                          checkboxInput(inputId = "outliers", label = "Show outliers", value = FALSE)
                                                      )
                                               )),
                                           hr(),
                                           fluidRow(
                                               column(8,
                                                      plotOutput(outputId = "ui_plot"),
                                                      # textOutput("text_range")
                                               ),
                                               column(4,
                                                      wellPanel(
                                                          selectizeInput(inputId = "sel_col", label = "Choose a variable to display", choices = num_cols, selected = "sensor3"),
                                                          uiOutput('ui_range')
                                                          # sliderInput("bins","Number of bins:",min = 1,max = 50,value = 15)
                                                      )
                                               )
                                           )
                                  ),
                                  tabPanel("Correlation Overview",
                                           plotOutput(outputId = "Corrgram"),
                                     flowLayout(
                                         selectInput(inputId = "order", label = "Select correlation order", choices = list("TRUE"="TRUE","PCA"="PCA","OLO"="OLO"), selected = "TRUE"),
                                         selectInput(inputId = "cor_method", label = "Select correlation method", choices = list("pearson"="pearson","spearman"="spearman","kendall"="kendall"), selected = "pearson"),
                                         checkboxInput(inputId = "abs", label = "Grouping uses absolute correlation", value = TRUE)
                                     )
                                     )
                                 ),
                              ), # end of Overview
                     tabPanel("Continuty",
                              tabPanel("Novelty and Continuity",
                                       fluidRow(
                                           column(8,
                                               plotOutput(outputId = "rising_order_plot")
                                           ),
                                           column(4,
                                               selectizeInput(inputId = "col_rising_order", label = "Show variables:", choices = num_cols, multiple = TRUE, selected = default_num_cols)
                                           ),
                                           hr(),
                                           column(8,plotOutput(outputId = "TimeSeries"))
                                       )
                              )
                     ), # end of tabPanel Continuty
                     tabPanel("Correlation",
                              tabPanel("Sensor groups",
                                       tabsetPanel(
                                                   tabPanel("Pearson", 
                                                            plotOutput(outputId = "GGpairs_p"),
                                                            selectizeInput(inputId = "pearson_group", label = "Select one group:",
                                                                           choices = list("Group1"="sensor_group_col1","Group2"="sensor_group_col2","Group3"="sensor_group_col3","Group4"="sensor_group_col4"),
                                                                           multiple = FALSE, selected = "sensor_group_col1")
                                                            ),
                                                   tabPanel("Spearman", 
                                                            plotOutput(outputId = "GGpairs_s"),
                                                            selectizeInput(inputId = "spearman_group", label = "Select one group:",
                                                                           choices = list("Group1"="spear_group_col1","Group2"="spear_group_col2","Group3"="spear_group_col3"),
                                                                           multiple = FALSE, selected = "spear_group_col1")
                                                            )
                                                   ) # end of tabsetPanel
                                       ) # end of tabPanel Sensor groups
                              ), # end of Correlation
                     widths = c(2, 10) # widths of navlistPanel
                     ) # end of navlistPanel of Sensor Data
                 ), # end tabPanel of Numeric data
        tabPanel("Mixed Data",
                     tabPanel("Homegeneity",
                              tabsetPanel(
                              tabPanel("Mixed Tabplot",
                                      plotOutput(outputId = "ui_tab_plot"),
                                      hr(),
                                      fluidRow(
                                          column(4,
                                                 selectizeInput(inputId = "col_tab", label = "Select variables:", choices = num_cols, multiple = TRUE, selected = default_tab_cols)
                                          ),
                                          column(4,
                                                 # uiOutput('ui_col_sort_tab'),
                                                 selectizeInput(inputId = "col_sort_tab", label = "Select sort category:", choices = choice_tab_col, multiple = FALSE, selected = "rownum")
                                          ),
                                      )
                              ),
                              tabPanel("Mixed Pairs",
                                       plotOutput(outputId = "MixedPairs"),
                                       fluidRow(
                                           column(4,
                                                  selectizeInput(inputId = "select_group", label = "Select one sensor group:", choices = list("Group 1"="1","Group 2"="2","Group 3"="3"), multiple = FALSE, selected = 1)
                                           ),
                                           column(4, uiOutput("ui_group_sensor")),
                                           column(4,
                                                  selectizeInput(inputId = "col_mix_cate", label = "Select one category:", choices = cate_cols, multiple = FALSE, selected = cate_cols[3])
                                           )
                              ) # end of Mixed Pairs
                              ) # end of the tabset
                     ) # end of tabPanel Homogeneity
        ), # end tabPanel of Mixed Data
        # theme = "bootstrap.css",
        inverse = TRUE
        )
    )
))
