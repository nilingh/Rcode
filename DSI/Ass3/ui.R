shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3 - Zhen Huang"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             DT::dataTableOutput(outputId = "Table")
    ),
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             
             tabsetPanel(
               tabPanel("NULL Model",
                        br(),
                        fluidRow(
                          column(width = 4),
                          column(width = 1, 
                                 actionButton(inputId = "NullGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "NullGo", title = "This will train or retrain your model")
                                 )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "NullMetrics"),
                        hr(),
                        verbatimTextOutput(outputId = "NullRecipe"),
               ),
               tabPanel("GLMnet Model",
                        verbatimTextOutput(outputId = "GlmnetModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("bagimpute","YeoJohnson","center","scale","dummy")),
                                 bsTooltip(id = "GlmnetPreprocess", 
                                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 2, 
                                 actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "GlmnetGo", title = "This will train or retrain your model")
                                 ),
                          column(width = 4, 
                                 verbatimTextOutput(outputId = "GlmnetTimeConsuming")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "GlmnetMetrics"),
                        hr(),
                        plotOutput(outputId = "GlmnetModelPlots"),
                        verbatimTextOutput(outputId = "GlmnetRecipe"),
                        verbatimTextOutput(outputId = "GlmnetModelSummary2")
               ),
               tabPanel("PLS Model",
                        verbatimTextOutput(outputId = "PlsModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("bagimpute","YeoJohnson","center","scale","dummy")),
                                 bsTooltip(id = "PlsPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "PlsGo", title = "This will train or retrain your model")
                                 ),
                          column(width = 4, 
                                 verbatimTextOutput(outputId = "PLSTimeConsuming")
                          )
                        ),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "PlsMetrics"),
                        hr(),
                        plotOutput(outputId = "PlsModelPlots"),
                        verbatimTextOutput(outputId = "PlsRecipe"),
                        verbatimTextOutput(outputId = "PlsModelSummary2")
               ),
               tabPanel("Rpart Model",
                        verbatimTextOutput(outputId = "RpartModelSummary0"),
                        fluidRow(
                          column(width = 4, 
                                 selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c()),
                                 bsTooltip(id = "RpartPreprocess", 
                                           title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
                          ),
                          column(width = 1, 
                                 actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")),
                                 bsTooltip(id = "RpartGo", title = "This will train or retrain your model")
                          ),
                          column(width = 4, 
                                 verbatimTextOutput(outputId = "RpartTimeConsuming")
                        )),
                        hr(),
                        h3("Resampled performance:"),
                        tableOutput(outputId = "RpartMetrics"),
                        hr(),
                        plotOutput(outputId = "RpartModelPlots"),
                        plotOutput(outputId = "RpartModelTree"),
                        verbatimTextOutput(outputId = "RpartRecipe"),
               ),
               
######################################################### maintenance point ####################################################
#### rf ####
tabPanel("Rf Model",
          verbatimTextOutput(outputId = "RfModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "RfPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
                  bsTooltip(id = "RfPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "RfGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "RfGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "RfTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "RfMetrics"),
         hr(),
         plotOutput(outputId = "RfModelPlots"),
         verbatimTextOutput(outputId = "RfRecipe"),
         plotOutput(outputId = "RfFinalModelPlots"),
         plotOutput(outputId = "RfPredictorsPlot"),
         verbatimTextOutput(outputId = "RfModelSummary2")
),
#### xgbLinear  ####
tabPanel("xgbLinear Model",
         verbatimTextOutput(outputId = "xgbLinearModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "xgbLinearPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "xgbLinearPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "xgbLinearGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "xgbLinearGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "xgbLinearTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "xgbLinearMetrics"),
         hr(),
         plotOutput(outputId = "xgbLinearModelPlots"),
         verbatimTextOutput(outputId = "xgbLinearRecipe"),
         verbatimTextOutput(outputId = "xgbLinearModelSummary2")
),
#### gbm ####
tabPanel("gbm Model",
         verbatimTextOutput(outputId = "gbmModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "gbmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute")),
                  bsTooltip(id = "gbmPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "gbmGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "gbmGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "gbmTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "gbmMetrics"),
         hr(),
         plotOutput(outputId = "gbmModelPlots"),
         plotOutput(outputId = "gbmFinalModelPlots"),
         verbatimTextOutput(outputId = "gbmRecipe"),
         verbatimTextOutput(outputId = "gbmModelSummary2")
),
#### lasso ####
tabPanel("lasso Model",
         verbatimTextOutput(outputId = "lassoModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "lassoPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale","dummy")),
                  bsTooltip(id = "lassoPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "lassoGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "lassoGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "lassoTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "lassoMetrics"),
         hr(),
         plotOutput(outputId = "lassoModelPlots"),
         plotOutput(outputId = "lassoFinalModelPlots"),
         verbatimTextOutput(outputId = "lassoRecipe"),
         verbatimTextOutput(outputId = "lassoModelSummary2")
),
#### ridge ####
tabPanel("ridge Model",
         verbatimTextOutput(outputId = "ridgeModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "ridgePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale","dummy")),
                  bsTooltip(id = "ridgePreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "ridgeGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "ridgeGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "ridgeTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "ridgeMetrics"),
         hr(),
         plotOutput(outputId = "ridgeModelPlots"),
         plotOutput(outputId = "ridgeFinalModelPlots"),
         verbatimTextOutput(outputId = "ridgeRecipe"),
         verbatimTextOutput(outputId = "ridgeModelSummary2")
),
#### kernelpls ####
tabPanel("kernelpls Model",
         verbatimTextOutput(outputId = "kernelplsModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "kernelplsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale")),
                  bsTooltip(id = "kernelplsPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "kernelplsGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "kernelplsGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "kernelplsTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "kernelplsMetrics"),
         hr(),
         plotOutput(outputId = "kernelplsModelPlots"),
         # plotOutput(outputId = "kernelplsFinalModelPlots"),
         verbatimTextOutput(outputId = "kernelplsRecipe"),
         verbatimTextOutput(outputId = "kernelplsModelSummary2")
),
#### glmboost ####
tabPanel("glmboost Model",
         verbatimTextOutput(outputId = "glmboostModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "glmboostPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "glmboostPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "glmboostGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "glmboostGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "glmboostTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "glmboostMetrics"),
         hr(),
         plotOutput(outputId = "glmboostModelPlots"),
         # plotOutput(outputId = "glmboostFinalModelPlots"),
         verbatimTextOutput(outputId = "glmboostRecipe"),
         verbatimTextOutput(outputId = "glmboostModelSummary2")
),


#### svmLinear ####
tabPanel("svmLinear Model",
         verbatimTextOutput(outputId = "svmLinearModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "svmLinearPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "svmLinearPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "svmLinearGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "svmLinearGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "svmLinearTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "svmLinearMetrics"),
         hr(),
         plotOutput(outputId = "svmLinearModelPlots"),
         # plotOutput(outputId = "svmLinearFinalModelPlots"),
         verbatimTextOutput(outputId = "svmLinearRecipe"),
         verbatimTextOutput(outputId = "svmLinearModelSummary2")
)





################################################################################################################################
             ) # end of tabsetPanel
             ), # end of Methods
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             plotOutput(outputId = "TestPlot")
    )
  )
))
