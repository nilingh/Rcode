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
    # tabPanel("Data cont",
    #          plotOutput(outputId = "PredictorPlot1"),
    #          plotOutput(outputId = "PredictorPlot2"),
    #          plotOutput(outputId = "PredictorPlot3"),
    #          plotOutput(outputId = "PredictorPlot4")
    #),
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
#### brnn ####
tabPanel("brnn Model",
         verbatimTextOutput(outputId = "brnnModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "brnnPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "brnnPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "brnnGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "brnnGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "brnnTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "brnnMetrics"),
         hr(),
         plotOutput(outputId = "brnnModelPlots"),
         # plotOutput(outputId = "brnnFinalModelPlots"),
         verbatimTextOutput(outputId = "brnnRecipe"),
         verbatimTextOutput(outputId = "brnnModelSummary2")
),

#### qrnn ####
tabPanel("qrnn Model",
         verbatimTextOutput(outputId = "qrnnModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "qrnnPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("bagimpute","center","scale","dummy")),
                  bsTooltip(id = "qrnnPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "qrnnGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "qrnnGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "qrnnTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "qrnnMetrics"),
         hr(),
         plotOutput(outputId = "qrnnModelPlots"),
         # plotOutput(outputId = "qrnnFinalModelPlots"),
         verbatimTextOutput(outputId = "qrnnRecipe"),
         verbatimTextOutput(outputId = "qrnnModelSummary2")
),
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
         # plotOutput(outputId = "gbmFinalModelPlots"),
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
         # plotOutput(outputId = "lassoFinalModelPlots"),
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
         # plotOutput(outputId = "ridgeFinalModelPlots"),
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


#### gaussprLinear ####
tabPanel("gaussprLinear Model",
         verbatimTextOutput(outputId = "gaussprLinearModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "gaussprLinearPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale","dummy")),
                  bsTooltip(id = "gaussprLinearPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "gaussprLinearGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "gaussprLinearGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "gaussprLinearTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "gaussprLinearMetrics"),
         hr(),
         # plotOutput(outputId = "gaussprLinearModelPlots"),
         # plotOutput(outputId = "gaussprLinearFinalModelPlots"),
         verbatimTextOutput(outputId = "gaussprLinearRecipe"),
         verbatimTextOutput(outputId = "gaussprLinearModelSummary2")
),


#### svmLinear ####
tabPanel("svmLinear Model",
         verbatimTextOutput(outputId = "svmLinearModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "svmLinearPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("bagimpute","center","scale","dummy")),
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
),

#### mlpML ####
tabPanel("mlpML Model",
         verbatimTextOutput(outputId = "mlpMLModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "mlpMLPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "mlpMLPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "mlpMLGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "mlpMLGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "mlpMLTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "mlpMLMetrics"),
         hr(),
         plotOutput(outputId = "mlpMLModelPlots"),
         # plotOutput(outputId = "mlpMLFinalModelPlots"),
         verbatimTextOutput(outputId = "mlpMLRecipe"),
         verbatimTextOutput(outputId = "mlpMLModelSummary2")
),
#### xyf ####
tabPanel("xyf Model",
         verbatimTextOutput(outputId = "xyfModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "xyfPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "xyfPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "xyfGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "xyfGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "xyfTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "xyfMetrics"),
         hr(),
         plotOutput(outputId = "xyfModelPlots"),
         # plotOutput(outputId = "xyfFinalModelPlots"),
         verbatimTextOutput(outputId = "xyfRecipe"),
         verbatimTextOutput(outputId = "xyfModelSummary2")
),

#### kknn ####
tabPanel("kknn Model",
         verbatimTextOutput(outputId = "kknnModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "kknnPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "kknnPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "kknnGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "kknnGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "kknnTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "kknnMetrics"),
         hr(),
         plotOutput(outputId = "kknnModelPlots"),
         # plotOutput(outputId = "kknnFinalModelPlots"),
         verbatimTextOutput(outputId = "kknnRecipe"),
         verbatimTextOutput(outputId = "kknnModelSummary2")
),




#### rvmRadial ####
tabPanel("rvmRadial Model",
         verbatimTextOutput(outputId = "rvmRadialModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "rvmRadialPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "rvmRadialPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "rvmRadialGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "rvmRadialGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "rvmRadialTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "rvmRadialMetrics"),
         hr(),
         # plotOutput(outputId = "rvmRadialModelPlots"),
         # plotOutput(outputId = "rvmRadialFinalModelPlots"),
         verbatimTextOutput(outputId = "rvmRadialRecipe"),
         verbatimTextOutput(outputId = "rvmRadialModelSummary2")
),


#### glmStepAIC ####
tabPanel("glmStepAIC Model",
         verbatimTextOutput(outputId = "glmStepAICModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "glmStepAICPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "glmStepAICPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "glmStepAICGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "glmStepAICGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "glmStepAICTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "glmStepAICMetrics"),
         hr(),
         # plotOutput(outputId = "glmStepAICModelPlots"),
         # plotOutput(outputId = "glmStepAICFinalModelPlots"),
         verbatimTextOutput(outputId = "glmStepAICRecipe"),
         verbatimTextOutput(outputId = "glmStepAICModelSummary2")
),

#### pcaNNet ####
tabPanel("pcaNNet Model",
         verbatimTextOutput(outputId = "pcaNNetModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "pcaNNetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "pcaNNetPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "pcaNNetGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "pcaNNetGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "pcaNNetTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "pcaNNetMetrics"),
         hr(),
         # plotOutput(outputId = "pcaNNetModelPlots"),
         # plotOutput(outputId = "pcaNNetFinalModelPlots"),
         verbatimTextOutput(outputId = "pcaNNetRecipe"),
         verbatimTextOutput(outputId = "pcaNNetModelSummary2")
),

#### GFS.LT.RS ####
tabPanel("GFS.LT.RS Model",
         verbatimTextOutput(outputId = "GFS.LT.RSModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "GFS.LT.RSPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "GFS.LT.RSPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "GFS.LT.RSGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "GFS.LT.RSGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "GFS.LT.RSTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "GFS.LT.RSMetrics"),
         hr(),
         # plotOutput(outputId = "GFS.LT.RSModelPlots"),
         # plotOutput(outputId = "GFS.LT.RSFinalModelPlots"),
         verbatimTextOutput(outputId = "GFS.LT.RSRecipe"),
         verbatimTextOutput(outputId = "GFS.LT.RSModelSummary2")
),

#### spikeslab ####
tabPanel("spikeslab Model",
         verbatimTextOutput(outputId = "spikeslabModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "spikeslabPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale","dummy")),
                  bsTooltip(id = "spikeslabPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "spikeslabGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "spikeslabGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "spikeslabTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "spikeslabMetrics"),
         hr(),
         plotOutput(outputId = "spikeslabModelPlots"),
         # plotOutput(outputId = "spikeslabFinalModelPlots"),
         verbatimTextOutput(outputId = "spikeslabRecipe"),
         verbatimTextOutput(outputId = "spikeslabModelSummary2")
),
#### bayesglm ####
tabPanel("bayesglm Model",
         verbatimTextOutput(outputId = "bayesglmModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "bayesglmPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale","dummy")),
                  bsTooltip(id = "bayesglmPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "bayesglmGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "bayesglmGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "bayesglmTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "bayesglmMetrics"),
         hr(),
         plotOutput(outputId = "bayesglmModelPlots"),
         # plotOutput(outputId = "bayesglmFinalModelPlots"),
         verbatimTextOutput(outputId = "bayesglmRecipe"),
         verbatimTextOutput(outputId = "bayesglmModelSummary2")
),
#### rqlasso ####
tabPanel("rqlasso Model",
         verbatimTextOutput(outputId = "rqlassoModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "rqlassoPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale","dummy")),
                  bsTooltip(id = "rqlassoPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "rqlassoGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "rqlassoGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "rqlassoTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "rqlassoMetrics"),
         hr(),
         plotOutput(outputId = "rqlassoModelPlots"),
         # plotOutput(outputId = "rqlassoFinalModelPlots"),
         verbatimTextOutput(outputId = "rqlassoRecipe"),
         verbatimTextOutput(outputId = "rqlassoModelSummary2")
),
#### blasso ####
tabPanel("blasso Model",
         verbatimTextOutput(outputId = "blassoModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "blassoPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","center","scale","dummy")),
                  bsTooltip(id = "blassoPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "blassoGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "blassoGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "blassoTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "blassoMetrics"),
         hr(),
         plotOutput(outputId = "blassoModelPlots"),
         # plotOutput(outputId = "blassoFinalModelPlots"),
         verbatimTextOutput(outputId = "blassoRecipe"),
         verbatimTextOutput(outputId = "blassoModelSummary2")
),
#### cubist ####
tabPanel("cubist Model",
         verbatimTextOutput(outputId = "cubistModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "cubistPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("bagimpute","dummy")),
                  bsTooltip(id = "cubistPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "cubistGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "cubistGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "cubistTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "cubistMetrics"),
         hr(),
         plotOutput(outputId = "cubistModelPlots"),
         plotOutput(outputId = "cubistFinalModelPlots"),
         verbatimTextOutput(outputId = "cubistRecipe"),
         verbatimTextOutput(outputId = "cubistModelSummary2")
),

#### qrf ####
tabPanel("qrf Model",
         verbatimTextOutput(outputId = "qrfModelSummary0"),
         fluidRow(
           column(width = 4, 
                  selectizeInput(inputId = "qrfPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy")),
                  bsTooltip(id = "qrfPreprocess", 
                            title = "The order of preprocessing steps is important. The default steps should be set to your best selection for this method.")
           ),
           column(width = 1, 
                  actionButton(inputId = "qrfGo", label = "Train", icon = icon("play")),
                  bsTooltip(id = "qrfGo", title = "This will train or retrain your model")
           ),
           column(width = 4, 
                  verbatimTextOutput(outputId = "qrfTimeConsuming")
           )
         ),
         hr(),
         h3("Resampled performance:"),
         tableOutput(outputId = "qrfMetrics"),
         hr(),
         plotOutput(outputId = "qrfModelPlots"),
         # plotOutput(outputId = "qrfFinalModelPlots"),
         verbatimTextOutput(outputId = "qrfRecipe"),
         verbatimTextOutput(outputId = "qrfModelSummary2")
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
