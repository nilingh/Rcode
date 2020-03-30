# The UI part of a Shiny app
# Course: DATA423-20S1
# Task: Assignment 2
# Student: Zhen Huang
# ID: 74093323

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    #titlePanel("Assignment 2 - Zhen Huang"),
    
    # Navpage
    navbarPage(theme = shinytheme("flatly"),
        title = "Assignment 2 (by Zhen Huang)",
        tabPanel("Overview",
                 tabsetPanel(
                             tabPanel("Data Summary",
                                      sidebarLayout(
                                          sidebarPanel(
                                              h4("Data Backgroud"),
                                              "Dataset contains 162(54*3) mayonnaise observations with NIR-spectra of samples based on six different vegetable oils.",br(),
                                              "Researchers successfully identified oil types through analysing NIR-spectra data.",br(),
                                              h4("What I tried"),
                                              tags$ul(
                                                  tags$li("Feature Extraction"),
                                                  tags$li("Feature Selection"),
                                                  tags$li("Multi-dimensional scaling")
                                              ),
                                              h4("What I am still trying"),
                                              tags$ul(
                                                  tags$li("PCA followed by LDA"),
                                                  tags$li("PCA followed by feature selection"),
                                                  tags$li("Subset selection methods"),
                                                  tags$li("Alternative data set such as 'Hitter/Boston'")
                                              )
                                          ),
                                          mainPanel(
                                              h3("Original data set"),
                                              "This is a wide short datasets (n < p), which has 162 observations and 351 columns in 'NIR'.",br(),
                                              verbatimTextOutput("Str_Output"),
                                              hr(),
                                              h3("Missing data check"),
                                              "Data set has no missing value.",br(),
                                              plotOutput("Vis_Miss")
                                          )
                                      ) # sidebarLayout
                             ),
                             tabPanel("NIR Data Visualisation",
                                      sidebarLayout(
                                          sidebarPanel(
                                              h4("From right plot: "),
                                              tags$ul(
                                                  tags$li("NIR data of each observation (162 obs) has 351 variables"),
                                                  tags$li("Some NIRs are highly correlated with each other as shown in the matplot()"),
                                                  tags$li("Multiplicative Scatter Correction performs well in NIR-spectra matrix")
                                              )
                                          ),
                                          mainPanel(
                                              h3("NIR-spectra of samples"),
                                              plotOutput("Plot_NIR"),
                                              checkboxInput(inputId = "MSC", label = "Multiplicative Scatter Correction", value = FALSE)
                                          )
                                      )
                             ),
                             tabPanel("NIR Data Correlation",
                                      sidebarLayout(
                                          sidebarPanel(
                                              h4("There are far too many variables to show in one plot."),br(),
                                              actionButton(inputId = "Rand_Group", label = "Choose a random group of NIR data")
                                          ),
                                          mainPanel(
                                              h3("NIR Data Correlations"),
                                              plotOutput("Pairs_NIR")
                                          )
                                      )
                             ),
                             tabPanel("Oil Type",
                                      sidebarLayout(
                                          sidebarPanel(
                                              "Oil types are norminal, while column 'oil.type' is number in date set.",br(),
                                              "I kept this data type as some regression methods require numeric responses.",br(),
                                              h4("Different vegetable oils: "),
                                              "1-Soybean oil",br(),
                                              "2-Sunflower oil",br(),
                                              "3-Canola oil",br(),
                                              "4-Olive oil",br(),
                                              "5-Corn oil",br(),
                                              "6-Grapseed oil",
                                          ),
                                          mainPanel(
                                              h3("Number of observations in training and test data"),
                                              h4("Sored by oil types"),
                                              plotOutput("Bar_Oil"),
                                              width = 6
                                          )
                                      )
                             )
                 ) # tabsetPanel
        ), # tabPanel Overview
        tabPanel("Dimension Reduction",
                 tabsetPanel(
                             tabPanel("PLSR Extraction",
                                      sidebarLayout(
                                          sidebarPanel(
                                              h4("Explanation:"),
                                              "Partial Least Squares Regression (PLSR) attempts to find directions that help explain both the response and the predictors.",br(),
                                              "Cross validation helps us to have a better estimate to model error without test data set.",br(),
                                              p(strong("The optimal number of principal components I found is 16.")),hr(),
                                              sliderInput("ncomp", label = "Number of Components", min = 1, max = 351, step = 1, value = 50),
                                              selectizeInput("cv", label = "Choose a CV method", choices = c("LOO","CV"), selected = "CV"),
                                              selectizeInput("sc", label = "Scale", choices = c(TRUE,FALSE), selected = TRUE),
                                              selectizeInput("ce", label = "Center", choices = c(TRUE,FALSE), selected = TRUE),
                                              actionButton("go1","Submit"),hr(),
                                              selectizeInput("he", label = "Choose a heuristic method for PC Selection", choices = c("randomization","onesigma"), selected = "onesigma")
                                          ),
                                          mainPanel(
                                              h5("The validation results of Root Mean Squared Error of Prediction(RMSEP)"),
                                              plotOutput("Plot_RMSEP"),
                                              h5("Select the number of principal components"),
                                              plotOutput("Plot_SNcomp")
                                          )
                                      )
                             ),
                             tabPanel("PLSR 2dPlot",
                                      sidebarLayout(
                                          sidebarPanel(
                                              h4("2d-plot (upright):"),
                                              "The least number of PC is 16, so I choose first two PC to show all six oil types in training set.",hr(),
                                              h4("Measurement plot (downright):"),
                                              "We can access plotting functions through the 'plottype' argument.",br(),
                                              "Argument 'validation' is omit, because CV result has been shown in the last tab.",br(),
                                              "Argument 'biplot' is omit, because it is better plotted in between two PC.",hr(),
                                              selectizeInput("pt", label = "Choose a plot type", choices = c("prediction", "coefficients","scores", "loadings", "correlation"), selected = "scores")
                                              
                                          ),
                                          mainPanel(
                                              h4("Oil types projected on first two principal components"),
                                              plotOutput("Plsr_2d"),
                                              h4("Top five principal components in selected measurement"),
                                              plotOutput("Plsr_pairs"),                                              
                                          )
                                      )
                             ),
                             tabPanel("PCR Extraction",
                                      sidebarLayout(
                                          sidebarPanel(
                                              "When performing PCR, the response are not taken into account when finding the principal components.",br(),
                                              p(strong("The least number of principal components from PCR is 50.")),
                                              "At this data set, PLSR performed better than PCR.",hr(),
                                              sliderInput("ncomp2", label = "Number of Components", min = 1, max = 351, step = 1, value = 100),
                                              selectizeInput("cv2", label = "Choose a CV method", choices = c("LOO","CV"), selected = "CV"),
                                              selectizeInput("sc2", label = "Scale", choices = c(TRUE,FALSE), selected = TRUE),
                                              selectizeInput("ce2", label = "Center", choices = c(TRUE,FALSE), selected = TRUE),
                                              actionButton("go2","Submit"),hr(),
                                              selectizeInput("he2", label = "Choose a heuristic method for PC Selection", choices = c("randomization","onesigma"), selected = "onesigma")
                                          ),
                                          mainPanel(
                                              h5("The validation results of Root Mean Squared Error of Prediction(RMSEP)"),
                                              plotOutput("Plot_RMSEP_pcr"),
                                              h5("Select the number of principal components"),
                                              plotOutput("Plot_SNcomp_pcr")
                                          )
                                      )
                             )
                 ) # tabset 
        ), # tabPanel DimR
        tabPanel("Shrinkage",
                 tabsetPanel(
                     tabPanel("Ridge Regression and Lasso",
                              sidebarLayout(
                                  sidebarPanel(
                                      h4("Comparison with ridge and lasso: "),
                                      tags$ul(
                                          tags$li("Lasso could 'shrink' the number of variables to 23, while ridge regression minimises the coefficient (still keep 351 variables)."),
                                          tags$li("Different from dim-reduction methods like PCA, ridge and lasso give the most important 'orginal' variables, which could represent the data set."),
                                          tags$li("Cross validation helps to choose the proper penalty parameter 'lambda'.")
                                      ),
                                      h4("Results of two models:"),
                                      verbatimTextOutput("Shrink_output"),br(),
                                      h4("Some questions left here:"),
                                      tags$ol(
                                          tags$li("Ridge supposed to be L2 Norm, but the xlab name is showed as L1 Norm."),
                                          tags$li("The right side of coefficients plot supposed to be equivalent to linear model, while there are less variables shown in lasso plot."),
                                          tags$li("Responses here in the data set are actually norminal oil types, so MSE is not a proper measure to estimate model performance.")
                                      ),
                                      p(em("Part of code in fitting shrinkage model are based on ISLR 6.5.3 Lab"))
                                  ),
                                  mainPanel(
                                      h4("Ridge regression"),
                                      plotOutput("Shrink_plot0"),
                                      h4("Lasso"),
                                      plotOutput("Shrink_plot1")
                                      )
                              )
                     )
                 )
        ), # tab Shrink
        tabPanel("MDS",
                tabsetPanel(
                    tabPanel("Multi-dimensioinal scaling",
                             sidebarLayout(
                                 sidebarPanel(
                                     h4("Practice with three MDS methods: "),
                                     tags$ul(
                                         tags$li("MDS methods require to choose parameter k before fitting the model. However, we do not know the proper k at early stage."),
                                         tags$li("Ploting groups in 2d or 3d will not be intuitive when the model dimension > 3 actually. All three methods had only slightly difference in a variety of distance measurements."),
                                         tags$li("All three MDS method has limits on k based on ratio to observations/variables, so I limit choice of k at upto 30.")
                                     ),hr(),
                                     selectizeInput("dm", label = "Choose a distance measure", choices = c("euclidean","maximum","manhattan","canberra","minkowski"), selected = "manhattan"),
                                     sliderInput("kn", label = "Choose dimensional reduction parameter k", min = 1, max = 30, step = 1, value = 2),
                                     
                                 ),
                                 mainPanel(
                                     plotOutput("MDS_plot1"),
                                     plotOutput("MDS_plot2")
                                 )
                             )
                    )
                )
        ), # tabPanel S-S
        tabPanel("Summary",
                tabsetPanel(
                    tabPanel("Summary of this assignment",
                             sidebarLayout(
                                 sidebarPanel(
                                     h4("The number of variables extration or selection: "),
                                     tags$ul(
                                         tags$li("PLSR~16"),
                                         tags$li("PCR~50"),
                                         tags$li("Lasso~23"),
                                         tags$li("Ridge regression~351 (no zero coefficient)"),
                                         tags$li("nMDS~ (by pre-defined)"),
                                         tags$li("PLS~55"),
                                         tags$li("PCA~58"),
                                     ),
                                     h4("Another try is to fit PLS/PCA with example code of recipes and caret: "),
                                     "The result number of extract PCs are far more than PLSR.",br(),
                                     "What I want to try is to perform athother feather selection methods to reduce the number of variables, then implement LDA in the next stage."
                                 ),
                                 mainPanel(
                                     h4("Feature extraction with PLS"),
                                     plotOutput("PCA_plot1"),
                                     h4("Feature extraction with PCA"),
                                     plotOutput("PCA_plot2")
                                 )
                             )
                    )
                ) 
        ) # tabPanel About
    )
))
