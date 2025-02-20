# Load Dependencies #
library(shiny)
library(shinyBS)
library(shinyjs)
library(sf)
library(raster)
library(sp)
library(leaflet)
library(dplyr)
library(shinyWidgets)
library(DescTools)
library(RColorBrewer)
library(cowplot)
library(ggplot2)
library(leaflet.extras)

##### UI #####
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .version-pill {
      position: absolute;
      top: 10px;
      right: 10px;
      padding: 5px 10px;
      background-color: #6c757d;
      color: white;
      border-radius: 15px;
      font-size: 0.8em;
      z-index: 1000;
    }

    .version-pill:hover {
      background-color: #5a6268;
    }
  "))
  ),
  tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML")),
  tags$div(
    class = "version-pill",
    "v2.1.2"  # Update this version number as needed
  ),
  titlePanel("Great Plains Population & Trend Simulator"),

  tabsetPanel(
    # Tab for running and saving models
    tabPanel("Run Model",
             sidebarLayout(
               sidebarPanel(
                 # File input with info icon
                 # Input for naming the model
                 textInput("modelName", "Name this Scenario:"),

                 tags$p("Define Region of Interest:",
                        icon("info-circle", id = "info_roi"),
                        div(style = "margin-bottom: 10px;",
                            radioButtons("roi_method", label = NULL,
                                         choices = c("Draw on Map" = "draw",
                                                     "Upload Shapefile" = "upload"),
                                         selected = "upload")
                        )
                 ),
                 bsTooltip("info_roi", "Choose whether to draw the region of interest on the map or upload a shapefile (.zip). Zip must contain the following extensions: .cpg, .dbf, .prj, .sbn, .sbx, .shp, .shx", placement = "right"),

                 # Conditional UI for ROI upload
                 conditionalPanel(
                   condition = "input.roi_method == 'upload'",
                   fileInput("roiShapefile", label = NULL,
                             accept = c('.zip'),
                             multiple = FALSE,
                             buttonLabel = "Browse...",
                             placeholder = "No file selected")
                 ),

                 tags$p("Define Management Region:",
                        icon("info-circle", id = "info_management"),
                        div(style = "margin-bottom: 10px;",
                            radioButtons("management_method", label = NULL,
                                         choices = c("Draw on Map" = "draw",
                                                     "Upload Shapefile" = "upload"),
                                         selected = "upload")
                        )
                 ),
                 bsTooltip("info_management", "Choose whether to draw the management region on the map or upload a shapefile (.zip). Zip must contain the following extensions: .cpg, .dbf, .prj, .sbn, .sbx, .shp, .shx", placement = "right"),

                 # Conditional UI for management upload
                 conditionalPanel(
                   condition = "input.management_method == 'upload'",
                   fileInput("managementShapefile", label = NULL,
                             accept = c('.zip'),
                             multiple = FALSE,
                             buttonLabel = "Browse...",
                             placeholder = "No file selected")
                 ),

                 # Add a container for validation messages
                 tags$div(id = "validation-messages",
                          style = "color: red; margin-top: 10px;"),

                 # Model selection
                 tags$p("Select Model ",
                        icon("info-circle", id = "info_model"),
                        selectInput("modelChoice", label = NULL, choices = c("Base", "NFWF"))
                 ),
                 bsTooltip("info_model", "Select the model type to run.", placement = "right"),

                 # Species selection with info icon SEOW
                 # Some species have been commented out due to low-confidence in model and/or amount of data. Can be added back in if desired.
                 tags$p("Choose Species ",
                        icon("info-circle", id = "info_species"),
                        selectInput("species", label = NULL, choices = c(
                          "Thick-billed Longspur" = "TBLO",
                          "Baird's Sparrow" = "BAIS",
                          "Lark Bunting" = "LARB",
                          "Chestnut-collared Longspur" = "CCLO",
                          "Sprague's Pipit" = "SPPI",
                          "Brewer's Sparrow" = "BRSP",
                          "Grasshopper Sparrow" = "GRSP",
                          "Spotted Towhee" = "SPTO",
                          "Long-billed Curlew" = "LBCU",
                          #"Loggerhead Shrike" = "LOSH",
                          #"Northern Harrier" = "NOHA",
                          #"Northern Pintail" = "NOPI",
                          "Western Meadowlark" = "WEME",
                          #"Sedge Wren" = "STGR",
                          "Horned Lark" = "HOLA",
                          "Upland Sandpiper" = "UPSA",
                          "Bobolink" = "BBMA",
                          "Burrowing Owl" = "BUOW",
                          "Ferruginous Hawk" = "FEHA",
                          "Greater Sage-Grouse" = "GRSG",
                          "Marbled Godwit" = "MAGO",
                          #"Mallard" = "MALL",
                          "Mountain Plover" = "MOPL",
                          #"Short-eared Owl" = "SEOW",
                          #"Swainson's Hawk" = "SWHA",
                          "Wilson's Phalarope" = "WIPH"))
                 ),
                 bsTooltip("info_species", "Select a species from the dropdown list.", placement = "right"),

                 # End year selection with info icon
                 tags$p("Select End Year ",
                        icon("info-circle", id = "info_endYear"),
                        selectInput("endYear", label = NULL, choices = 2023:2050, selected = 2030)
                 ),
                 bsTooltip("info_endYear", "Choose the end year for this scenario. Scenario will begin by default with the year 2010", placement = "right"),

                 # Confidence interval slider with info icon
                 tags$p("Confidence Interval ",
                        icon("info-circle", id = "info_confidence"),
                        sliderInput("confidenceInterval", label = NULL, min = 80, max = 100, value = 90)
                 ),
                 bsTooltip("info_confidence", "Set the confidence interval for this scenario.", placement = "right"),

                 hr(style = "border-top: 2px solid #d3d3d3;"),

                 # Sliders for various adjustments with info icons
                 tags$p("Crop Cover Adjustment (%) ",
                        icon("info-circle", id = "info_crop"),
                        sliderInput("Crop_pct", label = NULL, min = -100, max = 100, value = 0)
                 ),
                 bsTooltip("info_crop", "Percent change in crop cover.", placement = "right"),

                 tags$p("Shrub Cover Adjustment (%) ",
                        icon("info-circle", id = "info_shrb"),
                        sliderInput("shrb", label = NULL, min = -100, max = 100, value = 0)
                 ),
                 bsTooltip("info_shrb", "Percent change in shrub cover.", placement = "right"),

                 tags$p("Tree Cover Adjustment (%) ",
                        icon("info-circle", id = "info_tree"),
                        sliderInput("tree", label = NULL, min = -100, max = 100, value = 0)
                 ),
                 bsTooltip("info_tree", "Percent change in tree cover.", placement = "right"),

                 tags$p("Annual Grass Cover Adjustment (%) ",
                        icon("info-circle", id = "info_agfc"),
                        sliderInput("agfc", label = NULL, min = -100, max = 100, value = 0)
                 ),
                 bsTooltip("info_agfc", "Percent change in annual grass cover.", placement = "right"),

                 tags$p("Perennial Grass Cover Adjustment (%) ",
                        icon("info-circle", id = "info_pgfc"),
                        sliderInput("pgfc", label = NULL, min = -100, max = 100, value = 0)
                 ),
                 bsTooltip("info_pgfc", "Percent change in perennial grass cover.", placement = "right"),

                 hr(style = "border-top: 2px solid #d3d3d3;"),

                 tags$p("Road Density Adjustment (%) ",
                        icon("info-circle", id = "info_road"),
                        sliderInput("Road_km", label = NULL, min = -100, max = 100, value = 0)
                 ),
                 bsTooltip("info_road", "Percent chane in road density.", placement = "right"),

                 hr(style = "border-top: 2px solid #d3d3d3;"),
                 # Button to run and save the model
                 actionButton("runSaveModel", "Run and Save Scenario"),
                 # Add download buttons
                 hr(style = "border-top: 2px solid #d3d3d3;"),
                 downloadButton("downloadMap", "Download Population Map"),
                 downloadButton("downloadTrendPlot", "Download Trend Plot"),
                 downloadButton("downloadPopPlot", "Download Population Plot"),
                 downloadButton("downloadCSV", "Download CSV"),
                 hr(style = "border-top: 2px solid #d3d3d3;"),
                 # Display logos
                 div(style = "display: flex; justify-content: space-around; align-items: center; margin-bottom: 20px;",
                     img(src = "https://www.birdconservancy.org/wp-content/uploads/2015/04/bcr-logo-retina-sm.png", height = "24px"),
                     img(src = "https://ngpjv.org/wp-content/uploads/2023/01/ngpjv-logo.png", height = "24px"),
                     img(src = "https://weconservepa.org/wp-content/uploads/2023/03/NFWF-logo.png", height = "24px")
                 )
               ),

               mainPanel(
                 fluidRow(
                   column(12, leafletOutput("populationMap", height = "60vh")),
                   column(6, plotOutput("trendPlot", height = "30vh")),
                   column(6, plotOutput("populationPlot", height = "30vh")))
               )
             )
    ),

    # Tab for comparing models
    tabPanel("Compare Models",
             sidebarLayout(
               sidebarPanel(
                 # Dynamic model selection UI
                 uiOutput("modelSelectionUI"),

                 # Add/Remove model buttons
                 actionButton("addModel", "Add Another Model",
                              icon = icon("plus")),
                 actionButton("removeModel", "Remove Last Model",
                              icon = icon("minus")),

                 hr(style = "border-top: 2px solid #d3d3d3;"),

                 # Button to trigger comparison
                 actionButton("compareModels", "Compare Models"),

                 hr(style = "border-top: 2px solid #d3d3d3;"),
                 # Display logos
                 div(style = "display: flex; justify-content: space-around; align-items: center; margin-bottom: 20px;",
                     img(src = "https://www.birdconservancy.org/wp-content/uploads/2015/04/bcr-logo-retina-sm.png", height = "24px"),
                     img(src = "https://ngpjv.org/wp-content/uploads/2023/01/ngpjv-logo.png", height = "24px"),
                     img(src = "https://weconservepa.org/wp-content/uploads/2023/03/NFWF-logo.png", height = "24px")
                 )
               ),
               mainPanel(
                 fluidRow(
                   column(12, leafletOutput("comparisonMap", height = "60vh")),
                   column(6, plotOutput("comparisonTrendPlot", height = "30vh")),
                   column(6, plotOutput("comparisonPopulationPlot", height = "30vh"))
                 )
               )
             )
    ),

    # tabPanel("Case Study",
    #          fluidPage(
    #            h2("Case Study: Modeling the Impact of Grass Cover Loss in the Central Grasslands"),
    #            h3("Background"),
    #            p("The Central Grasslands of North America, a vital ecosystem for numerous bird species and other wildlife, has been facing significant threats from land-use changes and encroachment by woody vegetation. The Central Grasslands Roadmap Initiative aims to support and grow our core of grasslands, ensuring the sustainability of this crucial habitat."),
    #            h3("Objective"),
    #            p("The case study leverages the Central Grasslands Assessment Map to simulate a model where the 'core grasslands' experience a loss of grass cover due to various factors, including the encroachment of woody vegetation and trees."),
    #            h3("Methodology"),
    #            p("Using the NGPJV v0.8.2 Shiny app, a model was run to predict the ecological impact of decreased grass cover and increased woody vegetation in core grassland areas. The model adjusted the corresponding sliders to reflect these changes:"),
    #            tags$ul(
    #              tags$li("Grass Cover Adjustment Slider: Set to a negative value to simulate loss of grass cover."),
    #              tags$li("Shrub Cover Adjustment Slider: Increased to simulate shrub encroachment."),
    #              tags$li("Tree Cover Adjustment Slider: Increased to reflect tree encroachment.")
    #            ),
    #            h3("Results"),
    #            p("The model predicted significant alterations in habitat suitability for grassland-dependent bird species. Key findings included:"),
    #            tags$ul(
    #              tags$li("A decrease in available nesting sites for ground-nesting birds."),
    #              tags$li("A shift in species composition, favoring those that can adapt to woody environments."),
    #              tags$li("Potential increases in avian nest predation rates due to more abundant perches for predators.")
    #            ),
    #            h3("Discussion"),
    #            p("The model results underscore the importance of proactive conservation efforts to manage grass cover and control woody vegetation encroachment. Strategies such as targeted grazing management, controlled burns, and conservation easements are vital to maintaining the ecological integrity of the Central Grasslands."),
    #            h3("Conclusion"),
    #            p("This case study demonstrates the utility of the Central Grasslands Assessment Map and the NGPJV Shiny app as powerful tools for visualizing and predicting the impacts of habitat changes. It also emphasizes the need for continued research and adaptive management to preserve these landscapes for future generations."),
    #            # Image integration
    #            div(style = "text-align: center;", img(src = "https://www.birdconservancy.org/wp-content/uploads/2022/12/CGR_GRM_2022_V1_1-796x1030.jpg", alt = "Central Grasslands Assessment Map", style = "width:80%; height:auto;")),
    #            h3("References"),
    #            p("Central Grasslands Roadmap: ", a(href = "https://www.birdconservancy.org/news-and-events/press-releases/central-grasslands-assessment-map-press-release/", "Press Release")),
    #            p("NGPJV: ", a(href = "https://ngpjv.org", "Partner Organization"))
    #          )
    # ),

    tabPanel("Documentation",
             fluidPage(
               # Overview Section
               h1("Great Plains Population & Trend Simulator Documentation",
                  class = "text-center",
                  style = "margin-bottom: 30px;"),

               # Overview Section
               h2("Overview"),
               p("The Great Plains Population & Trend Simulator (GPPTS) is a sophisticated ecological modeling tool designed for analyzing
      and predicting bird species populations across the Great Plains region. This application integrates complex population
      models with environmental data to provide insights into species trends and abundance patterns."),

               h3("Key Features"),
               tags$ul(
                 tags$li(strong("Population trend modeling and visualization:"), " Generate and visualize species-specific population trends"),
                 tags$li(strong("Environmental scenario testing:"), " Model the effects of environmental changes on populations"),
                 tags$li(strong("Multi-model comparison capabilities:"), " Compare different scenarios and management strategies"),
                 tags$li(strong("Spatial analysis and visualization:"), " Interactive mapping and spatial data integration"),
                 tags$li(strong("Customizable confidence intervals:"), " Adjust statistical confidence levels for analysis"),
                 tags$li(strong("Interactive mapping interface:"), " Draw or upload regions for analysis")
               ),

               # Quick Start Guide
               h2("Quick Start Guide"),
               p("Follow these steps to begin using the application:"),
               tags$ol(
                 tags$li(strong("Launch the Application"),
                         tags$ul(
                           tags$li("Access via modern web browser"),
                           tags$li("Ensure stable internet connection"),
                           tags$li("Check system requirements")
                         )),
                 tags$li(strong("Select a Species"),
                         tags$ul(
                           tags$li("Choose from curated list of Great Plains species"),
                           tags$li("Consider species-specific data availability"),
                           tags$li("Review any model limitations for selected species")
                         )),
                 tags$li(strong("Define Study Area"),
                         tags$ul(
                           tags$li("Upload shapefiles or draw regions directly on map"),
                           tags$li("Define Region of Interest (ROI)"),
                           tags$li("Specify Management Region within ROI")
                         )),
                 tags$li(strong("Set Parameters"),
                         tags$ul(
                           tags$li("Adjust environmental variables"),
                           tags$li("Set confidence intervals"),
                           tags$li("Define temporal range")
                         )),
                 tags$li(strong("Run Scenario"),
                         tags$ul(
                           tags$li("Execute model"),
                           tags$li("Review visualizations"),
                           tags$li("Analyze results")
                         )),
                 tags$li(strong("Save & Compare"),
                         tags$ul(
                           tags$li("Save scenarios for future reference"),
                           tags$li("Compare multiple scenarios"),
                           tags$li("Export results as needed")
                         ))
               ),

               hr(),
               # Detailed User Guide Section
               h2("Detailed User Guide"),

               h3("Getting Started"),
               h4("System Requirements"),
               tags$ul(
                 tags$li(strong("Web Browser:"), " Chrome (v90+), Firefox (v88+), Safari (v14+), Edge (v90+)"),
                 tags$li(strong("Internet Connection:"), " Stable broadband connection required"),
                 tags$li(strong("Screen Resolution:"), " Minimum 1024x768"),
                 tags$li(strong("Memory:"), " 4GB RAM recommended")
               ),

               h4("Browser Compatibility"),
               tags$table(class = "table table-striped",
                          tags$thead(
                            tags$tr(
                              tags$th("Browser"),
                              tags$th("Minimum Version"),
                              tags$th("Notes")
                            )
                          ),
                          tags$tbody(
                            tags$tr(
                              tags$td("Chrome"),
                              tags$td("90+"),
                              tags$td("Recommended browser")
                            ),
                            tags$tr(
                              tags$td("Firefox"),
                              tags$td("88+"),
                              tags$td("Fully supported")
                            ),
                            tags$tr(
                              tags$td("Safari"),
                              tags$td("14+"),
                              tags$td("Fully supported")
                            ),
                            tags$tr(
                              tags$td("Edge"),
                              tags$td("90+"),
                              tags$td("Fully supported")
                            )
                          )
               ),

               h3("Interface Navigation"),
               h4("Main Tabs"),
               tags$ul(
                 tags$li(strong("Run Model"),
                         p("Primary interface for creating and executing scenarios:",
                           tags$ul(
                             tags$li("Species selection"),
                             tags$li("Parameter adjustment"),
                             tags$li("Region definition"),
                             tags$li("Model execution")
                           ))),
                 tags$li(strong("Compare Models"),
                         p("Tool for comparing multiple scenarios:",
                           tags$ul(
                             tags$li("Select multiple scenarios"),
                             tags$li("Visual comparison"),
                             tags$li("Statistical analysis"),
                             tags$li("Export capabilities")
                           ))),
                 tags$li(strong("Documentation"),
                         p("Comprehensive guide and reference material:",
                           tags$ul(
                             tags$li("User guides"),
                             tags$li("Technical documentation"),
                             tags$li("Troubleshooting"),
                             tags$li("FAQs")
                           )))
               ),

               hr(),
               # Running a Scenario Section
               h2("Running a Scenario"),
               h3("Step-by-Step Guide"),

               h4("1. Name Your Scenario"),
               p("Begin by providing a descriptive name for your scenario:",
                 tags$ul(
                   tags$li("Use clear, descriptive names (e.g., 'TBLO_Cropland_Reduction_2030')"),
                   tags$li("Include relevant details like species code, main variable changed, target year"),
                   tags$li("Avoid special characters except underscores (_)")
                 )
               ),

               h4("2. Define Regions"),
               p("Two methods available for defining study regions:"),

               h5("Region of Interest (ROI)"),
               tags$ul(
                 tags$li(strong("Drawing Method:"),
                         tags$ul(
                           tags$li("Select 'Draw on Map' option"),
                           tags$li("Use drawing tools to define area"),
                           tags$li("Double-click to complete polygon"),
                           tags$li("Can be edited or redrawn as needed")
                         )
                 ),
                 tags$li(strong("Shapefile Upload:"),
                         tags$ul(
                           tags$li("Prepare .zip file containing all shapefile components"),
                           tags$li("Required files: .shp, .shx, .dbf, .prj"),
                           tags$li("Optional files: .cpg, .sbn, .sbx"),
                           tags$li("Must be in valid coordinate system"),
                           tags$li("Must be topologically correct and closed, with no gaps and self-intersections or overlapping polygons within the same layers"),
                           tags$li("Must be within Great Plains Simulator the study region")
                         )
                 )
               ),

               h5("Management Region"),
               tags$ul(
                 tags$li("Must be within or equal to ROI"),
                 tags$li("Defines area where management changes will be applied"),
                 tags$li("Same drawing/upload options as ROI"),
                 tags$li("Will be validated against ROI boundaries")
               ),

               h4("3. Select Model Type"),
               tags$div(
                 h5("Base Model (2010-present)"),
                 tags$ul(
                   tags$li(strong("Data Sources:"),
                           tags$ul(
                             tags$li("Rangeland Analysis Platform (RAP)"),
                             tags$li("Road density data"),
                             tags$li("Conservation practice information")
                           )
                   ),
                   tags$li(strong("Characteristics:"),
                           tags$ul(
                             tags$li("Longer temporal coverage"),
                             tags$li("Comprehensive environmental variables"),
                             tags$li("Validated against historical data")
                           )
                   )
                 ),

                 h5("NFWF Model (2017-present)"),
                 tags$ul(
                   tags$li(strong("Additional Features:"),
                           tags$ul(
                             tags$li("NFWF management practice integration"),
                             tags$li("Enhanced conservation metrics"),
                             tags$li("Specialized habitat parameters")
                           )
                   ),
                   tags$li(strong("Limitations:"),
                           tags$ul(
                             tags$li("Shorter temporal coverage"),
                             tags$li("Specific to NFWF project areas"),
                             tags$li("May have different variable sensitivities")
                           )
                   )
                 )
               ),

               hr(),
               # Technical Documentation Section
               h2("Technical Documentation"),

               h3("Model Architecture"),

               h4("Core Components"),
               tags$div(
                 h5("1. Population Model"),
                 p("The population model integrates multiple components to estimate species abundance and trends:"),
                 tags$ul(
                   tags$li(strong("Occupancy Dynamics"),
                           tags$ul(
                             tags$li("Initial occupancy (ψ) estimation"),
                             tags$li("Colonization (γ) and extinction (ε) processes"),
                             tags$li("Temporal persistence patterns")
                           )
                   ),
                   tags$li(strong("Abundance Estimation"),
                           tags$ul(
                             tags$li("Density-dependent factors"),
                             tags$li("Habitat capacity calculations"),
                             tags$li("Spatial distribution patterns")
                           )
                   ),
                   tags$li(strong("Trend Calculation"),
                           tags$ul(
                             tags$li("Year-to-year population changes"),
                             tags$li("Long-term trajectory analysis"),
                             tags$li("Confidence interval estimation")
                           )
                   )
                 ),

                 h5("2. Environmental Integration"),
                 p("Environmental factors are incorporated through various parameters:"),
                 tags$ul(
                   tags$li(strong("Land Cover Variables"),
                           tags$ul(
                             tags$li("Crop cover percentage"),
                             tags$li("Grassland types and extent"),
                             tags$li("Woody vegetation coverage"),
                             tags$li("Annual vs. perennial grass composition")
                           )
                   ),
                   tags$li(strong("Infrastructure Impacts"),
                           tags$ul(
                             tags$li("Road density effects"),
                             tags$li("Development pressure"),
                             tags$li("Habitat fragmentation metrics")
                           )
                   ),
                   tags$li(strong("Management Practices"),
                           tags$ul(
                             tags$li("Conservation actions"),
                             tags$li("Land use changes"),
                             tags$li("Restoration activities")
                           )
                   )
                 )
               ),

               h4("Statistical Framework"),
               tags$div(
                 h5("Model Parameters"),
                 tags$table(class = "table table-striped",
                            tags$thead(
                              tags$tr(
                                tags$th("Parameter"),
                                tags$th("Symbol"),
                                tags$th("Description")
                              )
                            ),
                            tags$tbody(
                              tags$tr(
                                tags$td("Occupancy"),
                                tags$td("ψ"),
                                tags$td("Probability of species presence")
                              ),
                              tags$tr(
                                tags$td("Detection"),
                                tags$td("p"),
                                tags$td("Probability of detecting species when present")
                              ),
                              tags$tr(
                                tags$td("Abundance"),
                                tags$td("N"),
                                tags$td("Estimated population size")
                              ),
                              tags$tr(
                                tags$td("Growth Rate"),
                                tags$td("λ"),
                                tags$td("Population change rate")
                              )
                            )
                 ),

                 h5("Model Equations"),
                 p("Key mathematical relationships in the model:"),
                 tags$ul(
                   tags$li(strong("Occupancy Dynamics:"),
                           tags$code("ψ(t+1) = ψ(t) * (1 - ε) + (1 - ψ(t)) * γ")),
                   tags$li(strong("Population Growth:"),
                           tags$code("N(t+1) = N(t) * λ(t)")),
                   tags$li(strong("Environmental Effects:"),
                           tags$code("logit(ψ) = β₀ + β₁X₁ + β₂X₂ + ... + βₙXₙ"))
                 )
               ),

               hr(),
               # Data Sources and Computational Methods
               h3("Data Sources"),

               h4("Environmental Data"),
               tags$ul(
                 tags$li(strong("Rangeland Analysis Platform (RAP)"),
                         tags$ul(
                           tags$li("Annual vegetation cover"),
                           tags$li("Temporal resolution: Annual"),
                           tags$li("Spatial resolution: 30m"),
                           tags$li("Coverage: 2010-present")
                         )
                 ),
                 tags$li(strong("Road Network Data"),
                         tags$ul(
                           tags$li("Source: OpenStreetMap and TIGER/Line"),
                           tags$li("Updates: Annual"),
                           tags$li("Density calculations: km/km²")
                         )
                 ),
                 tags$li(strong("Conservation Practice Database"),
                         tags$ul(
                           tags$li("NFWF project data"),
                           tags$li("Conservation easements"),
                           tags$li("Management histories")
                         )
                 )
               ),

               h4("Species Data"),
               tags$ul(
                 tags$li(strong("Survey Data"),
                         tags$ul(
                           tags$li("Breeding Bird Survey"),
                           tags$li("eBird observations"),
                           tags$li("Targeted surveys")
                         )
                 ),
                 tags$li(strong("Historical Records"),
                         tags$ul(
                           tags$li("Long-term monitoring"),
                           tags$li("Research studies"),
                           tags$li("State wildlife data")
                         )
                 ),
                 tags$li(strong("Expert Knowledge"),
                         tags$ul(
                           tags$li("Habitat associations"),
                           tags$li("Behavioral patterns"),
                           tags$li("Population constraints")
                         )
                 )
               ),

               h3("Computational Methods"),

               h4("Algorithm Overview"),
               tags$div(
                 h5("1. Initialization"),
                 tags$ul(
                   tags$li("Parameter loading and validation"),
                   tags$li("Spatial data processing"),
                   tags$li("Environmental variable preparation"),
                   tags$li("Model configuration setup")
                 ),

                 h5("2. Processing Pipeline"),
                 tags$ol(
                   tags$li(strong("Data Preparation:"),
                           tags$ul(
                             tags$li("Coordinate system alignment"),
                             tags$li("Raster processing"),
                             tags$li("Variable scaling")
                           )
                   ),
                   tags$li(strong("Model Execution:"),
                           tags$ul(
                             tags$li("Parameter estimation"),
                             tags$li("Population modeling"),
                             tags$li("Uncertainty calculation")
                           )
                   ),
                   tags$li(strong("Result Compilation:"),
                           tags$ul(
                             tags$li("Spatial aggregation"),
                             tags$li("Temporal trending"),
                             tags$li("Uncertainty bounds")
                           )
                   )
                 )
               ),

               hr(),
               # Troubleshooting Section
               h2("Troubleshooting"),

               h3("Common Issues"),
               tags$div(
                 h4("1. Upload Problems"),
                 tags$table(class = "table table-striped",
                            tags$thead(
                              tags$tr(
                                tags$th("Issue"),
                                tags$th("Possible Cause"),
                                tags$th("Solution")
                              )
                            ),
                            tags$tbody(
                              tags$tr(
                                tags$td("Shapefile upload fails"),
                                tags$td("Missing required files"),
                                tags$td("Ensure .zip contains all required files (.shp, .shx, .dbf, .prj)")
                              ),
                              tags$tr(
                                tags$td("Invalid geometry error"),
                                tags$td("Self-intersecting polygons"),
                                tags$td("Check and repair geometry in GIS software")
                              ),
                              tags$tr(
                                tags$td("Coordinate system error"),
                                tags$td("Incompatible projection"),
                                tags$td("Ensure shapefile is in a supported coordinate system (preferably WGS 84)")
                              )
                            )
                 ),

                 h4("2. Processing Errors"),
                 tags$table(class = "table table-striped",
                            tags$thead(
                              tags$tr(
                                tags$th("Error Message"),
                                tags$th("Cause"),
                                tags$th("Resolution")
                              )
                            ),
                            tags$tbody(
                              tags$tr(
                                tags$td("Memory allocation error"),
                                tags$td("Study area too large"),
                                tags$td("Reduce extent of analysis area")
                              ),
                              tags$tr(
                                tags$td("Model convergence failure"),
                                tags$td("Invalid parameter combinations"),
                                tags$td("Check parameter ranges and constraints")
                              ),
                              tags$tr(
                                tags$td("Visualization error"),
                                tags$td("Data processing incomplete"),
                                tags$td("Ensure all model steps completed successfully")
                              )
                            )
                 )
               ),

               h3("Performance Optimization"),
               tags$ul(
                 tags$li(strong("Browser Performance:"),
                         tags$ul(
                           tags$li("Clear browser cache regularly"),
                           tags$li("Limit number of active browser tabs"),
                           tags$li("Use recommended browser versions")
                         )
                 ),
                 tags$li(strong("Data Management:"),
                         tags$ul(
                           tags$li("Optimize shapefile sizes"),
                           tags$li("Use appropriate study area extents"),
                           tags$li("Consider temporal range carefully")
                         )
                 ),
                 tags$li(strong("Model Execution:"),
                         tags$ul(
                           tags$li("Run scenarios sequentially"),
                           tags$li("Save results before comparing"),
                           tags$li("Monitor system resources")
                         )
                 )
               ),

               hr(),
               # Support and Updates Section
               h2("Support and Updates"),

               h3("Contact Information"),
               tags$div(
                 style = "margin-bottom: 20px;",
                 tags$ul(
                   tags$li(strong("Technical Support: "),
                           tags$a(href = "mailto:science@christopher.eco",
                                  "Christopher Kilner")),
                   tags$li(strong("Bug Reports: "),
                           tags$a(href = "https://github.com/ClassicCK/NGPJV-ShinyApp/issues",
                                  "GitHub Issue Tracker")),
                   tags$li(strong("Feature Requests: "),
                           "Submit through GitHub or email support")
                 )
               ),

               h3("Version Information"),
               tags$div(
                 tags$table(class = "table table-striped",
                            tags$thead(
                              tags$tr(
                                tags$th("Version"),
                                tags$th("Release Date"),
                                tags$th("Key Changes")
                              )
                            ),
                            tags$tbody(
                              tags$tr(
                                tags$td("0.0.8"),
                                tags$td("2023-12-15"),
                                tags$td("Initial Release to NGPJV Partners for Feedback")
                              ),
                              tags$tr(
                                tags$td("1.0.0"),
                                tags$td("2024-01-12"),
                                tags$td("Public Release")
                              ),
                              tags$tr(
                                tags$td("1.1.0"),
                                tags$td("2024-02-18"),
                                tags$td("Incorporated Sliders")
                              ),
                              tags$tr(
                                tags$td("1.2.0"),
                                tags$td("2024-03-15"),
                                tags$td("Incorporated Error Messages")
                              ),
                              tags$tr(
                                tags$td("1.3.0"),
                                tags$td("2024-05-17"),
                                tags$td("Updated Plots")
                              ),
                              tags$tr(
                                tags$td("2.0.0"),
                                tags$td("2024-10-30"),
                                tags$td("Added ability to draw regions of interst")
                              ),
                              tags$tr(
                                tags$td("2.1.0"),
                                tags$td("2024-10-31"),
                                tags$td("Added ability to compare multiple models")
                              ),
                              tags$tr(
                                tags$td("2.1.1"),
                                tags$td("2024-11-01"),
                                tags$td("Updated Documentation")
                              ),
                            tags$tr(
                                tags$td("2.1.2"),
                                tags$td("2025-02-19"),
                                tags$td("Corrected shapefile validation errors and optimized processing and validation checks of shapefiles and drawn areas. Updated code to reflect changes in R dependency packages.")
                              ),
                            tags$tr(
                              tags$td("2.2.0"),
                              tags$td("Planned"),
                              tags$td("Add Custom Colors for Plots")
                            )
                            )
                 )
               ),
               hr(),
               h3("Appendix B: Variable Definitions"),
               tags$table(class = "table table-striped",
                          tags$thead(
                            tags$tr(
                              tags$th("Variable"),
                              tags$th("Definition"),
                              tags$th("Units"),
                              tags$th("Range")
                            )
                          ),
                          tags$tbody(
                            tags$tr(
                              tags$td("Crop_pct"),
                              tags$td("Percentage of cropland cover"),
                              tags$td("%"),
                              tags$td("0-100")
                            ),
                            tags$tr(
                              tags$td("shrb"),
                              tags$td("Shrubland cover"),
                              tags$td("%"),
                              tags$td("0-100")
                            ),
                            tags$tr(
                              tags$td("tree"),
                              tags$td("Tree canopy cover"),
                              tags$td("%"),
                              tags$td("0-100")
                            ),
                            tags$tr(
                              tags$td("agfc"),
                              tags$td("Annual grass cover"),
                              tags$td("%"),
                              tags$td("0-100")
                            ),
                            tags$tr(
                              tags$td("pgfc"),
                              tags$td("Perennial grass cover"),
                              tags$td("%"),
                              tags$td("0-100")
                            ),
                            tags$tr(
                              tags$td("Road_km"),
                              tags$td("Road density"),
                              tags$td("km/km²"),
                              tags$td("≥0")
                            )
                          )
               ),

               h3("Appendix C: Model Assumptions"),
               tags$ul(
                 tags$li(strong("Spatial Assumptions:"),
                         tags$ul(
                           tags$li("Spatial independence between sites"),
                           tags$li("Uniform habitat quality within pixels"),
                           tags$li("Representative sampling of landscape")
                         )
                 ),
                 tags$li(strong("Temporal Assumptions:"),
                         tags$ul(
                           tags$li("Markovian population dynamics"),
                           tags$li("Annual time steps adequate"),
                           tags$li("Consistent seasonal timing")
                         )
                 ),
                 tags$li(strong("Population Assumptions:"),
                         tags$ul(
                           tags$li("Closed population during sampling"),
                           tags$li("Density-dependent effects"),
                           tags$li("Representative detection")
                         )
                 )
               ),

               h3("References"),
               tags$div(
                 tags$p(strong("Primary Sources")),
                 tags$ol(
                   tags$li("Latif, Q. et al. (2024). [Forthcoming publication on model development]"),
                   tags$li("Bird Conservancy of the Rockies. (2023). Integrated population models."),
                   tags$li("Northern Great Plains Joint Venture. (2023). Conservation planning tools.")
                 ),

                 tags$p(strong("Additional Resources")),
                 tags$ul(
                   tags$li(tags$a(href = "https://www.birdconservancy.org", "Bird Conservancy of the Rockies")),
                   tags$li(tags$a(href = "https://ngpjv.org", "Northern Great Plains Joint Venture")),
                   tags$li(tags$a(href = "https://www.nfwf.org", "National Fish and Wildlife Foundation"))
                 )
               ),

               style = "margin-bottom: 50px;"
             )
    )
  )
)

##### Load Data #####
spp_area <- read.csv("Data/spp_area.csv")[,2:3]
sp_index <- readRDS("Data/sp_index.RData")
raster_bricks <- readRDS("Data/raster_bricks.RData")
study_region <- st_read("Data/NGPJV_Study_Area.shp", quiet = TRUE)

# Ensure the CRS is compatible with Leaflet (typically WGS 84)
study_region <- st_transform(study_region, crs = 4326)

# Creating a named vector
species_list <- c("TBLO", "BAIS", "LARB", "CCLO", "SPPI", "BRSP", "GRSP", "SPTO", "LBCU", "LOSH","NOHA",
                  "NOPI", "WEME", "STGR", "HOLA", "UPSA", "BBMA", "BUOW", "FEHA", "GRSG", "MAGO", "MALL",
                  "MOPL", "SEOW", "SWHA", "WIPH")

eta_delta_indices <- c("Trt_EA" = 1, "Trt_GZ" = 2, "Crop_pct" = 8, "Road_km" = 12, "agfc" = 13,
                       "pgfc" = 14, "shrb" = 15, "tree" = 16, "ESD_mid_prod" = 17, "ESD_high_prod" = 18,
                       "devprd" = 19, "pgfcXdevprd" = 20, "ESD_mid_prodXdevprd" = 21, "ESD_high_prodXdevprd" = 22)

beta_indices <- c("Trt_EA" = 1, "Trt_GZ" = 2, "Crop_pct" = 8, "Wetland_Count" = 9, "Wetland_Area" = 10,
                  "Road_km" = 12, "agfc" = 13, "pgfc" = 14, "shrb" = 15, "tree" = 16, "ESD_mid_prod" = 17,
                  "ESD_high_prod" = 18, "devprd" = 19, "pgfcXdevprd" = 20, "ESD_mid_prodXdevprd" = 21,
                  "ESD_high_prodXdevprd" = 22)

alpha_indices <- c("Trt_EA" = 1, "Trt_GZ" = 2, "Lat" = 3, "Lon" = 4, "Lat2" = 5, "Lon2" = 6, "RANG_pct" = 7,
                   "Crop_pct" = 8, "Wetland_Count" = 9, "Wetland_Area" = 10, "VRM" = 11, "Road_km" = 12,
                   "agfc" = 13, "pgfc" = 14, "shrb" = 15, "tree" = 16, "ESD_mid_prod" = 17, "ESD_high_prod" = 18,
                   "devprd" = 19, "pgfcXdevprd" = 20, "ESD_mid_prodXdevprd" = 21, "ESD_high_prodXdevprd" = 22)

###### Define Model Functions ######
expit <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

# Functions to process and visualize results
calculate_confidence_intervals <- function(samples, confidence_level) {
  lower_bound <- quantile(samples, (1 - confidence_level)/2)
  upper_bound <- quantile(samples, 1 - (1 - confidence_level)/2)
  return(c(lower_bound, upper_bound))
}

# Function with ability ot project into the future now
calculate_P_C <- function(year, clipped_raster, eta0_tibble, delta0_tibble, eta_delta_tibble, ETA0_sd, DELTA0_sd, dev_ETA0, dev_DELTA0) {
  calculate_values <- function(cell_values, coef, dev_value, covariates) {
    expit(coef + dev_value + sum(covariates * cell_values))
  }

  calc_for_interval <- function(year, raster, coef, sd, covariates, dev_values, interval) {
    if (year <= 2021) {
      dev_value <- dev_values[[interval]]  # Use the specific dev value for sampled years
    } else {
      dev_value <- rnorm(1, 0, sd[[interval]])  # Simulate a dev value for projection years
    }

    raster::calc(raster, function(cell_values) {
      calculate_values(cell_values, coef[[interval]], dev_value, covariates[interval])
    })
  }

  # Calculate for mean, lower, and upper
  P_mean <- calc_for_interval(year, clipped_raster, eta0_tibble, ETA0_sd, eta_delta_tibble, dev_ETA0, "mean")
  P_lower <- calc_for_interval(year, clipped_raster, eta0_tibble, ETA0_sd, eta_delta_tibble, dev_ETA0, "lower")
  P_upper <- calc_for_interval(year, clipped_raster, eta0_tibble, ETA0_sd, eta_delta_tibble, dev_ETA0, "upper")

  C_mean <- calc_for_interval(year, clipped_raster, delta0_tibble, DELTA0_sd, eta_delta_tibble, dev_DELTA0, "mean")
  C_lower <- calc_for_interval(year, clipped_raster, delta0_tibble, DELTA0_sd, eta_delta_tibble, dev_DELTA0, "lower")
  C_upper <- calc_for_interval(year, clipped_raster, delta0_tibble, DELTA0_sd, eta_delta_tibble, dev_DELTA0, "upper")

  # Combine into rasters
  P_brick <- stack(P_mean, P_lower, P_upper)
  C_brick <- stack(C_mean, C_lower, C_upper)

  return(list(P = P_brick, C = C_brick))
}

# Function to calculate Nc
calculate_Nc <- function(rt_previous_tibble, Nc_previous_tibble) {
  # Calculate Nc for mean, lower, and upper intervals
  Nc_mean <- rt_previous_tibble[[1]] * Nc_previous_tibble[[1]]
  Nc_lower <- rt_previous_tibble[[2]] * Nc_previous_tibble[[2]]
  Nc_upper <- rt_previous_tibble[[3]] * Nc_previous_tibble[[3]]

  # Create a raster stack
  Nc_stack <- stack(Nc_mean, Nc_lower, Nc_upper)

  return(Nc_stack)
}

# Help remove outliers
winsorize_layer <- function(layer) {
  # Convert raster layer to a vector, apply Winsorize, and convert back to raster
  layer_values <- getValues(layer)
  winsorized_values <- Winsorize(layer_values, probs = c(0.01, 0.95), na.rm = T)
  setValues(layer, winsorized_values)
}

# Function to calculate rt with confidence intervals
calculate_rt <- function(brick, delta0_tibble, covariate_params_tibble) {
  calculate_values <- function(tibble, cell_values, interval) {
    if (interval == "mean") {
      return(exp(tibble$mean + sum(tibble$mean * cell_values)))
    } else if (interval == "lower") {
      return(exp(tibble$lower + sum(tibble$lower * cell_values)))
    } else {  # interval == "upper"
      return(exp(tibble$upper + sum(tibble$upper * cell_values)))
    }
  }

  rt_mean <- raster::calc(brick, fun = function(cell_values) calculate_values(delta0_tibble, cell_values, "mean"))
  rt_lower <- raster::calc(brick, fun = function(cell_values) calculate_values(delta0_tibble, cell_values, "lower"))
  rt_upper <- raster::calc(brick, fun = function(cell_values) calculate_values(delta0_tibble, cell_values, "upper"))

  rt_brick <- stack(rt_mean, rt_lower, rt_upper)
  return(rt_brick)
}

# Update occupancy
update_occupancy <- function(P_brick, C_brick, psi_previous_brick) {
  calculate_occupancy <- function(P_layer, C_layer, psi_previous_layer) {
    return(P_layer * psi_previous_layer + C_layer * (1 - psi_previous_layer))
  }

  psi_mean <- calculate_occupancy(P_brick[[1]], C_brick[[1]], psi_previous_brick[[1]])
  psi_lower <- calculate_occupancy(P_brick[[2]], C_brick[[2]], psi_previous_brick[[2]])
  psi_upper <- calculate_occupancy(P_brick[[3]], C_brick[[3]], psi_previous_brick[[3]])

  psi_brick <- stack(psi_mean, psi_lower, psi_upper)
  return(psi_brick)
}

# Function to calculate initial Psi with confidence intervals
calculate_initial_psi <- function(brick, alpha_vars_tibble) {
  calculate_psi_values <- function(cell_values, alpha_vars) {
    mean_val <- nimble::expit(alpha_vars$mean + sum(alpha_vars$mean * cell_values))
    lower_val <- nimble::expit(alpha_vars$lower + sum(alpha_vars$lower * cell_values))
    upper_val <- nimble::expit(alpha_vars$upper + sum(alpha_vars$upper * cell_values))
    return(c(mean = mean_val, lower = lower_val, upper = upper_val))
  }

  psi_values <- raster::calc(brick, fun = function(cell_values) {
    calculate_psi_values(cell_values, alpha_vars_tibble)
  })

  return(psi_values)
}

# Function to calculate initial Nc with confidence intervals
calculate_initial_nc <- function(brick, beta0_tibble, covariate_params_tibble, area) {
  calculate_nc_values <- function(cell_values, beta0, covariate_params) {
    mean_val <- exp(beta0$mean + sum(covariate_params$mean * cell_values)) / area
    lower_val <- exp(beta0$lower + sum(covariate_params$lower * cell_values)) / area
    upper_val <- exp(beta0$upper + sum(covariate_params$upper * cell_values)) / area
    return(c(mean = mean_val, lower = lower_val, upper = upper_val))
  }

  nc_values <- raster::calc(brick, fun = function(cell_values) {
    calculate_nc_values(cell_values, beta0_tibble, covariate_params_tibble)
  })

  return(nc_values)
}

# Function to validate shapefile relationship
validate_shapefiles <- function(roi_sf, management_sf, status_handler = NULL) {
  if (is.null(roi_sf) || is.null(management_sf)) {
    return(FALSE)
  }

  # Ensure we're working with sf objects
  if (!inherits(roi_sf, "sf") && !inherits(roi_sf, "sfc")) {
    roi_sf <- st_as_sf(roi_sf)
  }
  if (!inherits(management_sf, "sf") && !inherits(management_sf, "sfc")) {
    management_sf <- st_as_sf(management_sf)
  }

  # Ensure both shapefiles are in the same CRS
  if (st_crs(roi_sf) != st_crs(management_sf)) {
    management_sf <- st_transform(management_sf, st_crs(roi_sf))
  }

  # Calculate areas
  roi_area <- st_area(st_union(roi_sf))
  management_area <- st_area(st_union(management_sf))

  # Check if management region is within ROI
  is_within <- st_contains(st_union(roi_sf), st_union(management_sf), sparse = FALSE)[1, 1]

  # Check if ROI is larger than or equal to management area
  tolerance <- units::set_units(1, "m^2")  # 1 square meter tolerance
  is_valid_size <- (roi_area + tolerance) >= management_area

  if (!is_within) {
    if (!is.null(status_handler)) {
      status_handler("Error: Management region must be completely contained within the Region of Interest")
    }
    return(FALSE)
  }

  if (!is_valid_size) {
    if (!is.null(status_handler)) {
      status_handler("Error: Region of Interest must be equal to or larger than the Management region")
    }
    return(FALSE)
  }

  # Check if areas are approximately equal
  is_equal <- abs(as.numeric(roi_area - management_area)) < as.numeric(tolerance)
  if (!is.null(status_handler)) {
    if (is_equal) {
      status_handler("Valid: Regions are equal in size and properly aligned")
    } else {
      status_handler("Valid: Region of Interest is larger than Management region and properly aligned")
    }
  }

  return(TRUE)
}

#Calculate Abundance and Trend
calculate_abundance_and_trend <- function(Nc_tibble, Psi_tibble) {
  N_results <- list()
  lambda_results <- list()
  previous_N_mean <- 0
  previous_N_lower <- 0
  previous_N_upper <- 0

  for (year in names(Nc_tibble)) {
    Nc_raster <- Nc_tibble[[as.character(year)]]
    Psi_raster <- Psi_tibble[[as.character(year)]]

    # Extract individual layers
    Nc_mean_layer <- Nc_raster[["layer.1"]]
    Nc_lower_layer <- Nc_raster[["layer.2"]]
    Nc_upper_layer <- Nc_raster[["layer.3"]]
    Psi_mean_layer <- Psi_raster[["layer.1"]]
    Psi_lower_layer <- Psi_raster[["layer.2"]]
    Psi_upper_layer <- Psi_raster[["layer.3"]]

    # Calculate N for each layer
    N_mean <- Nc_mean_layer * Psi_mean_layer
    N_lower <- Nc_lower_layer * Psi_lower_layer
    N_upper <- Nc_upper_layer * Psi_upper_layer

    # Replace negative or infinite values with NA
    N_mean[is.infinite(N_mean)] <- 0
    N_lower[is.infinite(N_lower)] <- 0
    N_upper[is.infinite(N_upper)] <- 0

    N_results[[year]] <- stack(N_mean, N_lower, N_upper)

    # Calculate lambda if previous year's data is available and non-zero
    lambda_mean <- N_mean / previous_N_mean
    lambda_lower <- N_lower / previous_N_lower
    lambda_upper <- N_upper / previous_N_upper

    # Replace negative or infinite values with NA
    lambda_mean[is.infinite(lambda_mean)] <- 0
    lambda_lower[is.infinite(lambda_lower)] <- 0
    lambda_upper[is.infinite(lambda_upper)] <- 0

    lambda_results[[year]] <- stack(lambda_mean, lambda_lower, lambda_upper)

    # Store current year's N layers for the next iteration
    previous_N_mean <- N_mean
    previous_N_lower <- N_lower
    previous_N_upper <- N_upper
  }

  return(list(N = N_results, lambda = lambda_results))
}

# Clip to Shapefile - adjusted to ensure that NULL results do not cause a breakage in the code later on,
# switch to is valid approach instead of is not valid approach
clip_raster_with_shapefile <- function(raster_brick, management_shapefile, roi_shapefile) {

  # Run validation before proceeding
  is_valid <- validate_shapefiles(roi_shapefile, management_shapefile)

  if (!is_valid) {
    message("Validation failed. Exiting function.")
    return(NULL)
  }

  # Ensure inputs are sf objects
  if (!inherits(management_shapefile, "sf")) {
    management_shapefile <- st_as_sf(management_shapefile)
  }
  if (!inherits(roi_shapefile, "sf")) {
    roi_shapefile <- st_as_sf(roi_shapefile)
  }

  # Transform to the raster's CRS
  management_shapefile <- st_transform(management_shapefile, crs = projection(raster_brick))
  roi_shapefile <- st_transform(roi_shapefile, crs = projection(raster_brick))

  # Convert sf objects to sp objects
  management_sp <- as(management_shapefile, "Spatial")
  roi_sp <- as(roi_shapefile, "Spatial")

  # Perform the masking and cropping
  masked_raster <- mask(raster_brick, management_sp)
  cropped_raster <- crop(masked_raster, extent(management_sp))

  final_masked_raster <- mask(cropped_raster, roi_sp)
  final_cropped_raster <- crop(final_masked_raster, extent(roi_sp))

  return(final_cropped_raster)
}

# Function for pulling data from MCMC output
get_variables_base <- function(species_index, summary_data, confidence_level, covariate_indices, selected_year) {
  # Determine the correct year index
  # If the selected year is 2021 or later, use index 11
  year_index <- ifelse(selected_year >= 2021, 11, selected_year - 2010)

  variables <- list()

  calculate_confidence_bounds <- function(data, confidence_level) {
    confidence_fraction <- (confidence_level - 50) / (95 - 50)
    lower_bound <- data$mean - (data$mean - data$l95) * confidence_fraction
    upper_bound <- data$mean + (data$u95 - data$mean) * confidence_fraction
    return(list(mean = data$mean, lower = lower_bound, upper = upper_bound))
  }

  # Parameters with no index
  no_params <- c("DELTA0.sd.yr", "ETA0.sd.yr")
  for (param in no_params) {
    param_pattern <- sprintf("%s", param)
    param_rows <- summary_data %>%
      filter(grepl(param_pattern, Parameter)) %>%
      select(mean, l95, u95)

    if (nrow(param_rows) > 0) {
      stats <- calculate_confidence_bounds(param_rows, confidence_level)
      variables[[param]] <- stats
    }
  }

  # Parameters with species and year index
  species_index_params <- c("beta0", "DELTA0", "delta0", "ETA0", "alpha0")
  for (param in species_index_params) {
    param_pattern <- sprintf("%s\\[%d\\]", param, species_index)
    param_rows <- summary_data %>%
      filter(grepl(param_pattern, Parameter)) %>%
      select(mean, l95, u95)

    if (nrow(param_rows) > 0) {
      stats <- calculate_confidence_bounds(param_rows, confidence_level)
      variables[[param]] <- stats
    }
  }

  # Parameters with species and year index
  year_index_params <- c("dev.DELTA0", "dev.delta0", "dev.ETA0")
  for (param in year_index_params) {
    param_pattern <- sprintf("%s\\[%d, %d\\]", param, species_index, year_index)
    param_rows <- summary_data %>%
      filter(grepl(param_pattern, Parameter)) %>%
      select(mean, l95, u95)

    if (nrow(param_rows) > 0) {
      stats <- calculate_confidence_bounds(param_rows, confidence_level)
      variables[[param]] <- stats
    }
  }

  # Parameters with species and covariate index
  covariate_index_params <- c("betaVec", "deltaVec", "ETAVec", "DELTAVec", "alphaVec")
  for (param in covariate_index_params) {
    for (covariate in names(covariate_indices)) {
      covariate_index <- covariate_indices[covariate]
      param_pattern <- sprintf("%s\\[%d, %d\\]", param, covariate_index, species_index)

      param_rows <- summary_data %>%
        filter(grepl(param_pattern, Parameter)) %>%
        select(mean, l95, u95)

      if (nrow(param_rows) > 0) {
        stats <- calculate_confidence_bounds(param_rows, confidence_level)
        variables[[paste(param, covariate, sep = "_")]] <- stats
      }
    }
  }

  return(variables)
}

# Function for pulling data from MCMC output
get_variables_nfwf <- function(species_index, summary_data, confidence_level, covariate_indices, selected_year) {
  # Determine the correct year index
  # If the selected year is 2021 or later, use index 11
  year_index <- ifelse(selected_year >= 2021, 4, selected_year - 2017)

  variables <- list()

  calculate_confidence_bounds <- function(data, confidence_level) {
    confidence_fraction <- (confidence_level - 50) / (95 - 50)
    lower_bound <- data$mean - (data$mean - data$l95) * confidence_fraction
    upper_bound <- data$mean + (data$u95 - data$mean) * confidence_fraction
    return(list(mean = data$mean, lower = lower_bound, upper = upper_bound))
  }

  # Parameters with no index
  no_params <- c("DELTA0.sd.yr", "ETA0.sd.yr")
  for (param in no_params) {
    param_pattern <- sprintf("%s", param)
    param_rows <- summary_data %>%
      filter(grepl(param_pattern, Parameter)) %>%
      select(mean, l95, u95)

    if (nrow(param_rows) > 0) {
      stats <- calculate_confidence_bounds(param_rows, confidence_level)
      variables[[param]] <- stats
    }
  }

  # Parameters with species and year index
  species_index_params <- c("beta0", "DELTA0", "delta0", "ETA0", "alpha0")
  for (param in species_index_params) {
    param_pattern <- sprintf("%s\\[%d\\]", param, species_index)
    param_rows <- summary_data %>%
      filter(grepl(param_pattern, Parameter)) %>%
      select(mean, l95, u95)

    if (nrow(param_rows) > 0) {
      stats <- calculate_confidence_bounds(param_rows, confidence_level)
      variables[[param]] <- stats
    }
  }

  # Parameters with species and year index
  year_index_params <- c("dev.DELTA0", "dev.delta0", "dev.ETA0")
  for (param in year_index_params) {
    param_pattern <- sprintf("%s\\[%d, %d\\]", param, species_index, year_index)
    param_rows <- summary_data %>%
      filter(grepl(param_pattern, Parameter)) %>%
      select(mean, l95, u95)

    if (nrow(param_rows) > 0) {
      stats <- calculate_confidence_bounds(param_rows, confidence_level)
      variables[[param]] <- stats
    }
  }

  # Parameters with species and covariate index
  covariate_index_params <- c("betaVec", "deltaVec", "ETAVec", "DELTAVec", "alphaVec")
  for (param in covariate_index_params) {
    for (covariate in names(covariate_indices)) {
      covariate_index <- covariate_indices[covariate]
      param_pattern <- sprintf("%s\\[%d, %d\\]", param, covariate_index, species_index)

      param_rows <- summary_data %>%
        filter(grepl(param_pattern, Parameter)) %>%
        select(mean, l95, u95)

      if (nrow(param_rows) > 0) {
        stats <- calculate_confidence_bounds(param_rows, confidence_level)
        variables[[paste(param, covariate, sep = "_")]] <- stats
      }
    }
  }

  return(variables)
}

# Transforming 'Vec_' outputs into a tibble for different prefixes
create_covariate_tibble <- function(output, indices, prefix) {
  rows <- lapply(names(indices), function(name) {
    vec_name <- paste0(prefix, "_", name)
    if (is.null(output[[vec_name]])) {
      return(tibble(
        covariate = name,
        mean = NA,
        lower = NA,
        upper = NA
      ))
    } else {
      return(tibble(
        covariate = name,
        mean = output[[vec_name]]$mean,
        lower = output[[vec_name]]$lower,
        upper = output[[vec_name]]$upper
      ))
    }
  })
  bind_rows(rows)
}

# Transforming '0' outputs into a tibble for parameters that do not require year indices
create_params_tibble <- function(output, param_name) {
  if (is.null(output[[param_name]])) {
    return(tibble(
      mean = NA,
      lower = NA,
      upper = NA
    ))
  } else {
    return(tibble(
      mean = output[[param_name]]$mean,
      lower = output[[param_name]]$lower,
      upper = output[[param_name]]$upper
    ))
  }
}

# Function to select values based on confidence level
get_confidence_values <- function(summary_stats, confidence_level) {
  mean_val <- summary_stats$mean
  l95_val <- summary_stats$l95
  u95_val <- summary_stats$u95

  if (confidence_level == 95) {
    return(list(mean = mean_val, lower = l95_val, upper = u95_val))
  } else {
    # Interpolate for other confidence levels
    confidence_fraction <- (confidence_level - 50) / (95 - 50)
    lower_interpolated <- mean_val - (mean_val - l95_val) * confidence_fraction
    upper_interpolated <- mean_val + (u95_val - mean_val) * confidence_fraction

    return(list(mean = mean_val, lower = lower_interpolated, upper = upper_interpolated))
  }
}

# Function to remove outliers from a raster layer
remove_outliers <- function(raster_layer) {
  # Calculate quartiles and interquartile range
  qnts <- quantile(raster_layer[], probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- IQR(raster_layer[], na.rm = TRUE)

  # Define upper and lower bounds for outliers
  upper <- qnts[2] + 1.5 * iqr
  lower <- qnts[1] - 1.5 * iqr

  # Replace outliers with NA
  raster_layer[raster_layer[] > upper | raster_layer[] < lower] <- NA
  return(raster_layer)
}

# Apply the function to each layer of a raster stack
remove_outliers_stack <- function(raster_stack) {
  layers <- nlayers(raster_stack)
  for (i in 1:layers) {
    raster_stack[[i]] <- remove_outliers(raster_stack[[i]])
  }
  return(raster_stack)
}

# Adjust Covariates
adjustCovariates <- function(raster_brick, adjustments, road_adjustment) {
  # Define the mapping from adjustment names to raster layer indices
  layer_indices <- c("Crop_pct" = 8, "shrb" = 15, "tree" = 16, "pgfc" = 14, "agfc" = 13, "Road_km" = 12)

  # Apply adjustments to each relevant layer
  for (adjustment_name in names(adjustments)) {
    layer_index <- layer_indices[[adjustment_name]]
    if (!is.null(layer_index)) {
      adjustment_factor <- 1 + adjustments[[adjustment_name]] / 100
      raster_brick[[layer_index]] <- raster_brick[[layer_index]] * adjustment_factor
    }
  }

  # Apply road adjustment
  if (!is.null(road_adjustment)) {
    road_layer_index <- layer_indices[["Road_km"]]
    road_adjustment_factor <- 1 + road_adjustment / 100
    raster_brick[[road_layer_index]] <- raster_brick[[road_layer_index]] * road_adjustment_factor
  }

  return(raster_brick)
}

# Function to create the trend plot
createTrendPlot <- function(data, start_year, end_year, species) {
  ggplot(data, aes(x = Year)) +
    geom_smooth(aes(y = Lambda_Mean), method = "loess", se = TRUE, color = "purple") +
    labs(x = "Year", y = "Trend (λ)",
         title = paste("Trend Plot for", start_year, "-", end_year, "for", species)) +
    theme_cowplot() +
    geom_hline(yintercept = 1, linetype = "dotted", color = "grey20") +
    scale_y_continuous(limits = c(NA, NA))
}

# Function to create the population plot
# createPopulationPlot function for the main tab:
createPopulationPlot <- function(data, start_year, end_year, species) {
  # Get initial population value
  initial_pop <- data$Population[1]

  ggplot(data, aes(x = Year)) +
    # Add reference line first so it appears behind the data
    geom_hline(yintercept = initial_pop,
               linetype = "dotted",
               color = "grey20") +
    geom_smooth(aes(y = Population),
                method = "loess",
                se = TRUE,
                color = "purple") +
    labs(x = "Year",
         y = "Population",
         title = paste("Population Plot for", start_year, "-", end_year, "for", species)) +
    theme_cowplot() +
    coord_cartesian(ylim = c(0, NA))
}

compileDataForCSV <- function(trend_data, abundance_data, start_year, end_year) {
  compiled_data <- data.frame()

  for (year in start_year:end_year) {
    # Extracting the mean layer for trend and abundance for each year
    trend_layer <- raster::subset(trend_data[[as.character(year)]], 1)  # Mean trend layer
    abundance_layer <- raster::subset(abundance_data[[as.character(year)]], 1)  # Mean abundance layer

    # Extracting values
    trend_values <- raster::values(trend_layer)
    abundance_values <- raster::values(abundance_layer)

    # Getting coordinates
    coords <- raster::xyFromCell(trend_layer, seq_len(ncell(trend_layer)))

    # Creating a data frame for the current year
    year_data <- data.frame(Latitude = coords[, 2], Longitude = coords[, 1],
                            Year = year, Trend = trend_values, Abundance = abundance_values)

    # Combining with the compiled data
    compiled_data <- rbind(compiled_data, year_data)
  }

  return(compiled_data)
}


# Helper function to calculate SE from raster data
calculate_se <- function(values) {
  se <- sd(values, na.rm = TRUE) / sqrt(length(values))
  return(se)
}

prepareTrendData <- function(trend_data, years, model_name) {
  plot_data <- data.frame(
    Year = years,
    Lambda_Mean = rep(NA, length(years)),
    Lambda_SE = rep(NA, length(years)),
    Model = model_name
  )

  for (year in years) {
    if (!is.null(trend_data[[as.character(year)]])) {
      lambda_mean_raster <- raster::subset(trend_data[[as.character(year)]], 1)

      # Calculate mean and SE
      mean_values <- values(lambda_mean_raster)
      mean_lambda <- mean(mean_values, na.rm = TRUE)
      se_lambda <- calculate_se(mean_values)

      plot_data[plot_data$Year == year, c("Lambda_Mean", "Lambda_SE")] <-
        c(mean_lambda, se_lambda)
    }
  }

  plot_data$Lambda_Lower <- plot_data$Lambda_Mean - 1.96 * plot_data$Lambda_SE
  plot_data$Lambda_Upper <- plot_data$Lambda_Mean + 1.96 * plot_data$Lambda_SE

  return(plot_data)
}

preparePopulationData <- function(abundance_data, years, model_name) {
  plot_data <- data.frame(
    Year = years,
    Population = rep(NA, length(years)),
    Population_SE = rep(NA, length(years)),
    Model = model_name
  )

  for (year in years) {
    if (!is.null(abundance_data[[as.character(year)]])) {
      pop_mean_raster <- raster::subset(abundance_data[[as.character(year)]], 1)

      # Calculate mean and SE
      mean_values <- values(pop_mean_raster)
      mean_pop <- sum(mean_values, na.rm = TRUE)
      se_pop <- calculate_se(mean_values)

      plot_data[plot_data$Year == year, c("Population", "Population_SE")] <-
        c(mean_pop, se_pop)
    }
  }

  plot_data$Pop_Lower <- plot_data$Population - 1.96 * plot_data$Population_SE
  plot_data$Pop_Upper <- plot_data$Population + 1.96 * plot_data$Population_SE

  return(plot_data)
}

# Function to update model selection inputs
updateModelSelectionInputs <- function(model_runs, session) {
  model_names <- names(model_runs)
  updateSelectInput(session, "model1", choices = model_names)
  updateSelectInput(session, "model2", choices = model_names)
}

# Function to compare two RasterStacks
compareRasterStacks <- function(stack1, stack2) {
  if (nlayers(stack1) != nlayers(stack2)) {
    stop("The number of layers in the RasterStacks does not match.")
  }

  # Create an empty list to store the results
  result_layers <- vector("list", nlayers(stack1))

  for (i in 1:nlayers(stack1)) {
    # Perform subtraction (or any other operation) layer by layer
    result_layers[[i]] <- raster::calc(stack1[[i]], stack2[[i]], fun = function(x, y) x - y)
  }

  # Combine the results into a new RasterStack
  result_stack <- stack(result_layers)
  return(result_stack)
}


##### Server #####

server <- function(input, output, session) {
  # Named vector mapping species codes to full names
  species_names <- c(
    TBLO = "Thick-billed Longspur",
    BAIS = "Baird's Sparrow",
    LARB = "Lark Bunting",
    CCLO = "Chestnut-collared Longspur",
    SPPI = "Sprague's Pipit",
    BRSP = "Brewer's Sparrow",
    GRSP = "Grasshopper Sparrow",
    SPTO = "Spotted Towhee",
    LBCU = "Long-billed Curlew",
    LOSH = "Loggerhead Shrike",
    NOHA = "Northern Harrier",
    NOPI = "Northern Pintail",
    WEME = "Western Meadowlark",
    STGR = "Sedge Wren",
    HOLA = "Horned Lark",
    UPSA = "Upland Sandpiper",
    BBMA = "Bobolink",
    BUOW = "Burrowing Owl",
    FEHA = "Ferruginous Hawk",
    GRSG = "Greater Sage-Grouse",
    MAGO = "Marbled Godwit",
    MALL = "Mallard",
    MOPL = "Mountain Plover",
    SEOW = "Short-eared Owl",
    SWHA = "Swainson's Hawk",
    WIPH = "Wilson's Phalarope"
  )

  # Create reactive values
  uploaded_management_shapefile <- reactiveVal(NULL)
  uploaded_roi_shapefile <- reactiveVal(NULL)
  validation_status <- reactiveVal(NULL)
  model_runs <- reactiveValues()
  adjusted_percentages <- reactiveValues(Crop_pct = 0, shrb = 0, tree = 0, pgfc = 0, agfc = 0)
  percent_change_raster <- reactiveVal(NULL)

  # Add this reactive expression near the beginning of your server function
  model_start_year <- reactive({
    if (input$modelChoice == "NFWF") {
      return(2017)
    } else {
      return(2010)
    }
  })

  # Add this function definition inside the server function, after the reactive values
  get_active_regions <- function() {
    # Get ROI based on selected method
    roi <- if (input$roi_method == "draw") {
      if (is.null(drawn_features$roi)) return(NULL)
      drawn_features$roi
    } else {
      if (is.null(uploaded_roi_shapefile())) return(NULL)
      uploaded_roi_shapefile()
    }

    # Get management region based on selected method
    management <- if (input$management_method == "draw") {
      if (is.null(drawn_features$management)) return(NULL)
      drawn_features$management
    } else {
      if (is.null(uploaded_management_shapefile())) return(NULL)
      uploaded_management_shapefile()
    }

    list(roi = roi, management = management)
  }


  # Function to validate shapefile relationship
  validate_shapefiles <- function(roi_sf, management_sf) {
    if (is.null(roi_sf) || is.null(management_sf)) {
      return(FALSE)
    }

    # Ensure we're working with sf objects
    if (!inherits(roi_sf, "sf") && !inherits(roi_sf, "sfc")) {
      roi_sf <- st_as_sf(roi_sf)
    }
    if (!inherits(management_sf, "sf") && !inherits(management_sf, "sfc")) {
      management_sf <- st_as_sf(management_sf)
    }

    # Ensure both shapefiles are in the same CRS
    if (st_crs(roi_sf) != st_crs(management_sf)) {
      management_sf <- st_transform(management_sf, st_crs(roi_sf))
    }

    # Calculate areas
    roi_area <- st_area(st_union(roi_sf))
    management_area <- st_area(st_union(management_sf))

    # Check if management region is within ROI
    is_within <- st_contains(st_union(roi_sf), st_union(management_sf), sparse = FALSE)[1, 1]

    # Check if ROI is larger than or equal to management area
    tolerance <- units::set_units(1, "m^2")  # 1 square meter tolerance
    is_valid_size <- (roi_area + tolerance) >= management_area

    if (!is_within) {
      validation_status("Error: Management region must be completely contained within the Region of Interest")
      return(FALSE)
    }

    if (!is_valid_size) {
      validation_status("Error: Region of Interest must be equal to or larger than the Management region")
      return(FALSE)
    }

    # Check if areas are approximately equal
    is_equal <- abs(as.numeric(roi_area - management_area)) < as.numeric(tolerance)
    if (is_equal) {
      validation_status("Valid: Regions are equal in size and properly aligned")
    } else {
      validation_status("Valid: Region of Interest is larger than Management region and properly aligned")
    }

    return(TRUE)
  }

  # Function to process uploaded shapefiles
  process_shapefile <- function(input_file, reactive_val, is_roi = FALSE) {
    unzip_dir <- tempfile()
    dir.create(unzip_dir)

    unzip(input_file$datapath, exdir = unzip_dir)

    shp_files <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
    if (length(shp_files) == 0) {
      showNotification("No .shp file found in the zip archive.", type = "error")
      return()
    }

    shapefile <- tryCatch({
      sf::st_read(shp_files[1], quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error reading shapefile:", e$message), type = "error")
      NULL
    })

    if (!is.null(shapefile)) {
      reactive_val(shapefile)

      if (!is.null(uploaded_roi_shapefile()) && !is.null(uploaded_management_shapefile())) {
        validate_shapefiles(uploaded_roi_shapefile(), uploaded_management_shapefile())
      }

      print(paste(if(is_roi) "ROI" else "Management", "shapefile uploaded and processed."))
    } else {
      print(paste(if(is_roi) "ROI" else "Management", "shapefile processing failed."))
    }
  }

  # Observe ROI shapefile upload
  observe({
    req(input$roiShapefile)
    process_shapefile(input$roiShapefile, uploaded_roi_shapefile, is_roi = TRUE)
  })

  # Observe management shapefile upload
  observe({
    req(input$managementShapefile)
    process_shapefile(input$managementShapefile, uploaded_management_shapefile, is_roi = FALSE)
  })

  # Observer to update validation message in UI
  observe({
    validation_message <- validation_status()
    if (!is.null(validation_message)) {
      message_color <- if (startsWith(validation_message, "Error")) "red" else "green"
      shinyjs::runjs(sprintf(
        "document.getElementById('validation-messages').innerHTML = '<span style=\"color: %s\">%s</span>';",
        message_color,
        validation_message
      ))
    }
  })

  # Initialize Default Leaflet Map
  output$populationMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(data = study_region,
                  color = "gray70",
                  weight = 1,
                  fillColor = "#f37042",
                  fillOpacity = 0.5,
                  group = "Study Region",
                  label = ~"Maximum Scenario Extent") %>%
      setView(lng = -104.0472, lat = 45.9447, zoom = 6) %>%
      addLegend(position = "topleft",
                title = "Map Legend",
                values = c("Study Area"),
                labels = c("Northern Great Plains"),
                colors = "#f37042",
                opacity = 0.5) %>%
      addDrawToolbar(
        targetGroup = "drawn_features",
        editOptions = editToolbarOptions(
          selectedPathOptions = selectedPathOptions()),
        polylineOptions = FALSE,
        rectangleOptions = TRUE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = drawPolygonOptions(
          shapeOptions = drawShapeOptions(
            fillColor = "blue",
            color = "blue"
          )
        )
      ) %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap"),
        overlayGroups = c("Study Region", "drawn_features", "roi_layer", "management_layer"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("roi_layer") %>%
      hideGroup("management_layer")
  })

  # Add reactive values for drawn features
  drawn_features <- reactiveValues(
    roi = NULL,
    management = NULL
  )

  # Observer for drawing events
  observeEvent(input$populationMap_draw_new_feature, {
    feature <- input$populationMap_draw_new_feature

    if (input$roi_method == "draw" && is.null(drawn_features$roi)) {
      drawn_features$roi <- st_sfc(st_polygon(list(matrix(
        unlist(feature$geometry$coordinates[[1]]),
        ncol = 2, byrow = TRUE
      ))), crs = 4326)

      leafletProxy("populationMap") %>%
        clearGroup("roi_layer") %>%
        addPolygons(data = drawn_features$roi,
                    layerId = "roi_polygon",
                    color = "blue",
                    fillColor = "blue",
                    fillOpacity = 0.2,
                    weight = 2,
                    group = "roi_layer") %>%
        showGroup("roi_layer")
    }
    else if (input$management_method == "draw" && is.null(drawn_features$management)) {
      drawn_features$management <- st_sfc(st_polygon(list(matrix(
        unlist(feature$geometry$coordinates[[1]]),
        ncol = 2, byrow = TRUE
      ))), crs = 4326)

      leafletProxy("populationMap") %>%
        clearGroup("management_layer") %>%
        addPolygons(data = drawn_features$management,
                    layerId = "management_polygon",
                    color = "red",
                    fillColor = "red",
                    fillOpacity = 0.2,
                    weight = 2,
                    group = "management_layer") %>%
        showGroup("management_layer")
    }
  })

  # Clear drawings when switching methods
  observeEvent(input$roi_method, {
    if (input$roi_method == "draw") {
      drawn_features$roi <- NULL
      leafletProxy("populationMap") %>%
        clearGroup("roi_layer") %>%
        hideGroup("roi_layer")
    }
  })

  observeEvent(input$management_method, {
    if (input$management_method == "draw") {
      drawn_features$management <- NULL
      leafletProxy("populationMap") %>%
        clearGroup("management_layer") %>%
        hideGroup("management_layer")
    }
  })

  # Modified validation function to handle both drawn and uploaded features
  validate_regions <- function() {
    roi <- if (input$roi_method == "draw") drawn_features$roi else uploaded_roi_shapefile()
    management <- if (input$management_method == "draw") drawn_features$management else uploaded_management_shapefile()

    if (is.null(roi) || is.null(management)) {
      return(FALSE)
    }

    validate_shapefiles(roi, management)
  }

  # Default Blank Plots
  output$trendPlot <- renderPlot({
    ggplot() +
      labs(x = "Year", y = "Trend (λ)", title = "Trend Over Time") +
      theme_cowplot()
  })

  output$populationPlot <- renderPlot({
    ggplot() +
      labs(x = "Year", y = "Abundance", title = "Population Over Time") +
      theme_cowplot()
  })

  # Create a reactive expression to calculate slider sum
  sliderSum <- reactive({
    sum(c(input$Crop_pct, input$shrb, input$tree, input$pgfc, input$agfc))
  })

  # Create observer for slider warning
  observe({
    sum_value <- sliderSum()
    if (sum_value != 0) {
      warning_msg <- sprintf(
        "Warning: The sum of cover adjustments is currently %.1f%%. The sum should equal 0%% for valid scenarios.",
        sum_value
      )
      shinyjs::runjs(sprintf(
        "document.getElementById('slider-warning').innerHTML = '%s';",
        warning_msg
      ))
    } else {
      shinyjs::runjs(
        "document.getElementById('slider-warning').innerHTML = '';"
      )
    }
  })

  # Function to load the selected model and adjust the initial year accordingly
  load_model_and_year <- function() {
    if (input$modelChoice == "NFWF") {
      return(list(
        model = readRDS("Data/nfwf_community.RData"),
        start_year = 2017,
        spp_scale = read.csv("Data/Pars_scale.csv")
      ))
    } else {
      return(list(
        model = readRDS("Data/mod_community.RData"),
        start_year = 2010,
        spp_scale = read.csv("Data/spp_scale.csv")[,2:4]
      ))
    }
  }

  # Begin Modeling
  observeEvent(input$runSaveModel, {
    # Slider sum check remains the same
    slider_sum <- sum(c(input$Crop_pct, input$shrb, input$tree, input$pgfc, input$agfc))
    if (slider_sum != 0) {
      showNotification(
        sprintf("Cannot run model: The sum of cover adjustments is %.1f%%. Adjustments must sum to 0%%.",
                slider_sum),
        type = "error",
        duration = NULL
      )
      return()
    }

    # Get active regions
    regions <- get_active_regions()

    if (is.null(regions$roi) || is.null(regions$management)) {
      showNotification("Please define both Region of Interest and Management Region",
                       type = "error")
      return()
    }

    if (!validate_shapefiles(regions$roi, regions$management)) {
      showNotification("Invalid region configuration. Please check the requirements.", type = "error")
      return()
    }

    # Model info loading remains the same
    model_info <- load_model_and_year()
    mod_community <- model_info$model
    start_year <- model_info$start_year
    spp_scale <- model_info$spp_scale

    species_name <- species_names[input$species]

    # Reset outputs remains the same
    model_output <- reactiveValues(trend = NULL, abundance = NULL)
    Nc_tibble <<- list()
    Psi_tibble <<- list()
    rt_tibble <<- list()

    sapply(names(adjusted_percentages), function(name) {
      adjusted_percentages[[name]] <<- input[[name]]
    })

    if(start_year == 2010){
      # Start the progress bar with an initial message
      withProgress(message = 'Running Model...', value = 0, {
        req(input$endYear)

        # Total number of years to process
        total_years <- as.numeric(input$endYear) - start_year

        print("Starting modeling process...")

        # Obtain species index
        species_index <- which(species_list == input$species)
        confidence_interval <- input$confidenceInterval
        area <- ifelse(input$species %in% spp_area$Spp_code,
                       spp_area[spp_area$Spp_code == input$species, "correction_factor"],
                       1)

        print("Calculating initial alpha and beta variables for 2010")
        initial_alpha_variables <- get_variables_base(species_index, mod_community$summary,
                                                      confidence_interval, alpha_indices, 1)
        initial_beta_variables <- get_variables_base(species_index, mod_community$summary,
                                                     confidence_interval, beta_indices, 1)

        # Create tibbles from initial variables
        initial_alpha_tibble <- create_params_tibble(initial_alpha_variables, "alpha0")
        beta0_tibble <- create_params_tibble(initial_beta_variables, "beta0")
        covariate_params_tibble <- create_covariate_tibble(initial_beta_variables,
                                                           beta_indices, "betaVec")

        print("Calculating initial Psi and Nc values")
        initial_psi_value <- calculate_initial_psi(
          clip_raster_with_shapefile(raster_bricks[["2010"]],
                                     regions$management,
                                     regions$roi),
          initial_alpha_tibble)

        initial_nc_value <- calculate_initial_nc(
          clip_raster_with_shapefile(raster_bricks[["2010"]][[beta_indices]],
                                     regions$management,
                                     regions$roi),
          beta0_tibble,
          covariate_params_tibble,
          area)

        initial_nc_stack <- stack(lapply(1:nlayers(initial_nc_value), function(i) {
          winsorize_layer(initial_nc_value[[i]])
        }))

        print("Initial Psi and Nc values calculated and stored")

        for (year in 2011:input$endYear) {
          # Calculate the progress value
          progress_val <- ((year - 2010) / total_years)

          # Update progress bar and message
          incProgress(amount = 1/total_years, detail = paste("Calculating", year))

          if(year == 2011){
            psi_previous <- initial_psi_value
          }

          tryCatch({
            # Determine the correct RasterBrick for the year
            if(year > 2021) {
              raster_brick_year <- adjustCovariates(raster_bricks[["2021"]],
                                                    adjusted_percentages,
                                                    input$Road_km)
            } else {
              raster_brick_year <- raster_bricks[[as.character(year)]]
            }
            print("Raster brick selected for the year.")

            # Rest of model variables calculation remains the same
            eta_delta_variables <- get_variables_base(species_index, mod_community$summary,
                                                      input$confidenceInterval,
                                                      eta_delta_indices, year)
            beta_variables <- get_variables_base(species_index, mod_community$summary,
                                                 input$confidenceInterval,
                                                 beta_indices, year)
            print("Model variables for eta_delta and beta obtained.")

            # Create tibbles from model variables
            eta_delta_tibble <- create_covariate_tibble(eta_delta_variables,
                                                        eta_delta_indices, "ETAVec")
            beta_tibble <- create_covariate_tibble(beta_variables,
                                                   beta_indices, "betaVec")
            print("Covariate tibbles created.")

            # Pull out SD
            ETA0_sd <- create_params_tibble(eta_delta_variables, "ETA0.sd.yr")
            DELTA0_sd <- create_params_tibble(eta_delta_variables, "DELTA0.sd.yr")

            # Calculate P, C, Psi, Nc, and rt
            P_C_results <- calculate_P_C(
              year = year,
              clipped_raster = clip_raster_with_shapefile(
                raster_brick_year[[eta_delta_indices]],
                regions$management,
                regions$roi
              ),
              eta0_tibble = eta_delta_variables$ETA0,
              delta0_tibble = eta_delta_variables$DELTA0,
              eta_delta_tibble = eta_delta_tibble,
              dev_ETA0 = eta_delta_variables$dev.ETA0,
              dev_DELTA0 = eta_delta_variables$dev.DELTA0,
              ETA0_sd = ETA0_sd,
              DELTA0_sd = DELTA0_sd
            )
            print("P and C calculated.")

            # Calculate rt first
            rt_current_tibble <- calculate_rt(
              clip_raster_with_shapefile(
                raster_brick_year[[beta_indices]],
                regions$management,
                regions$roi
              ),
              beta_variables$delta0,
              beta_tibble
            )
            print("rt calculated.")

            # Rest of the code remains the same
            if(year == 2011){
              rt_initial_tibble <- rt_current_tibble
              values(rt_initial_tibble) <- 1
            }

            # Retrieve previous year's values
            Nc_previous_tibble <- if(year == 2011) initial_nc_stack else Nc_tibble[[as.character(year - 1)]]
            rt_previous_tibble <- if(year == 2011) rt_initial_tibble else rt_tibble[[as.character(year - 1)]]

            # Calculate current year's Nc
            Nc_current_tibble <- calculate_Nc(rt_previous_tibble, Nc_previous_tibble)
            print("Nc calculated.")

            # Then calculate Psi
            psi_current_tibble <- update_occupancy(P_C_results$P, P_C_results$C, psi_previous)
            print("Psi updated.")

            # Store the results of the current year
            Nc_tibble[[as.character(year)]] <- Nc_current_tibble
            Psi_tibble[[as.character(year)]] <- psi_current_tibble
            rt_tibble[[as.character(year)]] <- rt_current_tibble

            # Save the results
            if(year > 2010) {
              abundance_trend_results <- calculate_abundance_and_trend(Nc_tibble, Psi_tibble)
              print("Abundance and Trend Calculated.")

              model_output$abundance[[as.character(year)]] <- abundance_trend_results$N[[as.character(year)]]
              print("Abundance Results Saved.")

              model_output$trend[[as.character(year)]] <- abundance_trend_results$lambda[[as.character(year)]]
              print("Trend Results Saved.")

              print(paste("Year", year, "processed successfully."))
            }
          }, error = function(e) {
            print(paste("Error processing year", year, ":", e$message))
          })
        }
      })
      print("Modeling process completed.")
    } else {
      # Start the progress bar with an initial message
      withProgress(message = 'Running Model...', value = 0, {
        req(input$endYear)

        # Total number of years to process
        total_years <- as.numeric(input$endYear) - start_year

        print("Starting modeling process...")

        # Obtain species index
        species_index <- which(species_list == input$species)
        confidence_interval <- input$confidenceInterval
        area <- ifelse(input$species %in% spp_area$Spp_code,
                       spp_area[spp_area$Spp_code == input$species, "correction_factor"],
                       1)

        print("Calculating initial alpha and beta variables for 2017")
        initial_alpha_variables <- get_variables_nfwf(species_index, mod_community$summary,
                                                      confidence_interval, alpha_indices, 1)
        initial_beta_variables <- get_variables_nfwf(species_index, mod_community$summary,
                                                     confidence_interval, beta_indices, 1)

        # Create tibbles from initial variables
        initial_alpha_tibble <- create_params_tibble(initial_alpha_variables, "alpha0")
        beta0_tibble <- create_params_tibble(initial_beta_variables, "beta0")
        covariate_params_tibble <- create_covariate_tibble(initial_beta_variables, beta_indices, "betaVec")

        print("Calculating initial Psi and Nc values")
        initial_psi_value <- calculate_initial_psi(
          clip_raster_with_shapefile(raster_bricks[["2017"]],
                                     regions$management,
                                     regions$roi),
          initial_alpha_tibble)

        initial_nc_value <- calculate_initial_nc(
          clip_raster_with_shapefile(raster_bricks[["2017"]][[beta_indices]],
                                     regions$management,
                                     regions$roi),
          beta0_tibble,
          covariate_params_tibble,
          area)

        initial_nc_stack <- stack(lapply(1:nlayers(initial_nc_value), function(i) {
          winsorize_layer(initial_nc_value[[i]])
        }))

        print("Initial Psi and Nc values calculated and stored")

        for (year in 2018:input$endYear) {
          # Calculate the progress value
          progress_val <- ((year - 2017) / total_years)

          # Update progress bar and message
          incProgress(amount = 1/total_years, detail = paste("Calculating", year))

          if(year == 2018){
            psi_previous <- initial_psi_value
          }

          tryCatch({
            # Determine the correct RasterBrick for the year
            if(year > 2021) {
              raster_brick_year <- adjustCovariates(raster_bricks[["2021"]],
                                                    adjusted_percentages,
                                                    input$Road_km)
            } else {
              raster_brick_year <- raster_bricks[[as.character(year)]]
            }
            print("Raster brick selected for the year.")

            # Get model variables for eta_delta and beta
            eta_delta_variables <- get_variables_nfwf(species_index, mod_community$summary,
                                                      input$confidenceInterval,
                                                      eta_delta_indices, year)
            beta_variables <- get_variables_nfwf(species_index, mod_community$summary,
                                                 input$confidenceInterval,
                                                 beta_indices, year)
            print("Model variables for eta_delta and beta obtained.")

            # Create tibbles from model variables
            eta_delta_tibble <- create_covariate_tibble(eta_delta_variables,
                                                        eta_delta_indices, "ETAVec")
            beta_tibble <- create_covariate_tibble(beta_variables,
                                                   beta_indices, "betaVec")
            print("Covariate tibbles created.")

            # Pull out SD
            ETA0_sd <- create_params_tibble(eta_delta_variables, "ETA0.sd.yr")
            DELTA0_sd <- create_params_tibble(eta_delta_variables, "DELTA0.sd.yr")

            # Calculate P, C, Psi, Nc, and rt
            P_C_results <- calculate_P_C(
              year = year,
              clipped_raster = clip_raster_with_shapefile(
                raster_brick_year[[eta_delta_indices]],
                regions$management,
                regions$roi
              ),
              eta0_tibble = eta_delta_variables$ETA0,
              delta0_tibble = eta_delta_variables$DELTA0,
              eta_delta_tibble = eta_delta_tibble,
              dev_ETA0 = eta_delta_variables$dev.ETA0,
              dev_DELTA0 = eta_delta_variables$dev.DELTA0,
              ETA0_sd = ETA0_sd,
              DELTA0_sd = DELTA0_sd
            )
            print("P and C calculated.")

            # Calculate rt first
            rt_current_tibble <- calculate_rt(
              clip_raster_with_shapefile(
                raster_brick_year[[beta_indices]],
                regions$management,
                regions$roi
              ),
              beta_variables$delta0,
              beta_tibble
            )
            print("rt calculated.")

            if(year == 2018){
              rt_initial_tibble <- rt_current_tibble
              values(rt_initial_tibble) <- 1
            }

            # Retrieve previous year's values
            Nc_previous_tibble <- if(year == 2018) initial_nc_stack else Nc_tibble[[as.character(year - 1)]]
            rt_previous_tibble <- if(year == 2018) rt_initial_tibble else rt_tibble[[as.character(year - 1)]]

            # Calculate current year's Nc
            Nc_current_tibble <- calculate_Nc(rt_previous_tibble, Nc_previous_tibble)
            print("Nc calculated.")

            # Then calculate Psi
            psi_current_tibble <- update_occupancy(P_C_results$P, P_C_results$C, psi_previous)
            print("Psi updated.")

            # Store the results of the current year
            Nc_tibble[[as.character(year)]] <- Nc_current_tibble
            Psi_tibble[[as.character(year)]] <- psi_current_tibble
            rt_tibble[[as.character(year)]] <- rt_current_tibble

            # Save the results
            if(year > 2017) {
              abundance_trend_results <- calculate_abundance_and_trend(Nc_tibble, Psi_tibble)
              print("Abundance and Trend Calculated.")

              model_output$abundance[[as.character(year)]] <- abundance_trend_results$N[[as.character(year)]]
              print("Abundance Results Saved.")

              model_output$trend[[as.character(year)]] <- abundance_trend_results$lambda[[as.character(year)]]
              print("Trend Results Saved.")

              print(paste("Year", year, "processed successfully."))
            }
          }, error = function(e) {
            print(paste("Error processing year", year, ":", e$message))
          })
        }
      })
      print("Modeling process completed.")
    }


    # Output for the Leaflet map showing the end year population
    # Currently, the data is Winsorized and extremes are limited in the plot - percntile changes
    # changes for sub-populations, which may be misleading when comparing to the overall population figure)
    # Reactive expression to store the percent change raster
    percent_change_raster <- reactive({
      req(model_output$abundance)

      # Get initial and final abundance rasters
      first_year_data <- raster::subset(model_output$abundance[[as.character(start_year+1)]], 1)
      N_data <- raster::subset(model_output$abundance[[as.character(input$endYear)]], 1)

      req(!is.null(first_year_data), !is.null(N_data))
      req(ncell(first_year_data) > 0, ncell(N_data) > 0)

      # Add a small epsilon to avoid division by very small numbers
      epsilon <- 0.0001

      # Calculate percent change with a threshold for meaningful changes
      percent_change <- ((N_data - first_year_data) / (first_year_data + epsilon)) * 100

      # Set very small absolute values to NA to avoid showing misleading large percent changes
      threshold <- mean(values(first_year_data), na.rm = TRUE) * 0.01  # 1% of mean population
      percent_change[first_year_data < threshold] <- NA

      # Winsorize extreme values
      values(percent_change)[values(percent_change) > 100] <- 100
      values(percent_change)[values(percent_change) < -100] <- -100

      return(percent_change)
    })

    # Update the map rendering to use a more appropriate color scale
    output$populationMap <- renderLeaflet({
      req(percent_change_raster())
      percent_change <- percent_change_raster()

      # Define the color palette with more reasonable breaks
      color_pal <- colorNumeric(
        palette = brewer.pal(11, "PuOr"),
        domain = c(-100, 100),  # Fixed domain
        na.color = "transparent"
      )

      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addRasterImage(percent_change,
                       colors = color_pal,
                       opacity = 0.8) %>%
        addLegend(
          position = "bottomleft",
          pal = color_pal,
          values = c(-100, 100),  # Fixed range
          title = "Percent Change (%)",
          opacity = 1,
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
    })

    # Trend Plot
    output$trendPlot <- renderPlot({
      req(model_output$trend)
      years <- seq(start_year+1, input$endYear)

      # Prepare data for the trend plot
      plot_data <- data.frame(Year = years, Lambda_Mean = rep(NA, length(years)),
                              Lambda_Lower = rep(NA, length(years)), Lambda_Upper = rep(NA, length(years)))

      for (year in years) {
        if (!is.null(model_output$trend[[as.character(year)]])) {
          lambda_mean_raster <- raster::subset(model_output$trend[[as.character(year)]], 1)
          lambda_lower_raster <- raster::subset(model_output$trend[[as.character(year)]], 2)
          lambda_upper_raster <- raster::subset(model_output$trend[[as.character(year)]], 3)

          plot_data[plot_data$Year == year, c("Lambda_Mean", "Lambda_Lower", "Lambda_Upper")] <-
            c(mean(values(lambda_mean_raster), na.rm = TRUE),
              mean(values(lambda_lower_raster), na.rm = TRUE),
              mean(values(lambda_upper_raster), na.rm = TRUE))
        }
      }

      createTrendPlot(plot_data, start_year+1, input$endYear, species_name)
    })

    # Abundance Plot
    output$populationPlot <- renderPlot({
      req(model_output$abundance)
      years <- seq(start_year+1, input$endYear)

      # Prepare data for the population plot
      plot_data <- data.frame(Year = years, Population = rep(NA, length(years)),
                              Pop_Lower = rep(NA, length(years)), Pop_Upper = rep(NA, length(years)))

      for (year in years) {
        if (!is.null(model_output$abundance[[as.character(year)]])) {
          pop_mean_raster <- raster::subset(model_output$abundance[[as.character(year)]], 1)
          pop_lower_raster <- raster::subset(model_output$abundance[[as.character(year)]], 2)
          pop_upper_raster <- raster::subset(model_output$abundance[[as.character(year)]], 3)

          plot_data[plot_data$Year == year, c("Population", "Pop_Lower", "Pop_Upper")] <-
            c(sum(values(pop_mean_raster), na.rm = TRUE),
              sum(values(pop_lower_raster), na.rm = TRUE),
              sum(values(pop_upper_raster), na.rm = TRUE))
        }
      }

      createPopulationPlot(plot_data, start_year+1, input$endYear, species_name)
    })

    # Download handler for the population map raster
    output$downloadMap <- downloadHandler(
      filename = function() {
        paste("population_map_", Sys.Date(), ".tif", sep = "")
      },
      content = function(file) {
        req(percent_change_raster())
        raster_data <- percent_change_raster()
        raster::writeRaster(raster_data, file, format = "GTiff")
      },
      contentType = "image/tiff"
    )

    # Download handler for the trend plot
    output$downloadTrendPlot <- downloadHandler(
      filename = function() {
        paste("trend_plot_", input$species, "_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        req(model_output$trend, input$species, input$endYear)

        years <- seq(start_year+1, input$endYear)

        # Prepare data for the trend plot
        plot_data <- data.frame(Year = years, Lambda_Mean = rep(NA, length(years)),
                                Lambda_Lower = rep(NA, length(years)), Lambda_Upper = rep(NA, length(years)))

        for (year in years) {
          if (!is.null(model_output$trend[[as.character(year)]])) {
            lambda_mean_raster <- raster::subset(model_output$trend[[as.character(year)]], 1)
            lambda_lower_raster <- raster::subset(model_output$trend[[as.character(year)]], 2)
            lambda_upper_raster <- raster::subset(model_output$trend[[as.character(year)]], 3)

            plot_data[plot_data$Year == year, c("Lambda_Mean", "Lambda_Lower", "Lambda_Upper")] <-
              c(mean(values(lambda_mean_raster), na.rm = TRUE),
                mean(values(lambda_lower_raster), na.rm = TRUE),
                mean(values(lambda_upper_raster), na.rm = TRUE))
          }
        }

        # Creating the caption
        caption_text <- sprintf(
          "Confidence Interval: %s%%\nCrop Cover Adjustment: %s%%\nShrub Cover Adjustment: %s%%\nTree Cover Adjustment: %s%%\nAnnual Grass Cover Adjustment: %s%%\nPerennial Grass Cover Adjustment: %s%%\nRoad Density Adjustment: %s%%",
          input$confidenceInterval,
          input$Crop_pct,
          input$shrb,
          input$tree,
          input$agfc,
          input$pgfc,
          input$Road_km
        )

        # Start PNG output
        png(file)

        # Generate the plot
        plot <- createTrendPlot(plot_data, start_year+1, input$endYear, species_name) +
          geom_hline(yintercept = 1, linetype = "dotted", color = "grey20") +
          labs(caption = caption_text)

        # Print the plot to the PNG device
        print(plot)

        # Close the PNG device
        dev.off()
      },
      contentType = "image/png"
    )

    # Download handler for the population plot
    output$downloadPopPlot <- downloadHandler(
      filename = function() {
        paste("population_plot_", input$species, "_", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        req(model_output$abundance, input$species, input$endYear)
        years <- seq(start_year+1, input$endYear)

        # Prepare data for the population plot
        plot_data <- data.frame(Year = years, Population = rep(NA, length(years)),
                                Pop_Lower = rep(NA, length(years)), Pop_Upper = rep(NA, length(years)))

        for (year in years) {
          if (!is.null(model_output$abundance[[as.character(year)]])) {
            pop_mean_raster <- raster::subset(model_output$abundance[[as.character(year)]], 1)
            pop_lower_raster <- raster::subset(model_output$abundance[[as.character(year)]], 2)
            pop_upper_raster <- raster::subset(model_output$abundance[[as.character(year)]], 3)

            plot_data[plot_data$Year == year, c("Population", "Pop_Lower", "Pop_Upper")] <-
              c(sum(values(pop_mean_raster), na.rm = TRUE),
                sum(values(pop_lower_raster), na.rm = TRUE),
                sum(values(pop_upper_raster), na.rm = TRUE))
          }
        }
        # Creating the caption
        caption_text <- sprintf(
          "Confidence Interval: %s%%\nCrop Cover Adjustment: %s%%\nShrub Cover Adjustment: %s%%\nTree Cover Adjustment: %s%%\nAnnual Grass Cover Adjustment: %s%%\nPerennial Grass Cover Adjustment: %s%%\nRoad Density Adjustment: %s%%",
          input$confidenceInterval,
          input$Crop_pct,
          input$shrb,
          input$tree,
          input$agfc,
          input$pgfc,
          input$Road_km
        )

        # Start PNG output
        png(file)

        # Generate the plot
        plot <- createPopulationPlot(plot_data, start_year+1, input$endYear, species_name) +
          labs(caption = caption_text)

        # Print the plot to the PNG device
        print(plot)

        # Close the PNG device
        dev.off()
      },
      contentType = "image/png"
    )

    # Download handler for the CSV file
    output$downloadCSV <- downloadHandler(
      filename = function() {
        paste("model_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(model_output$trend, model_output$abundance)

        # Compile the data
        compiled_data <- compileDataForCSV(model_output$trend, model_output$abundance, start_year+1, input$endYear)

        # Write to CSV
        write.csv(compiled_data, file, row.names = FALSE)
      }
    )

    # Check if model outputs are valid
    if (!is.null(model_output$trend) && !is.null(model_output$abundance)) {
      # Save the model output
      model_runs[[input$modelName]] <- list(
        trend = model_output$trend,
        abundance = model_output$abundance
      )

      # Update the model selection inputs with the new list of models
      updateModelSelectionInputs(model_runs, session)
    } else {
      showNotification("Model output is empty or invalid", type = "error")
    }
  })

  ## Model Comparison ##
  # Keep track of number of models to compare
  modelCount <- reactiveVal(2)  # Start with 2 models

  # Generate dynamic model selection UI
  output$modelSelectionUI <- renderUI({
    count <- modelCount()
    model_choices <- names(model_runs)

    tagList(
      lapply(1:count, function(i) {
        selectInput(
          inputId = paste0("model", i),
          label = paste("Model", i),
          choices = model_choices
        )
      })
    )
  })

  # Handle adding models
  observeEvent(input$addModel, {
    modelCount(modelCount() + 1)
  })

  # Handle removing models
  observeEvent(input$removeModel, {
    if (modelCount() > 2) {  # Keep minimum of 2 models
      modelCount(modelCount() - 1)
    }
  })

  observeEvent(input$compareModels, {
    count <- modelCount()
    model_names <- sapply(1:count, function(i) input[[paste0("model", i)]])

    req(all(sapply(model_names, function(name) !is.null(model_runs[[name]]))))

    # Get the last year's data for all models
    year_data <- lapply(model_names, function(name) {
      raster::subset(model_runs[[name]]$abundance[[as.character(input$endYear)]], 1)
    })

    # Calculate percent differences relative to first model
    base_layer <- year_data[[1]]

    # Prepare trend data for all models
    trend_data <- lapply(model_names, function(name) {
      years <- seq(2011, input$endYear)
      prepareTrendData(model_runs[[name]]$trend, years, name)
    })
    combined_trend_data <- do.call(rbind, trend_data)

    # Prepare population data for all models
    pop_data <- lapply(model_names, function(name) {
      years <- seq(2011, input$endYear)
      preparePopulationData(model_runs[[name]]$abundance, years, name)
    })
    combined_pop_data <- do.call(rbind, pop_data)

    # Update plots
    output$comparisonTrendPlot <- renderPlot({
      ggplot(combined_trend_data, aes(x = Year, y = Lambda_Mean, color = Model)) +
        geom_smooth(se = TRUE) +
        geom_hline(yintercept = 1, linetype = "dotted", color = "grey20") +
        labs(title = "Comparison of Trend Data",
             x = "Year",
             y = "Trend (λ)",
             color = "Model") +
        theme_cowplot()
    })

    output$comparisonPopulationPlot <- renderPlot({
      # Get initial population values for each model
      initial_pops <- combined_pop_data %>%
        group_by(Model) %>%
        summarise(initial_pop = first(Population))

      ggplot(combined_pop_data, aes(x = Year, y = Population, color = Model)) +
        # Add reference lines for each model
        geom_hline(data = initial_pops,
                   aes(yintercept = initial_pop,
                       color = Model),
                   linetype = "dotted") +
        geom_smooth(se = TRUE) +
        labs(title = "Comparison of Population Data",
             x = "Year",
             y = "Population",
             color = "Model") +
        theme_cowplot()
    })

    # For the map, we'll show percent difference from the first model
    difference_raster <- ((year_data[[2]] - base_layer) / base_layer) * 100

    # Create legend title with model names
    legend_title <- if (count == 2) {
      # For two models, show direct comparison
      sprintf("Percent Difference Between\n'%s'\nand\n'%s'",
              model_names[2],  # Second model
              model_names[1])  # Base model
    } else {
      # For more than two models, show base model reference
      sprintf("Percent Difference from\nBase Model: '%s'",
              model_names[1])
    }

    # Update map with legend
    output$comparisonMap <- renderLeaflet({
      color_pal <- colorNumeric(
        palette = "PuOr",
        domain = values(difference_raster),
        na.color = "transparent"
      )

      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addRasterImage(
          difference_raster,
          colors = color_pal,
          opacity = 0.7
        ) %>%
        addLegend(
          position = "bottomright",
          pal = color_pal,
          values = values(difference_raster),
          title = legend_title,
          opacity = 1,
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
    })
  })
}

shinyApp(ui = ui, server = server)
