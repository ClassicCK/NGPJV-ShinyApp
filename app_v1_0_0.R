# Load Dependencies #
library(shiny)
library(shinyBS)
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

##### UI #####
ui <- fluidPage(
  titlePanel("Great Plains Population & Trend Simulator (v 1.0.0)"),

  tabsetPanel(
    # Tab for running and saving models
    tabPanel("Run Model",
             sidebarLayout(
               sidebarPanel(
                 # File input with info icon
                 # Input for naming the model
                 textInput("modelName", "Name this Scenario:"),
                 tags$p("Upload Shapefile ",
                        icon("info-circle", id = "info_shapefile"),
                        fileInput("shapefile", label = NULL,
                                  accept = c('.zip'),
                                  multiple = FALSE,
                                  buttonLabel = "Browse...",
                                  placeholder = "No file selected")
                 ),
                 bsTooltip("info_shapefile", "Upload a .zip file containing the Shapefile. Zip must contain the following extensions in sub-folder: .cpg, .dbf, .prj, .sbn, .sbx, .shp, .shx", placement = "right"),

                 # Model selection
                 tags$p("Select Model ",
                        icon("info-circle", id = "info_model"),
                        selectInput("modelChoice", label = NULL, choices = c("Base", "NFWF"))
                 ),
                 bsTooltip("info_model", "Select the model type to run.", placement = "right"),

                 # Species selection with info icon SEOW
                 tags$p("Choose Species ",
                        icon("info-circle", id = "info_species"),
                        selectInput("species", label = NULL, choices = c(
                          "Thick-billed Longspur" = "TBLO", #
                          "Baird's Sparrow" = "BAIS", #
                          "Lark Bunting" = "LARB", #
                          "Chestnut-collared Longspur" = "CCLO", #
                          "Sprague's Pipit" = "SPPI", #
                          "Brewer's Sparrow" = "BRSP", #
                          "Grasshopper Sparrow" = "GRSP", #
                          "Spotted Towhee" = "SPTO", #
                          "Long-billed Curlew" = "LBCU", #
                          #"Loggerhead Shrike" = "LOSH",
                          #"Northern Harrier" = "NOHA",
                          #"Northern Pintail" = "NOPI",
                          "Western Meadowlark" = "WEME", #
                          #"Sedge Wren" = "STGR",
                          "Horned Lark" = "HOLA", #
                          "Upland Sandpiper" = "UPSA", #
                          "Bobolink" = "BBMA", #
                          "Burrowing Owl" = "BUOW", #
                          "Ferruginous Hawk" = "FEHA",
                          "Greater Sage-Grouse" = "GRSG",
                          "Marbled Godwit" = "MAGO", #
                          #"Mallard" = "MALL",
                          "Mountain Plover" = "MOPL", #
                          #"Short-eared Owl" = "SEOW",
                          #"Swainson's Hawk" = "SWHA",
                          "Wilson's Phalarope" = "WIPH")) #
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
                        sliderInput("Crop_pct", label = NULL, min = -100, max = 100, value = 0),
                        checkboxInput("lock_Crop_pct", label = "Lock", value = FALSE)
                 ),
                 bsTooltip("info_crop", "Percent change in crop cover.", placement = "right"),

                 tags$p("Shrub Cover Adjustment (%) ",
                        icon("info-circle", id = "info_shrb"),
                        sliderInput("shrb", label = NULL, min = -100, max = 100, value = 0),
                        checkboxInput("lock_shrb", label = "Lock", value = FALSE)
                 ),
                 bsTooltip("info_shrb", "Percent change in shrub cover.", placement = "right"),

                 tags$p("Tree Cover Adjustment (%) ",
                        icon("info-circle", id = "info_tree"),
                        sliderInput("tree", label = NULL, min = -100, max = 100, value = 0),
                        checkboxInput("lock_tree", label = "Lock", value = FALSE)
                 ),
                 bsTooltip("info_tree", "Percent change in tree cover.", placement = "right"),

                 tags$p("Annual Grass Cover Adjustment (%) ",
                        icon("info-circle", id = "info_agfc"),
                        sliderInput("agfc", label = NULL, min = -100, max = 100, value = 0),
                        checkboxInput("lock_agfc", label = "Lock", value = FALSE)
                 ),
                 bsTooltip("info_agfc", "Percent change in annual grass cover.", placement = "right"),

                 tags$p("Perennial Grass Cover Adjustment (%) ",
                        icon("info-circle", id = "info_pgfc"),
                        sliderInput("pgfc", label = NULL, min = -100, max = 100, value = 0),
                        checkboxInput("lock_pgfc", label = "Lock", value = FALSE)
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
                 # Dropdown to select saved models for comparison
                 selectInput("model1", "First Model", choices = NULL),
                 selectInput("model2", "Second Model", choices = NULL),
                 #selectInput("start_year", "Starting Year", choices = 2011:2023, selected = 2011),
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
                   column(6, plotOutput("comparisonPopulationPlot", height = "30vh")))
               )
             )
    ),
    #
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
               h2("Introduction"),
               p("The NGPJV v1.1.0 Shiny app is designed for ecological data analysis, focusing on bird species population modeling and habitat analysis."),
               p("The functions and tools within this application predict abundance according to population models developed by
               Quresh Latif and described in (insert citation here). The first tabular tool allows a user to select a species of interest
               and visualize the trend and abundance in that species over time according to the environmental parameters established by the
               user within an area of interest. The output is a total abundance estimate (with a confidence interval) for
               each year of the data series, and into the future as specified by the user. Abundance and trend estimates are also produced in raster form,
               allowing the user to spatiall visualize changes in abundance. The application also permits the user to save these scenarios
               and compare multiple scenarios trends and abunadances within a graphical framework. Finally, this application also provides
               use and case study scenarios for population trends and abundances of many grassland species of interest that will be updated
               as the user community finds novel uses for the application and its tools."),


               h2("User Manual"),
               h3("Getting Started"),
               p("The is no special installation required to use this tool; it is universally accessible via a web browser."),
               p("For best use, ensure internet connectivity and compatibility with modern web browsers like Chrome, Firefox, or Safari."),

               h3("Navigating the Interface"),
               p("The app has a straightforward layout structured with two main tool tabs: 'Run Scenario' for executing new scenarios based on
               species of interest, time, and environmental conditions (land use practices). The 'Compare Scenarios' tab provides the user the
               opportunity for comparing different scenarios. It should be noted that we do not advise the comparison of scenarios with different
               species or confidence intervals, as these will not be statistically or ecologically meaningful."),

               h3("Running a Scenario"),
               p("Step 1: Name your model run in the 'Name this Model Run' text input. This will allow you to select the model in the
                 'Comapre Scenarios' tool and also be the name of the file that is downloaded once your scenario run is complete."),
               p("Step 2: Upload a shapefile for geographical data. The extent of the shapefile must be within the boundaries of
               the Great Plains Region as defined on the map upon tool loading. The tool requires a shapefile to be uploaded within a
               Zipped file. The Zip file smust contain the following extensions in sub-folder: .cpg, .dbf, prj, sbn, sbx, .shp, .shx"),
               p("Step 3: Choose base model. The tool provides for two base scenarios to generate scenarios based upon. The first is the
                 default model, based upon Rangeland Analysis Platform, road density, and conservation practice data. The second is the
                 'NFWF Effects' model which incorporates National Fish and Wildlife Foundation land management practices into the model.
                 Notably, the two models will differ in there start date. The base model initilaizes in 2010, while the NFWF model
                 initializes in 2017."),
               p("Step 4: Choose a species and set the end year for the analysis. The species available are curated as species of interest
                 and importance to the Great Plains and for which there is enough data to generate models and scenarios with confidence. The
                 end year determines the final year you wish to project trend and abundance estimates through. However, any and all changes
                 you make to the roads and environmental/management factors will take effect in 2024, so that 2010-2023 estimates will be the
                 baseline observed through the model, with slight changes based upon you confidence interval selection."),
               p("Step 5: Adjust confidence interval and other environmental factors using sliders. We recommend setting your confidence interval
                 to 90 or 95. For environmental and management factors, the tool will ensure that for crop, tree, shrub, and grass cover all changes
                 zero out, so that increases in some will lead to decreases in others. To induce changes in only factors of interest, you can lock
                 the factors you do not wish to change before making slider changes to the factors of interest. Road density is independent of the
                 other listed environmental factors, and therefore will not affect your slider inputs to those other factors."),
               p("Step 6: Click 'Run and Save Scenario' to execute. View generated maps and plots in the main panel. When the scenario is compeleted running
                 it will download automatically the files you may need to upload for the comparison tool if your session crashes. You can also
                 independently download the maps, plots, and a .csv file of the trend and abundance estimates. The species and end-year
                 you selected along with the changes and values you chose for the confidence interval, environmental factors, and road denstiy will be saved along with thses downloads."),

               h3("Comparing Models"),
               p("Select two different models from the dropdown menus."),
               p("Click 'Compare Models' to view the comparative analyses. The map will display the differential in abundance between the two
                 scenarios under comparison. This is why we recommend only comparing scenarios with the same species, end-year, and confidence intervals,
                 with changes to the environmental factors. There will also be plots generated with the scenarios trend and abundances to allow for
                 direct visual comparison. If you are working within the same online session, all models run in that session will be availble for selection.
                 If you are in a new session or your session has expired, you can upload the downloaded .R file to the 'Compare Scenarios' Tool (v 1.2+ only)."),

               h3("Downloading Results"),
               p("Use the download buttons to save maps, plots, and CSV files for both the 'Run Scenarios' and 'Compare Scenarios' tools."),

               h3("Tips and Troubleshooting"),
               p("Check file formats and sizes for uploads. The tool can run with shapefiles that are as large as the maximum extent
                 displayed in the base map. If the 'Run and Save Scenario' tool is not running, this could be for two reasons: (1) your
                 session is expired and you should see a greyed-out screen. Reload the session by clicking the refresh button that should be
                 displayed at the bottom left of the screen in this scenario. If the session is active but the tool is not running, your
                 shapefile may be corrupted or beyond the extent of the Northern Great Plains region, and you will need to upload a new
                 shpaefile according to the parameters layed out in the documentation above. Any other issues are most likely back-end, and
                 will require an email to support and the developer outlined below."),

               h2("Technical Documentation"),
               h3("Architecture Overview"),
               p("The app follows a client-server architecture with a front-end user interface and a back-end processing server. All code is
                 housed on GitHub for open-sourcing commenting and trouble shooting."),

               h3("Detailed Component Description"),
               p("The back-end (Server) handles data processing, model execution, and response generation."),
               p("The front-end (UI) includes interactive elements for user inputs and displaying results."),

               h3("External Dependencies"),
               p("The app relies on various R packages like shiny, leaflet, ggplot2 for its functionality. If the app crashes, this
                 may be due to issues with these dependncies. In this case, reach out to the support and developer outlined below."),

               h2("Contact Information"),
               p("Support Email: christopher.kilner@birdconservancy.org"),
               p("Developer Contact: christopher.kilner@birdconservancy.org")
             )
    )
  )
)

##### Load Data #####
spp_area <- read.csv("Data/spp_area.csv")[,2:3]
#spp_scale <- read.csv("Data/spp_scale.csv")[,2:4] #this also needs to change
sp_index <- readRDS("Data/sp_index.RData")
#mod_community <- readRDS("Data/mod_community.RData") # base model
#mod_community <- readRDS("Data/mod_community_nfwf.RData")
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

# Clip to Shapefile
clip_raster_with_shapefile <- function(raster_brick, shapefile) {
  # Ensure the CRS matches
  shapefile <- st_transform(shapefile, crs = projection(raster_brick))

  # Mask the raster with the shapefile
  masked_raster <- mask(raster_brick, shapefile)

  # Crop the masked raster to the extent of the shapefile
  cropped_raster <- crop(masked_raster, extent(shapefile))

  return(cropped_raster)
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
createPopulationPlot <- function(data, start_year, end_year, species) {
  ggplot(data, aes(x = Year)) +
    geom_smooth(aes(y = Population), method = "loess", se = TRUE, color = "purple") +
    labs(x = "Year", y = "Population",
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


# # Functions for comparison
# prepareTrendData <- function(trend_data, years, model_name) {
#   plot_data <- data.frame(Year = years, Lambda_Mean = rep(NA, length(years)), Model = model_name)
#
#   for (year in years) {
#     if (!is.null(trend_data[[as.character(year)]])) {
#       lambda_mean_raster <- raster::subset(trend_data[[as.character(year)]], 1)
#       plot_data[plot_data$Year == year, "Lambda_Mean"] <- mean(values(lambda_mean_raster), na.rm = TRUE)
#     }
#   }
#
#   return(plot_data)
# }
#
# preparePopulationData <- function(abundance_data, years, model_name) {
#   plot_data <- data.frame(Year = years, Population = rep(NA, length(years)), Model = model_name)
#
#   for (year in years) {
#     if (!is.null(abundance_data[[as.character(year)]])) {
#       pop_mean_raster <- raster::subset(abundance_data[[as.character(year)]], 1)
#       plot_data[plot_data$Year == year, "Population"] <- sum(values(pop_mean_raster), na.rm = TRUE)
#     }
#   }
#
#   return(plot_data)
# }

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

  uploaded_shapefile <- reactiveVal(NULL)
  model_runs <- reactiveValues()

  # Reactive value to store the percent change raster
  percent_change_raster <- reactiveVal(NULL)

  # Store the user-adjusted percentages as reactive values
  adjusted_percentages <- reactiveValues(Crop_pct = 0, shrb = 0, tree = 0, pgfc = 0, agfc = 0)

  # Initialize Default Leaflet Map
  output$populationMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(data = study_region, color = "gray70", weight = 1, fillColor = "#f37042", fillOpacity = 0.5, label = ~"Maximum Scenario Extent") %>%
      setView(lng = -104.0472, lat = 45.9447, zoom = 6)  %>%
      addLegend(position = "topleft",
                title = "Map Legend",
                values = c("Study Area"),
                labels = c("Northern Great Plains"),
                colors = "#f37042",
                opacity = 0.5)
  })

  # Default Blank Plots
  output$trendPlot <- renderPlot({
    # Create a blank plot (customize as needed)
    ggplot() +
      labs(x = "Year", y = "Trend (λ)", title = "Trend Over Time") +
      theme_cowplot()
  })

  output$populationPlot <- renderPlot({
    # Create another blank plot (customize as needed)
    ggplot() +
      labs(x = "Year", y = "Abundance", title = "Population Over Time") +
      theme_cowplot()
  })

  observe({
    req(input$shapefile)

    # Create a new temporary directory for each upload
    unzip_dir <- tempfile()
    dir.create(unzip_dir)
    # Unzip the file
    unzip(input$shapefile$datapath, exdir = unzip_dir)

    # Log the contents of the unzip directory for debugging
    print(list.files(unzip_dir))

    # Find the .shp file in the unzipped directory
    shp_files <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE, recursive = T)
    if (length(shp_files) == 0) {
      showNotification("No .shp file found in the zip archive.", type = "error")
      return()
    }

    # Read the first shapefile found
    shapefile <- tryCatch({
      sf::st_read(shp_files[1], quiet = TRUE)
    }, error = function(e) {
      showNotification(paste("Error reading shapefile:", e$message), type = "error")
      NULL
    })

    if (!is.null(shapefile)) {
      uploaded_shapefile(shapefile)
      print("Shapefile uploaded and processed.")
    } else {
      print("Shapefile processing failed.")
    }
  })

  # Percent UI Sliders
  previous_values <- reactiveValues(Crop_pct = 0, shrb = 0, tree = 0, pgfc = 0, agfc = 0)

  observe({
    # List all sliders
    sliders <- c("Crop_pct", "shrb", "tree", "pgfc", "agfc")

    # Helper function to get lock status safely
    is_locked <- function(name) {
      lock_input <- input[[paste("lock", name, sep = "_")]]
      return(!is.null(lock_input) && lock_input)
    }

    # Determine which sliders are not locked
    unlocked_sliders <- sliders[!sapply(sliders, is_locked)]

    # Calculate the net change considering only the unlocked sliders
    net_change <- sum(sapply(unlocked_sliders, function(name) input[[name]]) - sapply(unlocked_sliders, function(name) previous_values[[name]]))

    # Detect which slider(s) were changed and are not locked
    changedSliders <- which(sapply(unlocked_sliders, function(name) input[[name]] != previous_values[[name]]))

    # Handle adjustments
    if (net_change != 0 && length(changedSliders) == 1 && length(unlocked_sliders) > 1) {
      changedSliderName <- unlocked_sliders[changedSliders]
      amount_to_adjust <- -input[[changedSliderName]]

      # Find the other sliders to adjust
      other_sliders <- setdiff(unlocked_sliders, changedSliderName)
      num_other_sliders <- length(other_sliders)

      # Distribute the adjustment evenly across the other unlocked sliders
      adjustment_per_slider <- amount_to_adjust / num_other_sliders

      # Apply the adjustment to the other unlocked sliders
      for (sliderName in other_sliders) {
        new_value <- input[[sliderName]] + adjustment_per_slider
        # Prevent the slider from exceeding its bounds
        new_value <- min(100, max(-100, new_value))
        updateSliderInput(session, sliderName, value = new_value)
      }
    }

    # Update previous values for all sliders
    sapply(sliders, function(name) previous_values[[name]] <<- input[[name]])
  })

  #
  # # Create reactive values to store the previous values of the sliders
  # previous_values <- reactiveValues(Crop_pct = 0, shrb = 0, tree = 0, pgfc = 0, agfc = 0)
  #
  # sliders <- c("Crop_pct", "shrb", "tree", "pgfc", "agfc")
  #
  # observe({
  #   # Calculate the net change from the previous values
  #   net_change <- sum(sapply(sliders, function(name) input[[name]]) - sapply(sliders, function(name) previous_values[[name]]))
  #
  #   # Detect the slider(s) that were changed
  #   changedSliders <- which(sapply(sliders, function(name) input[[name]] != previous_values[[name]]))
  #
  #   # If there's a net change and exactly one slider has changed, adjust the others
  #   if (net_change != 0 && length(changedSliders) == 1) {
  #     changedSliderName <- sliders[changedSliders]
  #
  #     # Calculate the adjustment needed per other slider
  #     adjustment_per_slider <- -net_change / (length(sliders) - 1)
  #
  #     # Apply the adjustment to the other sliders
  #     for (sliderName in sliders) {
  #       if (sliderName != changedSliderName) {
  #         new_value <- input[[sliderName]] + adjustment_per_slider
  #         updateSliderInput(session, sliderName, value = new_value)
  #       }
  #     }
  #   }
  #
  #   # Update the previous values
  #   sapply(sliders, function(name) previous_values[[name]] <<- input[[name]])
  # })

  # Initialize lists to store results for each year
  Nc_tibble <- list()
  Psi_tibble <- list()
  rt_tibble <- list()

  # Function to load the selected model and adjust the initial year accordingly
  load_model_and_year <- reactive({
    if (input$modelChoice == "NFWF") {
      list(model = readRDS("Data/nfwf_community.RData"), start_year = 2017, spp_scale = read.csv("Data/Pars_scale.csv"))
    } else {
      list(model = readRDS("Data/mod_community.RData"), start_year = 2010, spp_scale = read.csv("Data/spp_scale.csv")[,2:4])
    }
  })

  # Begin Modeling
  observeEvent(input$runSaveModel, {
    # Load the selected model
    # Load the selected model and the starting year
    model_info <- load_model_and_year()
    mod_community <- model_info$model
    start_year <- model_info$start_year
    spp_scale <- model_info$spp_scale

    species_name <- species_names[input$species]
    # Reset model_output and adjusted_percentages
    model_output <- reactiveValues(trend = NULL, abundance = NULL)

    # Reset Nc_tibble, Psi_tibble, and rt_tibble
    Nc_tibble <<- list()
    Psi_tibble <<- list()
    rt_tibble <<- list()

    # Now update adjusted_percentages with current input values
    sapply(names(adjusted_percentages), function(name) {
      adjusted_percentages[[name]] <<- input[[name]]
    })

    # Initialize Default Leaflet Map
    output$populationMap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 5)
    })

    # Default Blank Plots
    output$trendPlot <- renderPlot({
      # Create a blank plot (customize as needed)
      ggplot() +
        labs(x = "Year", y = "Trend (λ)", title = "Trend Over Time") +
        theme_cowplot()
    })

    output$populationPlot <- renderPlot({
      # Create another blank plot (customize as needed)
      ggplot() +
        labs(x = "Year", y = "Abundance", title = "Population Over Time") +
        theme_cowplot()
    })

    if(start_year == 2010){
    # Start the progress bar with an initial message
    withProgress(message = 'Running Model...', value = 0, {
      req(uploaded_shapefile())
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
      initial_alpha_variables <- get_variables_base(species_index, mod_community$summary, confidence_interval, alpha_indices, 1)  # 1 for 2010
      initial_beta_variables <- get_variables_base(species_index, mod_community$summary, confidence_interval, beta_indices, 1)  # 1 for 2010

      # Create tibbles from initial variables
      initial_alpha_tibble <- create_params_tibble(initial_alpha_variables, "alpha0")
      beta0_tibble <- create_params_tibble(initial_beta_variables, "beta0")
      covariate_params_tibble <- create_covariate_tibble(initial_beta_variables, beta_indices, "betaVec")

      print("Calculating initial Psi and Nc values")
      initial_psi_value <- calculate_initial_psi(clip_raster_with_shapefile(raster_bricks[["2010"]], uploaded_shapefile()), initial_alpha_tibble)
      initial_nc_value <- calculate_initial_nc(clip_raster_with_shapefile(raster_bricks[["2010"]][[beta_indices]], uploaded_shapefile()), beta0_tibble, covariate_params_tibble, area)
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
            raster_brick_year <- adjustCovariates(raster_bricks[["2021"]], adjusted_percentages, input$Road_km)
          } else {
            raster_brick_year <- raster_bricks[[as.character(year)]]
          }
          print("Raster brick selected for the year.")

          # Get model variables for eta_delta and beta
          eta_delta_variables <- get_variables_base(species_index, mod_community$summary, input$confidenceInterval, eta_delta_indices, year)
          beta_variables <- get_variables_base(species_index, mod_community$summary, input$confidenceInterval, beta_indices, year)
          print("Model variables for eta_delta and beta obtained.")

          # Create tibbles from model variables
          eta_delta_tibble <- create_covariate_tibble(eta_delta_variables, eta_delta_indices, "ETAVec")
          beta_tibble <- create_covariate_tibble(beta_variables, beta_indices, "betaVec")
          print("Covariate tibbles created.")

          # Pull out SD
          ETA0_sd <- create_params_tibble(eta_delta_variables, "ETA0.sd.yr")
          DELTA0_sd <- create_params_tibble(eta_delta_variables, "DELTA0.sd.yr")

          # Calculate P, C, Psi, Nc, and rt
          P_C_results <- calculate_P_C(year = year, clipped_raster = clip_raster_with_shapefile(raster_brick_year[[eta_delta_indices]], uploaded_shapefile()),
                                       eta0_tibble = eta_delta_variables$ETA0, delta0_tibble = eta_delta_variables$DELTA0, eta_delta_tibble = eta_delta_tibble,
                                       dev_ETA0 =  eta_delta_variables$dev.ETA0, dev_DELTA0 = eta_delta_variables$dev.DELTA0, ETA0_sd = ETA0_sd, DELTA0_sd = DELTA0_sd)
          print("P and C calculated.")

          # Calculate rt first
          rt_current_tibble <- calculate_rt(clip_raster_with_shapefile(raster_brick_year[[beta_indices]], uploaded_shapefile()), beta_variables$delta0, beta_tibble)
          print("rt calculated.")

          if(year == 2011){
            rt_initial_tibble <- rt_current_tibble
            values(rt_initial_tibble) <- 1
          }

          # Retrieve previous year's Nc value
          Nc_previous_tibble <- if(year == 2011) initial_nc_stack else Nc_tibble[[as.character(year - 1)]]
          rt_previous_tibble <- if(year == 2011) rt_initial_tibble else rt_tibble[[as.character(year - 1)]]

          # Calculate current year's Nc
          Nc_current_tibble <- calculate_Nc(rt_previous_tibble, Nc_previous_tibble)
          print("Nc calculated.")

          # Then calculate Psi
          psi_current_tibble <- update_occupancy(P_C_results$P, P_C_results$C, psi_previous)
          print("Psi updated.")

          # Store the results of the current year in the lists
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
    }
    else{
      # Start the progress bar with an initial message
      withProgress(message = 'Running Model...', value = 0, {
        req(uploaded_shapefile())
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
        initial_alpha_variables <- get_variables_nfwf(species_index, mod_community$summary, confidence_interval, alpha_indices, 1)  # 8 for 2017
        initial_beta_variables <- get_variables_nfwf(species_index, mod_community$summary, confidence_interval, beta_indices, 1)  # 8 for 2017

        # Create tibbles from initial variables
        initial_alpha_tibble <- create_params_tibble(initial_alpha_variables, "alpha0")
        beta0_tibble <- create_params_tibble(initial_beta_variables, "beta0")
        covariate_params_tibble <- create_covariate_tibble(initial_beta_variables, beta_indices, "betaVec")

        print("Calculating initial Psi and Nc values")
        initial_psi_value <- calculate_initial_psi(clip_raster_with_shapefile(raster_bricks[["2017"]], uploaded_shapefile()), initial_alpha_tibble)
        initial_nc_value <- calculate_initial_nc(clip_raster_with_shapefile(raster_bricks[["2017"]][[beta_indices]], uploaded_shapefile()), beta0_tibble, covariate_params_tibble, area)
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
              raster_brick_year <- adjustCovariates(raster_bricks[["2021"]], adjusted_percentages, input$Road_km)
            } else {
              raster_brick_year <- raster_bricks[[as.character(year)]]
            }
            print("Raster brick selected for the year.")

            # Get model variables for eta_delta and beta
            eta_delta_variables <- get_variables_nfwf(species_index, mod_community$summary, input$confidenceInterval, eta_delta_indices, year)
            beta_variables <- get_variables_nfwf(species_index, mod_community$summary, input$confidenceInterval, beta_indices, year)
            print("Model variables for eta_delta and beta obtained.")

            # Create tibbles from model variables
            eta_delta_tibble <- create_covariate_tibble(eta_delta_variables, eta_delta_indices, "ETAVec")
            beta_tibble <- create_covariate_tibble(beta_variables, beta_indices, "betaVec")
            print("Covariate tibbles created.")

            # Pull out SD
            ETA0_sd <- create_params_tibble(eta_delta_variables, "ETA0.sd.yr")
            DELTA0_sd <- create_params_tibble(eta_delta_variables, "DELTA0.sd.yr")

            # Calculate P, C, Psi, Nc, and rt
            P_C_results <- calculate_P_C(year = year, clipped_raster = clip_raster_with_shapefile(raster_brick_year[[eta_delta_indices]], uploaded_shapefile()),
                                         eta0_tibble = eta_delta_variables$ETA0, delta0_tibble = eta_delta_variables$DELTA0, eta_delta_tibble = eta_delta_tibble,
                                         dev_ETA0 =  eta_delta_variables$dev.ETA0, dev_DELTA0 = eta_delta_variables$dev.DELTA0, ETA0_sd = ETA0_sd, DELTA0_sd = DELTA0_sd)
            print("P and C calculated.")

            # Calculate rt first
            rt_current_tibble <- calculate_rt(clip_raster_with_shapefile(raster_brick_year[[beta_indices]], uploaded_shapefile()), beta_variables$delta0, beta_tibble)
            print("rt calculated.")

            if(year == 2018){
              rt_initial_tibble <- rt_current_tibble
              values(rt_initial_tibble) <- 1
            }

            # Retrieve previous year's Nc value
            Nc_previous_tibble <- if(year == 2018) initial_nc_stack else Nc_tibble[[as.character(year - 1)]]
            rt_previous_tibble <- if(year == 2018) rt_initial_tibble else rt_tibble[[as.character(year - 1)]]

            # Calculate current year's Nc
            Nc_current_tibble <- calculate_Nc(rt_previous_tibble, Nc_previous_tibble)
            print("Nc calculated.")

            # Then calculate Psi
            psi_current_tibble <- update_occupancy(P_C_results$P, P_C_results$C, psi_previous)
            print("Psi updated.")

            # Store the results of the current year in the lists
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
    # Reactive expression to store the percent change raster
    percent_change_raster <- reactive({
      req(model_output$abundance, uploaded_shapefile())

      # Ensure the specified years are available in the model output
      req(model_output$abundance[[as.character(start_year+1)]])
      req(model_output$abundance[[as.character(input$endYear)]])

      first_year_data <- raster::subset(model_output$abundance[[as.character(start_year+1)]], 1)
      N_data <- raster::subset(model_output$abundance[[as.character(input$endYear)]], 1)

      # Check if rasters are not NULL and have data
      req(!is.null(first_year_data), !is.null(N_data))
      req(ncell(first_year_data) > 0, ncell(N_data) > 0)

      # Calculate percent change
      percent_change <- ((N_data - first_year_data) / first_year_data) * 100
      percent_change[is.infinite(percent_change)] <- 0  # Handle NA and infinite values

      return(percent_change)
    })

    output$populationMap <- renderLeaflet({
      req(percent_change_raster())
      percent_change <- percent_change_raster()

      # Define the color palette
      color_pal <- colorNumeric(
        palette = brewer.pal(11, "PuOr"),
        domain = range(values(percent_change), na.rm = TRUE)
      )

      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addRasterImage(percent_change, colors = color_pal, opacity = 0.8) %>%
        addLegend("bottomleft", pal = color_pal, values = values(percent_change),
                  title = "Percent Change (%)", opacity = 1, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
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

  observeEvent(input$compareModels, {
    req(input$model1, input$model2)
    req(model_runs[[input$model1]], model_runs[[input$model2]])

    # Extracting the last available year's RasterBrick for each model
    latest_year_brick1 <- model_runs[[input$model1]]$abundance[[as.character(input$endYear)]]
    latest_year_brick2 <- model_runs[[input$model2]]$abundance[[as.character(input$endYear)]]

    # Assuming the first layer is the relevant one for the comparison
    latest_year_layer1 <- raster::subset(latest_year_brick1, 1)
    latest_year_layer2 <- raster::subset(latest_year_brick2, 1)

    # Perform the comparison
    difference_raster <- ((latest_year_layer2 - latest_year_layer1) / latest_year_layer1) * 100

    req(difference_raster)

    # Render the Leaflet map for comparison
    output$comparisonMap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addRasterImage(difference_raster, colors = colorNumeric(palette = "PuOr", domain = NULL))
    })

    output$comparisonTrendPlot <- renderPlot({
      req(model_runs[[input$model1]]$trend, model_runs[[input$model2]]$trend)
      years <- seq(2011, input$endYear)

      # Prepare data for the first model
      trend_data_model1 <- prepareTrendData(model_runs[[input$model1]]$trend, years, input$model1)

      # Prepare data for the second model
      trend_data_model2 <- prepareTrendData(model_runs[[input$model2]]$trend, years, input$model2)

      # Combine data for plotting
      combined_trend_data <- rbind(trend_data_model1, trend_data_model2)

      ggplot(combined_trend_data, aes(x = Year, y = Lambda_Mean, color = Model)) +
        geom_smooth(se = T) +
        geom_hline(yintercept = 1, linetype = "dotted", color = "grey20") +
        labs(title = "Comparison of Trend Data", x = "Year", y = "Trend (λ)", color = "Model") +
        theme_cowplot()
    })

    output$comparisonPopulationPlot <- renderPlot({
      req(model_runs[[input$model1]]$abundance, model_runs[[input$model2]]$abundance)
      years <- seq(2011, input$endYear)

      # Prepare data for the first model
      population_data_model1 <- preparePopulationData(model_runs[[input$model1]]$abundance, years, input$model1)

      # Prepare data for the second model
      population_data_model2 <- preparePopulationData(model_runs[[input$model2]]$abundance, years, input$model2)

      # Combine data for plotting
      combined_population_data <- rbind(population_data_model1, population_data_model2)

      ggplot(combined_population_data, aes(x = Year, y = Population, color = Model)) +
        geom_smooth(se = T) +
        labs(title = "Comparison of Population Data", x = "Year", y = "Population", color = "Model") +
        theme_cowplot()
    })
    # output$comparisonTrendPlot <- renderPlot({
    #   req(model_runs[[input$model1]]$trend, model_runs[[input$model2]]$trend)
    #   years <- seq(start_year + 1, input$endYear)
    #
    #   # Prepare data for the first model
    #   trend_data_model1 <- prepareTrendData(model_runs[[input$model1]]$trend, years, input$model1)
    #
    #   # Prepare data for the second model
    #   trend_data_model2 <- prepareTrendData(model_runs[[input$model2]]$trend, years, input$model2)
    #
    #   # Combine data for plotting
    #   combined_trend_data <- rbind(trend_data_model1, trend_data_model2)
    #
    #   ggplot(combined_trend_data, aes(x = Year, y = Lambda_Mean, color = Model, fill = Model)) +
    #     geom_ribbon(aes(ymin = Lambda_Lower, ymax = Lambda_Upper), alpha = 0.2, linetype = 0) +
    #     geom_line(size = 1.2) +
    #     geom_hline(yintercept = 1, linetype = "dotted", color = "grey20") +
    #     labs(title = "Comparison of Trend Data", x = "Year", y = "Trend (λ)", color = "Model", fill = "Model") +
    #     theme_cowplot()
    # })
    #
    # output$comparisonPopulationPlot <- renderPlot({
    #   req(model_runs[[input$model1]]$abundance, model_runs[[input$model2]]$abundance)
    #   years <- seq(input$start_year + 1, input$endYear)
    #
    #   # Prepare data for the first model
    #   population_data_model1 <- preparePopulationData(model_runs[[input$model1]]$abundance, years, input$model1)
    #
    #   # Prepare data for the second model
    #   population_data_model2 <- preparePopulationData(model_runs[[input$model2]]$abundance, years, input$model2)
    #
    #   # Combine data for plotting
    #   combined_population_data <- rbind(population_data_model1, population_data_model2)
    #
    #   ggplot(combined_population_data, aes(x = Year, y = Population, color = Model, fill = Model)) +
    #     geom_ribbon(aes(ymin = Pop_Lower, ymax = Pop_Upper), alpha = 0.2, linetype = 0) +
    #     geom_line(size = 1.2) +
    #     labs(title = "Comparison of Population Data", x = "Year", y = "Population", color = "Model", fill = "Model") +
    #     theme_cowplot()
    # })
    #

  })
}

shinyApp(ui = ui, server = server)
