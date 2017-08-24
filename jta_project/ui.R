# ui.r alteredData app
# Libraries ----
require(shiny)
require(shinydashboard)
require(data.table)
require(dplyr)
require(ggplot2)
require(plotly)
require(MASS)
require(statisticalModeling)
require(DT)
require(factoextra)
require(arules)
require(arulesViz)
require(visNetwork)
require(rgl)

# data loading ----
setwd("C:/Users/Trevor.JTAWEB/Desktop/asdf/rstudio/jta_project")
data <- fread("scrambledData.csv")
data[, V1 := NULL]
a <- data
da <- data

# data cleaning ----
programs.list <- list("office" = names(data)[22:34], "web.browser" = names(data)[35:38], "music" = names(data)[39:41], "cloud.storage" = names(data)[42:46], "video.chat" = names(data)[47:50], "other" = names(data)[51:54])

# ordering some factors for bar charts
a[, continent:= factor(continent, ordered = T, levels = c("America_North", "Europe", "America_South", "Asia", "Africa"))]
a[, physical_server_group:= factor(physical_server_group, ordered = T, levels = c("1-4", "5-9", "10-19", "20-29", "30-49", "50+", "NA"))]
a[, pcs := factor(pcs, ordered = T, levels = c("1-4", "5-24", "25-49", "50-249", "500+"))]
a[, Employees := factor(Employees, ordered = T, levels = c("1-9", "10-49", "50-99", "100-499", "500-999", "1000+"))]
a[, device_count_group := factor(device_count_group, ordered = T, levels = c("1", "2", "3", "4-9", "10-19", "20+"))]
a[, Segment := factor(Segment, ordered = T, levels = c("SB", "MM", "ENT", "PUB"))]

# variables for linear model tab
pred_vars <- c("oem", "form_factor", "os.grep", "Segment", "industry", "continent", "G22Row", "born_on_year", "device_count", "physical_server_count", "Employees", "pc_count")

# pca columns
progs <- unlist(programs.list)
pcaData <- da[, ..progs]
# Market basket association rules data cleaning
arules.data <- da[,c("uuid", progs), with = F]
arules.data <- arules.data[, lapply(.SD, sum), by = uuid, .SDcols = progs]
arules.data <- melt.data.table(arules.data, id.vars = "uuid")
arules.data <- arules.data[value > 0,]
arules.data$value <- NULL
arules.data <- dcast.data.table(arules.data, uuid ~ variable)
arules.data$uuid <- NULL
arules.data <- as.list(as.data.frame(t(arules.data)))
arules.transactions <- list(NULL)
for(i in 1:length(arules.data)) {arules.transactions[[i]] <- levels(arules.data[[i]])}
arules.transactions <- as(arules.transactions, "transactions")

# color palettes
jtaColors <- c("#ee5c42", "#ee7600", "#eead0e")
moreColors <- c("#8F8F8F", "#8F0B04", "#000000", "#A1A100", "#D1044C", "#ffffff")
spectral <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2", "#B15928", "#666666", "#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2", "#B15928", "#666666")
colorz <- c("#009999", "#99ffe6", "#0069cc", "#1a90ff", "#b3daff", "#008060", "#00e6ac", "#1aff66", "#004280")
redGradient <- c("#cc0000", "#e60000", "#ff0000", "#ff1a1a", "#ff3333", "#ff4d4d", "#ff6666", "#ff8080", "#ff9999", "#ffb3b3")
#^data clean ----
# ui start ----
shinyUI(dashboardPage(
  dashboardHeader(title = "Microsoft Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bar Plot", tabName = "bars", icon = icon("bar-chart-o")),
      menuItem("LM Analysis", tabName = "lm", icon = icon("line-chart")),
      menuItem("PCA Clustering", tabName = "pca", icon = icon("braille")),
      menuItem("Association Rules", icon = icon("arrow-right"),
               menuSubItem("Association Rules", tabName = "arules", icon = icon("bar-chart")),
               menuSubItem("Association Rules Graphic", tabName = "arules2", icon = icon("ravelry"))), 
      menuItem("LDA", tabName = "lda", icon = icon("arrows-h"))
    )),
  
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tabItems(
      # tab 1 Bars ---- 
      tabItem(tabName = "bars",
              fluidRow(box(title = "Exploring the data", status = "danger", width = 12, collapsible = T, solidHeader = T, 
                           htmlOutput("html.barplot"))),
              fluidRow(
                box(title = "Pick x Variable", status = "danger", width = 6, collapsible = T,
                    submitButton("update", icon("refresh")),
                    selectInput(inputId = "x3", label = NULL, choices = c("# Devices at observation" = "device_count_group", "# of PC's total" = "pcs", "# Employees" = "Employees", "# Servers" = "physical_server_group", "Year of Computer" = "born_on_year_group", "Computer Brand" = "oem", "Operating System" = "os.grep", "Segment", "Industry" = "industry", "Continent" = "continent", "Company Email No/Yes" = "email", "Programs: Cloud Storage" = "cloud.storage", "Programs: Music" = "music", "Programs: Office" = "office", "Programs: Video Chat" = "video.chat", "Programs: Web Browsers" = "web.browser", "Programs: Other" = "other"), selected = c("Segment"))
                ),
                box(title = "Pick y Variable", status = "danger", width = 6, collapsible = T,
                    selectInput(inputId = "y3", label = NULL, choices = c("# Devices at observation" = "device_count_group", "# of PC's total" = "pcs", "# Employees" = "Employees", "# Servers" = "physical_server_group", "Year of Computer" = "born_on_year_group", "Computer Brand" = "oem", "Operating System" = "os.grep", "Segment", "Industry" = "industry", "Continent" = "continent", "Company Email No/Yes" = "email", "Programs: Cloud Storage" = "cloud.storage", "Programs: Music" = "music", "Programs: Office" = "office", "Programs: Video Chat" = "video.chat", "Programs: Web Browsers" = "web.browser", "Programs: Other" = "other"), selected = c("web.browser"))
                )),
              fluidRow(
                box(title = "Plot", status = "danger", width = 12, collapsible = T,
                    plotOutput(outputId = "bars"))
              ),
              fluidRow(
                box(title = "Table", status = "danger", width = 12, collapsible = T,
                    DT::dataTableOutput("plotTable")))
      ),
      # tab 2: lm ----
      tabItem(tabName = "lm",
              fluidRow(box(title = "Linear Model to predict the percentage of Microsoft programs", width = 12, solidHeader = T, collapsible = T, status = "danger", "Build your own linear model.  Choose which variables you would like to use to predict the percentage of microsoft programs on a computer.  On the bottom left hand side, you can see a graphical representation of how the variables affect the number of Microsoft programs.  The table on the bottom right hand side shows the coefficients of each variable.  A large positive value means this variable has a positive impact on the percentage of Microsoft programs while a negative value means the opposite.")),
              fluidRow(box(title = "Select Variables for LM", width = 12, solidHeader = F, status = "danger", collapsible = T,
                           submitButton("update", icon("refresh")),
                           checkboxGroupInput(inputId = "checkboxLm", label = "Select Variables", inline = T, choices = c(pred_vars), selected = c("Segment", "industry", "continent")))),
              fluidRow(box(title = "Graph", width = 8, solidHeader = F, status = "danger", collapsible = T, plotOutput("plotFmodel")),
                       box(title = "Variable Influence", width = 4, solidHeader = F, status = "danger", collapsible = T, "If an observation contains this variable, we expect the percentage of microsoft programs to change by this amount." ,DT::dataTableOutput("tableLm")))),
      
      # tab 3: pca ----
      tabItem(tabName = "pca",
              fluidRow(box(title = "Principle Component Analysis", width = 12, solidHeader = T, status = "danger", collapsible = T,
                           htmlOutput("pca.html")
              )),
              fluidRow(box(title = "Algorithm", width = 6, solidHeader = F, status = "danger", collapsible = T,
                           submitButton("update", icon("refresh")),
                           selectInput("pcaAlgorithm", label = NULL, choices = c("K-Means" = "kmeans", "Hierarchical Clustering" = "hclust"), selected = "K-Means"),
                           radioButtons("pcaScaled", label = "Scale Data with z-score?", choices = c("yes", "no"), selected = "no"))
                       ,
                       box(title = "More Options", width = 6, solidHeader = F, status = "danger", collapsible = T,
                           sliderInput(inputId = "pcaNumPcs", label = "Number of principle components to include in algorithm", min = 3, max = 10, value = 5),
                           sliderInput(inputId = "pcaNumClusters", label = "Number of clusters", min = 1, max = 10, value = 3))),
              fluidRow(box(title = "2D Plot", width = 6, solidHeader = F, status = "danger", collapsible = T,
                           plotOutput("cluster_plot")),
                       box(title = "3D Plot", width = 6, solidHeader = F, status = "danger", collapsible = T,
                           rglwidgetOutput("pca_3d_plot"))),
              fluidRow(box(title = NULL, width = 12, status = "danger", collapsible = T, DT::dataTableOutput("cluster_table")))
      ),
      # tab 4: arules ----
      tabItem(tabName = "arules",
              fluidRow(box(title = "Association Rules", width = 12, solidHeader = T, status = "danger", collapsible = T, htmlOutput("arules_text"))),
              fluidRow(box(title = "Inputs", width = 4, solidHeader = F, status = "danger", collapsible = T,
                           submitButton("update", icon("refresh")),
                           sliderInput("slider_support", label = "Minimum Support Level", min = .00001, max = .9, value = .0001),
                           sliderInput("slider_top_items", label = "Number of Programs for chart", min = 1, max = 33, value = 5)),
                       box(title = "Most popular programs", width = 8, solidHeader = F, status = "danger", collapsible = T,
                           plotOutput("arulesPlot"))),
              fluidRow(box(title = "Market Basket Results", width = 12, solidHeader = F, status = "danger", collapsible = T,
                           DT::dataTableOutput("mkt_basket_table")))
      ),
      # Tab 4.5: arules2 ----
      tabItem(tabName = "arules2",
              fluidRow(box(title = "Association Rules Visual Representation", width = 12, solidHeader = T, status = "danger", collapsible = T, "Here is a visual representation of the association rules analysis that we ran.  By clicking on a program, the associated programs are highlighted.  Meaning that these programs are likely to be used with the program selected.  You can also choose to adjust the minimum support level for the variables in the graphic and choose to filter by type of program.")),
              fluidRow(box(title = NULL, width = 12, solidHeader = F, status = "danger", collapsible = T,
                           submitButton("update", icon("refresh")),
                           sliderInput("slider_support2", label = "Minimum Support Level", min = .00001, max = .9, value = .19)
              )),
              fluidRow(box(title = "Association Rules Graphic", width = 12, solidHeader = F, status = "danger", collapsible = T,
                           visNetworkOutput(outputId = "apJelly", height = "750px")
              ))
      ),
      # tab 5: lda ----
      tabItem(tabName = "lda",
              fluidRow(box(title = "Linear Discriminant Analysis", width = 12, solidHeader = T, status = "danger", collapsible = T, "Here we use linear discriminant analysis to discover how each of these variables affect whether an organization has more Microsoft programs, or other programs.  LDA works by plotting all the points in a multidimentional plot, creating a line of best fit, and casting all the points onto that line.  This line is shown below")),
              fluidRow(
                box(title = "Select Variables for LDA", width = 12, solidHeader = F, status = "danger", collapsible = T,
                    submitButton("update", icon("refresh")),
                    checkboxGroupInput(inputId = "checkboxLda", label = "Select Variables", choices = c(pred_vars), selected = c(pred_vars), inline = T)
                )),
              fluidRow(
                box(title = "Output", width = 12, solidHeader = F, status = "danger", collapsible = T,
                    plotlyOutput(outputId = "ldaPlotly")
                ))
      )
      
      
    )
    
  )
))
# end of ui ----

# Things to fix


