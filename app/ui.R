# Description: R Shiny Project
# Author: Gary C. Lin

# Overview Map Panel
overviewPanel <- tabPanel(
  title = "Overview",
  icon = icon("map"),
  br(),
  br(),
  br(),
  fluidRow(
    column(
      width = 2,
      br(),
      selectInput(
        inputId = "overviewMetro",
        label = "METRO",
        choices = c("All", sort(unique(cbsa$MetroName))),
        selected = "All"
      ),
      selectInput(
        "overviewYear",
        label = "YEAR",
        choices = sort(seq(2015, 2020), decreasing = TRUE),
        selected = 2020
      ),
      selectInput(
        inputId = "overviewVariable",
        label = "YOY CHANGE IN",
        choice = c("ZHVI", "ZORI"),
        selected = "ZHVI"
      ),
      wellPanel(
        h5(strong("Real Estate Prices")),
        p(
          em("Zillow Home Value Index (ZHVI): "),
          "local housing price for typical homes",
          style = "text-align: justify;"
        ),
        p(
          em("Zillow Observed Rent Index (ZORI): "),
          "local rental rates for typical homes",
          style = "text-align: justify;"
        ),
        p("Not all variables are available for all metros.", style = "text-align: justify;"),
        p(em("Source: Zillow.com."))
      )
    ),
    column(
      width = 10,
      fluidRow(
        h2(strong("Year-over-Year (YOY) Change in MSA Real Estate Prices")),
        h4("Average YOY changes in real esate prices across 100 largest metros.
           Select a metro to explore the within-metro variation in real estate price changes across
           ZIP Code Tabulation Areas (ZCTAs).", style = "text-align: justify;"),
        leafletOutput("overviewMap", width = "100%", height = 450)
      ),
      fluidRow(
        column(
          width = 12,
          column(
            width = 2,
            h3(
              strong("No. Units:"),
              HTML("<br/>"),
              htmlOutput("numObs")
            )
          ),
          column(
            width = 2,
            h3(
              strong("Percent"),
              HTML("<br/>"),
              strong("Declined:"),
              HTML("<br/>"),
              htmlOutput("pctDeclined")
            )
          ),
          column(
            width = 2,
            h3(
              strong("Avg. YOY"),
              HTML("<br/>"),
              strong("Change (%):"),
              HTML("<br/>"),
              htmlOutput("meanPctChange")
            )
          ),
          column(
            width = 2,
            h3(
              strong("Avg. YOY"),
              HTML("<br/>"),
              strong("Change ($):"),
              HTML("<br/>"),
              htmlOutput("meanDollarChange")
            )
          ),
          column(
            width = 2,
            h3(
              strong("Top 5"),
              HTML("<br/>"),
              strong("Winners")
            ),
            h4(htmlOutput("overviewTop5List"))
          ),
          column(
            width = 2,
            h3(
              strong("Top 5"),
              HTML("<br/>"),
              strong("Losers")
            ),
            h4(htmlOutput("overviewBottom5List"))
          )
        )
      )
    )
  )
)


# National Trends Panel
nationalPanel <- tabPanel(
  title = "Trends",
  icon = icon("chart-line"),
  br(),
  br(),
  br(),
  fluidRow(
    column(
      width = 2,
      br(),
      sliderInput(
        "nationalDateRange",
        label = "DATE RANGE",
        min = as.Date("2015-01-31", "%Y-%m-%d"),
        max = as.Date("2020-12-31", "%Y-%m-%d"),
        value = c(as.Date("2015-01-31", "%Y-%m-%d"), as.Date("2020-12-31", "%Y-%m-%d")),
        timeFormat = "%b %Y"
      ),
      selectInput(
        inputId = "nationalVariable",
        label = "HOUSING VARIABLE",
        choice = list(
          "ZHVI" = "ZHVI",
          "ZORI" = "ZORI",
          "Listing Price" = "MedianListingPrice",
          "Sale Price" = "MedianSalePrice",
          "Days to Pending" = "MedianDOZ",
          "For-Sale Inventory" = "InventoryForSale"
        ),
        selected = "ZHVI"
      ),
      selectInput(
        inputId = "nationalVariableType",
        label = "VARIABLE TYPE",
        choices = list(
          "Units" = "LVL_",
          "YOY Change (Units)" = "DLVL_",
          "YOY Change (%)" = "DPCT_"
        ),
        selected = "LVL_"
      ),
      selectInput(
        inputId = "nationalGroup",
        label = "GROUPING VARIABLE",
        choices = c("None", nhgisVariables),
        selected = "None"
      ),
      wellPanel(
        h5(strong("Housing Variables")),
        p(
          em("Zillow Home Value Index (ZHVI): "),
          "local housing price for typical homes",
          style = "text-align: justify;"
        ),
        p(
          em("Zillow Observed Rent Index (ZORI): "),
          "local rental rates for typical homes",
          style = "text-align: justify;"
        ),
        p(
          em("Listing Price: "),
          "median listing price",
          style = "text-align: justify;"
        ),
        p(
          em("Sale Price: "),
          "median sale price",
          style = "text-align: justify;"
        ),
        p(
          em("For-Sale Inventory: "),
          "unique active listings in a month",
          style = "text-align: justify;"
        ),
        p(
          em("Days to Pending: "),
          "days to pending status after first being shown for sale",
          style = "text-align: justify;"
        ),
        p("Not all variables are available for all metros."),
        p(em("Source: Zillow.com."))
      )
    ),
    column(
      width = 10,
      h2(strong("Explore Macro Trends and Relationships in US Housing Market")),
      column(
        width = 6,
        h4("Select a grouping variable to observe the evolution of real estate prices
              according to metro attributes.
           Red line marks the onset of the COVID-19 pandemic
              in the United States at the end of 2019.
           Hover over points for more details.", style = "text-align: justify;"),
        br(),
        rbokehOutput("nationalLinePlot", width = "100%", height = 600)
      ),
      column(
        width = 6,
        h4(
          "Relationship between average housing variable and grouping variable.",
          style = "text-align: justify;"
        ),
        br(),
        br(),
        rbokehOutput("nationalScatterPlot", width = "100%", height = 600)
      )
    )
  )
)


# Dive Deep (Bivariate Map) Panel
zctaBivariateMapPanel <- tabPanel(
  title = "Dive Deep",
  icon = icon("city"),
  br(),
  br(),
  br(),
  column(
    width = 2,
    br(),
    selectInput(
      inputId = "zctaBivariateMapMetro",
      label = "METRO",
      choices = sort(unique(cbsa$MetroName)),
      selected = "New York"
    ),
    selectInput(
      inputId = "zctaBivariateMapVar1",
      label = "VARIABLE 1",
      choices = nhgisVariables
    ),
    selectInput(
      inputId = "zctaBivariateMapVar2",
      label = "VARIABLE 2",
      choices = NULL
    ),
    sliderInput(
      "zctaBivariateMapDistCBD",
      label = "DISTANCE TO CITY CENTER",
      min = 0,
      max = 300,
      value = 100
    ),
    wellPanel(
      p(
        "Vary the distance slider to see how the spatial relationship between the attributes change.
      Estimates are based on the 2015-2019 American Community Survey.
      City centers are approximated using the centroids of the densiest ZCTA.",
        style = "text-align: left;"
      ),
      p(em("Source: IPUMS NHGIS, University of Minnesota,  www.nhgis.org."))
    )
  ),
  column(
    width = 10,
    fluidRow(
      h2(strong("Explore Spatial Relationships in Individual Metros")),
      column(
        width = 6,
        h4(
          "Darker regions indicate higher values of the selected ZCTA attributes.",
          style = "text-align: justify;"
        ),
        br(),
        plotOutput("zctaBivariateMap", width = "100%", height = 550)
      ),
      column(
        width = 6,
        h4("Selected variables are plotted against distance to city center (km).",
          style = "text-align: justify;"
        ),
        rbokehOutput("zctaScatter1", width = "100%", height = 275),
        br(),
        rbokehOutput("zctaScatter2", width = "100%", height = 275)
      )
    ),
    fluidRow(
      column(
        width = 4,
        h3(
          strong("Mean Variable 1"),
          HTML("<br/>"),
          htmlOutput("zctaVariable1")
        )
      ),
      column(
        width = 4,
        h3(
          strong("Mean Variable 2"),
          HTML("<br/>"),
          htmlOutput("zctaVariable2")
        )
      ),
      column(
        width = 4,
        h3(
          strong("Mean Distance to CBD"),
          HTML("<br/>"),
          htmlOutput("zctaDistCBD")
        )
      )
    )
  )
)


# Distance Gradient Panel
zctaDistancePanel <- tabPanel(
  title = "Suburbanization",
  icon = icon("car"),
  br(),
  br(),
  br(),
  fluidRow(
    column(
      width = 2,
      br(),
      selectInput(
        inputId = "zctaGradientMetro",
        label = "METRO",
        choices = sort(unique(cbsa[PopRank <= 30]$MetroName)),
        selected = "New York"
      ),
      selectInput(
        inputId = "zctaGradientVariable",
        label = "OUTCOME",
        choices = c("ZHVI", "ZORI"),
        selected = "ZHVI"
      ),
      wellPanel(
        h4(strong("The Flattening")),
        p('Many metros experienced a "flattening" of price gradients, where real estate prices
            are expressed as a function of distance to the city center.
          City centers are approximated using the centroids of the densiest ZCTA in a given metro.
          The flattening can be seen from the rotation of the price gradients,
            signaling an increase in demand for housing in the suburbs relative to the city center.',
          style = "text-align: justify;"
        ),
        br(),
        p("The flattening effect is stronger for rents than home values, suggesting
              that some of the intra-city reconfigurations may be temporary.",
          style = "text-align: justify;"
        )
      )
    ),
    column(
      width = 10,
      h2(strong("Explore How Real Estate Price Gradients Have Changed Since the Pandemic")),
      h4("Animation shows the evolution of the relationship between
           real estate prices and distance to city center among 30 largest
           metro areas during 2019 and 2020.",
        style = "text-align: justify;"
      ),
      imageOutput("zctaGradientGIF", width = "100%")
    )
  )
)


# Info Panel
infoPanel <- tabPanel(
  title = "About",
  icon = icon("user"),
  fluidRow(
    br(),
    br(),
    br(),
    column(
      width = 6,
      offset = 3,
      h2(strong("Welcome to my application!"), style = "text-align: center;"),
      img(src = "Lin_photo.png", width = "100%", style = "display: block; margin-left: auto; margin-right: auto;"),
      div(
        h4("I am a postdoctoral fellow at the Johns Hopkins University
            studying cities. Recently, I have been investigating how the US housing markets
           responded to the COVID-19 pandemic and the acceleration of telework in the workplace.",
          style = "text-align: justify"
        ),
        h4("This application presents exploratory analysis of how the US housing market has
           changed since 2015, with a focus on the last two years.
           To perform the analysis, I harmonize data from multiple sources.
           Housing variables, including real estate prices, for-sale inventory,
            and days to pending come from Zillow.
           Demographic and economic variables come from the 2015-2019 American Community Survey.",
          style = "text-align: justify"
        ),
        h4("If you have questions, comments, or suggestions for improvements, please contact me.",
          style = "text-align: justify"
        )
      ),
      h3(
        tags$a(
          href = "https://garycl.github.io",
          img(src = "globe-solid.svg", title = "Website", height = "50px")
        ),
        HTML("&nbsp;&nbsp;"),
        tags$a(
          href = "https://www.linkedin.com/in/gary-lin-97aa001b0/",
          img(src = "linkedin-in-brands.svg", title = "LinkedIn", height = "50px")
        ),
        HTML("&nbsp;&nbsp;"),
        tags$a(
          href = "https://twitter.com/Gary_C_Lin",
          img(src = "twitter-brands.svg", title = "Twitter", height = "50px")
        ),
        HTML("&nbsp;&nbsp;"),
        tags$a(
          href = "https://github.com/garycl",
          img(src = "github-brands.svg", title = "Github", height = "50px")
        ),
        style = "display: block; margin-left: auto; margin-right: auto; text-align: center;"
      )
    )
  )
)


# Combining Everything
fluidPage(
  includeCSS("www/flatly.css"),
  navbarPage(
    title = "US HOUSING",
    id = "nav",
    position = "fixed-top",
    overviewPanel,
    nationalPanel,
    zctaBivariateMapPanel,
    zctaDistancePanel,
    infoPanel
  )
)
