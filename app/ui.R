# Description: R Shiny Project
# Author: Gary C. Lin

# Overview Map Panel
overviewPanel <- tabPanel(
  title = "Overview",
  icon = icon("map"),
  tags$br(),
  tags$br(),
  tags$br(),
  fluidRow(
    column(
      width = 2,
      tags$br(),
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
        tags$p(
          tags$strong("Zillow Home Value Index (ZHVI): "),
          tags$br(),
          "value of the typical homes in a given region.",
          style = "text-align: justify;"
        ),
        tags$p(
          tags$strong("Zillow Observed Rent Index (ZORI): "),
          tags$br(),
          "rental rates of the typical homes in a given region.",
          style = "text-align: justify;"
        ),
        tags$p("Not all variables are available for all metros."),
        tags$p(em("Source: Zillow.com."))
      )
    ),
    column(
      width = 10,
      fluidRow(
        tags$h2(tags$strong("Year-over-Year (YOY) Change in MSA Real Estate Prices")),
        tags$h4("Average YOY changes in real esate prices across the 91 largest metros.
           Select a metro to explore how real estate prices have changed across
           ZIP Code Tabulation Areas (ZCTAs) within a given metro.", style = "text-align: justify;"),
        leafletOutput("overviewMap", width = "100%", height = 450)
      ),
      fluidRow(
        column(
          width = 12,
          column(
            width = 2,
            tags$h3(
              tags$strong("No. Units:"),
              HTML("<br/>"),
              htmlOutput("numObs")
            )
          ),
          column(
            width = 2,
            tags$h3(
              tags$strong("Percent"),
              HTML("<br/>"),
              tags$strong("Declined:"),
              HTML("<br/>"),
              htmlOutput("pctDeclined")
            )
          ),
          column(
            width = 2,
            tags$h3(
              tags$strong("Avg. YOY"),
              HTML("<br/>"),
              tags$strong("Change (%):"),
              HTML("<br/>"),
              htmlOutput("meanPctChange")
            )
          ),
          column(
            width = 2,
            tags$h3(
              tags$strong("Avg. YOY"),
              HTML("<br/>"),
              tags$strong("Change ($):"),
              HTML("<br/>"),
              htmlOutput("meanDollarChange")
            )
          ),
          column(
            width = 2,
            tags$h3(
              tags$strong("Top 5"),
              HTML("<br/>"),
              tags$strong("Winners")
            ),
            tags$h4(htmlOutput("overviewTop5List"))
          ),
          column(
            width = 2,
            tags$h3(
              tags$strong("Top 5"),
              HTML("<br/>"),
              tags$strong("Losers")
            ),
            tags$h4(htmlOutput("overviewBottom5List"))
          )
        )
      )
    )
  )
)


# National Trends Panel
nationalPanel <- tabPanel(
  title = "Macro Trends",
  icon = icon("chart-line"),
  tags$br(),
  tags$br(),
  tags$br(),
  fluidRow(
    column(
      width = 2,
      tags$br(),
      sliderInput(
        "nationalDateRange",
        label = "DATE RANGE",
        min = as.Date("2015-01-31", "%Y-%m-%d"),
        max = as.Date("2020-12-31", "%Y-%m-%d"),
        value = c(
          as.Date("2015-01-31", "%Y-%m-%d"),
          as.Date("2020-12-31", "%Y-%m-%d")
        ),
        timeFormat = "%b %Y"
      ),
      selectInput(
        inputId = "nationalVariable",
        label = "HOUSING VARIABLE",
        choice = list(
          "ZHVI" = "ZHVI",
          "ZORI" = "ZORI",
          "ZHVI-ZORI Ratio" = "PriceRentRatio"
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
        tags$p(
          tags$strong("Zillow Home Value Index (ZHVI): "),
          tags$br(),
          "national average value of the typical homes in the 91 largest metro areas.",
          style = "text-align: justify;"
        ),
        tags$p(
          tags$strong("Zillow Observed Rent Index (ZORI): "),
          tags$br(),
          "national average rental rates of the typical homes in the 91 largest metro areas.",
          style = "text-align: justify;"
        ),
        tags$p(
          tags$strong("ZHVI-ZORI (Price-Rent) Ratio: "),
          tags$br(),
          "the ratio of ZHVI to ZORI provides an estimate of expected future home price appreciation.",
          style = "text-align: justify;"
        ),
        tags$p("Not all variables are available for all metros."),
        tags$p(em("Source: Zillow.com."))
      )
    ),
    column(
      width = 10,
      tags$h2(tags$strong("Explore Macro Trends and Relationships in US Housing Market")),
      fluidRow(
        column(
          width = 6,
          tags$h4("Select a grouping variable to observe how real estate prices having evolved
              according to metro attributes.
           Metro groups are created according to whether a metro's attribute is above or
              below the sample median of that attribute.
           The red line marks the onset of the COVID-19 pandemic
              in the United States, which began at the end of 2019.",
            style = "text-align: justify;"
          )
        ),
        column(
          width = 6,
          tags$h4(
            "Select a grouping variable to explore the cross-sectional relationship between
                average real restate prices and metro attributes.
            LOWESS line is shown.
            Hover over points for more details.",
            style = "text-align: justify;"
          )
        )
      ),
      fluidRow(
        column(
          width = 6,
          tags$br(),
          rbokehOutput("nationalLinePlot", width = "100%", height = 600)
        ),
        column(
          width = 6,
          tags$br(),
          rbokehOutput("nationalScatterPlot", width = "100%", height = 600)
        )
      ),
    )
  )
)


# Metro Congfigurations (Bivariate Map) Panel
zctaBivariateMapPanel <- tabPanel(
  title = "Monocentric Cities",
  icon = icon("city"),
  tags$br(),
  tags$br(),
  tags$br(),
  column(
    width = 2,
    tags$br(),
    selectInput(
      inputId = "zctaBivariateMapMetro",
      label = "METRO",
      choices = sort(unique(cbsa$MetroName)),
      selected = "New York, NY"
    ),
    selectInput(
      inputId = "zctaBivariateMapVar1",
      label = "VARIABLE 1",
      choices = nhgisVariables,
      selected = "CollegeGrad"
    ),
    selectInput(
      inputId = "zctaBivariateMapVar2",
      label = "VARIABLE 2",
      choices = nhgisVariables,
      selected = "PopDensity"
    ),
    sliderInput(
      "zctaBivariateMapDistCBD",
      label = "DISTANCE TO CITY CENTER (KM)",
      min = 0,
      max = 300,
      value = 100
    ),
    wellPanel(
      tags$p(
        "Vary the distance slider to investigate how the spatial relationship
        between the ZCTA attributes change in relation to distance to the city center.",
        style = "text-align: left;"
      ),
      tags$p("ZCTA attributes estimates come from the 2015-2019 American Community Survey."),
      tags$p(em("Source: IPUMS NHGIS, University of Minnesota,  www.nhgis.org."))
    )
  ),
  column(
    width = 10,
    fluidRow(
      tags$h2(tags$strong("Explore Spatial Relationships in Individual Metros")),
      tags$h4(
        HTML(paste(
          "In monocentric models, where people commute to city centers to work,
          metros' spatial configurations can inform us of how local real estate prices may change as
          more workers engage in telework.",
          "A key measure of the model is the distance between place of residence and the city center.",
          "City centers are shown as", tags$span(style = "color:orange", "orange dots"),
          "and are approximated using the Google Maps API."
        ), sep = " ")
      ),
      column(
        width = 6,
        tags$h4(
          "Darker regions indicate higher values of the selected ZCTA attributes.",
          style = "text-align: justify;"
        ),
        tags$br(),
        plotOutput("zctaBivariateMap", width = "100%", height = 550)
      ),
      column(
        width = 6,
        tags$h4("Selected attributes are plotted against distance to city center (km).",
          style = "text-align: justify;"
        ),
        rbokehOutput("zctaScatter1", width = "100%", height = 275),
        tags$br(),
        rbokehOutput("zctaScatter2", width = "100%", height = 275)
      )
    ),
    fluidRow(
      column(
        width = 4,
        tags$h3(
          tags$strong("Avg. Variable 1"),
          HTML("<br/>"),
          htmlOutput("zctaVariable1")
        )
      ),
      column(
        width = 4,
        tags$h3(
          tags$strong("Avg. Variable 2"),
          HTML("<br/>"),
          htmlOutput("zctaVariable2")
        )
      ),
      column(
        width = 4,
        tags$h3(
          tags$strong("Avg. Distance (km)"),
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
  tags$br(),
  tags$br(),
  tags$br(),
  fluidRow(
    column(
      width = 2,
      tags$br(),
      selectInput(
        inputId = "zctaGradientMetro",
        label = "METRO",
        choices = sort(unique(cbsa[PopRank <= 30]$MetroName)),
        selected = "New York, NY"
      ),
      selectInput(
        inputId = "zctaGradientVariable",
        label = "HOUSING VARIABLE",
        choices = list("ZHVI", "ZORI", "ZHVI-ZORI Ratio" = "ZHVI-ZORI"),
        selected = "ZORI"
      ),
      wellPanel(
        tags$h4(tags$strong("The Flattening")),
        tags$p('Many metros experienced a "flattening" of real estate price gradients, which express
            real estate prices as a function of distance to the city center.
          The flattening can be seen from the rotation of the price gradients,
            signaling an increase in demand for housing in the suburbs relative to the city center.',
          style = "text-align: justify;"
        ),
        tags$br(),
        tags$p("The flattening effect is stronger for rents than home values, suggesting
              that some of the within-metro population movements may be temporary.",
          style = "text-align: justify;"
        ),
        tags$br(),
        tags$p("Animation is shown for the 30 largest metros
            as smaller metros contain more missing ZORI values.
          City centers are approximated using the Google Maps API.",
          style = "text-align: justify;"
        )
      )
    ),
    column(
      width = 10,
      tags$h2(tags$strong("Explore How Real Estate Price Gradients Have Changed Since the Pandemic")),
      tags$h4("Animation shows the evolution of the relationship between
           real estate prices and distance to the city center bewteen 2019 and 2020.",
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
    tags$br(),
    tags$br(),
    tags$br(),
    column(
      width = 6,
      offset = 3,
      tags$h2(tags$strong("Welcome to my application!"), style = "text-align: center;"),
      img(src = "Lin_photo.png", width = "70%", style = "display: block; margin-left: auto; margin-right: auto;"),
      tags$div(
        tags$h4("I am a postdoctoral fellow at the Johns Hopkins University
            studying cities. Recently, I have been investigating how the US housing markets
           responded to the COVID-19 pandemic and the acceleration of telework in the workplace.",
          style = "text-align: justify"
        ),
        tags$h4("This application presents exploratory analysis of how the US housing market has
           changed since 2015, with a focus on the last two years.
           To perform the analysis, I harmonize data from multiple sources.
           Housing variables, including real estate prices, come from Zillow.
           Demographic and economic variables come from the 2015-2019 American Community Survey.",
          style = "text-align: justify"
        ),
        tags$h4("If you have questions, comments, or suggestions for improvements, please contact me.",
          style = "text-align: justify"
        )
      ),
      tags$h3(
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
