# Description: R Shiny Project
# Author: Gary C. Lin

# Overview Map Panel
overviewPanel <- tabPanel(
  title = 'Overview',
  icon = icon('map'),
  br(),
  br(),
  br(),
  fluidRow(
    column(
      width = 2,
      br(),
      br(),
      br(),
      selectInput(
        'overviewYear',
        label = 'YEAR',
        choices = list('2020' = 2020, '2019' = 2019),
        selected = 2020
      ),
      selectInput(
        inputId = 'overviewVariable',
        label = 'YOY CHANGE IN OUTCOME',
        choices = c('ZHVI', 'ZORI'),
        selected = 'ZHVI'
      ),
      selectInput(
        inputId = 'overviewMetro',
        label = 'METRO',
        choices = c('All', unique(zcta$MetroName)),
        selected = 'All'
      )
    ),
    column(
      width = 10,
      h2(strong('Year-over-Year (YOY) Change in MSA Real Estate Prices')),
      h4('Annual averages of YOY changes in ZHVI and ZORI across metros.'),
      leafletOutput('overviewMap', width = '100%', height = 400),
      h5(
        em(
          'Source: Zillow.com.
          Zillow Home Value Index (ZHVI) is an estimate of local housing prices.
          Zillow Observed Rent Index (ZORI) is an estimate of local rental rates.
          ZORI is much less frequently reported.'
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 10,
      offset = 2,
      column(
        width = 2,
        h3(
          strong('No. Units:'),
          HTML('<br/>'),
          htmlOutput('numObs')
        )
      ),
      column(
        width = 2,
        h3(
          strong('Percent'),
          HTML('<br/>'),
          strong('Declined:'),
          HTML('<br/>'),
          htmlOutput('pctDeclined')
          )
      ),
      column(
        width = 2,
        h3(
          strong('Mean YOY'),
          HTML('<br/>'),
          strong('Change (%):'),
          HTML('<br/>'),
          htmlOutput('meanPctChange')
          )
      ),
      column(
        width = 2,
        h3(
          strong('Mean YOY'),
          HTML('<br/>'),
          strong('Change ($):'),
          HTML('<br/>'),
          htmlOutput('meanDollarChange')
        )
      ),
      column(
        width = 2,
        h3(
          strong('Top 5'),
          HTML('<br/>'),
          strong('Winners')
        ),
        h4(htmlOutput('overviewTop5List'))
      ),
      column(
        width = 2,
        h3(
          strong('Top 5'),
          HTML('<br/>'),
          strong('Losers')
        ),
        h4(htmlOutput('overviewBottom5List'))
      )
    )
  )
)

nationalPanel <- tabPanel(
  title = 'Trends',
  icon = icon('chart-line'),
  br(),
  br(),
  br(),
  fluidRow(
    column(
      width = 2,
      br(),
      br(),
      br(),
      selectInput(
        inputId = 'nationalVariable',
        label = 'HOUSING VARIABLE',
        choice = list(
          'ZHVI' = 'ZHVI',
          'ZORI' = 'ZORI',
          'Days on Market' = 'MedianDOZ',
          'Inventory for Sale' = 'InventoryForSale',
          'Sale Price' = 'MedianSalePrice',
          'Listing Price' = 'MedianListingPrice'
        ),
        selected = 'ZHVI'
      ),
      selectInput(
        inputId = 'nationalVariableType',
        label = 'VARIABLE TYPE',
        choices = list(
          'Units' = 'LVL_',
          'YOY Change (Units)' = 'DLVL_',
          'YOY Change (%)' = 'DPCT_'
        ),
        selected = 'LVL_'
      ),
      selectizeInput(
        inputId = 'nationalGroup',
        label = 'GROUP BY',
        choices = c('None', nhgisVariables),
        selected = 'None'
      )
    ),
    column(
      width = 10,
      h2(strong('Explore Macro Trends and Relationships in US Housing Market')),
      column(
        width = 6,
        h4('Select a grouping variable to observe the evolution of real estate prices 
            according to metro attributes.
            Hover over points for more details.'),
        br(),
        rbokehOutput('nationalLinePlot', width = '100%', height = 550),
        h5(
          em(
            'Because of missing values, some groups are not available for ZORI.
            Values are in log points. 
            Red line marks the onset of the COVID-19 pandemic 
              in the United States at the end of 2019.'
           )
        )
      ),
      column(
        width = 6,
        h4('Annual mean of the housing variable is plotted against the grouping variable.'),
        br(),
        br(),
        rbokehOutput('nationalScatterPlot', width = '100%', height = 550)
      )
    )
  )
)


# Bivariate Map
zctaBivariateMapPanel <- tabPanel(
  title = 'Dive Deep',
  icon = icon('city'),
  br(),
  br(),
  br(),
  fluidRow(
    column(
      br(),
      br(),
      br(),
      width = 2,
      selectizeInput(
        inputId = 'zctaBivariateMapMetro',
        label = 'METRO',
        choices = sort(unique(zcta$MetroName))
      ),
      selectizeInput(
        inputId = 'zctaBivariateMapVar1',
        label = 'VARIABLE 1',
        choices = nhgisVariables
      ),
      selectizeInput(
        inputId = 'zctaBivariateMapVar2',
        label = 'VARIABLE 2',
        choices = NULL
      )
    ),
    column(
      width = 10,
      h2(strong('Explore Spatial Relationships in Individual Metros')),
      column(
        width = 6,
        h4('Darker regions indicate higher values of the selected ZCTA attributes.'),
        br(),
        plotOutput('zctaBivariateMap', width = '100%', height = 550)
      ),
      column(
        width = 6,
        h4('Selected variables are plotted against distance to city center (km).
           City centers are approximated using the centroids of the densiest ZCTA.'),
        rbokehOutput('zctaScatter1', width = '100%', height = 275),
        br(),
        rbokehOutput('zctaScatter2', width = '100%', height = 275),
      )
    )
  ),
  fluidRow(
    column(
      width = 10,
      offset = 2,
      column(
        width = 3,
        h3(
          strong('Mean Variable 1'),
          HTML('<br/>'),
          htmlOutput('zctaVariable1')
        )
      ),
      column(
        width = 3,
        h3(
          strong('Mean Variable 2'),
          HTML('<br/>'),
          htmlOutput('zctaVariable2')
        )
      ),
      column(
        width = 4,
        h3(
          strong('Mean Distance to CBD'),
          HTML('<br/>'),
          htmlOutput('zctaDistCBD')
        )
      )
    )
  )
)

# Distance Gradient
zctaDistancePanel <- tabPanel(
  title = 'Suburbanization',
  icon = icon('car'),
  br(),
  br(),
  br(),
  fluidRow(
    column(
      width = 2,
      br(),
      br(),
      br(),
      selectizeInput(
        inputId = 'zctaGradientMetro',
        label = 'METRO',
        choices = NULL
      ),
      selectInput(
        inputId = 'zctaGradientVariable',
        label = 'OUTCOME',
        choices = c('ZHVI', 'ZORI'),
        selected = 'ZHVI'
      )
    ),
    column(
      width = 10,
      h2(strong('Explore How Real Estate Price Gradients Have Changed Since the Pandemic')),
      h4('City centers are approximated using the centroids of the densiest ZCTA.'),
      column(
        width = 9,
        imageOutput('zctaGradientGIF', width = '100%', height = 600),
      ),
      column(
        width = 3,
        h4('Animation shows the evolution of the relation between
           real estate prices and distance to city center during 2019 and 2020.'),
        br(),
        h3('"The Flattening"'),
        h4('Many metros experienced a "flattening" of price gradients.
            This effect can be seen in the
              rotation of the linear relationship between real estate prices and distance to CBD, 
              signaling an increase in demand for housing in the suburbs relative to city center.'),
        br(),
        h4('The flattening effect is stronger for rents than home values, suggesting
              that some of the intra-city reconfigurations may be temporary.')
      )
    )
  )
)

infoPanel <- tabPanel(
  title = 'About',
  icon = icon('user'),
  fluidRow(
    br(),
    br(),
    br(),
    column(
      width = 4,
      offset = 4,
      h2(strong('Welcome to my application!'),  style = 'text-align: center'),
      img(src = 'Lin_photo.png',  width = "100%", style = "display: block; margin-left: auto; margin-right: auto;"),
      h4('I am a postdoctoral fellow at the Johns Hopkins University
          studying cities. Recently, I have been investigating how the US housing markets
         responded to the COVID-19 pandemic and the acceleration of telework in the workplace.'),
      h3(
        tags$a(href = 'https://garycl.github.io',
               img(src = 'globe-solid.svg',
                   title = 'Website',
                   height = '50px')
        ),
        HTML('&nbsp;&nbsp;'),
        tags$a(href = 'https://www.linkedin.com/in/gary-lin-97aa001b0/',
               img(src = 'linkedin-in-brands.svg',
                   title = 'LinkedIn',
                   height = '50px')
        ),
        HTML('&nbsp;&nbsp;'),
        tags$a(href = 'https://twitter.com/Gary_C_Lin',
               img(src = 'twitter-brands.svg',
                   title = 'Twitter',
                   height = '50px')
        ),
        HTML('&nbsp;&nbsp;'),
        tags$a(href = 'https://github.com/garycl', 
               img(src = 'github-brands.svg',
                   title = 'Github',
                   height = '50px')
        ),
        style = "display: block; margin-left: auto; margin-right: auto; text-align: center"
      ),
      br(),
      div(
        h2(strong('About this Application')),
        h4('This application presents exploratory analysis of how the US housing market has
         changed since 2020. 
         To perform the analysis, I harmonize data from multiple sources, including Zillow.com
             and the American Community Surveys. More details can be found on the data page.'),
        h4('If you have questions or comments, please contact me.')
      )
    )
  )
)

# Page
fluidPage(
  includeCSS('www/flatly.css'),
  navbarPage(
    title = 'US HOUSING',
    id = 'nav',
    position = 'fixed-top',
    overviewPanel,
    nationalPanel,
    zctaBivariateMapPanel,
    zctaDistancePanel,
    infoPanel
  )
)
