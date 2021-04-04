# Desc: Shiny Project
# Author: Gary C. Lin
library(scales)
library(ggcorrplot)
library(rbokeh)
library(biscale)
library(cowplot)

function(input, output, session) {

  # Overview Leaflet Map
  palOverview = colorFactor(c("#56B4E9", "#E69F00"), domain = c("Yes", "No"))
  output$overviewMap = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.85, lat = 37.45, zoom = 3) %>%
      addLegend(
        "bottomright",
        values = c('No', 'Yes'),
        pal = palOverview,
        title = "YOY Decline",
        opacity = 1
      )
  })
  
  overviewData = reactive({
    if (input$overviewMetro != "All") {
      overviewData = zcta[Year == input$overviewYear & MetroName == input$overviewMetro]
    } else {
      overviewData = cbsa[Year == input$overviewYear]
      overviewData = overviewData[, ':=' (CentralLat = Latitude, CentralLon = Longitude)]
    }
    
    SDcols = c(
      'ID',
      'CentralLat',
      'CentralLon',
      'Latitude',
      'Longitude',
      paste0('DPCT_', input$overviewVariable),
      paste0('DLVL_', input$overviewVariable)
    )
    overviewData = overviewData[, .SD, .SDcols = SDcols]
    colnames(overviewData) = c('ID',
                               'CentralLat',
                               'CentralLon',
                               'Latitude',
                               'Longitude',
                               'DPCT_var',
                               'DLVL_var')
    overviewData = overviewData[,
                                lapply(.SD, mean, na.rm = T),
                                by = .(ID, Latitude, Longitude, CentralLat, CentralLon),
                                .SDcols = c('DPCT_var', 'DLVL_var')]
    overviewData = overviewData[, decline := ifelse(DPCT_var < 0, 'Yes', 'No')]
  })
  
  observe({
    leaflet = leafletProxy('overviewMap', data = overviewData())
    if (input$overviewMetro == "All") {
      leaflet %>%
        clearMarkers() %>%
        flyTo(
          'overviewData',
          lng = -98.85,
          lat = 37.45,
          zoom = 3
        )
    } else {
      leaflet %>%
        clearMarkers() %>%
        flyTo(
          'overviewData',
          lng = overviewData()$CentralLon[1],
          lat = overviewData()$CentralLat[1],
          zoom = 8
        )
    }
    
    label = paste0(
      '<strong>',
      overviewData()$ID,
      '</strong><br>',
      'Percent: ',
      format(
        round(overviewData()$DPCT_var * 100, 2),
        format = 'f',
        drop0trailing = T
      ),
      '<br>',
      'Dollar: ',
      format(
        round(overviewData()$DLVL_var, 0),
        format = 'f',
        big.mark = ","
      )
    )
    
    leaflet = leaflet %>%
      addCircleMarkers(
        lat =  ~ Latitude,
        lng =  ~ Longitude,
        radius = ~ ifelse(DPCT_var < 0, 20, 5),
        color = ~ palOverview(decline),
        weight = 3,
        fillOpacity = 0.7,
        label = lapply(label, HTML),
        labelOptions = labelOptions(textsize = "16px")
      )
  })
  
  output$numObs = renderUI({
    numObs = overviewData() %>%
      filter(!is.na(DPCT_var)) %>%
      summarize(n())
    
    if (!is.na(numObs)) {
      HTML(paste0(as.character(numObs)))
    } else {
      HTML('NA')
    }
  })
  
  output$pctDeclined = renderUI({
    pctDeclined = overviewData() %>%
      mutate(pctDeclined = (decline == 'Yes')) %>%
      summarize(mean(pctDeclined, na.rm = T))
    
    pctDeclined = round(pctDeclined * 100, 2)
    if (!is.na(pctDeclined)) {
      HTML(paste0(as.character(pctDeclined), '%'))
    } else {
      HTML('NA')
    }
  })
  
  output$meanPctChange = renderUI({
    meanPctChange = overviewData() %>%
      summarize(mean(DPCT_var, na.rm = T))
    meanPctChange = round(meanPctChange * 100, 2)
    
    if (!is.na(meanPctChange)) {
      HTML(paste0(as.character(meanPctChange)))
    } else {
      HTML('NA')
    }
  })
  
  output$meanDollarChange = renderUI({
    meanDollarChange = overviewData() %>%
      summarize(mean(DLVL_var, na.rm = T))
    meanDollarChange = format(round(meanDollarChange, 2), big.mark = ',')
    
    if (!is.na(meanDollarChange)) {
      HTML(paste0(as.character(meanDollarChange)))
    } else {
      HTML('NA')
    }
  })
  
  overviewTop5Data = reactive({
    overviewData() %>%
      filter(decline == 'No' & !is.na(decline)) %>%
      select(ID, DPCT_var) %>%
      arrange(desc(DPCT_var))
  })
  
  output$overviewTop5List = renderUI({
    if (nrow(overviewTop5Data()) > 0) {
      overviewTop5List = as.vector(overviewTop5Data()$ID[1:5])
      overviewTop5List = overviewTop5List[!is.na(overviewTop5List)]
      rank = 1:length(overviewTop5List)
      overviewTop5List = paste0(rank, '. ', as.character(overviewTop5List))
      content = paste(overviewTop5List, collapse = '<br>')
      HTML(content)
    } else {
      HTML('NA')
    }
  })
  
  overviewBottom5Data = reactive({
    overviewData() %>%
      filter(decline == 'Yes' & !is.na(decline)) %>%
      select(ID, DPCT_var) %>%
      arrange(DPCT_var)
  })
  
  output$overviewBottom5List = renderUI({
    if (nrow(overviewBottom5Data() > 0)) {
      overviewBottom5List = as.vector(overviewBottom5Data()$ID[1:5])
      overviewBottom5List = overviewBottom5List[!is.na(overviewBottom5List)]
      rank = 1:length(overviewBottom5List)
      overviewBottom5List = paste0(rank, '. ', as.character(overviewBottom5List))
      content = paste(overviewBottom5List, collapse = "<br>")
      HTML(content)
    } else {
      HTML('NA')
    }
  })
  
  # National Trends
  nationalData = reactive({
    Variable = paste0(input$nationalVariableType, input$nationalVariable)
    
    if (input$nationalGroup == 'None') {
      cols = c('ID', 'Date', Variable)
      nationalData = cbsa[, .SD, .SDcols = cols]
      colnames(nationalData) = c('ID', 'Date', 'Variable')
      nationalData[, ':=' (GroupVariable = 0, Group = 0)]
    } else {
      cols = c('ID', 'Date', Variable, input$nationalGroup)
      nationalData = cbsa[, .SD, .SDcols = cols]
      colnames(nationalData) = c('ID', 'Date', 'Variable', 'GroupVariable')
      q = quantile(nationalData$GroupVariable, 0.5, na.rm = T)
      nationalData = nationalData[, Group :=
                                    ifelse(GroupVariable <= q, 'Below Median', 'Above Median')]
    }
    
    nationalData = nationalData[!is.na(Variable)]
    nationalData = nationalData[, .SD, .SDcols = c('ID',
                                                   'Date',
                                                   'Variable',
                                                   'GroupVariable',
                                                   'Group')]
  })
  
  output$nationalLinePlot = renderRbokeh({
    p = nationalData() %>%
      group_by(Group, Date) %>%
      summarize(Variable = mean(Variable, na.rm = T)) %>%
      mutate(Variable = round(Variable, 4))
    
    figure = figure(ylab = 'Housing Variable',
                    legend = "top_left")
    
    if (max(nationalData()$Group) == min(nationalData()$Group)) {
      figure = figure %>%
        ly_points(
          x = Date,
          y = Variable,
          data = p,
          hover = list(Variable, Date)
        )
    } else {
      figure = figure %>%
        ly_points(
          x = Date,
          y = Variable,
          color = Group,
          data = p,
          hover = list(Variable, Date, Group)
        ) %>%
        ly_abline(
          v = as.Date('2019-12-31', '%Y-%m-%d'),
          width = 3,
          color = 'tomato'
        )
    }
    
    figure %>%
      x_axis(use_scientific = FALSE) %>%
      y_axis(use_scientific = FALSE) %>%
      theme_axis(
        c("x", "y"),
        axis_label_text_font_size = "14pt",
        major_label_text_font_size = "14pt"
      )
  })
  
  output$nationalScatterPlot = renderRbokeh({
    p = nationalData() %>%
      group_by(ID, Group, Year = as.factor(year(Date))) %>%
      summarize_at(c('Variable', 'GroupVariable'), mean, na.rm = T) %>%
      mutate_at(c('Variable', 'GroupVariable'), function(x)
        round(x, 4))
    
    figure = figure(xlab = 'Grouping Variable',
                    ylab = 'Housing Variable',
                    legend = "top_left")
    
    if (max(nationalData()$Group) == min(nationalData()$Group)) {
      figure = figure %>%
        ly_text(x = 0,
                text = 'Select a group variable.',
                align = 'center')
    } else {
      figure = figure %>%
        ly_points(
          x = GroupVariable,
          y = Variable,
          color = Group,
          data = p,
          hover = list(Variable, GroupVariable, Year)
        )
    }
    
    figure %>%
      x_axis(use_scientific = FALSE) %>%
      y_axis(use_scientific = FALSE) %>%
      theme_axis(
        c("x", "y"),
        axis_label_text_font_size = "14pt",
        major_label_text_font_size = "14pt"
      )
  })

  
  # Bivariate Spatial Distribution
  zctaBaseMap = reactive({
    zcta_map %>%
      filter(MetroName == input$zctaBivariateMapMetro)
  })
  

  observe({
    nhgisVariables2 = nhgisVariables[!nhgisVariables %in% c(input$zctaBivariateMapVar1)]
    updateSelectizeInput(inputId = 'zctaBivariateMapVar2', choices = nhgisVariables2)
    
  })
  
  
  zctaData = reactive({
    zctaData = zcta[Year == 2020 & MetroName == input$zctaBivariateMapMetro]
    SDcols = c(
      'ID',
      'DistCBD',
      input$zctaBivariateMapVar1,
      input$zctaBivariateMapVar2,
      'CentralLat',
      'CentralLon'
    )
    zctaData = zctaData[, .SD, .SDcols = SDcols]
    colnames(zctaData) = c('ID',
                           'DistCBD',
                           'Variable.1',
                           'Variable.2',
                           'CentralLat',
                           'CentralLon')
    SDcols = colnames(zctaData)[c(2, 3:6)]
    zctaData = zctaData[!is.na(Variable.1) & !is.na(Variable.2)]
    zctaData = zctaData[, lapply(.SD, mean, na.rm = T), by = 'ID', .SDcols = SDcols]
  })
  
  
  output$zctaBivariateMap = renderPlot({
    zctaCityCenter = zctaData() %>%
      select(CentralLat, CentralLon) %>%
      distinct()
    
    zctaBaseMap = zctaBaseMap() %>%
      rename(ID = geoid) %>%
      right_join(zctaData(), by = 'ID')
    
    zctaBivariateMapData = bi_class(
      zctaBaseMap,
      x = Variable.1,
      y = Variable.2,
      style = "quantile",
      dim = 2
    )
    
    map = ggplot() +
      geom_sf(data = zctaBaseMap(), fill = 'white') +
      geom_sf(
        data = zctaBivariateMapData,
        mapping = aes(fill = bi_class),
        size = 0.1,
        show.legend = FALSE
      ) +
      bi_scale_fill(pal = 'GrPink', dim = 2) +
      bi_theme()
    
    legend = bi_legend(
      pal = "GrPink",
      dim = 2,
      xlab = "Larger Variable 1",
      ylab = "Larger Variable 2",
      size = 14
    )
    
    ggdraw() +
      draw_plot(map, 0.1, 0, 1, 1) +
      draw_plot(legend, 0, 0, 0.3, 0.3)
  })
  
  # output$zctaDensity1 = renderRbokeh({
  #
  #   figure(
  #     width = 600,
  #     height = 400,
  #     xlab = '',
  #     ylab = '',
  #     legend = "top_left"
  #   ) %>%
  #     ly_hist(
  #       x = Variable.1,
  #       color = "#C85A5A",
  #       breaks = 40,
  #       data = zctaData()
  #     ) %>%
  #     ly_density(x = Variable.1, data = zctaData()) %>%
  #     theme_axis(
  #       c("x", "y"),
  #       axis_label_text_font_size = "14pt",
  #       major_label_text_font_size = "14pt"
  #     ) %>%
  #     theme_legend(label_text_font_size = "14pt")
  #
  # })
  #
  #
  # output$zctaDensity2 = renderRbokeh({
  #
  #   figure(
  #     width = 600,
  #     height = 400,
  #     xlab = '',
  #     ylab = '',
  #     legend = "top_left"
  #   ) %>%
  #     ly_hist(
  #       x = Variable.2,
  #       color = "#64ACBE",
  #       breaks = 40,
  #       data = zctaData()
  #     ) %>%
  #     ly_density(x = Variable.2, data = zctaData()) %>%
  #     theme_axis(
  #       c("x", "y"),
  #       axis_label_text_font_size = "14pt",
  #       major_label_text_font_size = "14pt"
  #     ) %>%
  #     theme_legend(label_text_font_size = "14pt")
  #
  # })
  
  output$zctaVariable1 = renderUI({
    zctaVariable1 = zctaData() %>%
      summarize(mean(Variable.1, na.rm = T))
    
    if (!is.na(zctaVariable1)) {
      HTML(as.character(round(zctaVariable1, 4)))
    } else {
      HTML('NA')
    }
  })
  
  
  output$zctaVariable2 = renderUI({
    zctaVariable2 = zctaData() %>%
      summarize(mean(Variable.2, na.rm = T))
    
    if (!is.na(zctaVariable2)) {
      HTML(as.character(round(zctaVariable2, 4)))
    } else {
      HTML('NA')
    }
  })
  
  output$zctaDistCBD = renderUI({
    zctaDistCBD = zctaData() %>%
      summarize(mean(DistCBD, na.rm = T))
    
    if (!is.na(zctaDistCBD)) {
      HTML(as.character(round(zctaDistCBD, 4)))
    } else {
      HTML('NA')
    }
  })
  
  output$zctaScatter1 = renderRbokeh({
    zctaData1 = zctaData() %>%
      mutate(
        Variable.1 = round(Variable.1, 4),
        DistCBD = round(DistCBD, 4)
      )
    
    figure(
      width = 600,
      height = 300,
      xlab = '',
      ylab = '',
      title = 'Distance to CBD (km) and Variable 1',
      legend = "top_right"
    ) %>%
      ly_points(
        x = DistCBD,
        y = Variable.1,
        color = "#C85A5A",
        hover = list(DistCBD, Variable.1),
        data = zctaData1
      ) %>%
      ly_lines(
        lowess(x = DistCBD,
               y = Variable.1,),
        color = "#C85A5A",
        width = 3,
        data = zctaData1
      ) %>%
      theme_axis(
        c("x", "y"),
        axis_label_text_font_size = "14pt",
        major_label_text_font_size = "14pt"
      ) %>%
      theme_legend(label_text_font_size = "14pt")
  })
  
  
  output$zctaScatter2 = renderRbokeh({
    zctaData2 = zctaData() %>%
      mutate(
        Variable.2 = round(Variable.2, 4),
        DistCBD = round(DistCBD, 4)
      )
    
    figure(
      width = 600,
      height = 300,
      xlab = '',
      ylab = '',
      title = 'Distance to CBD (km) and Variable 2',
      legend = "top_left"
    ) %>%
      ly_points(
        x = DistCBD,
        y = Variable.2,
        color = "#64ACBE",
        data = zctaData2,
        hover = list(DistCBD, Variable.2)
      ) %>%
      ly_lines(
        lowess(x = DistCBD,
               y = Variable.2,),
        width = 3,
        color = "#64ACBE",
        data = zctaData2
      ) %>%
      theme_axis(
        c("x", "y"),
        axis_label_text_font_size = "14pt",
        major_label_text_font_size = "14pt"
      ) %>%
      theme_legend(label_text_font_size = "14pt")
  })
  
  observeEvent(input$zctaGradientVariable, {
    path = paste0('www/Individual/', input$zctaGradientVariable)
    filenames = list.files(path, pattern = '.gif', all.files = TRUE)
    zctaGradientMetros = lapply(filenames, function(x)
      gsub('.gif', '', x))
    choice = unlist(zctaGradientMetros)
    updateSelectizeInput(inputId = 'zctaGradientMetro', choice = sort(choice))
    
  })
  
  output$zctaGradientGIF = renderImage({
    src = paste0(
      'www/Individual/',
      input$zctaGradientVariable,
      '/',
      input$zctaGradientMetro,
      '.gif'
    )
    
    list(src = src,
         contentType = 'image/gif',
         width = '100%')
  }, deleteFile = FALSE)
}
