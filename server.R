server <- function(input, output) {
  source('global.R')
  values <- reactiveValues(confirmed = get_data(url[1], pop, 'confirmed'),
                           deaths = get_data(url[2], pop, 'deaths'))
  
  #### PREPARING DATA ####
  # Calculate rolling averages and re-name columns
  wide_data <- reactive({
    conf_roll <- roll_avg(values$confirmed, 'confirmed', input$roll_num) %>%
      dplyr::select(country_region, date, confirmed, 
                    new_conf = new,
                    X2020,
                    roll_conf = roll,
                    roll_percap_conf = roll_percap,
                    percap_conf = percap)
    death_roll <- roll_avg(values$deaths, 'deaths', input$roll_num) %>%
      dplyr::select(country_region, date, deaths,
                    new_death = new,
                    X2020,
                    roll_death = roll,
                    roll_percap_death = roll_percap,
                    percap_death = percap)
    wide_data <- dplyr::full_join(conf_roll, death_roll, 
                                  by = c('country_region', 'date', 'X2020'))
    return(wide_data)
  })
  
  # Create 10, 100, and 1000 deaths markers
  death_markers <- reactive({
    death_10 <- get_death_markers(values$deaths, 10, 'X')
    death_100 <- get_death_markers(values$deaths, 100, 'C')
    death_1000 <- get_death_markers(values$deaths, 1000, 'M')
    death_10000 <- get_death_markers(values$deaths, 10000, 'x̄')
    death_markers <- dplyr::bind_rows(death_10, death_100, death_1000, death_10000)
    return(death_markers)
  })
  
  # Create interventions list
  ints <- reactive({
    ints <- interventions %>%
      # Filter only interventions and categories to be included
      dplyr::filter(include & int %in% input$categories) %>% 
      dplyr::arrange(country_region, date) %>%
      dplyr::mutate(legend = toupper(stringr::str_extract(int, '(^[a-z]{1})')),
                    legend_l = c(letters, LETTERS)[seq( from = 1, to = nrow(.) )]) %>%
      # Put columns in the right order
      dplyr::select(country_region, date, measure, int, legend_l, legend, include)
    return(ints)
  })
  
  # Add markers (interventions & deaths) to data
  wide_rel <- reactive({
    wide_relevant <- wide_data() %>%
      dplyr::filter(country_region %in% c(input$countries, input$add_countries)) %>%
      dplyr::left_join(death_markers(), by = c('country_region', 'date')) %>% # Add 10, 100, and 1000 deaths markers
      dplyr::left_join(ints(), by = c('country_region', 'date')) # Add intervention markers
    return(wide_relevant)
  })
  
  # Filter data and prepare x-axis based on 'x_type' selection
  start_x <- reactive({
    if(input$x_type != 'date') { 
      # if we're doing 'days since': filter data down
      start_x <- wide_rel() %>%
        dplyr::filter((input$start_type == 'case' & confirmed >= input$x_start) |
                        (input$start_type == 'death' & deaths >= input$x_start)) 
    } else { 
      # If we're using the date x-axis, use all data we have
      start_x <- wide_rel()
    }
    # Calculate the 'days since' variable and calculate rolling averages
    start_x <- start_x %>%
      dplyr::group_by(country_region) %>%
      dplyr::mutate(day_since = row_number()-1,
                    last_day = max(day_since),
                    date = as.Date(date, origin = '1970-01-01'),
                    confirmed = percap_calc(confirmed, X2020, input$percap),
                    deaths = percap_calc(deaths, X2020, input$percap),
                    roll_conf = percap_calc(roll_conf, X2020, input$percap),
                    roll_death = percap_calc(roll_death, X2020, input$percap))
    # Create the x_axis variable based on which is selected
    if(input$x_type == 'date'){
      start_x$x_axis <- start_x$date
    } else {
      start_x$x_axis <- start_x$day_since
    }
    return(start_x)
  })
  
  ### Data for mapping ###
  map_data <- reactive({
    dplyr::full_join(values$confirmed, values$deaths) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::left_join(map.world, ., by = c('region' = 'country_region')) %>%
      dplyr::filter(!is.na(confirmed)) %>% 
      dplyr::mutate(conf_per100k = confirmed / (X2020 / 100000),
                    death_per100k = deaths / (X2020 / 100000))
  })
  
  
  ### Long data for heatmap only ###
  long_data <- reactive({
    conf_roll <- roll_avg(values$confirmed, 'confirmed', input$roll_num)
    death_roll <- roll_avg(values$deaths, 'deaths', input$roll_num)
    long_data <- dplyr::bind_rows(conf_roll, death_roll) %>%
      dplyr::filter(country_region %in% c(input$countries, input$add_countries)) %>% 
      dplyr::left_join(death_markers(), by = c('country_region', 'date')) %>% # Add 10, 100, and 1000 deaths markers
      dplyr::left_join(ints(), by = c('country_region', 'date')) %>% # Add intervention markers
      dplyr::mutate(date = as.Date(date, origin = '1970-01-01'))
  })
  
  
  #### VISUALISATIONS BELOW ####
  
  #### CONFIRMED CASES ####
  graph_conf <- reactive({
    plot <- cumulative_plot(start_x(), type = 'confirmed', input$line_size, input$text_move, 
                            input$text_size, input$x_type, input$x_start, 
                            input$percap, input$day_start, input$y_trans, input$x_marker, 
                            input$xlabel_size, input$int_marker, input$letter_marker)
    plot <- plot+  
      # Set main and axis titles
      labs(title = paste0('Cumulative number of cases (up to ', format(max(start_x()$date), '%d %B %Y'), ')'),
           caption = "The 'X' represents the date when 10 deaths were recorded in that country; 'C' represents 100 deaths, and 'M' represents 1000 deaths.\nNote that the impact of interventions on transmission can take up to 3 weeks to show in case numbers. \nDifferences between countries' trajectories can reflect differences in testing strategies (i.e. who is offered the test), \ntesting capacity, and interventions (e.g. social distancing measures) implemented. \nData source: Johns Hopkins University",
           x = ifelse(input$x_type == 'date', '',
                      paste0('Days since reaching ', input$x_start, ' confirmed ', input$start_type, 's')),
           y = paste0('Cumulative cases (', ifelse(input$percap == 'not transformed', '', 'per million; '), ifelse(input$y_trans == 'log10', 'logarithmic', 'linear'), ' scale)'))
    
    return(plot)
  })
  
  # Render the plot
  output$graph_conf <- renderPlot({
    graph_conf()
  })
  
  # Show interventions list as used
  output$data_int <- renderDataTable(ints(),
                                     options = list(
                                       pageLength = 20,
                                       scrollX = T))
  
  # Show raw data
  output$data_conf <- renderDataTable(wide_rel() %>% 
                                        tidyr::pivot_wider(id_cols = c('country_region', 'X2020'), 
                                                           names_from = date, 
                                                           values_from = confirmed),
                                      extensions = "FixedColumns",
                                      options = list(
                                        pageLength = 20,
                                        scrollX = T,
                                        fixedColumns = list(leftColumns = 2))
  )
  
  output$download_qa <- downloadHandler(
    filename = function() {
      paste("qa_data.csv", sep = "")
    },
    content = function(file) {
      write.csv(start_x() %>% 
                  dplyr::select(country_region, X2020, date, deaths), 
                file, row.names = FALSE)
    }
  )
  
  #### DEATHS ####
  graph_death <- reactive({
    plot <- cumulative_plot(start_x(), type = 'deaths', input$line_size, input$text_move, 
                            input$text_size, input$x_type, input$x_start, 
                            input$percap, input$day_start, input$y_trans, input$x_marker, 
                            input$xlabel_size, input$int_marker, input$letter_marker)
    plot <- plot+
      # Set main and axis titles
      labs(title = paste0('Cumulative number of deaths (up to ', format(max(start_x()$date), '%d %B %Y'), ')'),
           caption = "Differences between countries' trajectories can reflect differences in testing capacity, determining cause of death, \n and interventions (e.g. social distancing measures) implemented.\nNote that the impact of interventions on transmission can take up to 3 weeks to show in case numbers. \nData source: Johns Hopkins University",
           x = ifelse(input$x_type == 'date', '',
                      paste0('Days since reaching ', input$x_start, ' confirmed ', input$start_type, 's')),
           y = paste0('Cumulative deaths (', ifelse(input$percap == 'not transformed', '', 'per million; '), ifelse(input$y_trans == 'log10', 'logarithmic', 'linear'), ' scale)'))
    return(plot)
  })
  
  output$graph_death <- renderPlot({
    graph_death()
  })
  
  # Show raw data
  output$data_death <- renderDataTable(wide_rel() %>% 
                                         tidyr::pivot_wider(id_cols = c('country_region', 'X2020'), 
                                                            names_from = date, 
                                                            values_from = deaths),
                                       extensions = "FixedColumns",
                                       options = list(
                                         pageLength = 20,
                                         scrollX = T,
                                         fixedColumns = list(leftColumns = 2))
  )
  
  #### ROLLING NEW CASES / DEATHS PER CAPITA ####
  output$roll_both <- renderPlot({
    ggplot(long_data(), aes(x = date, 
                            y = country_region, 
                            fill = roll_percap))+
      geom_tile(colour = 'grey')+ 
      geom_text(aes(label = legend),
                size = 2)+
      scale_fill_distiller(palette = 'YlOrRd', 
                           direction = 1, 
                           na.value = 'grey90',
                           trans = 'log10',
                           labels = comma)+
      facet_wrap(~type)+
      labs(title = paste0('New confirmed cases and deaths per capita, ', input$roll_num, '-day rolling average'),
           subtitle = 'The letters stand for the start date of government interventions',
           caption = 'Data source: Johns Hopkins University, UN (population size)',
           x = 'Date',
           y = 'Country',
           fill = 'Cases/deaths per capita')+
      vis_theme+
      theme(legend.position = "right")
  })
  
  #### ROLLING NEW DEATHS ####
  roll_deaths <- reactive({
    plot <- rolling_plot(start_x(), type = 'roll_death', 
                         input$line_size, input$text_move, input$text_size,
                         input$x_start, input$start_type, input$percap, input$day_start, 
                         input$roll_num, input$y_trans, input$x_type, input$int_marker, 
                         input$xlabel_size, input$x_marker, label = 'deaths')
    return(plot)
  })
  
  output$roll_deaths <- renderPlot({
    roll_deaths()
  })
  
  output$roll_data <- renderDataTable(start_x(),
                                      extensions = "FixedColumns",
                                      options = list(
                                        pageLength = 20,
                                        scrollX = T,
                                        fixedColumns = list(leftColumns = 2))
  )
  
  #### ROLLING NEW CASES ####
  roll_cases <- reactive({
    plot <- rolling_plot(start_x(), type = 'roll_conf', 
                         input$line_size, input$text_move, input$text_size,
                         input$x_start, input$start_type, input$percap, input$day_start, 
                         input$roll_num, input$y_trans, input$x_type, input$int_marker, 
                         input$xlabel_size, input$x_marker, label = 'cases')
    return(plot)
  })
  
  output$roll_cases <- renderPlot({
    roll_cases()
  })
  
  # Show interventions list as used
  output$rolling_int <- renderDataTable(ints(),
                                        options = list(
                                          pageLength = 20,
                                          scrollX = T))
  
  #### MAP ####
  output$case_map <- renderPlot({
    ggplot()+
      geom_map(data = map_data(), map = map.world, aes(map_id = region, 
                                                      x = long, y = lat, 
                                                      fill = conf_per100k),
               colour = 'black')+
      scale_fill_gradientn(colors = rev(heat.colors(10))[c(1,4,6:10)])+
      theme_minimal()+
      labs(title = paste0("Cumulative COVID cases per 100.000 population in each country as of ", max(map_data()$date)),
           subtitle = "Note that some countries are not represented in JHU data.")+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
  })
  
  output$death_map <- renderPlot({
    ggplot()+
      geom_map(data = map_data(), map = map.world, aes(map_id = region, 
                                                       x = long, y = lat, 
                                                       fill = death_per100k),
               colour = 'black')+
      scale_fill_gradientn(colors = rev(heat.colors(10))[c(1,4,6:10)])+
      theme_minimal()+
      labs(title = paste0("Cumulative COVID deaths per 100.000 population in each country as of ", max(map_data()$date)),
           subtitle = "Note that some countries are not represented in JHU data.")+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
  })
  
  output$map_data <- renderDataTable(map_data() %>%
                                       dplyr::select(region, date, confirmed, deaths, population = X2020, conf_per100k, death_per100k) %>%
                                       unique(.),
                                     options = list(
                                       pageLength = 20,
                                       scrollX = T))
}
