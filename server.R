server <- function(input, output) {
  source('global.R')
  values <- reactiveValues(confirmed = confirmed,
                           deaths = deaths)
  
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
      dplyr::filter(country_region %in% input$countries) %>%
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
  
  
  ### Long data for heatmap only ###
  long_data <- reactive({
    conf_roll <- roll_avg(values$confirmed, 'confirmed', input$roll_num)
    death_roll <- roll_avg(values$deaths, 'deaths', input$roll_num)
    long_data <- dplyr::bind_rows(conf_roll, death_roll) %>%
      dplyr::filter(country_region %in% input$countries) %>% 
      dplyr::left_join(death_markers(), by = c('country_region', 'date')) %>% # Add 10, 100, and 1000 deaths markers
      dplyr::left_join(ints(), by = c('country_region', 'date')) %>% # Add intervention markers
      dplyr::mutate(date = as.Date(date, origin = '1970-01-01'))
  })
  
  
  #### VISUALISATIONS BELOW ####
  
  #### CONFIRMED CASES ####
  graph_conf <- reactive({
    plot <- base_plot(10)+
      geom_line(data = start_x(), aes(x = x_axis, y = confirmed, colour = country_region),
                size = input$line_size)+
      # Add country labels
      geom_text_repel(data = start_x() %>% dplyr::group_by(country_region) %>% dplyr::filter(x_axis == max(x_axis)),
                      aes(label = country_region, x = x_axis, y = confirmed, colour = country_region), 
                      force = input$text_move, hjust = (input$text_move * -1), alpha = 1, size = input$text_size,
                      min.segment.length = 0.8, segment.size = 0.2, segment.color = 'transparent') +
      # Set main and axis titles
      labs(title = paste0('Cumulative number of cases (up to ', format(max(start_x()$date), '%d %B %Y'), ')'),
           caption = "The 'X' represents the date when 10 deaths were recorded in that country; 'C' represents 100 deaths, and 'M' represents 1000 deaths.\nNote that the impact of interventions on transmission can take up to 3 weeks to show in case numbers. \nDifferences between countries' trajectories can reflect differences in testing strategies (i.e. who is offered the test), \ntesting capacity, and interventions (e.g. social distancing measures) implemented. \nData source: Johns Hopkins University",
           x = ifelse(input$x_type == 'date', '',
                      paste0('Days since reaching ', input$x_start, ' confirmed ', input$start_type, 's')),
           y = paste0('Cumulative case number (', ifelse(input$percap == 'not transformed', '', 'per million; '), ifelse(input$y_trans == 'log10', 'logarithmic', 'linear'), ' scale)'))
    if(input$y_trans == 'log10'){
      plot <- plot+
        scale_y_continuous(breaks = 10^(-25:10), # log10 line breaks
                           minor_breaks = rep(1:9, 21)*(10^rep(-25:10, each=9)), 
                           labels = scales::label_comma(trim = T),
                           trans = input$y_trans)
    } else {
      plot <- plot+
        scale_y_continuous(labels = scales::label_comma(trim = T))
    }
    # Add death count markers
    if(input$x_marker){ 
      plot_m <- plot+
        geom_point(data = subset(start_x(), !is.na(lab)), 
                   aes(x = x_axis, y = confirmed, colour = country_region),
                   size = input$xlabel_size + 2, alpha = 0.8)+
        geom_text(data = subset(start_x(), !is.na(lab)),
                  aes(x = x_axis, y = confirmed, label = lab), color = 'white',
                  size = input$xlabel_size)
    } else {
      plot_m <- plot
    }
    # Add intervention markers
    if(input$int_marker & input$letter_marker == 'Categories'){ 
      plot_i <- plot_m+
        geom_point(data = subset(start_x(), !is.na(legend)), 
                   aes(x = x_axis, y = confirmed, colour = country_region),
                   size = input$xlabel_size + 2, alpha = 0.8)+
        geom_text(data = subset(start_x(), !is.na(legend)),
                  aes(x = x_axis, y = confirmed, label = legend), color = 'black',
                  size = input$xlabel_size)
    } else if (input$int_marker & input$letter_marker == 'Individual letters'){
      plot_i <- plot_m+
        geom_point(data = subset(start_x(), !is.na(legend_l)), 
                   aes(x = x_axis, y = confirmed, colour = country_region),
                   size = input$xlabel_size + 2, alpha = 0.8)+
        geom_text(data = subset(start_x(), !is.na(legend_l)),
                  aes(x = x_axis, y = confirmed, label = legend_l), color = 'black',
                  size = input$xlabel_size)
    } else {
      plot_i <- plot_m
    }
    return(plot_i)
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
    plot <- base_plot(1)+
      geom_line(data= start_x(), aes(x = x_axis, y = deaths, colour = country_region),
                size = input$line_size)+
      # Add country labels
      geom_text_repel(data = start_x() %>% dplyr::group_by(country_region) %>% dplyr::filter(x_axis == max(x_axis)),
                      aes(label = country_region, x = x_axis, y = deaths, colour = country_region), 
                      force = input$text_move, alpha = 1, size = input$text_size,
                      segment.color = 'transparent')+
      # Set main and axis titles
      labs(title = paste0('Cumulative number of deaths (up to ', format(max(start_x()$date), '%d %B %Y'), ')'),
           caption = "Data for UK shows DHSC published daily totals for all those who have died following a positive test.\nDifferences between countries' trajectories can reflect differences in determining cause of death, \ntesting capacity, and interventions (e.g. social distancing measures) implemented.\nNote that the impact of interventions on transmission can take up to 3 weeks to show in case numbers. \nData source: Johns Hopkins University, PHE",
           x = ifelse(input$x_type == 'date', '',
                      paste0('Days since reaching ', input$x_start, ' confirmed ', input$start_type, 's')),
           y = paste0('Cumulative death number (', ifelse(input$percap == 'not transformed', '', 'per million; '), ifelse(input$y_trans == 'log10', 'logarithmic', 'linear'), ' scale)'))
    if(input$y_trans == 'log10'){
      plot <- plot+
        scale_y_continuous(breaks = 10^(-25:10), # log10 line breaks
                           minor_breaks = rep(1:9, 21)*(10^rep(-25:10, each=9)), 
                           labels = scales::label_comma(trim = T),
                           trans = input$y_trans)
    } else {
      plot <- plot+
        scale_y_continuous(labels = scales::label_comma(trim = T))
    }
    if(input$x_type == 'date') {
      plot <- plot +
        scale_x_date(date_labels = "%d %b",
                     date_breaks = "1 week")
    }
    
    # Add intervention markers
    if(input$int_marker){ 
      plot_i <- plot+
        geom_point(data = subset(start_x(), !is.na(legend)), 
                   aes(x = x_axis, y = deaths, colour = country_region),
                   size = input$xlabel_size + 2, alpha = 0.8)+
        geom_text(data = subset(start_x(), !is.na(legend)),
                  aes(x = x_axis, y = deaths, label = legend), color = 'black',
                  size = input$xlabel_size)
    } else {
      plot_i <- plot
    }
    return(plot_i)
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
    plot <- base_plot(1)+
      geom_line(data= start_x(), aes(x = x_axis, y = roll_death, colour = country_region),
                size = input$line_size)+
      # Add country labels
      geom_text_repel(data = start_x() %>% dplyr::group_by(country_region) %>% dplyr::filter(x_axis == max(x_axis)),
                      aes(label = country_region, x = x_axis, y = roll_death, colour = country_region), 
                      force = input$text_move, hjust = (input$text_move * -1), alpha = 1, size = input$text_size,
                      segment.color = 'transparent')+
      # Add main and axis titles 
      labs(title = paste0('Rolling ', input$roll_num, '-day average of new deaths per day (up to ', format(max(start_x()$date), '%d %B %Y'), ')'),
           caption = "Differences between countries' trajectories can reflect differences in determining cause of death, \ntesting capacity, and interventions (e.g. social distancing measures) implemented.\nNote that the impact of interventions on transmission can take up to 3 weeks to show in case numbers. \nData source: Johns Hopkins University",
           x = ifelse(input$x_type == 'date', '',
                      paste0('Days since reaching ', input$x_start, ' confirmed ', input$start_type, 's')),
           y = paste0('New deaths (', ifelse(input$percap == 'not transformed', '', 'per million; '), ifelse(input$y_trans == 'log10', 'logarithmic', 'linear'), ' scale; rolling ', input$roll_num, '-day average)'))
    if(input$y_trans == 'log10'){
      plot <- plot+
        scale_y_continuous(breaks = 10^(-25:10), # log10 line breaks
                           minor_breaks = rep(1:9, 21)*(10^rep(-25:10, each=9)), 
                           labels = scales::label_comma(trim = T),
                           trans = input$y_trans)
    } else {
      plot <- plot+
        scale_y_continuous(labels = scales::label_comma(trim = T))
    }
    if(input$x_type == 'date') {
      plot <- plot +
        scale_x_date(date_labels = "%d %b",
                     date_breaks = "1 week")
    }
    # Add intervention markers
    if(input$int_marker){ 
      plot_i <- plot+
        geom_point(data = subset(start_x(), !is.na(legend)), 
                   aes(x = x_axis, y = roll_death, colour = country_region),
                   size = input$xlabel_size + 2, alpha = 0.8)+
        geom_text(data = subset(start_x(), !is.na(legend)),
                  aes(x = x_axis, y = roll_death, label = legend), color = 'black',
                  size = input$xlabel_size)
    } else {
      plot_i <- plot
    }
    return(plot_i)
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
    plot <- base_plot(1)+
      geom_line(data= start_x(), aes(x = x_axis, y = roll_conf, colour = country_region),
                size = input$line_size)+
      # Add country labels
      geom_text_repel(data = start_x() %>% dplyr::group_by(country_region) %>% dplyr::filter(x_axis == max(x_axis)),
                      aes(label = country_region, x = x_axis, y = roll_conf, colour = country_region), 
                      force = input$text_move, hjust = (input$text_move * -1), alpha = 1, size = input$text_size,
                      segment.color = 'transparent')+
      # Add main and axis titles 
      labs(title = paste0('Rolling ', input$roll_num, '-day average of new cases per day (up to ', format(max(start_x()$date), '%d %B %Y'), ')'),
           caption = "Differences between countries' trajectories can reflect differences in testing capacity, \nand interventions (e.g. social distancing measures) implemented.\nNote that the impact of interventions on transmission can take up to 3 weeks to show in case numbers. \nData source: Johns Hopkins University",
           x = ifelse(input$x_type == 'date', '',
                      paste0('Days since reaching ', input$x_start, ' confirmed ', input$start_type, 's')),
           y = paste0('New cases (', ifelse(input$percap == 'not transformed', '', 'per million; '), ifelse(input$y_trans == 'log10', 'logarithmic', 'linear'), ' scale; rolling ', input$roll_num, '-day average)'))
    if(input$y_trans == 'log10'){
      plot <- plot+
        scale_y_continuous(breaks = 10^(-25:10), # log10 line breaks
                           minor_breaks = rep(1:9, 21)*(10^rep(-25:10, each=9)), 
                           labels = scales::label_comma(trim = T),
                           trans = input$y_trans)
    } else {
      plot <- plot+
        scale_y_continuous(labels = scales::label_comma(trim = T))
    }
    # Add death markers
    if(input$x_marker){
      plot_m <- plot+
        geom_point(data = subset(start_x(), !is.na(lab)), 
                   aes(x = x_axis, y = roll_conf, colour = country_region),
                   size = input$xlabel_size + 2, alpha = 0.8)+
        geom_text(data = subset(start_x(), !is.na(lab)),
                  aes(x = x_axis, y = roll_conf, colour = country_region, label = lab), color = 'white',
                  size = input$xlabel_size)
    } else {
      plot_m <- plot
    }
    # Add intervention markers
    if(input$int_marker){ 
      plot_i <- plot_m+
        geom_point(data = subset(start_x(), !is.na(legend)), 
                   aes(x = x_axis, y = roll_conf, colour = country_region),
                   size = input$xlabel_size + 2, alpha = 0.8)+
        geom_text(data = subset(start_x(), !is.na(legend)),
                  aes(x = x_axis, y = roll_conf, label = legend), color = 'black',
                  size = input$xlabel_size)
    } else {
      plot_i <- plot_m
    }
    return(plot_i)
  })
  
  output$roll_cases <- renderPlot({
    roll_cases()
  })
  
  # Show interventions list as used
  output$rolling_int <- renderDataTable(ints(),
                                        options = list(
                                          pageLength = 20,
                                          scrollX = T))
}
