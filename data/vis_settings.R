#### VISUAL SETTINGS ####
# Set some visualisation steps common to all plots
vis_theme <- theme(legend.position = "none", # Remove legend
                   axis.ticks.y = element_blank(), # Remove axis ticks
                   axis.ticks.x = element_blank(), 
                   panel.background = element_rect(fill = 'white'), # Set background colour
                   panel.grid.major = element_line(colour = 'grey75'), # Set line colours
                   panel.grid.minor = element_line(colour = 'grey94'))
base_plot <- function(low_limit){
  plot <- ggplot()+
    # Force the 10 to show on the y-axis
    scale_y_log10(limits = c(low_limit,NA))+
    scale_color_manual(values = country_col)+ # Fix colour palette to countries
    vis_theme # Apply tick and panel settings (above)
  return(plot)
}


cumulative_plot <- function(data, type = 'confirmed', line_size, text_move, text_size, x_type, x_start, 
                            percap, y_trans, x_marker, xlabel_size, int_marker, letter_marker){
  plot <- base_plot(10)+
    geom_line(data = data, aes_string(x = 'x_axis', y = type, colour = 'country_region'),
              size = line_size)+
    # Add country labels
    geom_text_repel(data = data %>% dplyr::group_by(country_region) %>% dplyr::filter(x_axis == max(x_axis)),
                    aes_string(label = 'country_region', x = 'x_axis', y = type, colour = 'country_region'), 
                    force = text_move, hjust = (text_move * -1), alpha = 1, size = text_size,
                    min.segment.length = 0.8, segment.size = 0.2, segment.color = 'transparent')
  if(y_trans == 'log10'){
    plot <- plot+
      scale_y_continuous(breaks = 10^(-25:10), # log10 line breaks
                         minor_breaks = rep(1:9, 21)*(10^rep(-25:10, each=9)), 
                         labels = scales::label_comma(trim = T),
                         trans = y_trans)
  } else {
    plot <- plot+
      scale_y_continuous(labels = scales::label_comma(trim = T))
  }
  # Add death count markers
  if(x_marker){ 
    plot_m <- plot+
      geom_point(data = subset(data, !is.na(lab)), 
                 aes_string(x = 'x_axis', y = type, colour = 'country_region'),
                 size = xlabel_size + 2, alpha = 0.8)+
      geom_text(data = subset(data, !is.na(lab)),
                aes_string(x = 'x_axis', y = type, label = 'lab'), color = 'white',
                size = xlabel_size)
  } else {
    plot_m <- plot
  }
  # Add intervention markers
  if(int_marker & letter_marker == 'Categories'){ 
    plot_i <- plot_m+
      geom_point(data = subset(data, !is.na(legend)), 
                 aes_string(x = 'x_axis', y = type, colour = 'country_region'),
                 size = xlabel_size + 2, alpha = 0.8)+
      geom_text(data = subset(data, !is.na(legend)),
                aes_string(x = 'x_axis', y = type, label = 'legend'), color = 'black',
                size = xlabel_size)
  } else if (int_marker & letter_marker == 'Individual letters'){
    plot_i <- plot_m+
      geom_point(data = subset(data, !is.na(legend_l)), 
                 aes_string(x = 'x_axis', y = type, colour = 'country_region'),
                 size = xlabel_size + 2, alpha = 0.8)+
      geom_text(data = subset(data, !is.na(legend_l)),
                aes_string(x = 'x_axis', y = type, label = 'legend_l'), color = 'black',
                size = xlabel_size)
  } else {
    plot_i <- plot_m
  }
}

rolling_plot <- function(data, type = 'roll_death', line_size, text_move, text_size,
                         x_start, start_type, percap, 
                         roll_num, y_trans, x_type, int_marker, 
                         xlabel_size, x_marker, label = 'deaths'){
  plot <- base_plot(1)+
    geom_line(data= data, aes_string(x = 'x_axis', y = type, colour = 'country_region'),
              size = line_size)+
    # Add country labels
    geom_text_repel(data = data %>% dplyr::group_by(country_region) %>% dplyr::filter(x_axis == max(x_axis)),
                    aes_string(label = 'country_region', x = 'x_axis', y = type, colour = 'country_region'), 
                    force = text_move, hjust = (text_move * -1), alpha = 1, size = text_size,
                    segment.color = 'transparent')+
    # Add main and axis titles 
    labs(title = paste0('Rolling ', roll_num, '-day average of new ', label, ' per day (up to ', format(max(data$date), '%d %B %Y'), ')'),
         caption = "Differences between countries' trajectories can reflect differences in testing capacity, determining cause of death, \n and interventions (e.g. social distancing measures) implemented.\nNote that the impact of interventions on transmission can take up to 3 weeks to show in case numbers. \nData source: Johns Hopkins University",
         x = ifelse(x_type == 'date', '',
                    paste0('Days since reaching ', x_start, ' confirmed ', start_type, 's')),
         y = paste0('New ', label, ' (', ifelse(percap == 'not transformed', '', 'per million; '), ifelse(y_trans == 'log10', 'logarithmic', 'linear'), ' scale; rolling ', roll_num, '-day average)'))
  
  if(y_trans == 'log10'){
    plot <- plot+
      scale_y_continuous(breaks = 10^(-25:10), # log10 line breaks
                         minor_breaks = rep(1:9, 21)*(10^rep(-25:10, each=9)), 
                         labels = scales::label_comma(trim = T),
                         trans = y_trans)
  } else {
    plot <- plot+
      scale_y_continuous(labels = scales::label_comma(trim = T))
  }
  if(x_type == 'date') {
    plot <- plot +
      scale_x_date(date_labels = "%d %b",
                   date_breaks = "1 week")
  }
  if(x_marker & type == 'roll_conf'){
    plot_m <- plot+
      geom_point(data = subset(data, !is.na(lab)), 
                 aes_string(x = 'x_axis', y = type, colour = 'country_region'),
                 size = xlabel_size + 2, alpha = 0.8)+
      geom_text(data = subset(data, !is.na(lab)),
                aes_string(x = 'x_axis', y = type, colour = 'country_region', label = 'lab'), color = 'white',
                size = xlabel_size)
  } else {
    plot_m <- plot
  }
  # Add intervention markers
  if(int_marker){ 
    plot_i <- plot_m+
      geom_point(data = subset(data, !is.na(legend)), 
                 aes_string(x = 'x_axis', y = type, colour = 'country_region'),
                 size = xlabel_size + 2, alpha = 0.8)+
      geom_text(data = subset(data, !is.na(legend)),
                aes_string(x = 'x_axis', y = type, label = 'legend'), color = 'black',
                size = xlabel_size)
  } else {
    plot_i <- plot_m
  }
  return(plot_i)
}

# Colors taken from: ggthemes::tableau_color_pal('Tableau 20')
color_palette <- tibble::tribble(~country_region, ~colour,
                                 'Italy', "#4E79A7",
                                 'South Korea', "#F28E2B",
                                 'Spain', "#A0CBE8",
                                 'France', "#FFBE7D",
                                 'US', "#59A14F",
                                 'Germany', "#B07AA1",
                                 'Japan', "#86BCB6",
                                 'UK', "#E15759")

countries <- unique(color_palette$country_region)

#### GOVERNMENT INTERVENTIONS ####
# Note: this is NOT an exhaustive list of government responses to the COVID-19 pandemic. 
# This list is provided for visualisation purposes only, and is not suitable for in-depth analysis.
interventions <- tibble::tribble( ~include, ~country_region, ~date, ~measure, ~int,
                                TRUE, 'Spain', '2020-03-14', 'School closures, non-essential shop closures, national lockdown', 'lockdown',
                                TRUE, 'US', '2020-03-14', 'Foreign travel restrictions', 'travel restrictions',
                                TRUE, 'US', '2020-03-17', 'State-wide lockdowns', 'lockdown',
                                TRUE, 'Germany', '2020-03-10', 'Ban on events >1000 people', 'social distancing',
                                TRUE, 'Germany', '2020-03-15', 'Closure of schools and hospitality businesses', 'social distancing',
                                TRUE, 'Italy', '2020-03-07', 'Lockdown of 15 provinces', 'lockdown',
                                TRUE, 'Italy', '2020-03-10', 'National lockdown', 'lockdown',
                                TRUE, 'France', '2020-03-12', 'School closures', 'social distancing',
                                TRUE, 'France', '2020-03-16', 'National lockdown', 'lockdown',
                                TRUE, 'Japan', '2020-04-07', 'State of emergency in Tokyo, Chiba, Kanagawa, Saitama, Osaka, Hyogo and Fukuoka', 'social distancing',
                                TRUE, 'South Korea', '2020-02-21', 'Special care zones declared', 'social distancing',
                                TRUE, 'UK', '2020-03-20', 'School closures', 'social distancing',
                                TRUE, 'UK', '2020-03-23', 'Closure of all non-essential business, limited movement enforced', 'lockdown',
                                TRUE, 'Japan', '2020-02-25', 'Suspension of large gatherings', 'social distancing',
                                TRUE, 'Japan', '2020-03-02', 'School closures', 'social distancing',
                                TRUE, 'Japan', '2020-04-16', 'National state of emergency', 'social distancing',
                                TRUE, 'South Korea', '2020-04-19', 'Social distancing rules relaxed', 'isolation restrictions easing', 
                                TRUE, 'Italy', '2020-05-04', 'Movement within reigons and family visits at home allowed', 'isolation restrictions easing', 
                                TRUE, 'Italy', '2020-05-04', 'Vists to relatives, parks open and takeway food and drink allowed', 'gatherings restrictions easing', 
                                TRUE, 'Germany', '2020-05-04', 'Pre-school, primary and secondary school open with social distancing', 'education restrictions easing', 
                                TRUE, 'Germany', '2020-05-06', 'Ban on gatherings lifted and social/cultural venues and small shops start to open', 'gatherings restrictions easing',
                                TRUE, 'US', '2020-04-24', 'At this point 8 states had eased physical distancing measures while thye remained in place in 42 states', 'gatherings restrictions easing',
                                TRUE, 'France', '2020-05-11', 'Some people return to work and public trasport service will increase', 'isolation restrictions easing',
                                TRUE, 'Spain', '2020-05-11', 'Social venues, e.g. bars, start to open with social distancing', 'gatherings restrictions easing', 
                                TRUE, 'France', '2020-05-11', 'Gatherings of up to 10 allowed, shops and some social/cultural venues opening', 'gatherings restrictions easing',
                                TRUE, 'UK', '2020-05-10', 'The prime minister announced a roadmap for phased easing in the UK', 'isolation restrictions easing',
                                TRUE, 'France', '2020-05-11', 'Phased, non-compulsory opening of primary schools', 'education restrictions easing')


