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


