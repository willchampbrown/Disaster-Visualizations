# Loading required libraries -----------------------------------------------------

library(jsonlite)
library(tidyverse)
library(scales)

# Accessing the data -----------------------------------------------------------------

fema_url <- 'https://www.fema.gov/api/open/v2/DisasterDeclarationsSummaries.jsona'
fema_df <- fromJSON(fema_url)

# Basic data transformation -----------------------------------------------------

fema_df %>%
  # Only want major disaster declarations and four 
  # incident types; want incidents that serve as 
  # indicators of climate change against one control (tornadoes)
  filter(declarationType == 'DR', 
        incidentType %in% c('Flood', 'Severe Storm(s)', 'Fire', 'Tornado'),
        fyDeclared > 1969,
        fyDeclared < 2020) %>%
  # Creating the decade field
  # 2020 onward rmeoved
  mutate(decade = fyDeclared - (fyDeclared %% 10),
         # Rebinned so I could show tornado on top, and compare
         # while moving vertically downward
         incidentType = factor(incidentType, levels = c('Tornado', 
                                                        'Fire',
                                                        'Flood',
                                                        'Severe Storm(s)'))) %>%
  group_by(decade, incidentType) %>%
  count(fyDeclared) %>%
  rename(Year = fyDeclared, Disasters = n) -> fema_count
  
# Creating my plot -----------------------------------------------------------

fema_count %>%
  # Creating rows, 1: number of disasters
  complete(Disasters = 1:Disasters) %>%
  ggplot(aes(x = 0, y = Disasters)) +
  # Tile for formatting and alignment
  geom_tile(fill = '#AB2836') +
  # Large vertical jitter required
  geom_jitter(aes(color = incidentType), width = 100, height = 1000, size = .5, shape = 15) +
  facet_grid(rows = vars(incidentType), cols = vars(decade), scales = "free", shrink = TRUE, switch = "y") +
  scale_color_manual(values = c("#053C5E", '#053C5E', '#053C5E', '#053C5E')) +
  labs(title = "Major Disaster Declarations Over the Years",
       subtitle = 
         "Each dot represents a major disaster declaration across four common disaster types: Floods, Severe 
Storms, Fires, and Tornadoes. Data is broken down by decade, beginning in 1970 and ending in 2010. 
Data from 2020 onward is incomplete and has been ommited.",
       caption = 'Data from OpenFEMA | Plot Created 1/15/22',
       x = '',
       y = '') +
  theme(plot.background = element_rect(fill = '#053C5E'),
        plot.margin = margin(10, 40, 10, 10),
        panel.background = element_rect(fill = '#AB2836', colour = '#7C2E41'),
        panel.spacing = unit(1.25, 'lines'),
        axis.text = element_text(size = 0),
        panel.grid = element_blank(),
        axis.title = element_text(size = 10),
        strip.placement = 'outside',
        strip.background = element_blank(),
        strip.text = element_text(family = 'Times', colour = 'white', size = 12),
        plot.title = element_text(family = 'Times', size = '24', face = 'bold', colour = 'white', margin = margin(10, 0, 20, 0)),
        plot.subtitle = element_text(family = 'Times', size = '12', colour = 'white', margin = margin(10, 0, 20, 0)),
        legend.position = 'none',
        plot.caption = element_text(family = 'Times', colour = 'white', size = 10)) -> fema_plot

# Saving the visual output -----------------------------------------------------
ggsave(filename = 'disaster_declarations_point_plot.png', 
       plot = fema_plot, device = 'png', dpi = 300, 
       width = 8, height = 8.5, units = 'in')

        