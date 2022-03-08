################################################################
### TidyTuesday for Week7
### Created by: Joynaline Hart
### Created on: 2022-03-08
############################################################### 

### Load libraries #####
# Always done first, and always be in the line to run the function
library(tidyverse) 
library(here) 
# For efficiency in highlighting the data on my ggplot
library(gghighlight) 


### Load data ######
# Reading the data from github tidytuesday
studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv') 
# Get an idea of what the full data looks like that opens on another tab
View(studio_album_tracks)   

### ggplot line #####
studio_album_tracks %>% #calling the dataframe to be used
  mutate(mode_name=fct_relevel(mode_name, "minor", "major")) %>%   #rearranging the pilot type on how it would look like 
  group_by(album_release_year) %>% #grouping the data by the album year; this will make everything easier to plot
  # Filtering from a certain album which is Spicegirl's topselling album
  filter(album_name=="Spice") %>% 
  # Filtering the key name where no data was shown
  filter(key_name!="C", 
         key_name!="E") %>%
  ggplot (aes(x=danceability, #setting what goes to the horizontal axis
              y=energy)) + #setting what goes to the vertical axis
  # Plotting using jitter
  geom_jitter(alpha=0.6, #setting the alpha for transparency 
              size=8) + #setting the size for the plots
  # Highlighting which plot by setting the parameters
  gghighlight(energy > 0.8 & danceability > 0.7) + 
  # Label the certain variable 
  geom_label(aes(label = track_name), 
             size = 5, #setting the size of the label
             # Placement of the label's parameter
             hjust = 0.6, #horizontal adjustment
             vjust = 1, #vertical adjustment
             # Setting color of the label
             fill = "purple", #what goes inside
             colour = "white", #what goes around like a border
             alpha= 0.5) + #setting the transparency of the label
  # Setting the theme of the whole plot
  theme_classic() + 
  # Changing the theme to customize the components of the plot
  theme(text = element_text(size=16,  #manipulate the size of the text
                            family="Segoe UI"), #manipulate the font of the text
        plot.title = element_text(color = "deeppink4", #plot title's color is changed
                                  hjust = 0.5), #plot title's arrangement is adjusted to the center (0.5)
        legend.position = "none", #making the legend invisible
        panel.background = element_rect(fill="linen")) + #changing the color of the plot's background
  # Setting the notes that goes around the plot
  labs(title = "SPICEGIRLS: 1996 Spice album", #allows to change the title
       caption = "Source: TidyTuesday(https://github.com/rfordatascience/tidytuesday#datasets) | Designed by: Jline", #adding captions so that people can access them
       x="Danceability", #allows to change what you see from the x-axis; what is it for
      y="Energy") + #allows to change what you see from the y-axis; what is it for
  # Setting the limit of what you can see from both the horizontal and vertical axis
  scale_x_continuous(limits=c(0.55, 0.85), 
                     breaks=c(0.55, 0.65, 0.75, 0.85)) +
  scale_y_continuous(limits=c(0.6, 0.9),
                     breaks=c(0.6, 0.7, 0.8, 0.9)) +
  # Facetting by grid for two variables
  facet_grid(mode_name~key_name)

#Saving the ggplot's output 
ggsave(here("Week7_Tidy", "Output", "Week7_tidytuesday_Spice.png"), #placing the output in the "OUTPUT" folder
       width=16, height=6) #setting the size of my plot
