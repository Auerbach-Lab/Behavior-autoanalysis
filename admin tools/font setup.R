library(hrbrthemes)
library(ggplot2)

import_econ_sans()
import_plex_sans()
import_roboto_condensed()
import_titillium_web()

ft_geom_defaults()
modern_geom_defaults() # this will output a list in red, ignore it
update_geom_font_defaults()

# Then browse to %localappdata%\R\win-library\4.2\hrbrthemes\fonts
# In each folder, select all the fonts, rightclick one, and choose 'Install for All Users'

ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") +
  theme_ipsum()

ggplot(mtcars, aes(mpg, wt)) +
  geom_point() +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") +
  theme_ipsum_rc()

flush_ticks(ggplot(mpg, aes(displ, hwy)) +
  geom_jitter(aes(color=class, fill=class), size=3, shape=21, alpha=1/2) +
  scale_x_continuous(expand=c(0,0), limits=c(1, 8), breaks=1:8) +
  scale_y_continuous(expand=c(0,0), limits=c(10, 50)) +
  scale_color_ipsum() +
  scale_fill_ipsum() +
  facet_wrap(~class, scales="free") +
  labs(
    title="Titillium Web",
    subtitle="This is a subtitle to see the how it looks in Titillium Web",
    caption="Source: hrbrthemes & Google"
  ) +
  theme_ipsum_tw(grid="XY", axis="xy") +
  theme(legend.position="none"))

# If any of these plots produced a pile of warnings about the Windows Font Database, then...
extrafont::loadfonts(device ='win')

# Restart R (and Rstudio) and try the plots again.
# If that still didn't work,
extrafont::font_import()

# This setup should only need to be done once per PC.
