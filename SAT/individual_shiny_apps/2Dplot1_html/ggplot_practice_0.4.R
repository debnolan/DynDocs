# setwd("~/Dropbox/Senior_Research/Stat_Summer15_Research/DynDocs/SAT/shiny_plot1_0.3")
load("../rda_data/satDF.rda")
load("../rda_data/satDF_one.rda")
library(ggplot2)
library(grid)
dat <- data.frame(expenditure = satDF$expend,
                  sat = satDF$sat,
                  region = state.region, 
                  population = satDF_one$population,
                  state_abb = state.abb)

p<- ggplot(dat, aes(expenditure, sat,
                    colour = region,
                    size = sqrt(population/pi),
                    label = state.abb))
p + geom_point() + 
  geom_text(size = 4, colour = "black", vjust = -1) + 
  # show state_abb above the points (use nearpoints function!!!!!!)
  # geom_text(size = 3, colour = "black", vjust = -1) +
  # rename size legend
  # scale_size(range = c(0, 20)) +
  # rename colour legend
  # scale_colour_brewer(name = "Region") +
  # turn on or off population legend
  scale_size_continuous(guide = FALSE) +
  # turn on or off region legend
  # scale_colour_continuous(guide = FALSE) +
  # black and white plot background
  theme_bw() +
  theme(axis.title.x = element_text(vjust = -1), 
           axis.title.y = element_text(angle = 90, vjust = 1), 
        # plot.title = element_text(colour = "black"), 
           plot.title = element_text(size = 15, vjust = 3),
           legend.position = c(0.8, 0.7), 
        # legend background 
           legend.background = element_rect(fill = "transparent", colour = "transparent"), 
           legend.key = element_rect(colour = "transparent")
        )
if(tmp.data[1]!="no value") {
  p = p + geom_point()
}
# p + geom_line()

