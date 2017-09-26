setwd()

library(tidyverse); library(doMC); library(stringr); library(maps); library(mapproj)

states <- read.csv("states.csv", as.is = TRUE)

eachStateScreeningRateRaw <- read.csv("eachStateScreeningRateTESTING.csv", as.is = TRUE)

eachStateScreeningRate <- eachStateScreeningRateRaw %>% dplyr::select(-starts_with("Footnote"))

eachStateScreeningRateMutate <- full_join(eachStateScreeningRate, states, by = c("State" = "Abbreviation")) %>%
	mutate(stateName = str_to_lower(State.y))

mclapply(eachStateScreeningRateMutate, function(i) summary(i, useNA="always"))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   library(grid)
 
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
 
   numPlots = length(plots)
 
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
     # Make the panel
     # ncol: Number of columns of plots
     # nrow: Number of rows needed, calculated from # of cols
     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
   }
 
  if (numPlots==1) {
     print(plots[[1]])
 
   } else {
     # Set up the page
     grid.newpage()
     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
 
     # Make each plot, in the correct location
     for (i in 1:numPlots) {
       # Get the i,j matrix positions of the regions that contain this subplot
       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
 
       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                       layout.pos.col = matchidx$col))
     }
   }
 }

 eachStateScreeningRateMutate %>%
      group_by(State) %>%
       summarise_each(funs = "mean(., na.rm=TRUE)") %>% View
 
 av.eachStateScreeningRateMutate <- eachStateScreeningRateMutate %>%
      group_by(State) %>%
       summarise_each(funs = "mean(., na.rm=TRUE)") %>%
       left_join(., states, by = c("State" = "Abbreviation")) %>%
       mutate(region = str_to_lower(State.y.y))

us <- map_data("state")

plot1b <- ggplot() + 
     geom_map(data = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#ffffff", color = "#ffffff", size = 0.15) + 
      geom_map(data = av.eachStateScreeningRateMutate, map = us, 
               aes(fill = Screening.for.depression.and.developing.a.follow.up.plan., map_id = region),
               color = "#ffffff", size = 0.15) + 
      scale_fill_gradientn(colours = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"), limits = c(0,100)) + 
      ggtitle("Average Screening Rate for depression and developing a follow up plan") +theme(plot.title = element_text(size = 6, face = "bold")) + 
      labs(x=NULL, y=NULL) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5))

plot2b <- ggplot() + 
     geom_map(data = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#ffffff", color = "#ffffff", size = 0.15) + 
      geom_map(data = av.eachStateScreeningRateMutate, map = us, 
               aes(fill = Screening.for.tobacco.use.and.providing.help.quitting.when.needed., , map_id = region),
               color = "#ffffff", size = 0.15) + 
      scale_fill_gradientn(colours = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"), limits = c(0,100)) + 
      ggtitle("Average Screening Rate for tobacco use and providing help quitting when needed") +theme(plot.title = element_text(size = 6, face = "bold")) + 
      labs(x=NULL, y=NULL) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5))

plot3b <- ggplot() + 
     geom_map(data = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#ffffff", color = "#ffffff", size = 0.15) + 
      geom_map(data = av.eachStateScreeningRateMutate, map = us, 
               aes(fill = Screening.for.an.unhealthy.body.weight.and.developing.a.follow.up.plan., , map_id = region),
               color = "#ffffff", size = 0.15) + 
      scale_fill_gradientn(colours = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"), limits = c(0,100)) + 
      ggtitle("Average Screening Rate for an unhealthy body weight and developing a follow up plan") +theme(plot.title = element_text(size = 6, face = "bold")) + 
      labs(x=NULL, y=NULL) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5))

plot4b <- ggplot() + 
     geom_map(data = us, map = us, aes(x=long, y=lat, map_id = region),
             fill = "#ffffff", color = "#ffffff", size = 0.15) + 
      geom_map(data = av.eachStateScreeningRateMutate, map = us, 
               aes(fill = Screening.for.high.blood.pressure.and.developing.a.follow.up.plan., , map_id = region),
               color = "#ffffff", size = 0.15) + 
      scale_fill_gradientn(colours = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58"), limits = c(0,100)) + 
      ggtitle("Average Screening Rate for high blood pressure and developing a follow up plan") +     theme(plot.title = element_text(size = 6, face = "bold")) + 
 
      labs(x=NULL, y=NULL) +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      theme(panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5))
 
multiplot(plot1b, plot2b, plot3b, plot4b, cols=2)
 #> `geom_smooth()` using method = 'loess'
multiplot(plot1, plot2, plot3, plot4, cols=2)
 #> `geom_smooth()` using method = 'loess'
