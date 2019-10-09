BOMST<- read_csv("Results/BOMST.csv")
BOMST <- BOMST %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure))


#Question 1
#For the Perth station (ID 9225), produce three scatter plots showing the 
#relationship between the maximum temperature and each other measurement recorded 
#(minimum temperature, rainfall and solar exposure).

Perth <-BOMST %>% 
  filter(Station_number == 9225)

ggplot(data = Perth, mapping = aes(x= max, y = min)) +
  geom_point()

ggplot(data = Perth, mapping = aes(x= max, y = Rainfall)) +
  geom_point()
       

ggplot(data = Perth, mapping = aes(x= max, y = Solar_exposure)) +
  geom_point()
#add transparancy


#Question 2
#Display these four measurements for the Perth station in a single scatter plot by using additional aesthetic mappings.
#You may need to try a few different data/aesthetic mappings to find one you like.

ggplot(data = Perth, 
       mapping = aes(x = max, y = min, colour = Solar_exposure, size = Rainfall)) +
  geom_point()


#Question 3
#Take the four plots you have produced in Q1 and Q2 and save them as a multi-panel figure.

install.packages("cowplot")
library(cowplot)

# Must save each graph as a variable

Plot1 <-ggplot(data = Perth, mapping = aes(x= max, y = min)) +
  geom_point(alpha = 0.2)

Plot2 <- ggplot(data = Perth, mapping = aes(x= max, y = Rainfall)) +
  geom_point(alpha = 0.2)

Plot3 <- ggplot(data = Perth, mapping = aes(x= max, y = Solar_exposure)) +
  geom_point(alpha = 0.2) 

Plot4 <- ggplot(data = Perth, 
                mapping = aes(x = max, y = min, colour = Solar_exposure, size = Rainfall)) +
  geom_point(alpha = 0.2)+
  theme(legend.position = "bottom")

  
  
Plot4

Rough_plot <- plot_grid(Plot1, Plot2, Plot3, Plot4)
                     

Rough_plot

ggsave("Figures/RoughPlot.jpg", plot = Rough_plot, width = 20, height = 15, units = 'cm')                    

# Question 4
#Using the entire BOM dataset, calculate the average monthly rainfall for each station. 
#Produce a lineplot to visualise this data and the state each station is in.

BOMST2 <- BOMST %>% 
  filter(Rainfall !="NA") %>%  # or create a new variable where rainfall is yes/no then map this probs should not ignore the NA b/c they are "o"
  group_by(Station_number,state,Month) %>% 
  summarise(meanRainfall = mean(Rainfall))

AvgStat<- ggplot(data = BOMST2,
       mapping = aes(x= factor(Month),
                     y=meanRainfall,
                     colour = factor(Station_number),
                     group = factor(Station_number)
                     )) +
  geom_line()+
  facet_wrap(~state)+
  labs(x = "Month")
AvgStat

ggsave("Figures/Q4.jpg", plot = AvgStat)  



