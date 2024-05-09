library(ggplot2)
library(ggalluvial)
library(gridExtra)
library(ggrepel)
library(reshape2)
library(dplyr)
library(tidyr)
library(scico)
library(readr)
library(cowplot)

# https://jkzorz.github.io/2020/01/22/alluvial-plots.html
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

file_dir <- "C:/Users/jx920/OneDrive - Imperial College London/1 - Projects/COP_Analysis/data/side_events"

### PREPARE DATASET

topic_result <- read_csv(file.path(file_dir,paste("topic/bertopic/BERT_result/distil_result.csv")))
topic_result_year <- read_csv(file.path(file_dir,paste("topic/bertopic/BERT_result/distil_topics_per_year_new.csv")))
topic_result_year <- topic_result_year %>% 
  mutate(Year = as.factor(Year)) %>% 
  arrange(Year)
# Fill 2020 with zeros
topic_result_year <- topic_result_year %>% 
  mutate_at(vars(-Year), ~ifelse(is.na(.), 0, .))

intertopic_map <- read_csv(file.path(file_dir,paste("topic/bertopic/BERT_result/distil_intertopic_new.csv")))

all_events <- read_csv(file.path(file_dir,paste("all_side_events_complete_lang.csv")))
all_events_count <- all_events %>% 
  group_by(Year) %>% 
  summarise(Count = n())

food_melt <- read_csv(file.path(file_dir,paste("topic/frequency/top_bigram_food_SUM.csv")))
redd_melt <- read_csv(file.path(file_dir,paste("topic/frequency/top_bigram_redd_SUM.csv")))



### PREPARE PLOTTING FEATURES 
topics <- c(1,2,19,41,61,62)
intertopic_Not <- intertopic_map[!intertopic_map$index %in% topics, ]
intertopic_Not$color <- "lightgray"
intertopic_Not$label <- "None"
intertopic_Not$IFlabel <- 0
intertopic_Not$x_label <- 0
intertopic_Not$y_label <- 0
intertopic_Yes <- intertopic_map[intertopic_map$index %in% topics, ]
intertopic_Yes$color <- c( "orange", "blue","lightcoral", "skyblue", 
                           "purple",  "dodgerblue")
intertopic_Yes$label<- c('Food agriculture systems', 'Renewable energy','Fossil fuel', 
                        'Bioenergy', 'Nuclear energy', 'Energy poverty')
intertopic_Yes$IFlabel <- 1
intertopic_Yes$x_label <- c(12, 11, 11, 11, 11, 11)
intertopic_Yes$y_label <- c(0, 20, 8, 17, 14, 11)
intertopic_new<- rbind(intertopic_Not,intertopic_Yes)

### PLOT
# Scatter plot for Intertopic distance map
scatter_plot <- ggplot(intertopic_new,aes(x = x, y = y)) +
  geom_point(colour=intertopic_new$color ,size = intertopic_new$size/17, alpha = 0.5) +
  geom_text(data = subset(intertopic_new, IFlabel ==1), aes(x = x_label, y = y_label, label = label), hjust = "left",
            colour=subset(intertopic_new, IFlabel ==1)$color) +
  geom_segment(data = subset(intertopic_new, IFlabel ==1), aes(x = x, y = y, xend = x_label, yend = y_label), 
               colour=subset(intertopic_new, IFlabel ==1)$color) +
  theme_minimal() +
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  ylim(-10, 25) +
  theme(legend.position = "none",plot.title = element_text(size = 12)) +
  labs(title = bquote(bold('a.')~'Intertopic distance map'))
  
coef <-3
# Bar plot for REDD topic occurrence
redd_plot_data <- topic_result_year[, "0"]
redd_plot_data$cop <- seq(1, 20, length.out=20)+8
colnames(redd_plot_data) <- c("val" , "cop")
redd_plot_data$total <- all_events_count$Count
redd_plot_data$percent <- redd_plot_data$val/redd_plot_data$total*100
bar_redd <- ggplot(data = redd_plot_data, aes(x = cop)) +
  geom_bar(stat = "identity", aes(y = val, fill = "REDD")) +
  geom_point(aes(y=percent*coef), size=1, color="gray") +
  geom_line(aes(y=percent*coef), size=0.5, color="gray")+
  theme_minimal() +
  scale_fill_manual(values = c("darkgreen")) +
  scale_y_continuous(name = "Absolute frequency",limits = c(0,40),
                     sec.axis = sec_axis(~./coef,name="Relative frequency (%)"))+
  theme(legend.position = "none", axis.title.y = element_text(color = "black", size=10),
        axis.title.y.right = element_text(color = "gray", size=10),
        axis.text.y.right = element_text(colour = "gray"),
        plot.title = element_text(size = 12),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines")) +
  xlab("COP") +
  ylab("Absolute frequency") +
  labs(title = bquote(bold('a.')~'Forest mgmt topics'))

# Bar plot for Food topic occurrence
food_plot_data <- topic_result_year[, "1"]
food_plot_data$cop <- seq(1, 20, length.out=20)+8
colnames(food_plot_data) <- c("val" , "cop")
food_plot_data$total <- all_events_count$Count
food_plot_data$percent <- food_plot_data$val/food_plot_data$total*100
bar_food <- ggplot(data = food_plot_data, aes(x = cop)) +
  geom_bar(stat = "identity", aes(y = val, fill = "Food agriculture systems")) +
  geom_point(aes(y=percent*coef), size=1, color="gray") +
  geom_line(aes(y=percent*coef), size=0.5, color="gray")+
  theme_minimal() +
  scale_fill_manual(values = c("orange")) +
  scale_y_continuous(name = "Absolute frequency",limits = c(0,40),
                     sec.axis = sec_axis(~./coef,name="Relative frequency (%)"))+
  theme(legend.position = "none", axis.title.y = element_text(color = "black", size=10),
        axis.title.y.right = element_text(color = "gray", size=10),
        axis.text.y.right = element_text(colour = "gray"),
        plot.title = element_text(size = 12),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines")) +
  xlab("COP") +
  ylab("Absolute frequency") +
  labs(title = bquote(bold('c.')~'Food topics'))

# Alluvial plot for redd bigrams
redd_melt_subset <- redd_melt %>%filter(variable == "28") 
redd_melt_subset <- redd_melt_subset[order(desc(redd_melt_subset$value)), ]
redd_melt_subset <- redd_melt_subset%>%
  mutate(cumulative_value = rev(cumsum(rev(value))),
         prev_cumulative_value = lead(cumulative_value))%>%
  mutate(prev_cumulative_value = coalesce(prev_cumulative_value, 0))
redd_melt_subset$plot_value <- (redd_melt_subset$prev_cumulative_value+redd_melt_subset$cumulative_value)/2
redd_melt_subset$label_x <- 30
redd_melt_subset$label_y <- 32-seq(1, 10, length.out=10)*3
alluvial_redd <- ggplot(redd_melt, aes( x = variable, y = value, alluvium = word)) + 
  geom_alluvium(aes(fill = factor(word), colour=word), alpha = 0.8, decreasing = FALSE) + 
  scale_fill_scico_d(palette = 'roma') +
  scale_color_scico_d(palette = 'roma') +
  geom_text(data = redd_melt_subset, aes(x = label_x, y = label_y, label = word, color=word), hjust = "left")+ 
  geom_segment(data = redd_melt_subset%>%filter(value >0), 
               aes(x = label_x, y = label_y, xend = 28, yend = plot_value, color=word))+
  theme(legend.position = "none",plot.title = element_text(size=12),
        plot.margin = unit(c(0.5,10,0.5,0.5), "lines"))+
  #scale_linetype_identity(guide='legend')+
  labs(x = "COP", y = "Absolute frequency",title = bquote(bold('b.')~'Forest management side event top bigrams')) + 
  coord_cartesian(xlim = c(9,28), ylim = c(0,30),# This focuses the x-axis on the range of interest
                  clip = 'off')

# Alluvial plot for food bigrams
food_melt_subset <- food_melt %>%filter(variable == "28") 
food_melt_subset <- food_melt_subset[order(desc(food_melt_subset$value)), ]
food_melt_subset <- food_melt_subset%>%
  mutate(cumulative_value = rev(cumsum(rev(value))),
         prev_cumulative_value = lead(cumulative_value))%>%
  mutate(prev_cumulative_value = coalesce(prev_cumulative_value, 0))
food_melt_subset$plot_value <- (food_melt_subset$prev_cumulative_value+food_melt_subset$cumulative_value)/2
food_melt_subset$label_x <- 30
food_melt_subset$label_y <- 64-seq(1, 10, length.out=10)*6
alluvial_food <- ggplot(food_melt%>%filter(variable >=14), aes( x = variable, y = value, alluvium = word)) + 
  geom_alluvium(aes(fill = factor(word), colour=word), alpha = 0.8, decreasing = FALSE) + 
  scale_fill_scico_d(palette = 'roma') +
  scale_color_scico_d(palette = 'roma') +
  geom_text(data = food_melt_subset, aes(x = label_x, y = label_y, label = word, color=word), hjust = "left")+ 
  geom_segment(data = food_melt_subset%>%filter(value >0), 
               aes(x = label_x, y = label_y, xend = 28, yend = plot_value, color=word))+
  theme(legend.position = "none",plot.title = element_text(size=12),
        plot.margin = unit(c(0.5,10,0.5,0.5), "lines"))+
  #scale_linetype_identity(guide='legend')+
  labs(x = "COP", y = "Absolute frequency",title = bquote(bold('d.')~'Food side event top bigrams')) + 
  coord_cartesian(xlim = c(9,28), ylim = c(0,60),# This focuses the x-axis on the range of interest
                  clip = 'off')


# Combine plots
# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
g1 <- arrangeGrob(bar_redd, bar_food, alluvial_redd,alluvial_food,
                  layout_matrix = rbind(c(1,3,3),c(2,4,4)))
#gsum <- grid.arrange(scatter_plot,g1, heights = c(1.2, 2))
ggsave(g1, filename= file.path(file_dir,paste("topic/topic_bigram_redd_food.pdf")), 
       device="pdf",height=5.5, width=7,units="in")
ggsave(g1, filename= file.path(file_dir,paste("topic/topic_bigram_redd_food.png")), 
       device="png",height=5.5, width=7,units="in")
