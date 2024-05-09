library(ggplot2)
library(ggalluvial)
library(gridExtra)
library(ggrepel)
library(reshape2)
library(dplyr)
library(tidyr)
library(scico)
library(readr)
library(ggplot2)
library(viridis)
library(patchwork)


file_dir <- "C:/Users/jx920/OneDrive - Imperial College London/1 - Projects/COP_Analysis/data/side_events"

# plotting data
energy_country_data <- read.csv(file.path(file_dir,paste("affiliation/plot_data_energy_country.csv")), encoding = "UTF-8")
energy_org_data <- read.csv(file.path(file_dir,paste("affiliation/plot_data_energy_org.csv")), encoding = "UTF-8")

country_order <- c("No match","Annex II","Annex I Only","Non-Annex","International")
energy_country_plot <- ggplot(data = energy_country_data, aes(x = cop_num))  + 
  geom_bar(data = energy_country_data, stat = "identity",position = "stack",
           aes(y = Weight, fill = factor(Country_Type,level=country_order))) +
  scale_fill_manual("Country Annex", 
                    values = c("Annex II" = "#d2b48c","Annex I Only" = "#a98565",
                               "Non-Annex"="#4d0001", "International" = "#5b92e5",
                               "No match" = "gainsboro"))+
  labs(title = bquote(bold('a.')~'Energy events by organiser Annex')) +
  xlab("COP") + ylab("Number of events") + theme_minimal() + ylim(0,40)+
  theme(plot.title = element_text(size = 12),legend.text.align = 0) 

org_order <- c("No match","NGO - Business & Industry","NGO - Environmental",
               "NGO - Research & independent","NGO - Indigenous peoples",
               "NGO - Women & Gender","NGO - Youth","NGO - Others",
               "IGO","Party","UN")
energy_org_plot <- ggplot(data = energy_org_data, aes(x = cop_num))  + 
  geom_bar(data = energy_org_data, stat = "identity",position = "stack",
           aes(y = Weight, fill = factor(Org_Type,level=org_order))) +
  scale_fill_viridis_d(option = "D", direction = -1) +
  scale_fill_manual("Organisation type", 
                    values = c("No match" = "gainsboro","NGO - Business & Industry"="#87aedf",
                               "NGO - Environmental" = "#bcee68","NGO - Others" = "dimgray",
                               "NGO - Research & independent" = "#b2874c", "NGO - Youth" = "#ffd701",
                               "IGO" = "black","Party" = "#db7093","UN" = "#5b92e5",
                               "NGO - Indigenous peoples" = "#556b2e",
                               "NGO - Women & Gender" = "#ffaeb9"))+
  labs(title = bquote(bold('b.')~'Energy events by organiser type')) +
  xlab("COP") + ylab("Number of events") + theme_minimal() +ylim(0,40)+
  theme(plot.title = element_text(size = 12),legend.text.align = 0) 

gsum <- energy_country_plot/energy_org_plot + plot_layout(guides = 'collect') # patchwork
ggsave(gsum, filename= file.path(file_dir,paste("affiliation/energy_affiliation.pdf")), 
       device="pdf",height=5.5, width=7,units="in")
ggsave(gsum, filename= file.path(file_dir,paste("affiliation/energy_affiliation.png")), 
       device="png",height=5.5, width=7,units="in")
