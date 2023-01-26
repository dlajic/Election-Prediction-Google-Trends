library(pacman)
p_load(gtrendsR,
       ggplot2,
       tidyverse,
       tidyr,
       rvest,
       xml2,
       data.table,
       patchwork,
       lubridate,
       ajfhelpR,
       jsonlite,
       kableExtra,
       grid)

# Load data

load(file = "Data_SearchTerms/ 2023-01-22 14-33-20.RData")

# display all available category numbers
df_cat <- data("categories")
summary(df_cat)

######### Search terms + period
#Bundestagswahlen: 26.09.2021; 24.09.2017; 22.09.2013; 27.09.2009; 18.09.2005
# Specifying vertical X intercepts for election dates
elec_vlines <- as.Date(c("2005-09-18", "2009-09-27", "2013-09-18", "2017-09-24", "2021-09-26"))

# CDU ####

termsCDU_df <- termsCDU$interest_over_time
termsCDU_df <- termsCDU_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

###Used in Paper
p1 <- ggplot(termsCDU_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

  



# Linke ####
termsLinke_df <- termsLinke$interest_over_time
termsLinke_df <- termsLinke_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

###Used in Paper
p2 <- ggplot(termsLinke_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)



p1 + p2 + 
  plot_layout(ncol = 1) + 
  plot_layout(guides="collect")


result <- p1 + p2 + 
  plot_layout(ncol = 1)
gt <- patchwork::patchworkGrob(result)
plot_searchterms <- gridExtra::grid.arrange(gt,
                        bottom=textGrob("Date", gp=gpar(fontsize=22)), 
                        left=textGrob("Searches (100 = max. interest in time period/territory)", gp=gpar(fontsize=22), rot=90))
ggsave(plot = plot_searchterms,
       filename = "Figure_2_searchterms.png", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "png", # e.g. change to pdf
       dpi = 300) 

