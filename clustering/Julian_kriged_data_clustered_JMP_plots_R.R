

library(ggplot2)
library(dplyr)
library(tidyverse)



Julian_cluster <- read.csv("V:/Marlborough regional/working_jaxs/for_mapping_may2022/vesper_Julian_days/Kriged_julian_days_pts_JMPout.csv")
                           
  
names(Julian_cluster)
Julian_cluster <- Julian_cluster %>% dplyr::select(for_join:Region_Nam, Rain7Cluster:Rain6Cluster)
Julian_cluster <- Julian_cluster %>% dplyr::select(-Distance_2,
                                                                         -Distance_3,
                                                                         -Distance_4,
                                                                         -Distance_5,
                                                                         -Distance_6,
                                                                         -Distance_7,
                                                                         -Distance_8,
                                                                         -Distance_9,
                                                                         -Distance_10)

Julian_cluster_narrow <-Julian_cluster %>% 
  pivot_longer(
    cols= c(Cluster_2:Cluster_10),
    names_to = "cluster_solution",
    values_to = "cluster"
  )
tail(Julian_cluster_narrow)
unique(Julian_cluster_narrow$cluster_solution)

Julian_cluster_narrow$cluster_solution <- factor(Julian_cluster_narrow$cluster_solution,
                                                 levels = c("Cluster_2",
                                                            "Cluster_3",
                                                            "Cluster_4",
                                                            "Cluster_5",
                                                            "Cluster_6",
                                                            "Cluster_7",
                                                            "Cluster_8",
                                                            "Cluster_9",
                                                            "Cluster_10") )


Julian_cluster_narrow %>% 
  ggplot(aes(x = X , y = Y, colour = factor(cluster)))+
  geom_point()+
  facet_wrap(.~ cluster_solution)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "bottom"
        #legend.title = element_blank()
  )



## how many clusters?
                               
CCC_Julian_days_df <- read.csv("V:/Marlborough regional/working_jaxs/for_mapping_may2022/vesper_Julian_days/Kriged_julian_days_pts_JMPIN - K Means ClusterCCC.csv")
str(CCC_Julian_days_df)                  

CCC_Julian_days_df %>% 
  ggplot(aes(NCluster, CCC))+
  geom_line(size=3)+
  scale_x_continuous(breaks = 1:10)+
  theme_bw()+
  theme(plot.title = element_text(size=20,face="bold"),
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"))+
  labs(x = "number of clusters",
       y = "Cubic Cluster Criterion",
       title = "CCC method")
