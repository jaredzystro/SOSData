#library(tidyverse)
library(dplyr)
library(ggplot2)
library(usmap)
library(plotly)
library(data.table)
library(plotly)
library(flexdashboard)
library(igraph)
library(ggraph)
library(mapproj)

#df.p.slim <- read.csv("~/Box/osa_networks/data_combined/surveys_combined_slim.csv")
#df.p.long <- read.csv("~/Box/osa_networks/data_combined/surveys_combined_long.csv")
#us_states <- read.csv("~/Box/osa_networks/data_combined/map_data.csv")
#us_states_r <- read.csv("~/Box/osa_networks/data_researcher/data_combined/map_data.csv")
#df.r <- fread("~/Box/osa_networks/data_researcher/data_combined/surveys_combined.csv")
#df.r.long <- df.r %>% 
#  pivot_longer(cols = crop1cat:crop3cat, names_to = "crop_number", 
#               values_to = "crop_type") %>% 
#  filter(crop_type != "") 
#edges <- fread("~/Documents/Davis/R-Projects/OSA_networks/combined/07_shiny/local_data/edges.csv")
#nodes <- read.csv("~/Documents/Davis/R-Projects/OSA_networks/combined/07_shiny/local_data/nodes.csv")
#combined <- read.csv("~/Documents/Davis/R-Projects/OSA_networks/combined/07_shiny/local_data/combined.csv") 
#
#fwrite(df.p.slim, "local_data/df.p.slim.csv")
#fwrite(df.p.long, "local_data/df.p.long.csv")
#fwrite(us_states, "local_data/us_states.csv")
#fwrite(us_states_r, "local_data/us_states_r.csv")
#fwrite(df.r, "local_data/df.r.csv")
#fwrite(df.r.long, "local_data/df.r.long.csv")
#fwrite(edges, "local_data/edges.csv")
#fwrite(nodes, "local_data/nodes.csv")
#fwrite(combined, "local_data/combined.csv")

df.f <- fread("local_data/public-funding-Dec21.csv")
df.p.slim <- fread("local_data/df.p.slim.csv")
df.p.long <- fread("local_data/df.p.long.csv")
us_states <- fread("local_data/us_states.csv")
us_states_r <- fread("local_data/us_states_r.csv")
df.r <- fread("local_data/df.r.csv")
df.r.long <- fread("local_data/df.r.long.csv")
edges <- fread("local_data/edges.csv")
nodes <- fread("local_data/nodes.csv")
combined <- fread("local_data/combined.csv") 

FONTSIZE <- 12
#FONTSIZE <- 36

TITLESIZE <- 10
#TITLESIZE <- 30


cb.pall.6 <- colorRampPalette(c("lightgrey", "#85c0f9"))(6)
grey.pall.6 <- colorRampPalette(c("lightgrey", "#666666"))(6)
template.dk.blue <- "#2d677f"
template.pall.6 <- colorRampPalette(c("lightgrey", template.dk.blue))(6)

COLOR_PALETTE_L <- list()
COLOR_PALETTE_L$default <- template.pall.6
COLOR_PALETTE_L$colorblind <- cb.pall.6
COLOR_PALETTE_L$grayscale <- grey.pall.6

COLOR_PALETTE_L3 <- list()
COLOR_PALETTE_L3$default <- c("#B9CCD4", "#739AAA", "#2d677f")
COLOR_PALETTE_L3$colorblind <- c("#85c0f9", "#a95aa1", "#f5793a")
COLOR_PALETTE_L3$grayscale <- c("#666666", "#bbbbbb", "#ffffff")


DEGREE_TYPE <- data.frame(type = c("all", "in", "out"))

nodes$role_refined <- ifelse(nodes$role_refined == "company" |
                               nodes$role_refined == "retail" |
                               nodes$role_refined == "processing" |
                               nodes$role_refined == "handler + retail", "Seed handlers & retailers",
                             ifelse(nodes$role_refined == "consultant" |
                                      nodes$role_refined == "org", "Organizations",
                                    ifelse(nodes$role_refined == "govt" |
                                             nodes$role_refined == "university_ext", "University & government",
                                           ifelse(is.na(nodes$role_refined) |
                                                    nodes$role_refined == "country" |
                                                    nodes$role_refined == "other" | 
                                                    nodes$role_refined == "", "Other",
                                                  "Seed producers"))))

nodes$color <- ifelse(nodes$role_refined == "Seed handlers & retailers", "darkred",
                      ifelse(nodes$role_refined == "Organizations", "darkorange",
                             ifelse(nodes$role_refined == "University & government", "darkblue",
                                    ifelse(nodes$role_refined == "Seed producers", "darkgreen",
                                           ifelse(nodes$role_refined == "Other", "grey", "grey")))))

nodes$colororder <- ifelse(nodes$role_refined == "Seed handlers & retailers", "c2",
                      ifelse(nodes$role_refined == "Organizations", "c3",
                             ifelse(nodes$role_refined == "University & government", "c4",
                                    ifelse(nodes$role_refined == "Seed producers", "c1",
                                           ifelse(nodes$role_refined == "Other", "c5", "c5")))))


df.f <- df.f %>% 
  mutate(year_cat = case_when(
    `2016AndEarlier` == T ~ "1996-2016",
    T ~ "2017-2021"
  )) %>% 
  select(-`2016AndEarlier`, -After2016)
