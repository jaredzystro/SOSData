###############################################################################
###############################################################################
## Initialize packages, read data, set graphics constants----
rm(list=ls())

install_or_load_pack <- function(pack){
  # from https://nhsrcommunity.com/blog/a-simple-function-to-install-and-load-packages-in-r/
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
  {
    install.packages(create.pkg, dependencies = TRUE, repos = "http://cran.us.r-project.org")
  }
  sapply(pack, require, character.only = TRUE)
}

# Comment out package installation prior to deploying
packages <- c("survey","ggplot2","shiny","plyr","dplyr","srvyr", "sf", "spData","stringr","maps","ggiraph","shinycssloaders", "memoise", "shadowtext")
install_or_load_pack(packages)

library(survey)
library(ggplot2)
library(shiny)
library(dplyr)
library(plyr)
library(srvyr)
library(sf)
library(spData)
library(stringr)
library(maps)
library(ggiraph)
library(shinycssloaders)
library(memoise)
library(shadowtext)
library(tidyr)
#library(simpleboot)
library(showtext)
#library(curl)
library(data.table)

# Read in data that was cleaned up in data_wrangle.R
data <- readRDS("data2021_cleaned.rds")
data2016 <- readRDS("data2016_cleaned.rds")
data2011 <- readRDS("data2011_cleaned.rds")

# Trying fread to speed things up
zipcode <- data.table::fread("zipcode.csv")
# zipcode <- read.csv("zipcode.csv", as.is = TRUE)

# Constant to set minimum number of responses to display
MINIMUM_N <- 3

# Constant to set plot heights in pixels
# Temporary fix until I can get a better dynamic plot sizing plan in place
# PLOTHEIGHT <- 600

# Constant to set plot title wrapping
WRAPWIDTH <- 80
X_WRAPWIDTH <- 20

# Constant to set plot text size
BASESIZE <- 22
#BASESIZE <- 44

PLOT_WIDTH <- 1222
PLOT_HEIGHT <- 620

# Constant to set response label size
LABELSIZE <- 8
#LABELSIZE <- 16

### Color palettes
# dummy variables to test
# input<- list()
# input$color_palette <- "default"

COLOR_PALETTE <- list()

COLOR_PALETTE$default <- list()
# COLOR_PALETTE$default$Y2021 <- "#2d677f"
# COLOR_PALETTE$default$Y2016 <- "#da413d"
# COLOR_PALETTE$default$Y2011 <- "#73853a"

# All blue
COLOR_PALETTE$default$Y2021 <- "#2d677f"
COLOR_PALETTE$default$Y2016 <- "#739AAA"
COLOR_PALETTE$default$Y2011 <- "#B9CCD4"

COLOR_PALETTE$colorblind <- list()
COLOR_PALETTE$colorblind$Y2021 <- "#f5793a"
COLOR_PALETTE$colorblind$Y2016 <- "#a95aa1"
COLOR_PALETTE$colorblind$Y2011 <- "#85c0f9"

COLOR_PALETTE$grayscale <- list()
COLOR_PALETTE$grayscale$Y2021 <- "#ffffff"
COLOR_PALETTE$grayscale$Y2016 <- "#bbbbbb"
COLOR_PALETTE$grayscale$Y2011 <- "#666666"

# Set default font to Hind
FONTTYPE <- "Hind"

# theme(text = element_text(family = FONTTYPE, size = FONTSIZE),
#       legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
#       axis.title = element_text(family = FONTTYPE, size = TITLESIZE))

# Turn on bookmarking
shiny::enableBookmarking(store = 'url')



###############################################################################
###############################################################################
### Create additional global variables ----

# Add column of state abbreviations to data based on zipcode
getzipcode <- zipcode$state
names(getzipcode) <- zipcode$zip
data$state <- unname(getzipcode[as.character(data[,"Q04A"])])
data2016$state <- unname(getzipcode[as.character(data2016[,"Zip1"])])
data2011$state <- data2011$State

# Adding SARE regions
western <- c("CA", "OR", "WA", "ID", "NV", "AZ", "UT", "NM", "CO", "WY", "MT", "AK", "HI")
northcentral <- c("ND", "SD", "KS", "NE", "MN", "IA", "MO", "WI", "IL", "MI", "IN", "OH")
southern <- c("TX", "OK", "AR", "LA", "KY", "TN", "MS", "AL", "FL", "GA", "SC", "NC", "VA")
northeast <- c("WV", "MD", "DE", "NJ", "PA", "NY", "CT", "RI", "MA", "VT", "NH", "ME")

data <- data %>%
  mutate(SARERegion = case_when(
    state %in% western ~ "West",
    state %in% northcentral ~ "North Central",
    state %in% southern ~ "South",
    state %in% northeast ~ "Northeast"))

data2016 <- data2016 %>%
  mutate(SARERegion = case_when(
    state %in% western ~ "West",
    state %in% northcentral ~ "North Central",
    state %in% southern ~ "South",
    state %in% northeast ~ "Northeast"))

data2011 <- data2011 %>%
  mutate(SARERegion = case_when(
    state %in% western ~ "West",
    state %in% northcentral ~ "North Central",
    state %in% southern ~ "South",
    state %in% northeast ~ "Northeast"))

data <- data %>% group_by(SARERegion) %>% mutate(RegionTotal = n())
data2016 <- data2016 %>% group_by(SARERegion) %>% mutate(RegionTotal = n())
data2011 <- data2011 %>% group_by(SARERegion) %>% mutate(RegionTotal = n())

# Create weight and survey object for survey based on current total population of organic producers (14,217 as of 2021)
tot_pop <- 14217
data$weight <- length(data$RespID) / tot_pop

mydesign <-
  svydesign(
    ids = ~1 ,
    data = data ,
    weights = ~weight
  )

# Create weight and survey object for 2016 survey based on 2014 total population of organic producers (14,093)
tot_pop2016 <- 14093
data2016$weight <- length(data2016$RespondentID) / tot_pop

mydesign2016 <-
  svydesign(
    ids = ~1 ,
    data = data2016 ,
    weights = ~weight
  )

# Create weight and survey object for 2009 survey based on 2009 total population of organic producers (14,093)
tot_pop2011 <- 10903
data2011$weight <- length(data2011$RespondentID) / tot_pop

mydesign2011 <-
  svydesign(
    ids = ~1 ,
    data = data2011 ,
    weights = ~weight
  )

# Get sp type shape data for states
# Add column of abbreviations
data("us_states")

# Added to avoid conflict with Liza's data
us_states_j <- us_states
us_states_j$state <- state.abb[match(us_states_j$NAME,state.name)]

# Dropping HI and AK for now because it is so far from everything
# Will need a way to incorporate eventually
rownames(zipcode) <- zipcode$zip
zipcode$zip <- as.character(zipcode$zip)
data$zip <- data$Q04A
zips <- left_join(data[,c("Q04A","zip","HasVeg","OnlyVeg","HasField","OnlyField","HasForage","OnlyForage","TotalAcreageBins","weight")], zipcode, by = "zip")
#zips <- zips[zips$state != "HI",]
#zips <- zips[zips$state != "AK",]

# 2016 zipcode wrangling
data2016$zip <- as.character(data2016$Zip1)
zips2016 <- left_join(data2016[,c("Zip1","zip","HasVeg","OnlyVeg","HasField","OnlyField","HasForage","OnlyForage","TotalAcreageBins","weight")], zipcode, by = "zip")
#zips2016 <- zips2016[zips2016$state != "HI",]
#zips2016 <- zips2016[zips2016$state != "AK",]

# No zipcode data for 2011


# Get state data for gradient maps and convert to upper case
# Add column of abbreviations
states_map <- map_data("state")
states_map$region <- str_to_title(states_map$region)
states_map$state <- state.abb[match(states_map$region,state.name)]

### Load data for miscellaneous canned figures ----

# Average percent acreage planted to organic seed for each crop type
ogbycrop <- read.csv("./misctables/ogbycrop.csv")

# Percent of total acreage planted to organic seed for each crop type
totalogacres <- read.csv("./misctables/totalogacres.csv")

# Research funding by year
fundingbyyear <- read.csv("./misctables/fundingbyyear.csv")

# Research funding by source
fundingbysource <- read.csv("./misctables/fundingbysource.csv")

# Research funding by source
fundingbyregion <- read.csv("./misctables/fundingbyregion.csv")

# Research funding by topic
fundingbytopic <- read.csv("./misctables/fundingbytopic.csv")

# Research funding by crop
fundingbycrop <- read.csv("./misctables/fundingbycrop.csv")

# Priority breeding by crop category
breedcropcat <- read.csv("./misctables/breedcropcat.csv")

# All organic seed by crop category
allogbycrop <- read.csv("./misctables/allogbycrop.csv")

# Reasons not using organic seed by crop category
noogbycrop <- read.csv("./misctables/noogbycrop.csv")

# Over the last three years, certifier requested greater steps to source organic seed
certifierrequest <-  read.csv("./misctables/certifierrequest.csv")

# Seed use and certifier request
veg <- data %>% filter (!is.na(CertifierRequest) & !is.na(IncreasedOGVegSeed)) %>% group_by(CertifierRequest,IncreasedOGVegSeed) %>% summarise(n = n(), drop.na = TRUE) %>% mutate(Change = IncreasedOGVegSeed, Vegetable = n / sum(n))
field <- data %>% filter (!is.na(CertifierRequest) & !is.na(IncreasedOGFieldSeed)) %>% group_by(CertifierRequest,IncreasedOGFieldSeed) %>% summarise(n = n(), drop.na = TRUE) %>% mutate(Change = IncreasedOGFieldSeed, Field = n / sum(n))
forage <- data %>% filter (!is.na(CertifierRequest) & !is.na(IncreasedOGForageSeed)) %>% group_by(CertifierRequest,IncreasedOGForageSeed) %>% summarise(n = n(), drop.na = TRUE) %>% mutate(Change = IncreasedOGForageSeed, Forage = n / sum(n))
cover <- data %>% filter (!is.na(CertifierRequest) & !is.na(IncreasedOGCoverSeed)) %>% group_by(CertifierRequest,IncreasedOGCoverSeed) %>% summarise(n = n(), drop.na = TRUE) %>% mutate(Change = IncreasedOGCoverSeed, Cover = n / sum(n))

seedusetable <- veg %>% full_join(field,  by = c("CertifierRequest","Change"))  %>% 
                        full_join(forage,  by = c("CertifierRequest","Change"))  %>%
                        full_join(cover,  by = c("CertifierRequest","Change")) %>% 
                        select (CertifierRequest,Change,Vegetable,Field,Forage,Cover) %>%
                        pivot_longer(!c(CertifierRequest,Change), names_to = "CropType", values_to = "Percent") %>%
                        filter (Change == "Increased the %") %>% 
                        select (CertifierRequest,CropType,Percent) %>%
                        mutate(Percent = Percent * 100)

# USDA Funding
usdafunding <-  read.csv("./misctables/usdafunding.csv")
