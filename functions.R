### Dataframe slimming function ----

DataSlim <- function (data,columns_to_keep) {
  
  descriptive_columns <- c("state",
                          "HasVeg",
                          "OnlyVeg",
                          "HasField",
                          "OnlyField",
                          "HasForage",
                          "OnlyForage",
                          "TotalAcreageBins",
                          "YearsCertified_bin",
                          "weight")
  
  return(dplyr::select(data,all_of(descriptive_columns),all_of(columns_to_keep)))
                          
}

### X-axis label function ----

XLabel <- function (input, label) {
  
  # Add language if farm size or crop type is selected
  producer_label <- ifelse(
    input$crop_size == "All" && input$crop_type == "all",
    "",
   "for producers"
  )
  
  # convert from value to label
  crop_type_labels<- list()
  crop_type_labels[["has_veg"]] <- "vegetables and other crops"
  crop_type_labels[["only_veg"]] <- "only vegetables"
  crop_type_labels[["has_field"]] <- "field crops and other crops"
  crop_type_labels[["only_field"]] <- "only field crops"
  crop_type_labels[["has_forage"]] <- "forage crops and other crops"
  crop_type_labels[["only_forage"]] <- "only forage crops"
  
  type_label <- ifelse(
    input$crop_type == "all",
    "",
    paste("that grow", crop_type_labels[[input$crop_type]])
  )
  
  # Add sizes to label if specific farm sizes were selected
  size_label <- ifelse(
    input$crop_size == "All",
    "",
    paste("with total acreages of:", paste(input$crop_size,collapse = ", "))
  )
  
  # Add states to label if specific states were selected
  state_label <- ifelse(
    input$state == "All",
    "",
    paste("in", paste(input$state,collapse = ", "))
  )
  
  # Add years certified if specific years were selected
  years_label <- ifelse(
    input$years == "All",
    "",
    paste("that have been certified for", paste(input$years,collapse = ", "), " years")
    
  )
  
  return(paste(label, producer_label, type_label, size_label[1], state_label[1], years_label, collapse = ""))
  
}

### Filter function----

FilterData <- function(data, input) {
  
  # dummy variables for testing
  # input <- list()
  # input$state <- "All"
  # input$Y2011 <- FALSE
  # input$Y2016 <- TRUE
  # input$Y2021 <- TRUE
  # input$show_n <- TRUE
  # question <-"FieldAcreage"
  # input$crop_type <- "only_veg"
  # input$crop_size <- ">480"
  # input$color_palette <- "colorblind"

  # filter by state
  if (input$state == "All") {
    sub <- data 
  } else {
    sub <- subset(data,state %in% input$state) 
  }
  
  # filter by crop type
  if (input$crop_type == "all") {
    sub <- sub
  } else if (input$crop_type == "has_veg") {
    sub <- subset(sub, HasVeg)
  } else if (input$crop_type == "only_veg") {
    sub <- subset(sub, OnlyVeg)
  } else if (input$crop_type == "has_field") {
    sub <- subset(sub, HasField)
  } else if (input$crop_type == "only_field") {
    sub <- subset(sub, OnlyField)
  } else if (input$crop_type == "has_forage") {
    sub <- subset(sub, HasForage)
  } else if (input$crop_type == "only_forage") {
    sub <- subset(sub, OnlyForage)
  }
  
  # filter by size
  
  if (input$crop_size == "All") {
    sub <- sub 
  } else {
    sub <- subset(sub, TotalAcreageBins %in% input$crop_size) 
  }
  
  return (sub)
  
}

### Graphing functions ----

### Function to generate map based on producer zip and selected states ----
MapPlot_internal <- function (input, color_palette, show_n) {
  
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  # Avoid red error message when nothing is selected for input$state by validating
  validate(need(input$state, 'Please choose a state.'))
  
  # Filter
    
    filtered_zips <- FilterData(zips, input)
    filtered_zips2016 <- FilterData (zips2016, input)
    
    if (input$state=="All") {
      filtered_states <- us_states_j
    } else {
      filtered_states <- us_states_j[us_states_j$state %in% input$state,]
  }
  # TODO: need to add filtering function based on crop type
  # Probably need to join lat, long data with full data_frame rather
  # than having a seperate zip data_frame like it is now
  # Also need to add forage crop options
  
  # ggplot of state shapes (goem_sf) and zip codes (goem_point)
  # shape=21 is open circle
  # coords_sf is set to get rid of longitude and latitude notations
  # theme is set to get rid of lines
  p <-  ggplot(data = filtered_states) +
    geom_sf(colour = "black", fill = "white") +
    coord_sf(crs = sf::st_crs(filtered_states), datum = NA) +
    theme(
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
 #     text = element_blank(),
      plot.background = element_rect(fill = "white")) + 
#    labs(title = "Figure 15. Map of respondent locations", color = "") + 
    theme_graph(base_family = FONTTYPE, title_size = FONTSIZE)
  
  # Fix coloring for grayscale
  if (input$color_palette == "grayscale") {
    
    # make outline black and fill according to year
    outline2021 <- "#000000"
    outline2016 <- "#000000"
    fill2021 <- COLOR_PALETTE[[input$color_palette]]$Y2021
    fill2016 <- COLOR_PALETTE[[input$color_palette]]$Y2016
    
  } else {
    
    # make outline according to year and fill empty
    outline2021 <- COLOR_PALETTE[[input$color_palette]]$Y2021
    outline2016 <- COLOR_PALETTE[[input$color_palette]]$Y2016
    fill2021 <- NA
    fill2016 <- NA
    
  }
  
  
   if (input$Y2016) {
     p <- p + geom_point(data = filtered_zips2016, mapping = aes(x = longitude, y = latitude), colour = outline2016, fill = fill2016, shape=21, size=3)
   }
  
  if (input$Y2021) {
    
    p <- p + geom_point(data = filtered_zips, mapping = aes(x = longitude, y = latitude), colour = outline2021, fill = fill2021, shape=21, size=3)
  }
  
  
  plot (p)
  
}

# External function with caching	
MapPlot <- memoise(MapPlot_internal, cache = getShinyOption("cache"))


### Function to make box plots of question means and errors ----
BoxPlot_internal <- function (input, question, title, xlab, ylab) {
  
  # dummy variables for testing
  # input <- list()
  # input$state <- "All"
  # input$Y2011 <- FALSE
  # input$Y2016 <- TRUE
  # input$Y2021 <- TRUE
  # input$show_n <- TRUE
  # input$crop_size <- "All"
  # question <-"FieldAcreage"
  # input$crop_type <- "only_veg"
  # input$color_palette <- "colorblind"
  # title <- "test"
  # xlab <- "test"
  # ylab <- "test"
  
  # Display while loading
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  # Avoid red error message when nothing is selected for input$state by validating
  validate(need(input$state, 'Please choose at least one state.'))   
  validate(need(input$crop_size, 'Please choose a farm size.'))
  validate(need(input$Y2011 | input$Y2016 | input$Y2021, 'Please choose at least one year.'))
  validate(need((!is.null(data[[question]]) & input$Y2021) |
                  (!is.null(data2016[[question]]) & input$Y2016) |
                  (!is.null(data2011[[question]]) & input$Y2011)
                , 'Data not available for this question for the year(s) selected'))
  
  # Empty summary dataframe to add year data to
  survey_summary <- data.frame (
    "year" = character(),
    "question" = integer(),
    "mean" = integer(),
    "CI" = integer(),
    "n" = integer()
  )
  
  # Empty color palette
  graph_color <- character()
  
  
  # code for 2011 data
  if (!is.null(data2011[[question]]) & input$Y2011) {
    
    # Trim the dataframe down to speed things up
    data2011 <- DataSlim(data2011, question)
    
    # subset data based on input filters
    sub2011 <- FilterData(mydesign2011, input)
    
    # Compile mean, error and total number of responses
    survey_mean2011 <- svymean(as.formula(paste0("~",question)), sub2011, na.rm=TRUE)
    responses2011 <- sub2011$variables[question]
    n2011 <- length(responses2011[!is.na(responses2011)])
    
    # Create new data frame from compiled data for ggplot
    survey_summary2011 <- data.frame (
      "year" = "2011",
      "question" = question,
      "mean" = as.data.frame(survey_mean2011)$mean,
      "CI" = as.data.frame(survey_mean2011)[[question]]*1.96,
      "n" = n2011
    )
    survey_summary2011$summary_labels <- paste("n = ",survey_summary2011$n,sep="")
    
    # Add to overall summary dataframe
    survey_summary <- merge(survey_summary,survey_summary2011, all=TRUE)
    
    # Add color to graph_color
    graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2011)
  }
  
  # code for 2016 data
  if (!is.null(data2016[[question]]) & input$Y2016) {
    
    # Trim the dataframe down to speed things up
    data2016 <- DataSlim(data2016, question)
    
    # subset data based on input filters
    sub2016 <- FilterData(mydesign2016, input)
    
    # Compile mean, error and total number of responses
    survey_mean2016 <- svymean(as.formula(paste0("~",question)), sub2016, na.rm=TRUE)
    responses2016 <- sub2016$variables[question]
    n2016 <- length(responses2016[!is.na(responses2016)])
    
    # Create new data frame from compiled data for ggplot
    survey_summary2016 <- data.frame (
      "year" = "2016",
      "question" = question,
      "mean" = as.data.frame(survey_mean2016)$mean,
      "CI" = as.data.frame(survey_mean2016)[[question]]*1.96,
      "n" = n2016
    )
    survey_summary2016$summary_labels <- paste("n = ",survey_summary2016$n,sep="")
    
    # Add to overall summary dataframe
    survey_summary <- merge(survey_summary,survey_summary2016, all=TRUE)
    
    # Add color to graph_color
    graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2016)
  }
  
  # code for 2021 data
  if (!is.null(data[[question]]) & input$Y2021) {
    
    # Trim the dataframe down to speed things up
    data <- DataSlim(data, question)
    
    # subset data based on input filters
    sub2021 <- FilterData(mydesign, input)
    
    # Compile mean, error and total number of responses
    survey_mean2021 <- svymean(as.formula(paste0("~",question)), sub2021, na.rm=TRUE)
    responses <- sub2021$variables[question]
    n2021 <- length(responses[!is.na(responses)])
    
    # Create new data frame from compiled data for ggplot
    survey_summary2021 <- data.frame (
      "year" = "2021",
      "question" = question,
      "mean" = as.data.frame(survey_mean2021)$mean,
      "CI" = as.data.frame(survey_mean2021)[[question]]*1.96,
      "n" = n2021
    )
    survey_summary2021$summary_labels <- paste("n = ",survey_summary2021$n,sep="")
    
    # Add to overall summary dataframe
    survey_summary <- merge(survey_summary,survey_summary2021, all=TRUE)
    
    # Add color to graph_color
    graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2021)
    
  }
  
  # ggplot plots mean (goem_bar) and error (geom_errorbar)
  # Mean and n are added onto bar via the two geom_text calls
  # Adds labels for the title, x, and y axis based on inputted variables title, xlab, and ylab
  # Currently the y axis is fixed at 1-100, should make that dynamic
  
  
  ## Scratch code to automatically wrap titles  
  # font_size <- 10
  # wrap_width <- ???plotsize?? / (font_size*ggplot2:::.pt)
  
  # ggplot(df2, aes_string("V1")) +
  # geom_bar() +
  # scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = wrap_width))
  
  validate(need(!is.null(survey_summary$mean), "Not enough data for plot with current filters"))
  
  # Only show title if title checkbox selected
  title <- ifelse (input$show_title == TRUE, title,"")
  
  p <- ggplot(survey_summary, aes(x=question, fill = year, y=mean)) +
    geom_bar(stat = "identity", color = "black", width = 0.3, position = position_dodge(width=0.9)) +
    geom_errorbar(alpha = 1, 
                  mapping = aes(x = question, ymin = mean-CI, ymax = mean+CI), 
                  position = position_dodge(width=0.9),  size=.75, width=.1) +
    scale_fill_manual(values = graph_color) +
    ylim(0,100) +
    geom_shadowtext(aes(label=round(mean,1), vjust=-0.2), color="black", size=round(LABELSIZE*0.75), bg.color="white", position = position_dodge(width = 0.9)) +
    theme_linedraw(base_size = BASESIZE) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         x = stringr::str_wrap(str_to_sentence(xlab), width = WRAPWIDTH), y = str_to_sentence(ylab), fill = "Year") +
    scale_x_discrete(labels = function(x) str_wrap(str_to_sentence(x), width = X_WRAPWIDTH)) + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank()) +
    theme(text = element_text(family = FONTTYPE)) +
    theme(legend.title = element_text(family = FONTTYPE)) +
    theme(axis.title = element_text(family = FONTTYPE)) +
    guides(fill=guide_legend(title="Year"))
  
  # Remove legend if only one year
  if (nrow(survey_summary) == 1) {
    p <- p +  theme(legend.position="none")
  }
  
  # Add n if checked
  if (input$show_n) {
    p <- p + geom_text(aes(label=summary_labels, vjust=4), color="black", size=LABELSIZE-1, position = position_dodge(width = 0.9))
  }
  
  plot(p)
  
}

# External function with caching	
BoxPlot <- memoise(BoxPlot_internal, cache = getShinyOption("cache"))

### Function to make box plot for responses that can be split into bins ----
BinPlot_internal <- function (input, question, title, xlab, ylab) {
  
  # dummy variables for testing
  # input <- list()
  # input$state <- "All"
  # input$Y2011 <- FALSE
  # input$Y2016 <- TRUE
  # input$Y2021 <- TRUE
  # input$show_n <- TRUE
  # question <-"VegiAcreageBins"
  # input$crop_type <- "only_veg"
  # input$color_palette <- "colorblind"
  # title <- "test"
  # xlab <- "test"
  # ylab <- "test"
  # input$crop_size <- "All"
  
  # Display while loading
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  # Avoid red error message when nothing is selected for input$state by validating
  validate(need(input$state, 'Please choose at least one state.'))
  validate(need(input$crop_size, 'Please choose a farm size.'))
  validate(need(input$Y2011 | input$Y2016 | input$Y2021, 'Please choose at least one year.'))
  validate(need((!is.null(data[[question]]) & input$Y2021) |
                  (!is.null(data2016[[question]]) & input$Y2016) |
                  (!is.null(data2011[[question]]) & input$Y2011)
                , 'Data not available for this question for the year(s) selected'))
  
  
  # Build empty dataframe to add year summary data to
  out <- data_frame(
    "summary" = integer(),
    "summary_low" = integer(),
    "summary_upp" = integer(),
    "n" = integer(),
    "summary_labels" = character(),
    "year" = character()
  )
  
  # Empty color palette
  graph_color <- character()
  
  # Code for 2011 data
  if (!is.null(data2011[[question]]) & input$Y2011) {
    
    # Trim the dataframe down to speed things up
    data2011 <- DataSlim(data2011, question)
    
  # Remove missing data
    data2011 <- data2011[!is.na(data2011[question]),]
    
  # Filter
    sub2011 <- FilterData(data2011, input)
    
    if (nrow(data2011) >= MINIMUM_N) {
      sub2011 <- sub2011 %>% as_survey_design(id = 1, weight  = weight)
    }
    
    if (nrow(data2011) >= MINIMUM_N) {
      # Create summary data of means and confidence intervals for bins
      out2011 <- sub2011 %>%
        group_by_at(question) %>%
        summarize(summary = survey_mean(vartype = "ci"), n = unweighted(n())) %>%
        mutate(summary = summary * 100, summary_low = summary_low * 100, summary_upp = summary_upp * 100)
      out2011$summary_labels <- paste("n=",out2011$n,sep="")
      out2011$year <- "2011"
      
      # merge with overall summary
      out <- merge(out,out2011, all=TRUE)
      
      # Add color to graph_color
      graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2011)
    }
  }
  
  # Code for 2016 data
  if (!is.null(data2016[[question]]) & input$Y2016) {
    
    data2016 <- DataSlim(data2016, question)
    
    # Remove missing data
    data2016 <- data2016[!is.na(data2016[question]),]
    
    # Filter
    sub2016 <- FilterData(data2016, input)
    
    if (nrow(data2016) >= MINIMUM_N) {
      sub2016 <- sub2016 %>% as_survey_design(id = 1, weight  = weight)
    }  
    
    if (nrow(data2016) >= MINIMUM_N) {
      # Create summary data of means and confidence intervals for bins
      out2016 <- sub2016 %>%
        group_by_at(question) %>%
        summarize(summary = survey_mean(vartype = "ci"), n = unweighted(n())) %>%
        mutate(summary = summary * 100, summary_low = summary_low * 100, summary_upp = summary_upp * 100)
      out2016$summary_labels <- paste("n=",out2016$n,sep="")
      out2016$year <- "2016"
      
      # merge with overall summary
      out <- merge(out,out2016, all=TRUE)
      
      # Add color to graph_color
      graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2016)
    }
  }
  
  # Code for 2021 data
  if (!is.null(data[[question]]) & input$Y2021) {
    
    data <- DataSlim(data, question)
    
    # Remove missing data
    data <- data[!is.na(data[question]),]
    
    # Filter by crop type
    sub2021 <- FilterData(data, input)
    
    if (nrow(data2011) >= MINIMUM_N) {
      sub2021 <- sub2021 %>% as_survey_design(id = 1, weight  = weight)
    }
    
    if (nrow(data) >= MINIMUM_N) {
      # Create summary data of means and confidence intervals for bins
      out2021 <- sub2021 %>%
        group_by_at(question) %>%
        summarize(summary = survey_mean(vartype = "ci"), n = unweighted(n())) %>%
        mutate(summary = summary * 100, summary_low = summary_low * 100, summary_upp = summary_upp * 100)
      out2021$summary_labels <- paste("n=",out2021$n,sep="")
      out2021$year <- "2021"
      
      # merge with overall summary
      out <- merge(out,out2021, all=TRUE)
      
      # Add color to graph_color
      graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2021)
    }
  }
  
  # Check if there are enough data points to show
 # validate(need(nrow(out) >= MINIMUM_N, "Not enough data for plot with current filters"))
  
  
  # Only show title if title checkbox selected
  title <- ifelse (input$show_title == TRUE, title,"")
  
  # ggplot of means (geom_bar) and error (geom_errorbar) for bins
  p <- ggplot(data = out, aes_string(x = question, y = "summary",  fill = "year")) +
    geom_bar(stat = "identity", color = "black", width = 0.8, position = position_dodge(width=0.9)) +
    geom_errorbar(alpha = 1, mapping = aes_string(x = question, ymin = "summary_low", ymax = "summary_upp",  fill = "year"), inherit.aes = FALSE, size=.75, width=.1, position = position_dodge(width=0.9)) +
    ylim(0,100) +
    geom_shadowtext(aes(label=round(summary,0), vjust=-0.2), color="black", size=round(LABELSIZE*0.75), bg.color="white", position = position_dodge(width = 0.9)) +
    theme_linedraw(base_size = BASESIZE) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         x = stringr::str_wrap(str_to_sentence(xlab), width = WRAPWIDTH), y = str_to_sentence(ylab), fill = "Year")  +
    scale_x_discrete(labels = function(x) str_wrap(str_to_sentence(x), width = X_WRAPWIDTH)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    theme(text = element_text(family = FONTTYPE)) +
    theme(legend.title = element_text(family = FONTTYPE)) +
    theme(axis.title = element_text(family = FONTTYPE)) +
    scale_fill_manual(values = graph_color)+
    guides(fill=guide_legend(title="Year"))
  
  # Remove legend if only one year
  if (length(levels(as.factor(out$year))) == 1) {
    p <- p +  theme(legend.position="none")
  }
  
  if (input$show_n) {
    p <- p + geom_text(aes(label=summary_labels, vjust = -2.7), color="black", size=LABELSIZE-4, position = position_dodge(width=0.9))
  }
  
  plot(p)
  
}

# External function with caching	
BinPlot <- memoise(BinPlot_internal, cache = getShinyOption("cache"))

### Function to make plots by descriptive categories

CatPlot_internal <- function (input, category, question, title, xlab, ylab) {
  
  # input <- list()
  # input$state <- "All"
  # input$Y2011 <- FALSE
  # input$Y2016 <- TRUE
  # input$Y2021 <- TRUE
  # input$show_n <- TRUE
  # question <- "OGVegiSeed"
  # category <- "TotalAcreageBins"
  # input$crop_type <- "only_veg"
  # input$crop_size <- "All"
  # input$color_palette <- "colorblind"
  # title <- "test"
  # xlab <- "test"
  # ylab <- "test"
  
  # Display while loading
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  validate(need(input$state, 'Please choose at least one state.'))
  validate(need(input$crop_size, 'Please choose a farm size.'))
  validate(need(input$Y2011 | input$Y2016 | input$Y2021, 'Please choose at least one year.'))
  validate(need((!is.null(data[[question]]) & input$Y2021) |
                  (!is.null(data2016[[question]]) & input$Y2016) |
                  (!is.null(data2011[[question]]) & input$Y2011)
                , 'Data not available for this question for the year(s) selected'))
  
  
  # Build empty dataframe to add year summary data to
  out <- data_frame(
    "summary" = integer(),
    "summary_low" = integer(),
    "summary_upp" = integer(),
    "n" = integer(),
    "summary_labels" = character(),
    "year" = character()
  )
  
  # Empty color palette
  graph_color <- character()
  
  # Code for 2011 data
  if (!is.null(data2011[[question]]) & input$Y2011) {
    
    # Trim the dataframe down to speed things up
    data2011 <- DataSlim(data2011, c(question,category))
    
    # Remove missing data
    data2011 <- data2011[!is.na(data2011[question]),]
    
    # Filter
    sub2011 <- FilterData(data2011, input)
    
    if (nrow(data2011) >= MINIMUM_N) {
      sub2011 <- sub2011 %>% as_survey_design(id = 1, weight  = weight)
    }
    
    if (nrow(data2011) >= MINIMUM_N) {
      # Create summary data of means and confidence intervals for bins
        
      out2011 <- sub2011 %>% 
        group_by_at(category) %>% 
        summarize(summary = survey_mean(get(question), na.rm=TRUE, vartype = 'ci'), n = unweighted(n())) %>%
        drop_na()
       
      out2011$summary_labels <- paste("n=",out2011$n,sep="")
      out2011$year <- "2011"
      
      # merge with overall summary
      out <- merge(out,out2011, all=TRUE)
      
      # Add color to graph_color
      graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2011)
    }
  }
  
  # Code for 2016 data
  if (!is.null(data2016[[question]]) & input$Y2016) {
    
    data2016 <- DataSlim(data2016, c(question,category))
    
    # Remove missing data
    data2016 <- data2016[!is.na(data2016[question]),]
    
    # Filter
    sub2016 <- FilterData(data2016, input)
    
    if (nrow(data2016) >= MINIMUM_N) {
      sub2016 <- sub2016 %>% as_survey_design(id = 1, weight  = weight)
    }  
    
    if (nrow(data2016) >= MINIMUM_N) {
      # Create summary data of means and confidence intervals for bins

      out2016 <- sub2016 %>% 
        group_by_at(category) %>% 
        summarize(summary = survey_mean(get(question), na.rm=TRUE, vartype = 'ci'), n = unweighted(n())) %>%
        drop_na()
      
      out2016$summary_labels <- paste("n=",out2016$n,sep="")
      out2016$year <- "2016"
      
      # merge with overall summary
      out <- merge(out,out2016, all=TRUE)
      
      # Add color to graph_color
      graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2016)
    }
  }
  
  # Code for 2021 data
  if (!is.null(data[[question]]) & input$Y2021) {
    
    data <- DataSlim(data, c(question,category))
    
    # Remove missing data
    data <- data[!is.na(data[question]),]
    
    # Filter by crop type
    sub2021 <- FilterData(data, input)
    
    if (nrow(data2011) >= MINIMUM_N) {
      sub2021 <- sub2021 %>% as_survey_design(id = 1, weight  = weight)
    }
    
    if (nrow(data) >= MINIMUM_N) {
      # Create summary data of means and confidence intervals for bins
      out2021 <- sub2021 %>% 
        group_by_at(category) %>% 
        summarize(summary = survey_mean(get(question), na.rm=TRUE, vartype = 'ci'), n = unweighted(n())) %>%
        drop_na()
      
      out2021$summary_labels <- paste("n=",out2021$n,sep="")
      out2021$year <- "2021"
      
      # merge with overall summary
      out <- merge(out,out2021, all=TRUE)
      
      # Add color to graph_color
      graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2021)
    }
  }
  
  # Check if there are enough data points to show
  validate(need(nrow(out) >= MINIMUM_N, "Not enough data for plot with current filters"))
  
  # Only show title if title checkbox selected
  title <- ifelse (input$show_title == TRUE, title,"")
  
  # ggplot of means (geom_bar) and error (geom_errorbar) for bins
  p <- ggplot(data = out, aes_string(x = category, y = "summary",  fill = "year")) +
    geom_bar(stat = "identity", color = "black", width = 0.8, position = position_dodge(width=0.9)) +
    geom_errorbar(alpha = 1, mapping = aes_string(category, ymin = "summary_low", ymax = "summary_upp",  fill = "year"), inherit.aes = FALSE, size=.75, width=.1, position = position_dodge(width=0.9)) +
    #    ylim(0,100) +
    geom_shadowtext(aes(label=round(summary,0), vjust=-0.2), color="black", size=LABELSIZE-1, bg.color="white", position = position_dodge(width = 0.9)) +
    theme_linedraw(base_size = BASESIZE) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         x = stringr::str_wrap(str_to_sentence(xlab), width = WRAPWIDTH), y = str_to_sentence(ylab), fill = "Year")  +
    scale_x_discrete(labels = function(x) str_wrap(str_to_sentence(x), width = X_WRAPWIDTH)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    theme(text = element_text(family = FONTTYPE)) +
    theme(legend.title = element_text(family = FONTTYPE)) +
    theme(axis.title = element_text(family = FONTTYPE)) +
    scale_fill_manual(values = graph_color)+
    guides(fill=guide_legend(title="Year"))
  
  # Remove legend if only one year
  if (length(levels(as.factor(out$year))) == 1) {
    p <- p +  theme(legend.position="none")
  }
  
  if (input$show_n) {
    p <- p + geom_text(aes(label=summary_labels, vjust = -2.7), color="black", size=LABELSIZE-4, position = position_dodge(width=0.9))
  }
  
  plot(p)
  
}

CatPlot <- memoise(CatPlot_internal, cache = getShinyOption("cache"))

### function to make plots from pre-built summary tables ----

CannedPlot <- function (input, dataset, question, category, title, xlab, ylab, bar_labels = TRUE, dollar_scale = FALSE) {
  
  # load correct canned dataframe
  # It should have:
  # - a column with the values that has a name that matches the "question" variable
  # - a column with the category factors that has a name that match the "category" variable
  # - a column named years
  
  # dataset <- "ogbycrop"
  # question <- "PercentAcres"
  # category <- "CropType"
  # title <- "Average percent acreage planted to organic seed"
  # xlab <- "Crop type"
  # ylab <- "Average percent organic seed used"
  # input <- list()
  # input$Y2011 <- TRUE
  # input$Y2016 <- TRUE
  # input$Y2021 <- TRUE
  # input$color_palette <- "default"
  # input$show_title <- TRUE
  
 # data <- dataset
  
  # Display while loading
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  data <- get(dataset)
  data[[category]] <- as.factor(data[[category]])
  data$year <- as.factor(data$year)
  
  graph_color <-  c(COLOR_PALETTE[[input$color_palette]]$Y2011,
                    COLOR_PALETTE[[input$color_palette]]$Y2016,
                    COLOR_PALETTE[[input$color_palette]]$Y2021)
  
  # Only show title if title checkbox selected
  title <- ifelse (input$show_title == TRUE, title,"")
  
  # Add column names "question" to make it easier for aes
  data$question <- data[[question]]
  
  p <- ggplot(data = data, aes_string(x = category, y = question,  fill = "year")) +
    geom_bar(stat = "identity", color = "black", width = 0.8, position = position_dodge(width=0.9)) +
    #    ylim(0,100) +
    theme_linedraw(base_size = BASESIZE) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         x = stringr::str_wrap(str_to_sentence(xlab), width = WRAPWIDTH), y = str_to_sentence(ylab), fill = "year")  +
    scale_x_discrete(labels = function(x) str_wrap(x, width = X_WRAPWIDTH)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    theme(text = element_text(family = FONTTYPE)) +
    theme(legend.title = element_text(family = FONTTYPE)) +
    theme(axis.title = element_text(family = FONTTYPE)) +
    scale_fill_manual(values = graph_color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3)))+
    guides(fill=guide_legend(title="Year"))
  
  # Add bar labels if specified 
  if (bar_labels) {
    p <- p + geom_shadowtext(aes(label=round(question,0), vjust=-0.2), color="black", size=LABELSIZE-1, bg.color="white", position = position_dodge(width = 0.9))
  }
  
  # Add dollars if specified
  if (dollar_scale) {
    p <- p + scale_y_continuous(labels=scales::dollar_format())
  }
  
  # Remove legend if only one year
  if (length(levels(as.factor(data$year))) == 1) {
    p <- p +  theme(legend.position="none")
  }
  
  plot(p)
  
}

#CannedPlot <- memoise(CannedPlot_internal, cache = getShinyOption("cache"))

# debugonce(CannedPlot)
# input <- list()
# input$Y2011 <- TRUE
# input$Y2016 <- TRUE
# input$Y2021 <- TRUE
# input$color_palette <- "default"
# input$show_title <- TRUE
#   CannedPlot (input = input, dataset = "ogbycrop", question = "PercentAcres", category = "CropType", title = "Average percent acreage planted to organic seed", xlab = "Crop type", ylab = "Average percent organic seed used")

### Plot for reasons why producers did not use organic seed
NoOGReasonsPlot <- function () {
  
  # Display while loading
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  data <- noogbycrop
  data$Reason <- as.factor(data$Reason)
  data$ProducerType <- as.factor(data$ProducerType)
  
  title <- "Figure 20. Moderate or significant reasons producers \ndid not source organic seed by crop type"
  xlab <- "Reason"
  ylab <- "Percent"
  
  graph_color <-  c("#2d677f",
                    "#6c95a5",
                    "#abc2cc",
                    "#eaf0f2")
  
  
  p <- ggplot(data = data, aes(x = Reason, y = Percent,  fill = ProducerType)) +
    geom_bar(stat = "identity", color = "black", width = 0.8, position = position_dodge(width=0.9)) +
    #    ylim(0,100) +
    theme_linedraw(base_size = BASESIZE-4) +
    labs(title = title)  +
    scale_x_discrete(labels = function(x) str_wrap(x, width = X_WRAPWIDTH)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=45, hjust = 1, size = LABELSIZE+1)) +
    theme(text = element_text(family = FONTTYPE)) +
    theme(legend.title = element_blank()) +
    theme(axis.title = element_text(family = FONTTYPE)) +
    scale_fill_manual(values = graph_color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

    p <- p + geom_shadowtext(aes(label=round(Percent,0), vjust=-0.2), color="black", size=LABELSIZE-4, bg.color="white", position = position_dodge(width = 0.9))
  
  plot(p)
  
}

### Plot for reasons change in organic seed use based on certifier request
CertifierChangeSeedPlot <- function () {
  
  # Display while loading
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  data <- seedusetable
  data$CertifierRequest <- as.factor(data$CertifierRequest)
  data$CropType <- as.factor(data$CropType)
  
  title <- "Figure 2. Organic seed usage based on certifier ratings"
  xlab <- "Crop Type"
  ylab <- "Average percent organic seed used"
  
  graph_color <-  c("#2d677f",
                    "#6c95a5",
                    "#abc2cc",
                    "#eaf0f2")
  
  
  p <- ggplot(data = data, aes(x = CropType, y = Percent,  fill = CertifierRequest)) +
    geom_bar(stat = "identity", color = "black", width = 0.8, position = position_dodge(width=0.9)) +
    #    ylim(0,100) +
    theme_linedraw(base_size = BASESIZE-4) +
    labs(title = title, fill = "Certifer requests extra steps", x = xlab, y = ylab)  +
    scale_x_discrete(labels = function(x) str_wrap(x, width = X_WRAPWIDTH)) + 
    #    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=45, hjust = 1, size = LABELSIZE+1)) +
    theme(text = element_text(family = FONTTYPE)) +
#    theme(legend.title = "Certifer requests extra steps") +
    theme(axis.title = element_text(family = FONTTYPE)) +
    scale_fill_manual(values = graph_color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
  
  p <- p + geom_shadowtext(aes(label=round(Percent,0), vjust=-0.2), color="black", size=LABELSIZE-4, bg.color="white", position = position_dodge(width = 0.9))
  
  plot(p)
  
}

### Plot for USDA funding
USDAPlot <- function () {
  
  # Display while loading
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  data <- usdafunding
  data$Year <- as.factor(data$Year)
  data$Program <- as.factor(data$Program)
  
  title <- "Figure 24. USDA investments in AFRI, OREI, and ORG programs from 2010 to 2022\n(data provided by the National Organic Coalition)"
  xlab <- "Year"
  ylab <- "Funding (millions of dollars)"
  
  graph_color <-  c("#2d677f",
                    "#6c95a5",
                    "#abc2cc",
                    "#eaf0f2")
  
  
  p <- ggplot(data = data, aes(x = Year, y = Funding,  fill = Program)) +
    geom_bar(stat = "identity", color = "black", width = 0.8, position = position_dodge(width=0.9)) +
    #    ylim(0,100) +
    theme_linedraw(base_size = BASESIZE-4) +
    labs(title = title, y = ylab)  +
    scale_x_discrete(labels = function(x) str_wrap(x, width = X_WRAPWIDTH)) + 
    #    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=45, hjust = 1, size = LABELSIZE+1)) +
    theme(text = element_text(family = FONTTYPE)) +
    #    theme(legend.title = "Certifer requests extra steps") +
    theme(axis.title = element_text(family = FONTTYPE)) +
    scale_fill_manual(values = graph_color) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
  
  p <- p + geom_shadowtext(aes(label=round(Funding,0), vjust=-0.2), color="black", size=LABELSIZE-4, bg.color="white", position = position_dodge(width = 0.9))
  
  plot(p)
  
}

### Function to make plots for breeding crops ----

BreedPlot_internal <- function (input, question, title, xlab, ylab) {
  
  # Display while loading
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  # Avoid red error message when nothing is selected for input$state by validating
  validate(need(input$state, 'Please choose at least one state.'))   
  validate(need(input$crop_size, 'Please choose a farm size.'))
  validate(need(input$Y2011 | input$Y2016 | input$Y2021, 'Please choose at least one year.'))
  validate(need((!is.null(data[["BreedCatA"]]) & input$Y2021) |
                  (!is.null(data2016[["BreedCatA"]]) & input$Y2016) |
                  (!is.null(data2011[["BreedCatA"]]) & input$Y2011)
                , 'Data not available for this question for the year(s) selected'))
  
  
  # Build empty dataframe to add year summary data to
  out <- data_frame(
    "summary" = integer(),
    "summary_low" = integer(),
    "summary_upp" = integer(),
    "n" = integer(),
    "summary_labels" = character(),
    "year" = character()
  )
  
  # Empty color palette
  graph_color <- character()
  
  # Code for 2011 data
  if (!is.null(data2011[["BreedCatA"]]) & input$Y2011) {
    
    # Filter
    data2011 <- FilterData(data2011, input)
    
  }
  
  # Compile breeding questions
  data2011 <- gather(data2011, Question, Category, BreedCatA, BreedCatB)
  data2011$BreedTop10 <- as.factor(ifelse (data2011$Category %in% names(sort(table(data2011$Category),decreasing = TRUE)[2:11]), data2011$Category , NA))
  
  # remove extra rows that did not have one of the top 10 crops for selected subset
  data2011 <- data2011[!is.na(data2011$BreedTop10),]
  
  if (nrow(data2011) >= MINIMUM_N) {
    
    mydesign2011 <- data2011 %>% as_survey_design(id = 1, weight  = weight)
    
    if (nrow(data2011) >= MINIMUM_N) {
      # Create summary data of means and confidence intervals for bins
      out2011 <- mydesign2011 %>%
        group_by_at(question) %>%
        summarize(summary = survey_mean(vartype = "ci"), n = unweighted(n())) %>%
        mutate(summary = summary * 100, summary_low = summary_low * 100, summary_upp = summary_upp * 100)
      out2011$summary_labels <- paste("n=",out2011$n,sep="")
      out2011$year <- "2011"
      
      # merge with overall summary
      out <- merge(out,out2011, all=TRUE)
      
      # Add color to graph_color
      graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2011)
    }
  }
  
  # Code for 2016 data
  if (!is.null(data2016[["BreedCatA"]]) & input$Y2016) {
    
    # Filter
    data2016 <- FilterData(data2016, input)
    
  }
  
  # Compile breeding questions
  data2016 <- gather(data2016, Question, Category, BreedCatA, BreedCatB)
  data2016$BreedTop10 <- as.factor(ifelse (data2016$Category %in% names(sort(table(data2016$Category),decreasing = TRUE)[2:11]), data2016$Category , NA))
  
  if (nrow(data2016) >= MINIMUM_N) {
    
    # remove extra rows that did not have one of the top 10 crops for selected subset
    data2016 <- data2016[!is.na(data2016$BreedTop10),]
    
    mydesign2016 <- data2016 %>% as_survey_design(id = 1, weight  = weight)
    
    if (nrow(data2016) >= MINIMUM_N) {
      # Create summary data of means and confidence intervals for bins
      out2016 <- mydesign2016 %>%
        group_by_at(question) %>%
        summarize(summary = survey_mean(vartype = "ci"), n = unweighted(n())) %>%
        mutate(summary = summary * 100, summary_low = summary_low * 100, summary_upp = summary_upp * 100)
      out2016$summary_labels <- paste("n=",out2016$n,sep="")
      out2016$year <- "2016"
      
      # merge with overall summary
      out <- merge(out,out2016, all=TRUE)
      
      # Add color to graph_color
      graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2016)
    }
  }
  
  # Code for 2021 data
  if (!is.null(data[["BreedCatA"]]) & input$Y2021) {
    
    # Filter
    data <- FilterData(data, input)
  }
  
  # Compile breeding questions
  data <- gather(data, Question, Category, BreedCatA, BreedCatB)
  data$BreedTop10 <- as.factor(ifelse (data$Category %in% names(sort(table(data$Category),decreasing = TRUE)[2:11]), data$Category , NA))
  
  if (nrow(data) >= MINIMUM_N) {
    
    # remove extra rows that did not have one of the top 10 crops for selected subset
    data <- data[!is.na(data$BreedTop10),]
    
    mydesign <- data %>% as_survey_design(id = 1, weight  = weight)
    
    if (nrow(data) >= MINIMUM_N) {
      # Create summary data of means and confidence intervals for bins
      out2021 <- mydesign %>%
        group_by_at(question) %>%
        summarize(summary = survey_mean(vartype = "ci"), n = unweighted(n())) %>%
        mutate(summary = summary * 100, summary_low = summary_low * 100, summary_upp = summary_upp * 100)
      out2021$summary_labels <- paste("n=",out2021$n,sep="")
      out2021$year <- "2021"
      
      # merge with overall summary
      out <- merge(out,out2021, all=TRUE)
      
      # Add color to graph_color
      graph_color <- c(graph_color, COLOR_PALETTE[[input$color_palette]]$Y2021)
    }
  }
  
  # Check if there are enough data points to show
  validate(need(nrow(out) >= MINIMUM_N, "Not enough data for plot with current filters"))
  
  # Only show title if title checkbox selected
  title <- ifelse (input$show_title == TRUE, title,"")
  
  # ggplot of means (geom_bar) and error (geom_errorbar) for bins
  p <- ggplot(data = out, aes_string(x = question, y = "summary",  fill = "year")) +
    geom_bar(stat = "identity", color = "black", width = 0.8, position = position_dodge(width=0.9)) +
    geom_errorbar(alpha = 1, mapping = aes_string(question, ymin = "summary_low", ymax = "summary_upp",  fill = "year"), inherit.aes = FALSE, size=.75, width=.1, position = position_dodge(width=0.9)) +
    #    ylim(0,100) +
    geom_shadowtext(aes(label=round(summary,0), vjust=-0.2), color="black", size=LABELSIZE-1, bg.color="white", position = position_dodge(width = 0.9)) +
    theme_linedraw(base_size = BASESIZE) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         x = str_to_sentence(xlab), y = str_to_sentence(ylab), fill = "Year")  +
    scale_x_discrete(labels = function(x) str_wrap(str_to_sentence(x), width = WRAPWIDTH)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    theme(text = element_text(family = FONTTYPE)) +
    theme(legend.title = element_text(family = FONTTYPE)) +
    theme(axis.title = element_text(family = FONTTYPE)) +
    scale_fill_manual(values = graph_color)
  
  # Remove legend if only one year
  if (length(levels(as.factor(out$year))) == 1) {
    p <- p +  theme(legend.position="none")
  }
  
  if (input$show_n) {
    p <- p + geom_text(aes(label=summary_labels, vjust = -2.7), color="black", size=LABELSIZE-4, position = position_dodge(width=0.9))
  }
  
  plot(p)
  
}

# External function with caching	
BreedPlot <- memoise(BreedPlot_internal, cache = getShinyOption("cache"))

GradientMap_internal <- function (input, question, title, catlab, year) {
  
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  validate(need(input$state, 'Please choose at least one state.'))   
  
   if (input$state == "All") {
     
     RegionMap(input, question, title, catlab, year)
     
   } else {
     
    StateMap(input, question, title, catlab, year)
     
   }
  
}

StateMap <- function (input, question, title, catlab, year) {
  # dummy data for testing
  # input$state <- c("AZ","CA","WA","OR","NV")
  # question <- "OGVegiSeed"
  # catlab <- "Percent Vegetable Seed Used"
  # input$show_n <- TRUE
  # year="2021"
  # title = "Percent Vegetable Seed Used"
  # input$color_palette<-"default"
  # input$crop_type <- "all"
  # input$crop_size <- "All"
  
  validate(need(input$state, 'Please choose at least one state.'))
  
  if (year == "2011") {
    
    mapdata <- data2011[!is.na(data2011[question]),]
    
  } else if (year == "2016") {
    
    mapdata <- data2016[!is.na(data2016[question]),]
    
  } else if (year == "2021") {
    
    mapdata <- data[!is.na(data[question]),]
    
  }
  
  if (input$state=="All") {
    
    filtered_state_map <- states_map
    filtered_state_names <- state.name
    filtered_data <- mapdata[,c(question,"state")]
    
    # Filter zip code points and state shapes based on input$state
  } else {
    
    filtered_state_map <- states_map[states_map$state %in% input$state,]
    filtered_data <- mapdata[,c(question,"state")]
    filtered_data <- filtered_data[filtered_data$state %in% input$state,]
  }
  
  # Retrieve the states map data and merge with pounds data
  data_sum <- filtered_data %>%
    group_by(state) %>%
    summarise(mean = round(mean(.data[[question]]), digits = 1), n = n())
  seed_map <- left_join(filtered_state_map, data_sum, by = "state")
  
  # Create the map
  p <- ggplot(seed_map, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = mean), color = "grey50") +
    coord_sf(crs = st_crs(seed_map), datum = NA) +
    theme_linedraw(base_size = BASESIZE) +
    theme(
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.text =  element_text(size = rel(0.5)),
      plot.background = element_rect(fill = "white")) +
    labs(title = stringr::str_wrap(title, width = WRAPWIDTH), x = "")
  
  # Add gradient colors
  # Scale white to black if grayscale, otherwise scale white to pallet color for the year
  if (year == "2011") {
    
    p <- p + scale_fill_gradient(low = "#ffffff", high = ifelse (input$color_palette == "grayscale", "#000000", COLOR_PALETTE[[input$color_palette]]$Y2011), na.value = "grey50", name = catlab)
    
  } else if (year == "2016") {
    
    p <- p + scale_fill_gradient(low = "#ffffff", high =  ifelse (input$color_palette == "grayscale", "#000000", COLOR_PALETTE[[input$color_palette]]$Y2016), na.value = "grey50", name = catlab) 
    
  } else if (year == "2021") {
    
    p <- p + scale_fill_gradient(low = "#ffffff", high =  ifelse (input$color_palette == "grayscale", "#000000", COLOR_PALETTE[[input$color_palette]]$Y2021), na.value = "grey50", name = catlab)
    
  }
 
  n_label <- aggregate(cbind(long, lat) ~ state, data=seed_map,
                       FUN=function(x)mean(range(x))) 
  n_label <- left_join(n_label, data_sum, by = "state")
  n_label$mean <- round(n_label$mean, digits = 1)
  
  if (input$show_n) {

    p <- p + theme (axis.title.x = element_text(hjust = 0)) + geom_text(data = n_label, aes(long, lat, group = state, label = paste0(mean," (n=",n,")"))) 
    
  } else {
    
    p <- p + theme (axis.title.x = element_text(hjust = 0)) + geom_text(data = n_label, aes(long, lat, group = state, label = mean)) 
    
  }
  
  #+ labs(x = "Number inside states indicates number of repondents")
  
  plot(p)
}

### Function to make gradient maps  ----
RegionMap <- function (input, question, title, catlab, year) {
  
  # dummy data for testing
  # input <- list()
   # input$state <- c("AZ","CA","WA","OR","NV")
   # input$state <- c("All")
   # question <- "OGVegiSeed"
   # catlab <- "Percent Vegetable Seed Used"
   # input$show_n <- TRUE
   # year="2021"
   # title = "Percent Vegetable Seed Used"
   # input$color_palette<-"default"
   # input$crop_type <- "all"
   # input$crop_size <- "All"
  
  # Display while loading
  validate(need(input$color_palette, 'Please wait, the figure is loading'))
  
  # Temp fix until I can get regional maps to handle subset of states
  validate(need(length(input$state)>0, 'Regional maps are only displayed when all states are selected.'))
  validate(need(!is.null(input$state), 'Regional maps are only displayed when all states are selected.'))
  validate(need(!is.na(input$state), 'Regional maps are only displayed when all states are selected.'))
  validate(need(input$state == "All",'Regional maps are only displayed when all states are selected.'))
  
  validate(need(input$crop_size, 'Please choose a farm size.'))
  
  if (year == "2011") {
    
    mapdata <- FilterData(data2011, input)
    mapdata <- mapdata[!is.na(mapdata[question]),]
    
  } else if (year == "2016") {
    
    mapdata <- FilterData(data2016, input)
    mapdata <- mapdata[!is.na(mapdata[question]),]
    
  } else if (year == "2021") {
    
    mapdata <- FilterData(data, input)
    mapdata <- mapdata[!is.na(mapdata[question]),]
    
  }
  
  # Group by SARE region if all states selected
  
  validate(need(input$state == "All",'Regional maps are only displayed when all states are selected.'))
  
  if (input$state=="All") {
    
    western <- c("CA", "OR", "WA", "ID", "NV", "AZ", "UT", "NM", "CO", "WY", "MT", "AK", "HI")
    northcentral <- c("ND", "SD", "KS", "NE", "MN", "IA", "MO", "WI", "IL", "MI", "IN", "OH")
    southern <- c("TX", "OK", "AR", "LA", "KY", "TN", "MS", "AL", "FL", "GA", "SC", "NC", "VA")
    northeast <- c("WV", "MD", "DE", "NJ", "PA", "NY", "CT", "RI", "MA", "VT", "NH", "ME")
    
    # Adding SARE regions
    mapdata <- mapdata %>%
      mutate(SARERegion = case_when(
        state %in% western ~ "West",
        state %in% northcentral ~ "North Central",
        state %in% southern ~ "South",
        state %in% northeast ~ "Northeast"))
    
    states_map <- states_map  %>%
      mutate(SARERegion = case_when(
        state %in% western ~ "West",
        state %in% northcentral ~ "North Central",
        state %in% southern ~ "South",
        state %in% northeast ~ "Northeast"))
    
    filtered_state_map <- states_map
    filtered_state_names <- state.name
    filtered_data <- mapdata[,c(question,"SARERegion")]
    
    data_sum <- filtered_data %>%
      group_by(SARERegion) %>%
      summarise(mean = mean(.data[[question]]), n = n())
    
    seed_map <- left_join(filtered_state_map, data_sum, by = "SARERegion")
    
    # Otherwise filter zip code points and state shapes based on input$state
  } else {
    
    
    filtered_state_map <- states_map[states_map$state %in% input$state,]
    filtered_data <- mapdata[,c(question,"state")]
    filtered_data <- filtered_data[filtered_data$state %in% input$state,]
  # Retrieve the states map data and merge with pounds data
  data_sum <- filtered_data %>%
    group_by(state) %>%
    summarise(mean = mean(.data[[question]]), n = n())
  seed_map <- left_join(filtered_state_map, data_sum, by = "state")
    
  }
  
  
  
  # Retrieve the states map data and merge with average data
  
  
  # Only show title if title checkbox selected
  title <- ifelse (input$show_title == TRUE, title,"")
  
  # Create the map
  p <- ggplot(seed_map, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = mean), color = "grey50") +
    coord_sf(crs = st_crs(seed_map), datum = NA) +
    theme_linedraw(base_size = BASESIZE) +
    theme(
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.text =  element_text(size = rel(1)),
      plot.background = element_rect(fill = "white")) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH)) +
    theme(text = element_text(family = FONTTYPE)) +
    theme(legend.title = element_text(family = FONTTYPE)) +
    theme(axis.title = element_text(family = FONTTYPE)) +
    theme(legend.key.height = unit(x = LABELSIZE*2, units = "points"))
  
  # Add gradient colors
  # Scale white to black if grayscale, otherwise scale white to pallet color for the year
  # if (year == "2011") {
  #   
  #   p <- p + scale_fill_gradient(low = "#ffffff", high = ifelse (input$color_palette == "grayscale", "#000000", COLOR_PALETTE[[input$color_palette]]$Y2011), na.value = "grey50", name = catlab)
  #   
  # } else if (year == "2016") {
  #   
  #   p <- p + scale_fill_gradient(low = "#ffffff", high =  ifelse (input$color_palette == "grayscale", "#000000", COLOR_PALETTE[[input$color_palette]]$Y2016), na.value = "grey50", name = catlab) 
  #   
  # } else if (year == "2021") {
  # 
  
  # Tweaked this to make all maps same color
    p <- p + scale_fill_gradient(low = "#ffffff", high =  ifelse (input$color_palette == "grayscale", "#000000", COLOR_PALETTE[[input$color_palette]]$Y2021), na.value = "grey50", name = catlab)
    
  #}

  if (input$state=="All") {

    label <- aggregate(cbind(long, lat) ~ SARERegion, data=seed_map,
                       FUN=function(x)mean(range(x)))
    label <- left_join(label, data_sum, by = "SARERegion")

    if (input$show_n) {
      p <- p + theme (axis.title.x = element_text(hjust = 0)) + geom_text(data = label, aes(long, lat, group = SARERegion, label = paste0(round(mean,digits=1),"\n(n=",n,")")), size = 10) + labs(x = "")
    }
    else {
      p <- p + theme (axis.title.x = element_text(hjust = 0)) + geom_text(data = label, aes(long, lat, group = SARERegion, label = round(mean,digits=1)), size = 10) + labs(x = "")
    }

  } else {

    label <- aggregate(cbind(long, lat) ~ state, data=seed_map,
                       FUN=function(x)mean(range(x)))
    label <- left_join(label, data_sum, by = "state")

    if (input$show_n) {
      p <- p + theme (axis.title.x = element_text(hjust = 0)) + geom_text(data = label, aes(long, lat, group = state, label = paste0(round(mean,digits=1),"\n(n=",n,")")), size = 10) + labs(x = "")
    }
    else {
      p <- p + theme (axis.title.x = element_text(hjust = 0)) + geom_text(data = label, aes(long, lat, group = state, label = round(mean,digits=1)), size = 10) + labs(x = "")
    }

    if (input$show_n) {
      n_label <- aggregate(cbind(long, lat) ~ state, data=seed_map,
                           FUN=function(x)mean(range(x)))
      n_label <- left_join(n_label, data_sum, by = "state")
      p <- p + theme (axis.title.x = element_text(hjust = 0)) + geom_text(data = n_label, aes(long, lat, group = state, label = n)) + labs(x = "Number inside states indicates number of repondents")

    }

  }
  
  
  plot(p)
  
}

# External function with caching	
GradientMap <- memoise(GradientMap_internal, cache = getShinyOption("cache"))
