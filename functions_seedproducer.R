# Filtering input data for seed producer
input_data_p <- function(input = input){
  
  if (input$crop_type_p == "All") {
    data.c <- df.p.long %>% select(-crop_type) %>% unique()
  } else {
    data.c <- subset(df.p.long, crop_type %in% input$crop_type_p)
  }
  
  if (nrow(data.c) >= MINIMUM_N) {
    data.c <- data.c
    
    if (input$region_p == "All") {
      data.cr <- data.c %>% select(-Region) %>% unique()
    } else {
      data.cr <- subset(data.c, Region %in% input$region_p)
    } 
  }
  
  if (nrow(data.cr) >= MINIMUM_N) {
    data.cr <- data.cr
    
    if (input$activity_p == "All") {
      data.cra <- data.cr %>% select(-act_refined) %>% unique()
    } else {
      data.cra <- subset(data.cr, act_refined %in% input$activity_p)
    } 
  }
}

# Producer functions ----
## Map plot

mapPlot <- function(input = input, title){
  
  validate(need(input$region_p, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_p]]
  
  plot <- ggplot(us_states, aes(x = x, y = y, 
                                fill = factor(regional_n), group = group,
                                text = paste0("N:", regional_n, "\n",
                                              "Response rate: ", 
                                              round((regional_rr*100), 0), "%"))) +
    geom_polygon(color = "white", size = 0.1) + 
    scale_fill_manual(values = COLOR[3:6]) + 
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         fill = "",
         x = "", y = "") + 
    theme_light(base_size = 10)  +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position="none", # was bottom
      panel.grid = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      axis.text = element_blank(),
      text = element_text(family = FONTTYPE, size = FONTSIZE),
      legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
      axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(orientation = "h", y = -0.2), font = list(size = 10))
  
}

## Crop plot
cropPlot <- function(input = input, title){
  
  validate(need(input$region_p, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_p]]
  
  plot <- df.p.slim %>% 
    dplyr::select(cropcat_field:cropcat_herbs) %>% 
    rename("Field crops" = "cropcat_field",
           "Forage" = "cropcat_forage",
           "Ornamentals" = "cropcat_orn",
           "Potatoes & propogules" = "cropcat_propogules",
           "Small grains" = "cropcat_grain",
           "Vegetables" = "cropcat_veg",
           "Herbs" = "cropcat_herbs") %>% 
    pivot_longer(cols = 1:7, names_to = "crop", values_to = "yes.no") %>% 
    group_by(crop, yes.no) %>% 
    filter(yes.no == T) %>% 
    tally %>% 
    dplyr::select(-yes.no) %>% 
    mutate(prop = (n/nrow(df.p.slim)*100), 0) %>% 
    ggplot(aes(x = reorder(crop, -n), y=prop, 
               text = paste0(prop, "%", "\n",
                             "N: ", n))) + 
    geom_col(fill = COLOR[6]) + 
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% responses", x = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 45, hjust = 1),
          text = element_text(family = FONTTYPE, size = FONTSIZE-2),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))

  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
}

incomePlot <- function(input = input, title){
  
  validate(need(input$region_p, 'Please wait, the figure is loading'))
  
  f.data <- input_data_p(input) 
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_p]]
  
  plot <- f.data %>% 
    select(income20_c) %>% 
    group_by(income20_c) %>% 
    tally %>% 
    mutate(prop = round(n/sum(n)*100, 0)) %>% 
    filter(income20_c != "") %>% 
    mutate(income20_c = factor(income20_c, levels = c("Under $50,000",
                                                      '50,000-$299,999',
                                                      '300,000-$1 million',
                                                      "Over $1 million"))) %>%
    ggplot(aes(x = income20_c, y=prop, 
               text = paste0(prop, "%", "\n",
                             "N: ", n))) + 
    geom_col(fill = COLOR[6]) + 
    theme_light(base_size = 10) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% of respondents", x = "") +
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
  
}

yrsPlot <- function(input = input, title){
  
  validate(need(input$region_p, 'Please wait, the figure is loading'))
  
  f.data <- input_data_p(input) 
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_p]]
  
  plot <- f.data %>% 
    select(yrs_orgseed, yrs_ag) %>% 
    na.omit() %>% 
    dplyr::rename("Seed Production" = "yrs_orgseed",
                  "Farming" = "yrs_ag") %>% 
    pivot_longer(cols = c('Seed Production', Farming), 
                 names_to = "roles", values_to = "yrs") %>% 
    ggplot(aes(x = roles, y = yrs, fill = roles,
               text = "")) + 
    geom_boxplot() +
    #stat_summary(fun = "mean") + 
    scale_fill_manual(values = COLOR[c(3,6)]) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "Years",
         fill = "") +
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5),
           axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
}


## Challenge plot

challPlot <- function(production = T, input = input, title){
  
  validate(need(input$region_p, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_p]]
  
  f.data <- input_data_p(input) 
  
  pchall.df <- f.data %>% dplyr::select(pchall_key:pchall_ow) %>% 
    rename("Identifying varieties with key traits" = "pchall_key",
           "Finding high quality stock seed" = "pchall_stock",
           "Achieving adequate seed yields" = "pchall_yield",
           "Estimating yields" = "pchall_estyield",
           "In-field seed production costs" = "pchall_pcosts",
           "Harvest costs" = "pchall_hcosts",
           "Seed cleaning costs" = "pchall_ccosts",
           "Sourcing appropriate seed harvest equipment" = "pchall_hequip",
           "Sourcing appropriate seed cleaning equipment" = "pchall_cequip",
           "Isolation distances" = "pchall_iso",
           "Soil fertility and crop nutrition" = "pchall_fert",
           "Irrigation and water use" = "pchall_irr",
           "Controlling weeds" = "pchall_weeds",
           "Controlling insect pests" = "pchall_pests",
           "Controlling disease pressure" = "pchall_disease",
           "Managing climatic effects" = "pchall_climate",
           "Adapting to climate change" = "pchall_cc",
           "Managing pollinator habitats" = "pchall_poll",
           "Vernalization for biennial crops" = "pchall_vern",
           "Overwintering for biennial crops" = "pchall_ow") %>% 
    pivot_longer(cols = 1:20, names_to = "challenge", values_to = "scale") %>% 
    group_by(challenge, scale) %>% 
    tally %>% 
    mutate(prop = n/sum(n)*100) %>% 
    mutate(scale = factor(scale, levels = 
                            c("Not applicable",
                              "Not a challenge", 
                              "Somewhat of a challenge", 
                              "Moderate challenge", 
                              "Serious challenge"))) %>% 
    mutate(challenge = factor(challenge))
  
  npchall.df <- f.data %>% dplyr::select(npchall_labor:npchall_ipr) %>% 
    rename("Accessing labor" = "npchall_labor",
           "Accessing land" = "npchall_land",
           "Accessing captial" = "npchall_capital",
           "Managing business activities" = "npchall_bactivity",
           "Farm business planning" = "npchall_bplanning",
           "Developing infrastructure" = "npchall_infas",
           "Finding/developing markets" = "npchall_markets",
           "Managing seed companies contracts" = "npchall_contracts",
           "Costs of organic certification" = "npchall_certcost",
           "Requirements of organic certification" = "npchall_certreq",
           "Recordkeeping for organic certification" = "npchall_certrecords",
           "Contamination from GE crops" = "npchall_gecontam",
           "Relations with other farmers" = "npchall_relations",
           "Managing intellectual property rights" = "npchall_ipr") %>% 
    pivot_longer(cols = 1:14, names_to = "challenge", values_to = "scale") %>% 
    group_by(challenge, scale) %>% 
    tally %>% 
    mutate(prop = n/sum(n)*100) %>% 
    mutate(scale = factor(scale, levels = 
                            c("Not applicable",
                              "Not a challenge", 
                              "Somewhat of a challenge", 
                              "Moderate challenge", 
                              "Serious challenge"))) %>% 
    mutate(challenge = factor(challenge))
  
  assign("production", pchall.df, envir = .GlobalEnv)
  assign("nonproduction", npchall.df, envir = .GlobalEnv)
  
  if(production == T){
    chall.df <- pchall.df}
  else{chall.df <- npchall.df}
  
  
  order <- if(length(chall.df$scale[chall.df$scale == "Serious challenge" | 
                                    chall.df$scale == "Serious challenge"]) == nrow(chall.df)/5){
    chall.df %>% 
      filter(scale == "Serious challenge" | scale == "Moderate challenge") %>% 
      group_by(challenge) %>% 
      mutate(combined_n = sum(n)) %>% 
      ungroup() %>% 
      arrange(desc(-combined_n)) %>% 
      dplyr::select(challenge) %>% 
      unique()
  } else {
    order1 <- chall.df %>% 
      filter(scale == "Serious challenge" | scale == "Moderate challenge") %>% 
      group_by(challenge) %>% 
      mutate(combined_n = sum(n)) %>% 
      ungroup() %>% 
      arrange(desc(-combined_n)) %>% 
      dplyr::select(challenge) %>% 
      unique()
    order2 <- chall.df %>% 
      filter(!(challenge %in% order1$challenge)) %>% 
      filter(scale == "Somewhat of a challenge" | scale == "Not a challenge") %>% 
      group_by(challenge) %>% 
      mutate(combined_n = sum(n)) %>% 
      ungroup() %>% 
      arrange(desc(-combined_n)) %>% 
      dplyr::select(challenge) %>% 
      unique()
    rbind(order2, order1)
  }
  
  
  # Plot
  plot <- ggplot(chall.df, 
                 aes(x = factor(challenge, levels = order$challenge),
                     y = prop, fill = scale,
                     text = paste0(round(prop,0), "%", "\n",
                                   "N: ", n))) + 
    geom_col() + 
    coord_flip() + 
    scale_fill_manual(values = COLOR[2:6]) +
    theme_light() +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% of responses", x = "",
         fill = "") +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
}


iprPlot <- function(input = input, title){
  
  validate(need(input$region_p, 'Please wait, the figure is loading'))
  
  f.data <- input_data_p(input) 
  COLOR <- COLOR_PALETTE_L[[input$color_palette_p]]
  
  ipr.df <- f.data %>% 
    dplyr::select(ipr_mta, ipr_pvp, ipr_pup, ipr_ossi) %>% 
    rename("Material Transfer Agreements" = "ipr_mta",
           "Plant Variety Protection" = "ipr_pvp",
           "Plant Utility Patents" = "ipr_pup",
           "Open Source Seed Initiative" = "ipr_ossi") %>% 
    pivot_longer(c(1:4), names_to = "ipr", values_to = "sentiment") %>% 
    group_by(ipr, sentiment) %>% 
    tally %>% 
    mutate(prop = round(n/sum(n)*100, 0)) %>% 
    filter(sentiment != "") %>% 
    mutate(sentiment = factor(sentiment, levels = c("Unsure",
                                                    "Very helpful", 
                                                    "Somewhat helpful", 
                                                    "Neither harmful nor helpful",
                                                    "Somewhat harmful", 
                                                    "Very harmful")))
  
  order <- if(length(ipr.df$sentiment[ipr.df$sentiment == "Very harmful" | 
                                      ipr.df$sentiment == "Somewhat harmful"]) == nrow(ipr.df)/5){
    ipr.df %>% filter(sentiment == "Very harmful" | 
                        sentiment == "Somewhat harmful") %>% 
      group_by(ipr) %>% 
      mutate(combined_n = sum(n)) %>% 
      ungroup() %>% 
      arrange(desc(-combined_n)) %>% 
      dplyr::select(ipr) %>% 
      unique()
  } else {
    order1 <- ipr.df %>% filter(sentiment == "Very harmful" | 
                                  sentiment == "Somewhat harmful") %>% 
      group_by(ipr) %>% 
      mutate(combined_n = sum(n)) %>% 
      ungroup() %>% 
      arrange(desc(-combined_n)) %>% 
      dplyr::select(ipr) %>% 
      unique()
    order2 <- ipr.df %>% 
      filter(!(ipr %in% order1$ipr)) %>% 
      filter(sentiment == "Very helpful" | sentiment == "Somewhat helpful") %>% 
      group_by(ipr) %>% 
      mutate(combined_n = sum(n)) %>% 
      ungroup() %>% 
      arrange(desc(-combined_n)) %>% 
      dplyr::select(ipr) %>% 
      unique()
    rbind(order2, order1)
  }
  
  # Assigning these levels to the factor
  ipr.df <- ipr.df %>% 
    mutate(ipr = factor(ipr, levels = order$ipr))
  
  # Plot
  plot <- ggplot(ipr.df, aes(x = ipr, y=prop, fill = sentiment,
                             text = paste0(round(prop, 0), "%", "\n",
                                           "N: ", n))) + 
    geom_col() + 
    coord_flip() + 
    scale_fill_manual(values = COLOR[1:6]) +
    theme_light() +
    labs(title = stringr::str_wrap(title, width = WRAPWIDTH),
         y = "% of responses", x = "",
         fill = "") +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text"))
}

cceffPlot <- function(input = input, title){
  
  validate(need(input$region_p, 'Please wait, the figure is loading'))
  
  f.data <- input_data_p(input) 
  COLOR <- COLOR_PALETTE_L[[input$color_palette_p]]
  
  plot <- f.data %>% select(cc_effect) %>% 
    mutate(cc_effect = case_when(
      cc_effect == "Climate change will significantly harm agriculture" ~ "Significantly harm",
      cc_effect == "Climate change will somewhat harm agriculture" ~ "Somewhat harm",
      cc_effect == "Climate change will be neutral" ~ "Neutral",
      cc_effect == "Climate change will somewhat improve agriculture" ~ "Somewhat improve",
      cc_effect == "Climate change will significantly improve agriculture" ~ "Significantly improve")) %>% 
    mutate(cc_effect = factor(cc_effect, c("Significantly harm", 
                                           "Somewhat harm",
                                           "Neutral", 
                                           "Somewhat improve", 
                                           "Significantly improve"))) %>% 
    mutate_all(na_if, "") %>% 
    na.omit() %>% 
    group_by(cc_effect) %>% 
    tally %>% 
    mutate(prop = round(n/sum(n)*100, 0)) %>% 
    ggplot(aes(x = cc_effect, y = prop, text= paste0(prop, "%", "\n",
                                                     "N: ", n))) +
    geom_col(fill = COLOR[6]) +
    theme_light() +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% of responses", x = "",
         fill = "") +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  
  ggplotly(plot, tooltip = c("text"))
}