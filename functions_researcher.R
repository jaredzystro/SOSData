# Filtering input data for researchers

input_data_r <- function(input = input){
  
  if (input$crop_type_r == "All") {
    data.c <- df.r 
  } else {
    data.c <- subset(df.r.long, crop_type %in% input$crop_type_r)
  }
  
  if (nrow(data.c) >= MINIMUM_N) {
    data.c <- data.c
    
    if (input$region_r == "All") {
      data.cr <- data.c %>% select(-Region) %>% unique()
    } else {
      data.cr <- subset(data.c, Region %in% input$region_r)
    } 
  }
}

researchermapPlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
  plot <- ggplot(us_states_r, aes(x = x, y = y, 
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

# Researcher crop
researchercropPlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
  plot <- df.r %>% 
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
    mutate(prop = n/nrow(df.r)) %>% 
    ggplot(aes(x = reorder(crop, -n), y=prop,
               text = paste0(round(prop*100,0), "%", "\n",
                             "N: ", n))) + 
    geom_col(fill = COLOR[6]) + 
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% of responses", x = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_text(angle = 45, hjust = 1),
          text = element_text(family = FONTTYPE, size = FONTSIZE-2),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
}


# Discipline
disciplinePlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
  disc <- colnames(select(df.r, contains("exp_")))
  
  f.data <- input_data_r(input) 
  
  norg <- df.r %>% 
    filter(survey == "org") %>% nrow()

  
  plot <- f.data %>% 
    filter(survey == "uni") %>% 
    pivot_longer(cols = all_of(disc),
                 names_to = "expert",
                 values_to = "logical") %>% 
    filter(logical == T) %>% 
    group_by(expert) %>% 
    tally() %>% 
    mutate(n = case_when(
      expert == "exp_socialscience" ~ 4,
      T ~ as.double(n)
    )) %>% 
    #select(expert, n) %>% # I don't know why this is suddenly necessary, count is being weird
    filter(expert != "exp_economics") %>% 
    rbind(data.frame(expert = "NGO", n = norg)) %>% 
    mutate(expert = case_when(
      expert == "exp_agronomy" ~ "Agronomy",
      expert == "exp_breeding" ~  "Breeding",
      expert == "exp_ecology" ~ "Ecology",
      expert == "exp_economics" ~ "Economics",
      expert == "exp_genetics" ~ "Genetics",
      expert == "exp_horticulture" ~ "Horticulture",
      expert == "exp_pathology" ~ "Pathology",
      expert == "exp_socialscience" ~ "Social Science",
      expert == "exp_soilscience" ~ "Soil Science",
      expert == "exp_weedscience" ~ "Weed Science",
      T ~ expert)) %>% 
    ungroup() %>% 
    mutate(prop = n/sum(n)) %>% 
    ggplot(aes(x = reorder(expert, -n), y=prop,
               text = paste0(round(prop*100,0), "%", "\n",
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

## Expertise plot
expertisePlot <- function(input = input, title){
  
  validate(need(input$region_p, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
  f.data <- input_data_r(input) 
  
  exp.df <- f.data %>% 
    filter(survey == "uni") %>% 
    dplyr::select(pchall_key:pchall_ow) %>% 
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
           "Manging pollinator habitats" = "pchall_poll",
           "Vernalization for biennial crops" = "pchall_vern",
           "Overwintering for biennial crops" = "pchall_ow") %>% 
    pivot_longer(cols = 1:20, names_to = "challenge", values_to = "scale") %>% 
    group_by(challenge, scale) %>% 
    tally %>% 
    mutate(prop = n/sum(n)*100) %>% 
    mutate(scale = factor(scale, levels = 
                            c("Not applicable",
                              "No expertise", 
                              "Low expertise", 
                              "Moderate expertise", 
                              "Strong expertise"))) %>% 
    mutate(challenge = factor(challenge))
  
  # Creating a vector with the order based on the most challenging
  order <- exp.df %>% filter(scale == "Strong expertise" | 
                               scale == "Moderate expertise") %>% 
    group_by(challenge) %>% 
    mutate(combined_n = sum(n)) %>% 
    ungroup() %>% 
    arrange(desc(-combined_n)) %>% 
    dplyr::select(challenge) %>% 
    unique()
  
  # Assigning these levels to the factor
  exp.df <- exp.df %>% 
    mutate(challenge = factor(challenge, levels = order$challenge))
  
  # Plot
  plot <- ggplot(exp.df, aes(x = challenge, y = prop, fill = scale,
                             text = paste0(round(prop,0), "%", "\n",
                                           "N: ", n))) + 
    geom_col() + 
    coord_flip() + 
    scale_fill_manual(values = COLOR[2:6]) +
    theme_light(base_size = 10) +
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

## Priority plot
priorityPlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
  f.data <- input_data_r(input) 
  
  if(input$crop_type_r == "All"){
    d1 <- df.r %>% 
      pivot_longer(cols = c(crop1_flavor:crop1_weeds, 
                            crop2_flavor:crop2_weeds, crop3_flavor:crop3_weeds),
                   names_to = "priority",
                   values_to = "rank") %>% 
      mutate(priority = tools::toTitleCase(str_remove(priority, "crop[1-3]_"))) %>% 
      filter(rank == "Very important") %>% 
      select(priority, rank, ResponseId)
    #group_by(crop_type, priority) %>% 
  } else {
    d1 <- f.data %>% 
      pivot_longer(cols = c(crop1_flavor:crop1_weeds, 
                            crop2_flavor:crop2_weeds, crop3_flavor:crop3_weeds),
                   names_to = "priority",
                   values_to = "rank") %>% 
      mutate(priority = tools::toTitleCase(str_remove(priority, "crop[1-3]_"))) %>% 
      filter(rank == "Very important") %>% 
      select(priority, rank, crop_type, ResponseId)
  }
  table <- d1 %>% 
    group_by(priority) %>% 
    tally() %>% 
    ungroup() %>% 
    mutate(prop = round(n/nrow(d1)*100)) 
  
  table$priority[table$priority == "Cold"] <- "Cold-hardiness/season extension"
  table$priority[table$priority == "Disease"] <- "Disease resistance/tolerance"
  table$priority[table$priority == "Abiotic"] <- "Abiotic stress resistance" 
  table$priority[table$priority == "Germ"] <- "Germination/seedling vigor"
  table$priority[table$priority == "Mature"] <- "Maturity/earliness"
  table$priority[table$priority == "Nue"] <- "Nutrient use efficiency"
  table$priority[table$priority == "Weeds"] <- "Competitiveness with weeds"
  table$priority[table$priority == "Disease"] <- "Disease resistance/tolerance"
  
  order <- table %>% 
    arrange(prop)
  
  plot <- table %>% 
    ggplot(aes(x = factor(priority, levels = order$priority), y = prop,
               text = paste0(round(prop,0), "%", "\n",
                             "N: ", round(n/9, 0)))) +  
    geom_col(fill = COLOR[6]) + 
    coord_flip() + 
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% of responses",
         x = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
}

## Success plot
successPlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
  f.data <- input_data_r(input)
  
  success.df <- f.data %>% 
    dplyr::select(proj_success_academic:proj_success_equip) %>% 
    rename("Academic publication and/or presentation" = "proj_success_academic",
           "Development of germplasm or plant breeding lines" = "proj_success_germlines",
           "Farmer training or outreach event" = "proj_success_training",
           "New techniques, systems or protocols" = "proj_success_protocols",
           "Non-academic publication and/or product" = "proj_success_nonacademic",
           "Plant variety development" = "proj_success_variety",
           "New equipment or tools" = "proj_success_equip") %>% 
    pivot_longer(cols = 1:7, names_to = "success", values_to = "logical") %>% 
    filter(logical == T) %>% 
    group_by(success) %>% 
    tally %>% 
    mutate(prop = round(n/nrow(f.data)*100)) %>% 
    arrange(prop)
  
  # Plot
  plot <- ggplot(success.df, aes(x = factor(success, success), y=prop,
                                 text = paste0(round(prop,0), "%", "\n",
                                               "N: ", n))) +
    geom_col(fill = COLOR[6]) + 
    coord_flip() + 
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% of responses", x = "",
         fill = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
}

projchallPlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
  f.data <- input_data_r(input)
  
  plot <- f.data %>% 
    dplyr::select(proj_challenge_funding:proj_challenge_ip) %>% 
    rename("Insufficient funding" = "proj_challenge_funding",
           "Limited access to germplasm" = "proj_challenge_germaccess",
           "Limited time and/or staff capacity" = "proj_challenge_capacity",
           "Unexpected environmental conditions" = "proj_challenge_envt",
           "Delays or alterations due to Covid-19" = "proj_challenge_covid",
           "Insufficient access to appropriate field locations" =
             "proj_challenge_fieldlocations",
           "Lack of expertise" = "proj_challenge_expertise",
           "Intellectual property protections/legal challenges" =
             "proj_challenge_ip") %>% 
    pivot_longer(cols = 1:8, names_to = "challenge", values_to = "logical") %>% 
    filter(logical == T) %>% 
    group_by(challenge) %>% 
    tally %>% 
    mutate(prop = round(n/nrow(f.data)*100)) %>% 
    arrange(prop) %>% 
    ggplot(aes(x = factor(challenge, challenge), y=prop,
               text = paste0(round(prop,0), "%", "\n",
                             "N: ", n))) + 
    geom_col(fill = COLOR[6]) + 
    coord_flip() + 
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% of responses", x = "",
         fill = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
}

finishedPlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
  f.data <- input_data_r(input)
  
  plot <- f.data %>% 
    group_by(proj_finished_material) %>% 
    tally() %>% 
    mutate(proj_finished_material = case_when(
      proj_finished_material == "" ~ "Does not develop germplasm\nor plant breeding lines",
      T ~ proj_finished_material
    )) %>% 
    ungroup() %>% 
    mutate(prop = round(100*n/sum(n), 0)) %>% 
    ggplot(aes(x = proj_finished_material, y = prop,
               text = paste0(round(prop,0), "%", "\n",
                             "N: ", n))) + 
    geom_col(fill = COLOR[6]) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% of responses", x = "",
         fill = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
}

mechanismsPlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
  f.data <- input_data_r(input)
  
  plot <- f.data %>% 
    filter(proj_finished_material == "Yes") %>% 
    rename("None" = "proj_material_protections_none",
           "PVP" = "proj_material_protections_pvp",
           "MTA" = "proj_material_protections_mta",
           "Utility" = "proj_material_protections_pup",
           "OSSI" = "proj_material_protections_ossi") %>% 
    pivot_longer(cols = PVP:None,
                 names_to = "protection",
                 values_to = "logical") %>% 
    filter(logical == T) %>% 
    group_by(protection) %>% 
    tally() %>% 
    ungroup() %>% 
    mutate(prop = round(100*n/sum(n), 0)) %>% 
    arrange(prop) %>% 
    ggplot(aes(x = factor(protection, protection), y = prop,
               text = paste0(round(prop,0), "%", "\n",
                             "N: ", n))) + 
    geom_col(fill = COLOR[6]) +
    coord_flip() +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         y = "% of responses", x = "",
         fill = "") +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(family = FONTTYPE, size = FONTSIZE),
          legend.title = element_text(family = FONTTYPE, size = TITLESIZE),
          axis.title = element_text(family = FONTTYPE, size = TITLESIZE))
  
  ggplotly(plot, tooltip = c("text")) %>% 
    layout(legend = list(font = list(size = 10)))
}

researcheriprPlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  f.data <- input_data_r(input) 
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
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

researchercceffPlot <- function(input = input, title){
  
  validate(need(input$region_r, 'Please wait, the figure is loading'))
  
  f.data <- input_data_r(input) 
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_r]]
  
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