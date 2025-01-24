
input_data_f <- function(input = input){
  
  if (input$`1996-2016` & input$`2017-2021`) {
    data.y <- df.f 
  } else if (input$`1996-2016`){
    data.y <- subset(df.f, year_cat == "1996-2016")
  } else if (input$`2017-2021`){
    data.y <- subset(df.f,  year_cat == "2017-2021")
  } else (validate('Please choose at least one time frame'))
  
  if (nrow(data.y) >= MINIMUM_N) {
    data.y <- data.y
    
    if (input$region_f == "All") {
      data.yr <- data.y 
      } else {
      data.yr <- subset(data.y, location %in% input$region_f)
    } 
  }
  
  if (nrow(data.yr) >= MINIMUM_N) {
    data.yr <- data.yr
    
    if (input$crop_f == "All") {
      data.yrc <- data.yr 
    } else {
      data.yrc <- subset(data.yr, proj_crop %in% input$crop_f)
    } 
  }
}

fundingbyYrPlot <- function(input = input, title = ""){
  
  COLOR <- COLOR_PALETTE_L3[[input$color_palette_f]]
  
  f.data <- input_data_f(input) 
  
  f.data %>% 
    ggplot(aes(x = as.factor(year), y = funding_amt)) +
    geom_bar(stat = "identity", width = 0.8, 
             position = position_dodge(width=0.9),
             fill = COLOR[2]) +
    theme_linedraw(base_size = BASESIZE) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         x = "Year", 
         y = "Dollars")  +
    scale_x_discrete(labels = function(x) str_wrap(x, width = X_WRAPWIDTH)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    theme(text = element_text(family = FONTTYPE)) +
    theme(legend.title = element_text(family = FONTTYPE)) +
    theme(axis.title = element_text(family = FONTTYPE)) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.3)))
}

fundingbyXPlot <- function(input = input, x, xlab, title, bar_labels = TRUE, dollar_scale = TRUE){
  
  COLOR <- COLOR_PALETTE_L[[input$color_palette_f]]
  
  f.data <- input_data_f(input) 
  
  f.data$x <- f.data[[x]]

  f.data %>% 
    ggplot(aes(x, y = funding_amt, fill = year_cat)) +
    geom_bar(stat = "identity", width = 0.8, # color = "black",
             position = position_dodge(width=0.9)) +
    theme_linedraw(base_size = BASESIZE) +
    labs(title = stringr::str_wrap(str_to_sentence(title), width = WRAPWIDTH),
         x = stringr::str_wrap(str_to_sentence(xlab), width = WRAPWIDTH), 
         y = str_to_sentence("Dollars"), fill = "year")  +
    scale_x_discrete(labels = function(x) str_wrap(x, width = X_WRAPWIDTH)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle=45, hjust = 1)) +
    theme(text = element_text(family = FONTTYPE)) +
    theme(legend.title = element_text(family = FONTTYPE)) +
    theme(axis.title = element_text(family = FONTTYPE)) +
    scale_fill_manual(values = COLOR[c(2,3)]) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.3))) +
    guides(fill=guide_legend(title="Year"))
}



