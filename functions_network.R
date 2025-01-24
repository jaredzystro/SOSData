library(ggmap)
MINIMUM_N <- 5
# This belongs somewhere eventually

#validate(need(input$Region, 'Please choose at least one region'))
#validate(need(input$activity, 'Please choose at least one role'))
#validate(need(input$crop_type, 'Please choose at least one crop type'))
input_data_nodes <- function(input){
  
  if (input$region_n == "All") {
    nodes.f <- nodes
  } else {
    nodes.f <- subset(nodes, Region %in% input$region_n)
  }
  
  #  if (input$activity == "All") {
  #  nodes.f <- nodes.f
  #} else {
  #  nodes.f <- subset(nodes.f, role_refined %in% input$activity)
  #}
} 

input_data_edges <- function(input){
  
  if (input$net_type == "All") {
    edges.f <- edges 
  } else {
    edges.f <- subset(edges, type %in% input$net_type)
  }
}



netPlot <- function(input = input){
  
   validate(need(input$region_n, 'Please wait, the figure is loading'))
  # validate(need(input$activity, 'Please choose a seed activity'))
  # validate(need(input$net_type, 'Please choose a connection type'))
  
  nodes.f <- input_data_nodes(input)
  edges.f <- input_data_edges(input)
  
  edges.f <- edges.f %>% 
    filter(to %in% nodes.f$Operation | from %in% nodes.f$Operation) 
  #filter(!(from %in% nodes.f$Operation[nodes.f$Region == "International"])) 
  # This line would to remove the international option from ego list, which should only be 2, but instead I am just removing it from the selection options so that the data summaries are still on
  
  nodes.f <- edges.f %>% 
    select(from, to) %>% 
    pivot_longer(cols = c(from, to), names_to = "link", values_to = "Operation")  %>% 
    select(-link) %>% 
    unique() %>% 
    left_join(nodes)
  
  net <- graph_from_data_frame(edges.f, nodes.f, directed = T)
  
  DEG_TYPE <- "in" #input$degree_type 
  V(net)$node_deg <- log(igraph::degree(net, mode = DEG_TYPE))+1
  # Set the size of no in degree to 0.2
  V(net)$node_deg[V(net)$node_deg == -Inf] <- 0.2
  
  #table(is.na(V(net)$role))
  V(net)$activity <- V(net)$role
  
  deg <- data.frame(deg = as.integer(V(net)$node_deg))
  deg %>% slice_max(deg, n = 3)
  # No longer by size
  # V(net)$label <- ifelse(V(net)$node_indeg > 4, V(net)$name, "")
  
  color_table <- unique(data.frame(
    role = V(net)$role_refined,
    color = V(net)$color,
    order = V(net)$colororder
  )) %>% 
    arrange(order)
  
  ggraph(net, layout = "stress", bbox = 20) +
    geom_edge_link(width = 1, alpha = 0.2, color = "gray50") + 
    geom_node_point(aes(color = factor(V(net)$role_refined, 
                                        levels = color_table$role)),
                    alpha = 0.5,
                    size = V(net)$node_deg) + 
    scale_color_manual(values = color_table$color) +
    #geom_node_text(aes(label = V(net)$label)) +
    #size = V(net)$label.size, 
    #color=V(net)$label.color, repel=T) +
    theme_graph() +
    theme(text= element_text(size=FONTSIZE+6, family=FONTTYPE),
          legend.position = "bottom") +
    labs(color = "") +
    guides(color=guide_legend(nrow=1,byrow=TRUE))
}

netmapPlot <- function(input = input){
  
  validate(need(input$region_n, 'Please wait, the figure is loading'))
  
  net <- graph_from_data_frame(edges, nodes, directed = T)
  
  DEG_TYPE <- "in"
  V(net)$node_deg <- log(igraph::degree(net, mode = DEG_TYPE))+1
  V(net)$node_deg[V(net)$node_deg == -Inf] <- 0.2
  
  V(net)$activity <- V(net)$role
  
  deg <- data.frame(deg = as.integer(V(net)$node_deg))
  
  color_table <- unique(data.frame(
    role = V(net)$role_refined,
    color = V(net)$color
  )) 
  color_table <- color_table %>% 
    arrange(role)
  
  map_na <- map_data(map = "world") %>%
    filter(region %in% c("USA", "Canada"))
  
  el_coords <-
    as_edgelist(net) %>%
    as_tibble() %>%
    full_join(nodes, by = c("V1" = "Operation")) %>%
    full_join(nodes, by = c("V2" = "Operation")) %>%
    drop_na(V1, V2) %>% 
    select(V1, V2, lat.x, long.x, lat.y, long.y)
  
  base_gg <- ggplot() +
    geom_polygon(data = map_na,
                 aes(long, lat, group = group), 
                 show.legend = FALSE,
                 alpha = 0.25,
                 size = 0.25,
                 color = "gray",
                 fill = "white"
    ) +
    geom_segment(data = el_coords, # these are the lines
                 aes(x = long.x, xend = long.y,
                     y = lat.x, yend = lat.y),
                 color = "darkgray",
                 size = 0.25,
                 alpha = 0.25) +
    geom_point(data = nodes, # these are the node long, lat
               aes(long, lat),
               #color = factor(V(net)$role_refined, 
               #                         levels = color_table$role)), 
               size = V(net)$node_deg, 
               color = "#2d677f", 
               alpha = 0.5) +
    #scale_color_manual(values = color_table$color) +
    #ylim(15, 75) + # 80 gives us all of Canada
    xlim(-158, -55) + 
    labs(title = "", color = "") + 
    theme_graph(base_family = FONTTYPE, title_size = FONTSIZE) +
    theme(legend.position = "bottom") +
    guides(color=guide_legend(nrow=2,byrow=TRUE))
  
  base_gg +
    coord_cartesian() +
    coord_map(ylim = c(15, 52.5)) # not including xlim because that keeps international
}


netDesc <- function(input = input, summary.object){
  nodes.d <- input_data_nodes(input)
  edges.d <- input_data_edges(input)
  
  edges.d <- edges.d %>% filter(to %in% nodes.d$Operation | from %in% nodes.d$Operation)
  nodes.d <- edges.d %>% 
    select(from, to) %>% 
    pivot_longer(cols = c(from, to), names_to = "link", values_to = "Operation")  %>% 
    select(-link) %>% 
    unique() %>% 
    left_join(nodes)
  
  net <- graph_from_data_frame(edges.d, nodes.d, directed = T)
  
  # Network summaries
  DEG_TYPE <- "in" #input$degree_type
  summary.deg <- summary(igraph::degree(net, mode = DEG_TYPE))
  summary.deg <- data.frame(
    Statistics = c("Median", "Mean", "Q3", "Max"),
    Value = c(unlist(round(summary.deg[[3]],0)), 
              unlist(round(summary.deg[[4]], 1)),
              unlist(round(summary.deg[[5]], 0)), 
              unlist(round(summary.deg[[6]], 0))))
  assign("summary.deg", summary.deg, envir = .GlobalEnv)
  
  summary.bet <- summary(igraph::betweenness(net, normalized = T))
  summary.bet <- data.frame(
    Minimum = summary.bet[[1]],
    Median = summary.bet[[3]],
    Mean = round(summary.bet[[4]],3),
    Maximum = round(summary.bet[[6]],3))
  assign("summary.bet", summary.bet, envir = .GlobalEnv)
  
  cent <- centralization.degree(net)
  cent <- data.frame(round(cent[[2]],3))[[1]]
  trans <- transitivity(net)
  triad.net <- triad.census(net)
  census.sum <- count_motifs(net)
  tri.sum <- sum(triad.net[c(9, 10, 12, 13:16)])
  tri.prop <- tri.sum/census.sum
  density.net <- edge_density(net)
  
  summary.net <- data.frame(
    Statistics = c("Coordination score", "Cooperation score", "Total # nodes"), #"Density",, "Triangles (%)"
    Value = c(round(cent, 4), 
              #round(density.net, 4),
              #round(tri.sum, 0)#, 
              #round(tri.prop*100, 4)
              round(trans, 3),
              as.integer(length(V(net)))
    ))
  assign("summary.net", summary.net, envir = .GlobalEnv)
  
  # Individual summaries
  
  V(net)$indvl.deg <- round(igraph::degree(net, mode = DEG_TYPE),0)
  indvl.deg <- data.frame(V(net)$name, V(net)$role_refined, V(net)$indvl.deg)
  colnames(indvl.deg) <- c("Operation", "Role", "Degree")
  
  top.10.deg <- indvl.deg %>% arrange(-(Degree)) %>% head(n = 10)
  assign("top.10.deg", top.10.deg, envir = .GlobalEnv)
  
  V(net)$indvl.bet <- round(igraph::betweenness(net, normalized = T), 3)
  indvl.bet <- data.frame(V(net)$name, V(net)$role_refined, V(net)$indvl.bet)
  colnames(indvl.bet) <- c("Operation", "Role", "Betweenness")
  
  top.10.bet <- indvl.bet %>% arrange(-(Betweenness)) %>% head(n = 10)
  assign("top.10.bet", top.10.bet, envir = .GlobalEnv)
  
  #roles <- data.frame(table(V(net)$role_refined))
  roles <- data.frame(role = V(net)$role_refined, deg = V(net)$indvl.deg)
  roles <- roles %>% 
    filter(role != "Other") %>% 
    filter(deg != 0) %>% # doing this because I don't want avg. degree to account for zeros of not-surveyed
    group_by(role) %>% 
    add_count(role) %>% 
    mutate(mean = round(mean(deg), 2)) %>% 
    select(-deg) %>% 
    unique() %>% 
    ungroup() %>% 
    mutate(prop = as.integer(round(n/sum(n)*100))) %>% 
    select(role, n, prop, mean) %>% 
    arrange(desc(mean))
  roles$role[roles$role == "Seed handlers & retailers"] <- "Handlers & retail"
  roles$role[roles$role == "University & government"] <- "University & govt."
  colnames(roles) <- c("Role", "Count of actors in the network", "% of actors in the network", "Avg. number of connections")
  assign("roles", roles, envir = .GlobalEnv)
  
  locations <- data.frame(location = V(net)$Region, deg = V(net)$indvl.deg)
  locations <- locations %>% 
    filter(location != "") %>% 
    filter(deg != 0) %>% # doing this because I don't want avg. degree to account for zeros of not-surveyed
    group_by(location) %>% 
    add_count(location) %>% 
    mutate(mean = round(mean(deg), 2)) %>% 
    select(-deg) %>% 
    unique() %>% 
    ungroup() %>% 
    mutate(prop = as.integer(round(n/sum(n)*100))) %>% 
    select(location, n, prop, mean) %>% 
    arrange(desc(mean))
  colnames(locations) <- c("Location", "Count of actors in the network", "% of actors in the network", "Avg. number of connections")
  assign("locations", locations, envir = .GlobalEnv)
  
  
  print(summary.object)
}