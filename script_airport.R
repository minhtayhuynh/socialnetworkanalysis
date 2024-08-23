#install.packages("tidyverse")
#install.packages("tidytext") Be careful, ggplot2 does not work well with maps
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("tidyr")
#install.packages("igraph")
#install.packages('maps')
#install.packages('geosphere')
#install.packages ('mapdata')

library(mapdata)
library(maps)
library(geosphere)
library(igraph)



#loading main dataset
data <- read.csv("E:/SUMMER SCHOOL AACHEN/SOCIAL NETWORK ANALYSIS/FINAL DATA ANALYSIS/finaldata.csv")
head(data)
data <- na.omit(data) #this function seems to delete all data from the UK
threshold <- 400  # PEOPLE, THIS IS TO SET THE THRESHOLD FOR THE FREQUENCY
filtered_data <- data[data$Frequency > threshold, ]




# Set up the plotting parameters
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))

# Plot the map of specific countries in Europe and Asia
plot_map <- function() {
  # List of countries in Europe and Asia
  european_countries <- c(
    "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", 
    "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
    "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", 
    "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", 
    "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", 
    "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", 
    "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", 
    "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", 
    "United Kingdom", "Vatican City")
  asian_countries <- c(
    "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", 
    "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", 
    "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", 
    "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", 
    "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", 
    "Philippines", "Qatar", "Russia", "Saudi Arabia", "Singapore", "South Korea", 
    "Sri Lanka", "Syria", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", 
    "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen")
  
  # Combine lists
  countries <- c(european_countries, asian_countries)
  
  # Plot the countries
  map("world", regions = countries, fill = TRUE, col = "lightblue", bg = "gray90", lwd = 0.1)
  }

# Call the function to plot the map

plot_map()

# Define colors for edges with transparency
col.1 <- adjustcolor("orange red", alpha = 0.4)
col.2 <- adjustcolor("orange", alpha = 0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)


#Plot the network 

line_color <- "orange"
line_width <- 1

xlim <- c(-30, 180)  # Longitudes that cover Asia and Europe
ylim <- c(-30, 85)   # Latitudes that cover Asia and Europe


# This function plots all the nodes and edges from the original dataset (data)
for(i in 1:nrow(data)) {
  
  # Extract the coordinates for the departure and arrival airports
  departure_coords <- c(data[i,]$departure_longitude, data[i,]$departure_latitude)
  arrival_coords <- c(data[i,]$arrival_longitude, data[i,]$arrival_latitude)
  
  # Check if the coordinates are valid (i.e., not NA)
  if (!all(is.na(departure_coords)) && !all(is.na(arrival_coords))) {
    
    # Draw a straight line between the departure and arrival airports
    lines(x = c(departure_coords[1], arrival_coords[1]), 
          y = c(departure_coords[2], arrival_coords[2]), 
          col = line_color, lwd = line_width)
  } else {
    cat("Invalid coordinates for row", i, "\n")
  }
}

dev.off() # This is to remove the map we just generated, we need to regenerate the map
################## CALCULATIONS

edges <- data.frame(
  from = filtered_data$departureAirport, 
  to = filtered_data$arrivalAirport
)

# Step 2: Create a network graph
Network <- graph_from_data_frame(edges, directed = TRUE)

# Step 3: Calculate network centrality measures

# 3a. Degree Centrality (all nodes)
degree_centrality <- degree(Network, mode = "all")

# 3b. Closeness Centrality (normalized)
closeness_centrality <- closeness(Network, mode = "all", weights = NA, normalized = TRUE)

# 3c. Betweenness Centrality (normalized)
betweenness_centrality <- betweenness(Network, directed = FALSE, weights = NA, normalized = TRUE)

# 3d. PageRank (damping = 0.85)
pagerank_85 <- page_rank(Network, damping = 0.85)$vector

# 3e. PageRank (damping = 0.1)
pagerank_10 <- page_rank(Network, damping = 0.1)$vector

# Print the centrality measures
cat("Degree Centrality:\n", degree_centrality, "\n\n")
cat("Closeness Centrality:\n", closeness_centrality, "\n\n")
cat("Betweenness Centrality:\n", betweenness_centrality, "\n\n")
cat("PageRank (damping = 0.85):\n", pagerank_85, "\n\n")
cat("PageRank (damping = 0.1):\n", pagerank_10, "\n\n")



#THIS SECTION HIGHLIGHTS THE DGREES AND LAYOUTS BUT FOR THE FILTERED DATASET
# Step 1: Create the edge list
edges <- data.frame(
  from = filtered_data$departureAirport, 
  to = filtered_data$arrivalAirport
)

# Step 2: Create the network graph
Network <- graph_from_data_frame(edges, directed = TRUE)

# Step 3: Calculate Degree Centrality
degree_centrality <- degree(Network, mode = "all")

# Step 4: Map Degree Centrality to Airports
# Assume we have a dataframe 'airport_coords' with columns 'airport_code', 'latitude', 'longitude'
# containing unique airport codes and their corresponding coordinates

# Merge degree centrality with the airport coordinates
airport_coords <- data.frame(
  airport_code = V(Network)$name,
  degree = degree_centrality
)

# Assuming we have a dataframe 'data' with coordinates for each airport
# Merge the degree centrality with the coordinates
filtered_data$degree <- degree_centrality[match(filtered_data$departureAirport, V(Network)$name)]

# Step 5: Plot the Map again, people
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))

# Plot the map of specific countries in Europe and Asia
plot_map <- function() {
  # List of countries in Europe and Asia
  european_countries <- c(
    "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", 
    "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
    "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", 
    "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", 
    "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", 
    "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", 
    "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", 
    "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", 
    "United Kingdom", "Vatican City")
  asian_countries <- c(
    "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", 
    "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", 
    "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", 
    "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", 
    "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", 
    "Philippines", "Qatar", "Russia", "Saudi Arabia", "Singapore", "South Korea", 
    "Sri Lanka", "Syria", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", 
    "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen")
  
  # Combine lists
  countries <- c(european_countries, asian_countries)
  
  # Plot the countries
  map("world", regions = countries, fill = TRUE, col = "lightblue", bg = "gray90", lwd = 0.1)
}

# Call the function to plot the map
plot_map()


# Step 6: Plot Airports with Size Based on Degree Centrality


for (i in 1:nrow(filtered_data)) {
  arc <- gcIntermediate(
    c(filtered_data$departure_longitude[i], filtered_data$departure_latitude[i]), 
    c(filtered_data$arrival_longitude[i], filtered_data$arrival_latitude[i]),
    n = 100, addStartEnd = TRUE
  )
  lines(arc, col = "red", lwd = 1)  # Plot each route in blue
}


colors <- rainbow(length(unique(filtered_data$degree)))[as.numeric(cut(filtered_data$degree, breaks=length(unique(filtered_data$degree))))]

# Plot the airports with colors based on degree centrality
points(
  x = filtered_data$departure_longitude, 
  y = filtered_data$departure_latitude, 
  pch = 12, 
  cex = 2,
  col = colors
)

points(
  x = filtered_data$arrival_longitude, 
  y = filtered_data$arrival_latitude, 
  pch = 12, 
  cex = 2,  
  col = colors
)



dev.off() #remove map, we need to regenerate the map
##############################
#COMMUNITY DETECTION, MATES

library(RColorBrewer)

g <- graph_from_data_frame(edges, directed = FALSE)

# Remove multi-edges and self-loops
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)

# Perform community detection using edge betweenness
communities <- cluster_edge_betweenness(g)

# Add community information to the data
membership <- membership(communities)
filtered_data$communities <- membership[match(filtered_data$departureAirport, names(membership))]

# Define colors for communities
community_colors <- rainbow(max(membership) + 1)

custom_labels <- c(
  "Northern Europe Hub",
  "Central Asia Network",
  "Southern Europe Connections",
  "East Asia Cluster",
  "Western Asia Link"
)

# Assuming community IDs are 1, 2, 3, 4, 5 respectively

# Calculate centroids for each community
library(dplyr)

community_centroids <- filtered_data %>%
  group_by(communities) %>%
  summarise(
    centroid_longitude = mean(c(departure_longitude, arrival_longitude), na.rm = TRUE),
    centroid_latitude = mean(c(departure_latitude, arrival_latitude), na.rm = TRUE)
  )



####### AGAIN THE STUPID MAP
par(mfrow = c(1, 1), mar = c(4, 4, 2, 2))

# Plot the map of specific countries in Europe and Asia
plot_map <- function() {
  # List of countries in Europe and Asia
  european_countries <- c(
    "Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", 
    "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
    "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany", "Greece", 
    "Hungary", "Iceland", "Ireland", "Italy", "Kazakhstan", "Kosovo", "Latvia", 
    "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", 
    "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", 
    "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", 
    "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "Ukraine", 
    "United Kingdom", "Vatican City")
  asian_countries <- c(
    "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", 
    "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", 
    "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", 
    "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", 
    "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", 
    "Philippines", "Qatar", "Russia", "Saudi Arabia", "Singapore", "South Korea", 
    "Sri Lanka", "Syria", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", 
    "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen")
  
  # Combine lists
  countries <- c(european_countries, asian_countries)
  
  # Plot the countries
  map("world", regions = countries, fill = TRUE, col = "lightblue", bg = "gray90", lwd = 0.1)
}

# Call the function to plot the map
plot_map()



#### Plot our communities

for (i in 1:nrow(filtered_data)) {
  departure_coords <- c(filtered_data$departure_longitude[i], filtered_data$departure_latitude[i])
  arrival_coords <- c(filtered_data$arrival_longitude[i], filtered_data$arrival_latitude[i])
  
  if (all(!is.na(departure_coords)) && all(!is.na(arrival_coords))) {
    arc <- tryCatch(
      gcIntermediate(departure_coords, arrival_coords, n = 1000, addStartEnd = TRUE),
      error = function(e) NULL
    )
    
    if (!is.null(arc)) {
      lines(arc, col = community_colors[filtered_data$communities[i]], lwd = 0.5)  # Color by community
    } else {
      cat("Error computing route for row", i, "\n")
    }
  } else {
    cat("Invalid coordinates for row", i, "\n")
  }
}



# Plot points for departure airports

points(
  x = filtered_data$departure_longitude, 
  y = filtered_data$departure_latitude, 
  pch = 16, 
  cex =  sqrt(filtered_data$degree) / 2, 
  col = community_colors[filtered_data$communities]
)

# Plot points for arrival airports
points(
  x = filtered_data$arrival_longitude, 
  y = filtered_data$arrival_latitude, 
  pch = 16, 
  cex =  sqrt(filtered_data$degree) / 2, 
  col = community_colors[filtered_data$communities]
)


# Add a legend

custom_labels <- c(
  "South-Central Asia - Europe Network",
  "East Asia - Europe Hub",
  "Southern Europe Connections",
  "Southeast Asia Cluster",
  "Western Asia Link",
  "Other")



# Add a legend to the map
legend("topright", 
       legend = custom_labels, 
       col = community_colors, 
       pch = 16, 
       cex = 0.8)


