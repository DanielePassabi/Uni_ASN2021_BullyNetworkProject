#################################################################
# Exercise 1
#################################################################

#####################
# Point 1.1
#####################

# Import libraries
library(openxlsx)   # open and read excel files
library(dplyr)      # data manipulation
library(sna)

# Import .xlsx file
bully_xlsx = read.xlsx("data/EXERCISE006.xlsx", sheet = "Bullying", colNames = TRUE)

# Fix the column types (from dbl to int)
bully_xlsx = bully_xlsx %>% 
  mutate_if(is.numeric, as.integer)

# Set the first column as Row Index
bully_xlsx = data.frame(bully_xlsx[,-1], row.names = bully_xlsx[,1])

# Convert the data to a matrix
bully_matrix = as.matrix(bully_xlsx)

# Show first 9 nodes
bully_matrix[1:9,1:9]

# Convert the matrix into a "network object"
bully_network = as.network(bully_matrix, directed = TRUE)
bully_network

# Plot the network
gplot(bully_network, 
      gmode = "digraph",
      
      # layout
      mode = "fruchtermanreingold",
      jitter = F,
      
      # ties
      edge.col = "grey70",
      
      # nodes
      vertex.col = "#59A5D8",
      vertex.cex = 1,
      
      # arrow
      arrowhead.cex = 0.5,
      
      # labels
      displaylabels = T,
      label.pos = 1,
      label.cex = 1
)


#####################
# Point 1.2
#####################

# Import .xlsx attributes data
attributes_xlsx = read.xlsx("data/EXERCISE006.xlsx", sheet = "Attributes", colNames = TRUE)
attributes_xlsx

# Prepare color vector for gender
genderVectorColor = c()
for (g in attributes_xlsx$Gender) {
  if (g == 1) {
    genderVectorColor = append(genderVectorColor, "#63A088")
  } else {
    genderVectorColor = append(genderVectorColor, "#A975A9")
  }
}

# Prepare color vector for ethnicity
ethnicityVectorColor = c()
for (e in attributes_xlsx$Ethnicity) {
  if (e == 1) {
    ethnicityVectorColor = append(ethnicityVectorColor, 50)
  } else if (e == 2) {
    ethnicityVectorColor = append(ethnicityVectorColor, 3)
  } else {
    ethnicityVectorColor = append(ethnicityVectorColor, 4)
  }
}

# Plot the network
plot = gplot(bully_network, 
             gmode = "digraph",
             
             # layout
             mode = "fruchtermanreingold",
             jitter = T,
             
             # ties
             edge.col = "grey70",
             
             # nodes
             vertex.col = genderVectorColor,
             vertex.cex = attributes_xlsx$Grade/5, # size of the vectors represent grade value
             vertex.sides = ethnicityVectorColor,
             
             # arrow
             arrowhead.cex = 0.5,
             
             # labels
             displaylabels = F,
             label.pos = 1,
             label.cex = 1
)


#####################
# Point 1.3
#####################

# Prepare color vector for gender
genderVectorColor = c()
for (g in attributes_xlsx$Gender) {
  if (g == 1) {
    genderVectorColor = append(genderVectorColor, "#63A088")
  } else {
    genderVectorColor = append(genderVectorColor, "#A975A9")
  }
}

# Prepare color vector for ethnicity
ethnicityVectorColor = c()
for (e in attributes_xlsx$Ethnicity) {
  if (e == 1) {
    ethnicityVectorColor = append(ethnicityVectorColor, 50)
  } else if (e == 2) {
    ethnicityVectorColor = append(ethnicityVectorColor, 3)
  } else {
    ethnicityVectorColor = append(ethnicityVectorColor, 4)
  }
}

# Plot the network
gplot(bully_network, 
      gmode = "digraph",
      
      # layout
      mode = "fruchtermanreingold",
      jitter = T,
      
      # ties
      edge.col = "grey70",
      
      # nodes
      vertex.col = genderVectorColor,
      vertex.cex = attributes_xlsx$Grade/5,
      vertex.sides = ethnicityVectorColor,
      
      # arrow
      arrowhead.cex = 0.5,
      
      # labels
      displaylabels = F,
      label.pos = 1,
      label.cex = 1
)

# Customize the legend
legend(
  "topleft",
  
  legend = c(
    "GENDER",
    "Male", 
    "Female", 
    " ", 
    "ETHNICITY", 
    "1st Generation Immigrant (1)", 
    "2nd Generation Immigrant (2)", 
    "Italian (3)", 
    " ", 
    "GRADE", 
    "1", 
    "5",
    "10"),
  
  col = c(
    "white",
    "#63A088",
    "#A975A9",
    "white",
    "white",
    "gray",
    "gray",
    "gray",
    "white",
    "white",
    "gray",
    "gray",
    "gray"
  ), 
  
  bty = "n", 
  pch = c(19,19,19,19, 19,16,17,18,19, 16,16,16,16),
  pt.cex = c(0,2.1,2.1,0, 0,2.1,1.9,2.1,0, 2.1,1,1.55,2.1), 
  cex = 1, 
  text.col = "grey10", 
  horiz = F, 
  inset = c(0)
)



#################################################################
# Exercise 2
#################################################################

#####################
# Point 2.1
#####################

# Set nodes name as id
id = rownames(bully_xlsx)

# Calculate indegree
indegree = sna::degree(bully_network, gmode = "digraph", cmode = "indegree")
# Calculate outdegree
outdegree = sna::degree(bully_network, gmode = "digraph", cmode = "outdegree")
# Calculate freeman centrality
freeman = sna::degree(bully_network, gmode = "digraph", cmode = "freeman")


# Create a dataset to store these information
degree_df = data.frame(id, indegree, outdegree, freeman)
degree_df

# Get the maximum indegree row
degree_df[degree_df$indegree == max(degree_df$indegree),]

# Get the maximum outdegree row
degree_df[degree_df$outdegree == max(degree_df$outdegree),]

# Prepare color vector for gender
genderVectorColor = c()
for (g in attributes_xlsx$Gender) {
  if (g == 1) {
    genderVectorColor = append(genderVectorColor, "#c5e3d7")
  } else {
    genderVectorColor = append(genderVectorColor, "#ebd8eb")
  }
}

# Set darker color for Sara and Francesco nodes
genderVectorColor[which(degree_df$id == "Sara")] = "#A975A9"
genderVectorColor[which(degree_df$id == "Francesco")] = "#63A088"

# Prepare color vector for ethnicity
ethnicityVectorColor = c()
for (e in attributes_xlsx$Ethnicity) {
  if (e == 1) {
    ethnicityVectorColor = append(ethnicityVectorColor, 50)
  } else if (e == 2) {
    ethnicityVectorColor = append(ethnicityVectorColor, 3)
  } else {
    ethnicityVectorColor = append(ethnicityVectorColor, 4)
  }
}

# Prepare label vector for Francesco and Sara's nodes.  
label_vector = c("","","","","","","","Francesco","","","","","","","","","","","","","Sara","","","","")

# Plot the network
gplot(bully_network, 
      gmode = "digraph",
      
      # layout
      mode = "fruchtermanreingold",
      jitter = T,
      
      # ties
      edge.col = "grey70",
      
      # nodes
      vertex.col = genderVectorColor,
      vertex.cex = attributes_xlsx$Grade/5,
      vertex.sides = ethnicityVectorColor,
      
      # arrow
      arrowhead.cex = 0.5,
      
      # labels
      label=label_vector,
      label.pos = 1,
      label.cex = 1
)

# Customize the legend
legend(
  "topleft",
  
  legend = c(
    "GENDER",
    "Male", 
    "Female", 
    " ", 
    "ETHNICITY", 
    "1st Generation Immigrant (1)", 
    "2nd Generation Immigrant (2)", 
    "Italian (3)", 
    " ", 
    "GRADE", 
    "1", 
    "5",
    "10"),
  
  col = c(
    "white",
    "#63A088",
    "#A975A9",
    "white",
    "white",
    "gray",
    "gray",
    "gray",
    "white",
    "white",
    "gray",
    "gray",
    "gray"
  ), 
  
  bty = "n", 
  pch = c(19,19,19,19, 19,16,17,18,19, 16,16,16,16),
  pt.cex = c(0,2.1,2.1,0, 0,2.1,1.9,2.1,0, 2.1,1,1.55,2.1), 
  cex = 1, 
  text.col = "grey10", 
  horiz = F, 
  inset = c(0)
)


#####################
# Point 2.2
#####################

# Add indegree and outdegree values to our dataset
merge_df = merge(attributes_xlsx, degree_df, by.x = "ID", by.y = "id")
merge_df

# Calculate correlation
cor.test(merge_df$Grade, merge_df$indegree)

library(ggplot2)

# Plot the indegree-grade scatterplot
ggplot(merge_df, aes(x=indegree, y=Grade)) + 
  geom_point(size=3) +
  # add regression line
  geom_smooth(method=lm , color="red", se=FALSE)

# Calculate correlation
cor.test(merge_df$Grade, merge_df$outdegree)

# Plot the indegree-grade scatterplot using ggplot2
ggplot(merge_df, aes(x=outdegree, y=Grade)) + 
  geom_point(size=3) +
  geom_smooth(method=lm , color="red", se=FALSE)


#####################
# Point 2.3
#####################

# Set seed in order to get a reproducible random result
set.seed(1)

# Create the number of permutations value (to be easily changed)
n_permutation = 20000

# Build a permutation based approach to test for significance
OUTPUT = matrix(NA, n_permutation,1)
for (k in c(1:n_permutation))
{
  grade_PERM = sample(merge_df$Grade) # grade permutation
  OUTPUT[k,1] = cor(merge_df$indegree, grade_PERM)
}

# Plot the results as an histrogram
hist(OUTPUT, nclass=30, prob=T)
# add upper bounder
abline(v = mean(OUTPUT) + sd(OUTPUT)*1.96, col = "red", lwd = 3) # 0.4012781
# add lower bounder
abline(v = mean(OUTPUT) - sd(OUTPUT)*1.96, col = "red", lwd = 3) # -0.4001141

# Assign the real value of correlation to a variable
corRealValue = cor(merge_df$Grade, merge_df$indegree)

message(
  "% of times that the correlation were >= the found correlation value: ", 
  sum(OUTPUT >= +abs(corRealValue))/n_permutation, "\n",
  "% of times that the correlation were < the found correlation value: ", 
  sum(OUTPUT <= -abs(corRealValue))/n_permutation, "\n",
  "% of times that the correlation were >= or < the found correlation value: ", 
  sum(OUTPUT >= +abs(corRealValue))/n_permutation + sum(OUTPUT <= -abs(corRealValue))/n_permutation
)



#################################################################
# Exercise 3
#################################################################

#####################
# Point 3.1
#####################

# Obtain maximum value of indegree and outdegree
max_indegree = degree_df[degree_df$indegree == max(indegree),]$indegree
max_outdegree = degree_df[degree_df$outdegree == max(outdegree),]$outdegree

# Add columns of indegree and outdegree centralization to df
degree_df$indegree_centralization = max_indegree - degree_df$indegree
degree_df$outdegree_centralization = max_outdegree - degree_df$outdegree

degree_df

# Obtain the values of indegree and outdegree centralization for the network
indegree_centralization_network = sum(degree_df$indegree_centralization)
outdegree_centralization_network = sum(degree_df$outdegree_centralization)

# Normalize the results

# obtain total number of nodes
n = nrow(degree_df)

# calculate maximum possible value of indegree/outdegree centralization for our network
max_indegree_centralization = (n-1)^2
max_outdegree_centralization = (n-1)^2

# obtain normalized values
normalized_indegree_centralization_network = indegree_centralization_network/max_indegree_centralization
normalized_outdegree_centralization_network = outdegree_centralization_network/max_outdegree_centralization

# Show results
message(
  "Indegree centralization: ", indegree_centralization_network, "\n",
  "Maximum indegree centralization: ", max_indegree_centralization, "\n",
  "Normalized indegree centralization: ", round(normalized_indegree_centralization_network, 2), "\n\n",
  
  "Outdegree centralization: ", outdegree_centralization_network, "\n",
  "Maximum outdegree centralization: ", max_outdegree_centralization, "\n",
  "Normalized outdegree centralization: ", round(normalized_outdegree_centralization_network, 2), "\n"
)


#####################
# Point 3.2
#####################

# Get the total number of mutual, asymmetric and null dyads
sna::dyad.census(bully_matrix)

# Arc-based reciprocity
sna::grecip(bully_matrix, measure = "edgewise")

# With IGRAPH
library(igraph)

# Convert in adjency matrix for igraph function
adj_matrix = graph_from_adjacency_matrix(bully_matrix, mode = c("directed"), diag = F)

# Arc-based reciprocity
igraph::reciprocity(adj_matrix)


