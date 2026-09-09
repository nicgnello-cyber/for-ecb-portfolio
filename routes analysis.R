library(igraph)

dataset<-read.csv("routes.csv here",na.strings = c("\\N"))  #the original dataset uses \N instead of NA
colnames(dataset)[5]<-"destination.airport"  #the name of this variable is wrong in the original dataset (destination.apirport)
data <- dataset[, c("source.airport", "destination.airport")] #we selecct only the variable that we need
data<-data.frame(data)

url_data <- "https://ourairports.com/data/airports.csv" 
all_aeroports <- read.csv(url_data,na.strings = "")  #load the other dataset
dict <- all_aeroports[, c("iata_code", "continent")]
dict <- dict[dict$iata_code != "", ]       



# Basic properties ####
network<- graph_from_data_frame(d = data, directed = T) #data was used to create a directed graph
ecount(network)  #67663 edges in network ( each edge is a route between two airport )
vcount(network)  #3425 vertices in network ( each vertices is an airport )
is.simple(network) #not simple
is.directed(network) #is directed
is.weighted(network) #is not weighted

#the existence of one-way routes validate the idea of a directed graph
#the existence of such routes can be verified by looking at dyads and reciprocity

census<-dyad_census(network)   #there are 918 asymmetric routes
reciprocity <- reciprocity(network) #not 100% reciprocity


#We will now convert our graph in a simple graph
E(network)$weight <- 1 #initialize the weight for the edges
graph<-simplify(network,edge.attr.comb = list(weight = "sum")) #we now have a weighted graph

ecount(graph)  #We have 37594 (almost half of what we had befoure)
vcount(graph)  #same number of vertices

is.simple(graph) #yes, now we have a simple graph
is.directed(graph) #still directed
is.weighted(graph) #now weighted


# Attributes ####

V(graph)$size <- sqrt(strength(graph))  #Link the dimension of our vertices to the importance if the airport it represents
E(graph)$width <- E(graph)$weight #Link the width of a edge with it's weight ( wide edges represent busy routes )
E(graph)$id <- seq_along(E(graph)) #we assign an id to each route

cut1<-quantile(V(graph)$size,0.99)
cut2<-quantile(V(graph)$size,0.70)
  
V(graph)$shape<-'circle'   #Force all our vertex to be circle
V(graph)$shape<-ifelse(V(graph)$size>cut1,"square","circle") #biggest airport will now be square
V(graph)$label<-ifelse(V(graph)$size>cut2,V(graph)$name,"") #suppress name for small airport


index <- match(V(graph)$name, dict$iata_code)  #assign to each airport the continent
V(graph)$cont <- dict$cont[index]
V(graph)$cont<-as.factor(V(graph)$cont)  
V(graph)$color <- 'grey'
V(graph)$color[V(graph)$cont == 'EU'] <- 'blue'
V(graph)$color[V(graph)$cont == 'NA'] <- 'red'
V(graph)$color[V(graph)$cont == 'AS'] <- 'green'
V(graph)$color[V(graph)$cont == 'AF'] <- 'yellow'
V(graph)$color[V(graph)$cont == 'OC'] <- 'purple'
V(graph)$color[V(graph)$cont == 'SA'] <- 'orange'
#we assign a specific colour to each vertex based on its geographic position


# Plot of complete graph ####

#it might take a lot of time to plot

par(mar = c(0, 0, 0, 0))

l2<-layout_with_fr(graph)
l3<-layout_with_kk(graph)
l4<-layout_with_mds(graph)
plot(graph,layout=l2,vertex.label=NA)
plot(graph,layout=l3,vertex.label=NA)
plot(graph,layout=l4,vertex.label=NA)

#due to the dimension of the graph all the plot are really hard to read
#this is true for all layout
#We will stick to plotting subgraph in future section


par(mar = c(5, 4, 4, 2) + 0.1)

# Analysis on the graph ####

d.graph <-  degree(graph)  #degree of the graph
summary(d.graph)
hist(d.graph, col="lightblue",xlab="Vertex Degree",ylab="Frequency", main="",
     breaks = 100)   
# this section confirm that we most of the airport have really low traffic 
# and a few airports with really high traffic 



par(mfrow = c(1, 2))
hist(degree(graph, mode = "in"),col="lightblue",xlab="Vertex in-Degree",ylab="Frequency", main="",
     breaks = 100)
hist(degree(graph, mode = "out"),col="lightblue",xlab="Vertex out-Degree",ylab="Frequency", main="",
     breaks = 100)
par(mfrow = c(1, 1))
#in and out degree distribution ( we are working with a directed graph)


plot(sort(d.graph))
head(sort(d.graph,decreasing = TRUE),5)

# log-log degree distribution
dd.graph <- degree_distribution(graph)
d <- 1:max(d.graph)-1
ind <- (dd.graph != 0)
plot(d[ind], dd.graph[ind], log="xy", col="blue",
     xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
     main="Log-Log Degree Distribution")

near<-knn(graph)
near<-near$knn
plot(degree(graph), near, 
     log = "xy",
     main = "",
     xlab = "Log-Degree", 
     ylab = "Log Average Neighb. degree",
     col = 'lightblue', 
     pch = 16)
#Positive correlation, assortative mixing


#Strength####
stren<-strength(graph, mode = "all")
par(mfrow = c(1, 2))
hist(stren,col="lightblue",xlab="Strength",ylab="Frequency", main="",
     breaks = 100)
plot(stren,d.graph,type = 'p',col='red',xlab="Strength",ylab="Degree",main='Strength and Degree relation')
par(mfrow = c(1, 1))

#we can compute the strength since we are using a weighted graph
#the strength tell us the same story of the degree

#Centrality####
closen<-closeness(graph,normalized = T)
hist(closen, breaks = 30, col='red',main="Closeness distribution")
head(sort(closen,decreasing = T),5)

betwen<-betweenness(graph,normalized = T)
hist(betwen,breaks = 30, col='blue',main="Betweenness distribution",xlab = 'betweenness')
head(sort(betwen,decreasing = T),5)

central<-eigen_centrality(graph)$vector
hist(central,breaks = 30,col='green',main="Eigenvector centrality", xlab="Centrality")
head(sort(central,decreasing = T),5)


#Hub-and-authority####

hub<-hub_score(graph)$vector  #hub value for each vertex
aut<-authority_score(graph)$vector #authority score for each vertex
V(graph)$hub<-hub 
V(graph)$auth<-aut
vect_hub <- V(graph)$hub
vect_auth <- V(graph)$auth
names(vect_hub) <- V(graph)$name   #get the name of the top hubs
names(vect_auth) <- V(graph)$name  #same for authority



#Comparison of top nodes among all metrics


top_n <- 10

top_deg_names <- names(head(sort(d.graph, decreasing = TRUE), top_n))
top_str_names <- names(head(sort(stren, decreasing = TRUE), top_n))
top_closen_names<-names(head(sort(closen,decreasing = TRUE),top_n))
top_betw_names <- names(head(sort(betwen, decreasing = TRUE), top_n))
top_eigen_names <- names(head(sort(central, decreasing = TRUE), top_n))
top_hub_names <- names(head(sort(vect_hub, decreasing = TRUE), top_n))
top_auth_names <- names(head(sort(vect_auth, decreasing = TRUE), top_n))

all_top_nodes <- unique(c(top_deg_names, top_str_names, top_closen_names,top_betw_names, top_eigen_names, top_hub_names))

comparison_table <- data.frame(
  Node = all_top_nodes,
  Degree = round(d.graph[all_top_nodes], 0),
  Strength = round(stren[all_top_nodes],0),
  Closeness = round(closen[all_top_nodes],4),
  Betweenness = round(betwen[all_top_nodes], 4),
  Eigenvector = round(central[all_top_nodes], 4),
  Hub = round(vect_hub[all_top_nodes], 4),
  Authority = round(vect_auth[all_top_nodes], 4)
)
comparison_table <- comparison_table[order(-comparison_table$Degree), ]
rownames(comparison_table) <- NULL 

print(comparison_table)


# EDGE ####

eb <- edge_betweenness(graph)  #assign betweenness value to each edge in the graph
E(graph)[order(eb, decreasing=T)[1:5]] #5 edges with highest betweenness


edge_density(graph)  #0.00320571, really low


library(dplyr)

edge_data <- E(graph)$weight
head(edge_data)
top10_weights <- sort(edge_data, decreasing = TRUE)[1:10] #top 10 routes by weight
print(top10_weights)


edge_data <- get.data.frame(graph, what = "edges")
vertice_di_interesse <- "IST"
edge_incidenti <- incident(network, vertice_di_interesse)
edge_data_inst<-get.data.frame(edge_incidenti, what = "edges")
top10_routes <- head(edge_data[order(-edge_data$weight), ], 10)
top10_ist<-head(edge_data_inst[order(-edge_data_inst$weight), ], 10)
print(top10_routes)
print(top10_ist)


#####
#CLUSTERING

transitivity(graph)   #clusetering coeff. /global transitivity.
local_clustering<-transitivity(graph, type="local")

#hub are showing low local transitivity since lots of their connection are small
#airports flying to the hub with no connection between them


plot(degree(graph), local_clustering, col="blue",ylab = "Local Transitivity",
     xlab = "Graph Degree", main="Local transitivity vs Degree",type = 'h')


#CLIQUE####

#too heavy
#it was not possible to compute these line of code

#clique.number(graph)
#clique.length <- sapply(cliques(graph), length)  #trova le clique
#table(clique.length)
#table(sapply(max_cliques(graph), length)) #clique massima

#CLIQUE 2##############################
dyad_census(graph)
motifs(graph,3)

#Clique for continents####
#it was possible to find the number of clique for Oceania, South America and Africa

sub <- induced_subgraph(graph,which(V(graph)$cont == "OC"))


clique_num(sub)
clique.length <- sapply(cliques(sub), length)
table(clique.length)
table(sapply(max_cliques(sub), length))


#RECIPROCITY#####
#both measures for reciprocity
reciprocity(graph, mode="default")  
reciprocity(graph, mode="ratio")
#both are really high, the graph is highly reciprocal


#CONNECTIVITY####

is_connected(graph) #FALSE
comps <- decompose(graph) #8 comp ( 1 really big, 7 really small)

sapply(comps, vcount)  #count vertices in each comp., biggest one has 3397 vertices
3397/vcount(graph) # 0.9918248
graph.gc <- comps[[1]]

mean_distance(graph.gc, directed = T) #mean distance in biggest comp, 5.189768, quite short
diameter(graph.gc) #17, longest possible trip
transitivity(graph.gc) #0.2483021, high
#small world effect in biggest comp. of the graph





### Partitioning

sub <- induced_subgraph(graph,which(V(graph)$cont == "SA"))
gc1 <- cluster_fast_greedy(as.undirected(sub))
gc2 <- cluster_edge_betweenness(sub)

len1<-length(gc1)
len2<-length(gc2)
size1<-sizes(gc1)
size2<-sizes(gc2)


print(modularity(gc1))
print(modularity(gc2))
membership(gc1)
membership(gc2)

l<-layout_with_fr(sub)   
par(mar = c(0, 0, 0, 0))
plot(gc2, sub ,vertex.label="",layout=l, edge.arrow.size=0.1)
par(mar = c(5, 4, 4, 2) + 0.1)
dendPlot(gc1)


#EGO GRAPH #####

ego.graph<-function(x="ATL",n=1){

ego.atl <- induced_subgraph(graph, neighborhood(graph , n, x)[[1]])
V(ego.atl)$label <- ifelse(V(ego.atl)$name==x,V(ego.atl)$name,"")
l<-layout_with_kk(ego.atl)
par(mar = c(0, 0, 0, 0))
plot(ego.atl,layout=l,
     vertex.label.dist = 0,   
     vertex.label.cex = 0.7,
     vertex.label.color="black")
par(mar = c(5, 4, 4, 2) + 0.1)
}


ego.graph("HNL",1)