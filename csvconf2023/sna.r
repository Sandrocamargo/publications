# Sandro Camargo <sandrocamargo@unipampa.edu.br> and Yanina Bellini <yabellini@gmail.com>
# Social Network Analysis 

# Clear the environment
rm(list=ls())

# Setting Parameters
col = 2 # Set the column to be analyzed 2:authors, 4: countries
mincount = 10 # Filter authors/countries with this minimum amount of contributions. Mincont=0 shows the complete network

# Loading Files 
setwd("~/Documents/bioinformatics/csvconf/")
dados <- read.csv("contributions_2023.csv")

# contributions can be filtered by type or year
#dados  <- dados[dados$contribution_type=='blog post',]
#dados  <- dados[dados$year==2014,]
  
# Loading packages
library(stringr)
library(dplyr)
library(igraph)
library(visNetwork)

print(paste("Missing values in",colnames(dados)[col],"column:",nrow(dados[is.na(dados[,col]),])))
print(paste("Complete values in",colnames(dados)[col],"column:",nrow(dados[!is.na(dados[,col]),])))

# Creating node list which is decreasingly sorted by contributions 
nodes = data.frame(sort(table(dados[,col]), decreasing = TRUE))
nodes$perc = round(100*nodes$Freq/length(dados[!is.na(dados[,col]),col]),2) # computing percentage
nodes$id = 1:nrow(nodes) # Defining ID number
nodes <- nodes[,c(4,1,2,3)] 
colnames(nodes) = c("id","label","value","Percent")
nodes = nodes[nodes$value>=mincount,]
write.table(file=paste(colnames(dados)[col],"-nodes-output.txt",sep=""), nodes, row.names = FALSE, col.names=TRUE, sep=",", quote=FALSE)

# Creating edge list
links <- data.frame()
for (i in 1:(nrow(dados)-1)) {
  for (j in (i+1):(nrow(dados))){
    if (!is.na(dados$title[i])) if (!is.na(dados$title[j])) if (dados$title[i]!=dados$title[j]) break
    if (dados[i,col] %in% nodes$label) if (dados[j,col] %in% nodes$label) links <- rbind(links,cbind(dados[i,col],dados[j,col]))
    print(paste(dados[i,col],",",dados[j,col]))
  }
}

links = links[complete.cases(links),] # Remove NA elements
sort(table(dados[,col]),decreasing=TRUE)
colnames(links) <- c("from","to")
write.table(file="autores.txt", links, row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)

# Ordering columns to group repeated links
for(i in 1:nrow(links))
  #if (!is.na(links$from[i])) if (!is.na(links$to[i])) 
  if (links$from[i]>links$to[i]) {
    tmp <- links$from[i]
    links$from[i] <- links$to[i]
    links$to[i] <- tmp
  }

# Grouping edges and counting
authorsr <- links %>%
  group_by(from,to) %>%
  summarise(weight = n())

authorsr = authorsr[complete.cases(authorsr),]
authorsr$label=""
authorsr$same.dept = FALSE
authorsr$friendship = 0
authorsr$advice = 0
authorsr$label[authorsr$weight>1]=authorsr$weight[authorsr$weight>1]
#colnames(authorsr) <- c("from","to","weight","same.dept","friendship","advice","label")
authorsr = authorsr[order(authorsr[,3], decreasing=TRUE),]

write.table(file=paste(colnames(dados)[col],"-edges-output.txt",sep=""), authorsr, row.names = FALSE, col.names=TRUE, sep=",", quote=FALSE)

routes_igraph <- graph_from_data_frame(d = authorsr,
                                       vertices = NULL,
                                       directed = FALSE)

# Computing node metrics
nodes$deg <- 0
nodes$bc <- 0
nodes$pr <- 0
nodes$com <- 0
pr <- page_rank(routes_igraph, directed=FALSE)
bc <- igraph::betweenness(routes_igraph, directed=FALSE, normalized = TRUE)
deg <- igraph::degree(routes_igraph)

for (i in 1:nrow(nodes)){
  authorsr$to[authorsr$to==nodes$label[i]]=i
  authorsr$from[authorsr$from==nodes$label[i]]=i
}
authorsr = authorsr[order(authorsr[,3], decreasing=TRUE),]
tmp <- make_graph(cbind(as.integer(authorsr$from),as.integer(authorsr$to)))
tmp2 <- cluster_walktrap(tmp)
tmp3 <- membership(tmp2)

for (i in 1:length(tmp3))
  nodes$com[i] = match(tmp3[i], order(table(tmp3), decreasing=TRUE))

for (i in 1:nrow(nodes)) {
  nodes$deg[i]=as.numeric(deg[as.character(nodes$label[i])])
  nodes$bc[i]=round(as.numeric(bc[as.character(nodes$label[i])]),6)
  nodes$pr[i]=round(as.numeric(pr$vector[as.character(nodes$label[i])]),6)
}

write.table(file=paste(colnames(dados)[col],"-nodes-ranking-output.txt",sep=""), nodes[nodes$value>=mincount,c(1,2,3,5,6,7,8)], row.names = FALSE, col.names=TRUE, sep=" & ", quote=FALSE, eol=" \\\\ \n")
write.table(file=paste(colnames(dados)[col],"-links-ranking-output.txt",sep=""), authorsr[authorsr$weight>=mincount,c(1,2,3,4)], row.names = FALSE, col.names=TRUE, sep=" & ", quote=FALSE, eol=" \\\\ \n")

# Viewing the networks
pdf(paste(colnames(dados)[col],"-igraph.pdf",sep=""))
plot(routes_igraph,
     #layout = layout_nicely(routes_igraph), #layout_with_fr(routes_igraph, dim=2, niter=5000), #layout_with_graphopt, #layout_with_fr, #
     mode = "linear",
     vertex.size = 10,
     vertex.label.cex = 0.5,
     edge.label.cex = 0.2,
     edge.arrow.size = 0.5)
dev.off()

visNetwork(nodes[nodes$value>=mincount,], authorsr) %>% 
  visIgraphLayout(layout = "layout_with_sugiyama", physics = TRUE) %>%
  visNodes(size = 2, font=list(size=12), opacity=0.5) %>%
  visEdges(font=list(size=8), color="gray") %>% 
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -200))

# Saving files which can be imported to gephi
write.table(file="sna-authors.csv", nodes[,c(1,2,3,5,6,7,8)], row.names = FALSE, col.names=TRUE, sep=",", quote=FALSE, na="0")
write.table(file="sna-colabs.csv", authorsr[,c(1,2,3,4)], row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE, na="0")

# Generating clusters
coords = layout_with_fr(routes_igraph)
c1 <- cluster_fast_greedy(routes_igraph)
pdf(paste(colnames(dados)[col],"-cluster.pdf",sep=""))
plot(c1, routes_igraph, layout=coords)
dev.off()
pdf(paste(colnames(dados)[col],"-dendrogram.pdf",sep=""))
plot_dendrogram(c1)
dev.off()

library(tidygraph)
library(ggplot2)
library(ggraph)
pdf(paste(colnames(dados)[col],"-circles.pdf",sep=""), width=25, height=10)
ggraph::ggraph(routes_igraph, layout = "linear") + #, circular=TRUE
  ggraph::geom_edge_arc(aes(width = weight, colour = weight), alpha = 0.8) +  # , strength = sign(authorsr$weight-mean(authorsr$weight))
  ggraph::scale_edge_width(range = c(0.01, 2)) +
  ggraph::geom_node_text(aes(label = name), repel=TRUE) + 
  labs(edge_width = "Letters") +
  theme_graph()
dev.off()
dev.off()

pdf(paste(colnames(dados)[col],"-graphopt.pdf",sep=""))
ggraph::ggraph(routes_igraph, layout = "graphopt") + 
  ggraph::geom_node_point() +
  ggraph::geom_edge_link(aes(width = weight, colour = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.1, 1)) +
  ggraph::geom_node_text(aes(label = name), repel = TRUE) + # , colour=c1$membership
  labs(edge_width = "Letters") +
  theme_graph()
dev.off()