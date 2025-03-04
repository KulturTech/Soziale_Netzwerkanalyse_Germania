# Lade die igraph Bibliothek
library(igraph)
library(visNetwork)


# Lade die Kanten- und Knoten-Tabellen
edges <- read.csv("Daten/edges1.csv")
nodes <- read.csv("Daten/nodes1.csv")

# Erstelle den Graphen mit den Kanten
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

# Plot des Netzwerks
plot(g, main="Netzwerk-Visualisierung", vertex.label=V(g)$label)

# Grundlegende Netzwerkmetriken:
# 1. Grad jedes Knotens
deg <- degree(g)
cat("Grad jedes Knotens:\n")
print(deg)

# 2. Durchschnittlicher Grad
avg_deg <- mean(deg)
cat("Durchschnittlicher Grad: ", avg_deg, "\n")

# 3. Clustering-Koeffizient (lokale Vernetzung)
clust_coeff <- transitivity(g, type="local")
cat("Clustering-Koeffizient für jeden Knoten:\n")
print(clust_coeff)

# 4. Durchschnittlicher Clustering-Koeffizient
avg_clust_coeff <- transitivity(g, type="global")
cat("Durchschnittlicher Clustering-Koeffizient: ", avg_clust_coeff, "\n")

# 5. Durchschnittliche Pfadlänge
avg_path_length <- mean_distance(g)
cat("Durchschnittliche Pfadlänge: ", avg_path_length, "\n")

# 6. Durchmesser des Netzwerks
diameter_val <- diameter(g)
cat("Durchmesser des Netzwerks: ", diameter_val, "\n")

# 7. Zentrale Knoten (z.B. betweenness centrality)
betweenness_centrality <- betweenness(g)
cat("Zwischenzentralität jedes Knotens:\n")
print(betweenness_centrality)

# Visualisierung der zentralen Knoten (z.B. nach betweenness)
plot(g, vertex.size=betweenness_centrality * 10, main="Netzwerk mit Zentralität")

library(visNetwork)
plot(g, 
     main="Netzwerk-Visualisierung", 
     vertex.label = V(g)$label,  # Zeige Knotenlabel an
     vertex.size = 10,           # Größe der Knoten
     vertex.color = "lightblue", # Farbe der Knoten
     edge.arrow.size = 0.5,      # Größe der Pfeile (bei gerichteten Netzwerken)
     edge.color = "grey",        # Farbe der Kanten
     edge.width = 1,             # Breite der Kanten
     layout = layout_with_fr)    # Layout der Knoten (Fruchterman-Reingold Layout)



betweenness_centrality <- betweenness(g)
# Plot mit Hervorhebung zentraler Knoten
plot(g, 
     vertex.label = V(g)$label,
     vertex.size = betweenness_centrality * 10,  # Größe basierend auf Zentralität
     vertex.color = ifelse(betweenness_centrality > 1, "red", "lightblue"), # Hervorhebung zentraler Knoten
     edge.color = "grey",
     edge.width = 1,
     layout = layout_randomly)  


eigenvector_centrality <- eigen_centrality(g)$vector

# Verwende Eigenvector-Zentralität für Knoten-Größe
# Knoten mit hoher Eigenvector-Zentralität werden größer dargestellt
plot(g, 
     main="Gerichtetes Netzwerk mit Eigenvector-Zentralität", 
     vertex.label = V(g)$label,  # Knotenlabel anzeigen
     vertex.size = eigenvector_centrality * 50,  # Knoten-Größe basierend auf Eigenvector-Zentralität
     vertex.color = "lightblue",  # Knotenfarbe
     edge.arrow.size = 1.5,       # Größe der Pfeile
     edge.color = "yellow",         # Kantenfarbe
     edge.width = 2,              # Kantenbreite
     layout = layout_randomly)     # Layout (Fruchterman-Reingold)


plot(g, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
     vertex.color=c( "pink", "skyblue")[1+(V(g)$media.type=="1")] )

#vereinfachen des Plots
g <- simplify(g, remove.multiple=T, remove.loops = T, 
                 edge.attr.comb = c(weight="sum", type="ignore")
                 )

plot(g, vertex.label.dist=1.5)

plot(g, edge.arrow.size=.4,vertex.label=NA)

degree(g, mode = "all") #Wie viele Kanten pro Knoten?
closeness(g, weights = NULL) # Berechnet die Nähezentralität der jeweiligen Knoten

betweenness(g)

edge_betweenness(g)
eigen_centrality(g)$vector

cent <- list(Degree=degree(g), 
             Closeness=closeness(g), 
             Betweenness=betweenness(g), 
             Eigenvector=eigen_centrality(g)$vector, 
             `PageRank`=page_rank(g)$vector)

cent #Anzeige von cent

plot(degree_distribution(g, mode="in"), log="xy") #  
plot(degree_distribution(g, mode="in"), log="x") # 
plot(degree_distribution(g, mode="in"), log="y") 
plot(degree_distribution(g, mode="in"), ylog=TRUE, xlog=TRUE) # 

plot(degree_distribution(g, mode="out"), log="xy") # 

degree <- c(max(degree(g, mode="in")))
degree(g, mode="out") #maximale Anzahl eingehender Beziehungen an einem Knoten: 7 
max(degree(g, mode="all"))
  #maximale Anzahl aller Beziehungen an einem Knoten: 13 
degree

graph.density(g) # Gibt die Chance von ca. 18.38% wieder, dass diese BEziehungen im Netz auch realisiert sind 
is_directed(g) # Ja, das Netzwerk ist gerichtet, wie die Plots bereits zeigten

{r exporting data}
#exporting data set into working directory 
write.csv2(cent, file = "cent.csv") # wir schreiben uns die centrality-Werte in eine csv-Datei, für jeden Knoten werden diese dann spaltenweise abgetragen.

# welche Angaben enthält die Datei?

#exporting data set into working directory # dasselbe nochmal, nur mit den anderen Dateien 
edges <- as_data_frame(g, what="edges") 
nodes <- as_data_frame(g, what="nodes")
write.csv2(edges, file = "edges1.csv") 
write.csv2(nodes, file = "nodes1.csv")

E(g)$width <- E(g)$weight/6 #Breite der Kanten nach Gewicht 
plot(g) # jetzt sind aber die Pfeile zu groß 
#change arrow size and edge color: 
E(g)$arrow.size <- .2 
E(g))$edge.color <- "gray80"
plot(g)

plot(g) 
legend(x=-1.1, y=-1.1, c("Leibbursche","Kommunikation", "Leibfux")) # funktioniert NICHT RICHTIG 
pch=21, col="#777777", pt.bg="gray80", pt.cex=2.5, bty="n", ncol=3)

# Kürzester Pfad zwischen zwei Knoten (z. B. "A" und "D")
start_node <- "1"
end_node <- "161"

shortest_path <- shortest_paths(g, from = start_node, to = end_node, output = "vpath")$vpath[[1]]

# Farben für Knoten und Kanten festlegen
vertex_colors <- ifelse(V(g)$name %in% names(shortest_path), "lightgreen", "lightblue")
edge_colors <- ifelse(apply(get.data.frame(g, what = "edges"), 1, function(edge) {
  all(edge %in% names(shortest_path))
}), "red", "grey")

# Graph visualisieren
plot(g, 
     main = paste("Kürzester Pfad von", start_node, "zu", end_node), 
     vertex.color = vertex_colors, 
     edge.color = edge_colors, 
     edge.arrow.size = 1.5, 
     vertex.size = 10, 
     layout = layout_randomly)

# Definierter Pfad (z. B. "A" -> "C" -> "D")
custom_path <- c("3", "42", "45", "65", "68", "73","76", "80", "103", "113", "123")

# Farben für Knoten und Kanten entsprechend dem definierten Pfad
vertex_colors <- ifelse(V(g)$name %in% custom_path, "yellow", "lightblue")
edge_colors <- ifelse(apply(get.data.frame(g, what = "edges"), 1, function(edge) {
  paste(edge[1], edge[2]) %in% paste(head(custom_path, -1), tail(custom_path, -1))
}), "orange", "grey")

# Visualisiere den Graphen
plot(g, 
     main = paste("Definierter Pfad:", paste(custom_path, collapse = " -> ")), 
     vertex.color = vertex_colors, 
     edge.color = edge_colors, 
     edge.arrow.size = 1.5, 
     vertex.size = 15, 
     layout = layout_randomly)

eigenvector_centrality <- eigen_centrality(g)$vector

# Verwende Eigenvector-Zentralität für Knoten-Größe
# Knoten mit hoher Eigenvector-Zentralität werden größer dargestellt
plot(g, 
     main="Gerichtetes Netzwerk mit Eigenvector-Zentralität", 
     vertex.label = V(g)$label,  # Knotenlabel anzeigen
     vertex.size = eigenvector_centrality * 50,  # Knoten-Größe basierend auf Eigenvector-Zentralität
     vertex.color = "lightblue",  # Knotenfarbe
     edge.arrow.size = 1.5,       # Größe der Pfeile
     edge.color = "black",         # Kantenfarbe
     edge.width = 2,              # Kantenbreite
     layout = layout_randomly)   

# 'Name' oder 'Label' aus der nodes-Tabelle setzen
V(g)$label <- nodes$Name  # Wenn die Spalte 'Name' heißt
# Oder, falls die Spalte 'Label' heißt:
# V(g)$label <- nodes$Label

# Plot mit Knotenlabels
plot(g,
     vertex.label = V(g)$label,  # Labels aus der Name/Label-Spalte
     vertex.label.cex = 1.0,     # Schriftgröße der Labels
     vertex.label.color = "black",  # Farbe der Labels
     vertex.size = 30,           # Größe der Knoten
     edge.arrow.size = 1.5,      # Größe der Pfeile
     main = "Graph mit benutzerdefinierten Labels")

degree(g)
plot(g, 
     main="Gerichtetes Netzwerk mit Degree-Zentralität", 
     vertex.label = V(g)$label,  # Knotenlabel anzeigen
     vertex.size = degree * 0.5,  # Knoten-Größe basierend auf Eigenvector-Zentralität
     vertex.color = "lightblue",  # Knotenfarbe
     edge.arrow.size = 1.5,       # Größe der Pfeile
     edge.color = "black",         # Kantenfarbe
     edge.width = 2,              # Kantenbreite
     layout = layout_randomly)

plot(g, 
     main="Gerichtetes Netzwerk mit Betweenness-Zentralität", 
     vertex.label = V(g)$label,  # Knotenlabel anzeigen
     vertex.size = betweenness_centrality * 3.5,  # Knoten-Größe basierend auf Eigenvector-Zentralität
     vertex.color = "lightblue",  # Knotenfarbe
     edge.arrow.size = 1.5,       # Größe der Pfeile
     edge.color = "black",         # Kantenfarbe
     edge.width = 2,              # Kantenbreite
     layout = layout_randomly)









