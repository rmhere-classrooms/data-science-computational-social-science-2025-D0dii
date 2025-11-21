library(igraph)

# Punkt 2: Wygeneruj graf wedle modelu Barabási-Albert z tysiącem węzłów
g <- barabasi.game(n = 1000)

# Punkt 3: Zwizualizuj graf layoutem Fruchterman & Reingold
layout <- layout.fruchterman.reingold(g)
plot(g, 
     layout = layout,
     vertex.size = 2,
     vertex.label = NA,
     edge.arrow.size = 0.2,
     main = "Graf Barabási-Albert (layout: Fruchterman-Reingold)")

# Punkt 4: Znajdź najbardziej centralny węzeł według miary betweenness, jaki ma numer?
cat("\n=== Punkt 4: Najbardziej centralny węzeł (betweenness) ===\n")
betweenness_values <- betweenness(g)
max_betweenness <- max(betweenness_values)
most_central_node <- which(betweenness_values == max_betweenness)

cat("Węzeł o najwyższym betweenness:", most_central_node, "\n")
cat("Wartość betweenness:", max_betweenness, "\n")

# Punkt 5: Jaka jest średnica grafu?
cat("\n=== Punkt 5: Średnica grafu ===\n")
graph_diameter <- diameter(g)
cat("Średnica grafu:", graph_diameter, "\n")

# Punkt 6: Czym różnią się grafy Barabási-Albert i Erdős-Rényi?
#
# Grafy Erdős-Rényi (losowe):
# - Każda para węzłów ma takie samo prawdopodobieństwo połączenia (p)
# - Rozkład stopni węzłów jest normalny (rozkład Poissona)
# - Większość węzłów ma podobny stopień (średni stopień)
# - Brak węzłów "hub" (centralnych węzłów o bardzo wysokim stopniu)
# - Model opisuje sieci przypadkowe, ale nie występujące w naturze
#
# Grafy Barabási-Albert (preferential attachment):
# - Nowe węzły łączą się preferencyjnie z węzłami o już wysokim stopniu
# - Rozkład stopni węzłów jest potęgowy (power-law): niewiele węzłów ma bardzo wysoki stopień
# - Występują węzły "hub" - wysoce połączone węzły centralne
# - Model lepiej opisuje rzeczywiste sieci (Internet, sieci społecznościowe, cytowania naukowe)
# - Mechanizm wzrostu: "bogaci stają się bogatsi" (rich get richer)
#
# Podsumowanie:
# Erdős-Rényi = równomierne połączenia, jednorodna struktura
# Barabási-Albert = nierównomierne połączenia, obecność hubów, skala wolna (scale-free)
