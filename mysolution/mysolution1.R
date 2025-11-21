library(igraph)

# Punkt 2: Wygeneruj sieć Erdős-Rényi o stu wierzchołkach i prawdopodobieństwie krawędzi = 0.05
g <- erdos.renyi.game(n = 100, p.or.m = 0.05)

# Punkt 3: Wydrukuj podsumowanie grafu - czy graf jest ważony?
# Odpowiedź: NIE, graf nie jest ważony - w podsumowaniu nie ma litery 'W' (Weighted)
cat("\n=== Punkt 3: Podsumowanie grafu przed dodaniem wag ===\n")
summary(g)
cat("\nCzy graf jest ważony? NIE - w podsumowaniu nie ma litery 'W' (Weighted)\n")

# Punkt 4: Wylistuj wszystkie wierzchołki i krawędzie
cat("\n=== Punkt 4: Lista wierzchołków i krawędzi ===\n")
cat("Wierzchołki:\n")
print(V(g))
cat("\nLiczba wierzchołków:", vcount(g), "\n")

cat("\nKrawędzie:\n")
print(E(g))
cat("\nLiczba krawędzi:", ecount(g), "\n")

# Punkt 5: Ustaw wagi wszystkich krawędzi na losowe z zakresu 0.01 do 1
E(g)$weight <- runif(length(E(g)), min = 0.01, max = 1)

# Punkt 6: Wydrukuj ponownie podsumowanie grafu - czy teraz graf jest ważony?
# Odpowiedź: TAK, graf jest teraz ważony - w podsumowaniu pojawia się litera 'W' (Weighted)
cat("\n=== Punkt 6: Podsumowanie grafu po dodaniu wag ===\n")
summary(g)
cat("\nCzy graf jest ważony? TAK - w podsumowaniu pojawia się litera 'W' (Weighted)\n")

# Punkt 7: Jaki jest stopień każdego węzła? Następnie stwórz histogram stopni węzłów
cat("\n=== Punkt 7: Stopnie węzłów ===\n")
cat("Stopnie wszystkich węzłów:\n")
node_degrees <- degree(g)
print(node_degrees)

cat("\nStatystyki stopni węzłów:\n")
cat("Minimum:", min(node_degrees), "\n")
cat("Maksimum:", max(node_degrees), "\n")
cat("Średnia:", mean(node_degrees), "\n")
cat("Mediana:", median(node_degrees), "\n")

hist(node_degrees, 
     main = "Histogram stopni węzłów w grafie Erdős-Rényi",
     xlab = "Stopień węzła",
     ylab = "Liczba węzłów",
     col = "#007bc2",
     border = "white",
     breaks = 10)

# Punkt 8: Ile jest klastrów (connected components) w grafie?
cat("\n=== Punkt 8: Connected components (klastry) ===\n")
cl <- clusters(g)
cat("Liczba klastrów (connected components):", cl$no, "\n")
cat("Rozmiary klastrów:\n")
print(cl$csize)
cat("\nPrzynależność węzłów do klastrów (pierwsze 20 węzłów):\n")
print(head(cl$membership, 20))

plot(g, 
     vertex.color = cl$membership,
     vertex.size = 8,
     vertex.label = NA,
     main = "Graf z zaznaczonymi klastrami (connected components)",
     edge.arrow.size = 0.3)

# Punkt 9: Zwizualizuj graf w taki sposób, aby rozmiar węzłów odpowiadał mierze PageRank
cat("\n=== Punkt 9: Wizualizacja z PageRank ===\n")
pr <- page.rank(g)$vector
cat("Statystyki PageRank:\n")
cat("Minimum:", min(pr), "\n")
cat("Maksimum:", max(pr), "\n")
cat("Średnia:", mean(pr), "\n")

cat("\nWęzły o najwyższym PageRank:\n")
top_pr <- sort(pr, decreasing = TRUE)[1:5]
print(top_pr)

plot(g,
     vertex.size = pr * 500,
     vertex.color = "#007bc2",
     vertex.label = NA,
     edge.arrow.size = 0.2,
     edge.width = E(g)$weight * 2,
     main = "Graf z rozmiarem węzłów proporcjonalnym do PageRank",
     layout = layout.fruchterman.reingold(g))

plot(g,
     vertex.size = pr * 500,
     vertex.color = "#007bc2",
     vertex.label = NA,
     edge.arrow.size = 0.2,
     edge.width = E(g)$weight * 2,
     main = "Graf z rozmiarem węzłów proporcjonalnym do PageRank (layout: circle)",
     layout = layout.circle(g))

cat("\n=== Zadanie I zakończone ===\n")
