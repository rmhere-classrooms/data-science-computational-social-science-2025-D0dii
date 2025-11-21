library(shiny)
library(bslib)
library(igraph)

# Punkt 4: Zaimportuj zbiór out.radoslaw_email_email do data frame 
# i zachowaj tylko pierwsze dwie kolumny (przeskocz dwa pierwsze wiersze),
# następnie stwórz z tego data frame'a graf skierowany
dfGraph <- read.csv2("https://bergplace.org/share/out.radoslaw_email_email", skip = 2, sep = " ")[, 1:2]
names(dfGraph) <- c("from", "to")

# Oblicz wagi krawędzi: wij = cntij / cnti
# cntij - liczba maili od vi do vj
# cnti - liczba wszystkich maili wysłanych przez vi

# Policz cntij - ile maili wysłano między każdą parą węzłów
edge_counts <- as.data.frame(table(dfGraph$from, dfGraph$to))
names(edge_counts) <- c("from", "to", "cntij")
edge_counts <- edge_counts[edge_counts$cntij > 0, ]

# Policz cnti - ile wszystkich maili wysłał każdy węzeł
node_total_emails <- as.data.frame(table(dfGraph$from))
names(node_total_emails) <- c("from", "cnti")

# Połącz dane i oblicz wagi: wij = cntij / cnti
edge_counts <- merge(edge_counts, node_total_emails, by = "from")
edge_counts$weight <- edge_counts$cntij / edge_counts$cnti

# Stwórz graf z obliczonymi wagami
g <- graph.data.frame(edge_counts[, c("from", "to", "weight")], directed = TRUE)

# Użyj funkcji simplify aby pozbyć się wielokrotnych krawędzi i pętli
g <- simplify(g, edge.attr.comb = "sum")

# Weryfikacja: graf powinien mieć 167 węzłów i 5783 krawędzie
cat("Liczba węzłów:", vcount(g), "\n")
cat("Liczba krawędzi:", ecount(g), "\n")
if(vcount(g) == 167 && ecount(g) == 5783) {
  cat("✓ Weryfikacja poprawna - można kontynuować\n")
} else {
  cat("✗ Uwaga: Liczby się nie zgadzają!\n")
}

# Weryfikacja sum wag wychodzących z węzłów (powinny wynosić ~1)
cat("\nWeryfikacja sum wag wychodzących:\n")
sample_nodes <- sample(V(g), min(5, vcount(g)))
for(node in sample_nodes) {
  out_edges <- incident(g, node, mode = "out")
  sum_weights <- sum(E(g)[out_edges]$weight)
  cat("Węzeł", as.character(V(g)[node]$name), "- suma wag wychodzących:", 
      round(sum_weights, 4), "\n")
}

# Punkt 7-8: Symulacja rozprzestrzeniania się informacji - Independent Cascades Model

# Funkcja symulująca proces independent cascades (implementacja własna)
# Zwraca historię aktywacji węzłów w każdej iteracji
# prob_multiplier - współczynnik skalujący prawdopodobieństwo (1.0 = 100% wij)
independent_cascade <- function(graph, initial_nodes, prob_multiplier = 1.0) {
  # Ustaw wszystkim węzłom atrybut activated na FALSE
  V(graph)$activated <- FALSE
  V(graph)$newly_activated <- FALSE
  
  # Aktywuj początkowe węzły
  V(graph)[initial_nodes]$activated <- TRUE
  V(graph)[initial_nodes]$newly_activated <- TRUE
  
  # Lista do zapisywania liczby NOWO aktywowanych węzłów w każdej iteracji
  activation_counts <- c(sum(V(graph)$newly_activated))
  
  # OPTYMALIZACJA: Stwórz macierz wag dla szybkiego dostępu
  # Pobierz wszystkie krawędzie i ich wagi
  el <- as_edgelist(graph, names = FALSE)
  weights <- E(graph)$weight
  
  # Stwórz listę sąsiadów z wagami dla każdego węzła
  neighbors_with_weights <- vector("list", vcount(graph))
  for(i in 1:nrow(el)) {
    from <- el[i, 1]
    to <- el[i, 2]
    if(is.null(neighbors_with_weights[[from]])) {
      neighbors_with_weights[[from]] <- list()
    }
    neighbors_with_weights[[from]][[as.character(to)]] <- weights[i]
  }
  
  iteration <- 0
  while(TRUE) {
    iteration <- iteration + 1
    
    # Pobierz węzły, które były aktywowane w poprzedniej iteracji
    active_nodes <- which(V(graph)$newly_activated)
    
    if(length(active_nodes) == 0) break
    
    # Zresetuj flagę newly_activated
    V(graph)$newly_activated <- FALSE
    
    # Dla każdego aktywnego węzła próbuj aktywować sąsiadów
    for(node_idx in active_nodes) {
      # Pobierz sąsiadów z wagami z prekompilowanej listy
      node_neighbors <- neighbors_with_weights[[node_idx]]
      
      if(is.null(node_neighbors) || length(node_neighbors) == 0) next
      
      for(neighbor_name in names(node_neighbors)) {
        neighbor_idx <- as.integer(neighbor_name)
        
        # Sprawdź czy sąsiad nie jest już aktywowany
        if(V(graph)[neighbor_idx]$activated) next
        
        # Pobierz wagę z prekompilowanej listy (O(1) zamiast O(E))
        prob <- node_neighbors[[neighbor_name]] * prob_multiplier
        
        # Próba aktywacji z prawdopodobieństwem wij * multiplier
        # Jeśli prob >= 1, węzeł jest zawsze aktywowany 
        if(runif(1) < prob) {
          V(graph)[neighbor_idx]$activated <- TRUE
          V(graph)[neighbor_idx]$newly_activated <- TRUE
        }
      }
    }
    
    # Zapisz liczbę NOWO aktywowanych węzłów w tej iteracji
    activation_counts <- c(activation_counts, sum(V(graph)$newly_activated))
  }
  
  return(activation_counts)
}

# Funkcja do wyboru 5% węzłów według różnych strategii
select_initial_nodes <- function(graph, strategy) {
  num_initial <- ceiling(vcount(graph) * 0.05)
  
  if(strategy == "outdegree") {
    # (i) Węzły o największym outdegree
    degrees <- degree(graph, mode = "out")
    return(order(degrees, decreasing = TRUE)[1:num_initial])
    
  } else if(strategy == "betweenness") {
    # (ii) Najbardziej centralne węzły według betweenness
    betw <- betweenness(graph)
    return(order(betw, decreasing = TRUE)[1:num_initial])
    
  } else if(strategy == "closeness") {
    # (iii) Węzły o największym closeness
    close <- closeness(graph, mode = "out")
    close[is.nan(close)] <- 0
    return(order(close, decreasing = TRUE)[1:num_initial])
    
  } else if(strategy == "random") {
    # (iv) Losowe węzły
    return(sample(1:vcount(graph), num_initial))
    
  } else if(strategy == "pagerank") {
    # (v) Węzły o największym PageRank
    # PageRank jest dobrą miarą wpływu w sieciach, uwzględnia zarówno
    # liczbę połączeń jak i ich jakość (połączenia z ważnymi węzłami)
    pr <- page.rank(graph)$vector
    return(order(pr, decreasing = TRUE)[1:num_initial])
  }
}

# Funkcja do uruchomienia eksperymentu dla wybranej strategii
run_single_experiment <- function(graph, strategy, num_experiments = 100, prob_multiplier = 1.0) {
  cat("Rozpoczynam symulację dla strategii:", strategy, 
      "(współczynnik prawdopodobieństwa:", prob_multiplier * 100, "%)\n")
  
  all_runs <- list()
  
  for(exp in 1:num_experiments) {
    initial_nodes <- select_initial_nodes(graph, strategy)
    activation_counts <- independent_cascade(graph, initial_nodes, prob_multiplier)
    all_runs[[exp]] <- activation_counts
  }
  
  # Uśrednij wyniki (różne eksperymenty mogą mieć różną długość)
  max_length <- max(sapply(all_runs, length))
  avg_activation <- numeric(max_length)
  
  for(i in 1:max_length) {
    values <- sapply(all_runs, function(x) if(length(x) >= i) x[i] else 0)
    avg_activation[i] <- mean(values)
  }
  
  cat("Średnia końcowa liczba aktywowanych węzłów:", 
      round(tail(avg_activation, 1), 2), "/", vcount(graph), "\n")
  
  return(avg_activation)
}

# Funkcja do uruchomienia eksperymentów dla WSZYSTKICH strategii
run_all_experiments <- function(graph, num_experiments = 100) {
  strategies <- c("outdegree", "betweenness", "closeness", "random", "pagerank")
  results <- list()
  
  cat("\n=== Rozpoczynam symulację dla wszystkich strategii ===\n\n")
  
  for(strategy in strategies) {
    results[[strategy]] <- run_single_experiment(graph, strategy, num_experiments)
    cat("\n")
  }
  
  cat("=== Wszystkie symulacje zakończone ===\n")
  
  return(results)
}

# Define UI for app
ui <- page_sidebar(
  title = "Rozprzestrzenianie się informacji w sieci e-mail",
  sidebar = sidebar(
    width = 300,
    tags$style(HTML("
      .sidebar { padding: 10px !important; }
      .form-group { margin-bottom: 8px !important; }
      h4 { margin-top: 0px; margin-bottom: 8px; font-size: 14px; }
      .checkbox { margin-top: 2px; margin-bottom: 2px; }
      label { margin-bottom: 2px; font-size: 13px; }
      hr { margin-top: 8px; margin-bottom: 8px; }
      #statusText { font-size: 12px; margin-top: 5px; }
    ")),
    h4("Strategie:"),
    checkboxGroupInput(
      inputId = "strategies",
      label = NULL,
      choices = c(
        "Outdegree" = "outdegree",
        "Betweenness" = "betweenness",
        "Closeness" = "closeness",
        "Random" = "random",
        "PageRank" = "pagerank"
      ),
      selected = c("outdegree", "betweenness", "closeness", "random", "pagerank")
    ),
    hr(),
    sliderInput(
      inputId = "prob_multiplier",
      label = "Prawdopodobieństwo (% wij):",
      value = 100,
      min = 10,
      max = 200,
      step = 10,
      post = "%"
    ),
    sliderInput(
      inputId = "num_exp",
      label = "Liczba iteracji:",
      value = 10,
      min = 1,
      max = 50,
      step = 1
    ),
    actionButton(
      inputId = "runButton",
      label = "Start symulacji",
      class = "btn-primary",
      width = "100%"
    ),
    hr(),
    textOutput("statusText")
  ),
  plotOutput(outputId = "cascadePlot", height = "600px")
)

# Define server logic
server <- function(input, output) {
  
  # Reaktywna wartość przechowująca wyniki dla wszystkich strategii
  all_results <- reactiveVal(NULL)
  
  # Obsługa kliknięcia przycisku
  observeEvent(input$runButton, {
    # Walidacja - sprawdź czy wybrano przynajmniej jedną strategię
    if(length(input$strategies) == 0) {
      output$statusText <- renderText({
        "Błąd: Wybierz przynajmniej jedną strategię!"
      })
      return()
    }
    
    # Oblicz multiplier (suwak podaje procenty, zamieniamy na współczynnik)
    prob_mult <- input$prob_multiplier / 100
    
    output$statusText <- renderText({
      paste("Trwa symulacja", length(input$strategies), "strategii z prawdopodobieństwem",
            input$prob_multiplier, "% bazowego wij... Proszę czekać...")
    })
    
    # Uruchom symulacje tylko dla wybranych strategii z paskiem postępu
    results <- list()
    
    cat("\n=== Rozpoczynam symulację dla wybranych strategii ===\n")
    cat("Współczynnik prawdopodobieństwa:", prob_mult, "(", input$prob_multiplier, "%)\n\n")
    
    withProgress(message = 'Trwa symulacja', value = 0, {
      n_strategies <- length(input$strategies)
      
      for(i in seq_along(input$strategies)) {
        strategy <- input$strategies[i]
        
        # Aktualizuj pasek postępu
        incProgress(1/n_strategies, detail = paste("Strategia:", strategy))
        
        results[[strategy]] <- run_single_experiment(g, strategy, input$num_exp, prob_mult)
        cat("\n")
      }
    })
    
    cat("=== Wszystkie symulacje zakończone ===\n")
    
    all_results(results)
    
    output$statusText <- renderText({
      paste("Symulacja zakończona! Przeanalizowano", input$num_exp, 
            "iteracji dla każdej z", length(input$strategies), "strategii.",
            "\nWspółczynnik prawdopodobieństwa:", input$prob_multiplier, "% bazowego wij.")
    })
  })
  
  output$cascadePlot <- renderPlot({
    
    # Sprawdź czy są wyniki do wyświetlenia
    if(is.null(all_results())) {
      plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10),
           main = "Kliknij 'Start symulacji' aby rozpocząć", axes = FALSE)
      text(5, 5, "Oczekiwanie na start symulacji...", cex = 1.5, col = "gray")
      return()
    }
    
    results <- all_results()
    
    # Kolory dla każdej strategii
    colors <- c("outdegree" = "#e41a1c", 
                "betweenness" = "#377eb8", 
                "closeness" = "#4daf4a", 
                "random" = "#984ea3", 
                "pagerank" = "#ff7f00")
    
    # Nazwy strategii do legendy
    strategy_names <- c("outdegree" = "Outdegree", 
                       "betweenness" = "Betweenness", 
                       "closeness" = "Closeness", 
                       "random" = "Random", 
                       "pagerank" = "PageRank")
    
    # Znajdź maksymalną długość dla osi X i Y
    max_length <- max(sapply(results, length))
    max_activation <- max(sapply(results, function(x) max(x, na.rm = TRUE)))
    
    # Stwórz pusty wykres
    plot(1, type = "n", 
         xlim = c(0, max_length), 
         ylim = c(0, max_activation * 1.1),
         xlab = "Numer iteracji",
         ylab = "Liczba nowo aktywowanych węzłów w iteracji",
         main = "Porównanie dyfuzji informacji dla różnych strategii wyboru węzłów początkowych")
    
    grid()
    
    # Dodaj linie dla każdej strategii
    for(strategy in names(results)) {
      lines(results[[strategy]], 
            col = colors[strategy], 
            lwd = 2.5, 
            type = "b",
            pch = 19)
    }
    
    # Dodaj legendę
    legend("topright", 
           legend = strategy_names[names(results)],
           col = colors[names(results)],
           lwd = 2.5,
           pch = 19,
           bty = "n",
           cex = 1.1)
    
    # Dodaj informacje o łącznej liczbie aktywowanych węzłów dla każdej strategii
    info_text <- sapply(names(results), function(s) {
      total <- sum(results[[s]])
      pct <- round(total / vcount(g) * 100, 1)
      paste0(strategy_names[s], ": ", round(total), " (", pct, "%)")
    })
    
    legend("topleft",
           legend = c("Łączna liczba aktywacji:", info_text),
           bty = "n",
           cex = 0.9)
  })
}

shinyApp(ui = ui, server = server)