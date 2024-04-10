

#First Question:

generate_LC <- function(n, a, b, m, y0) {
  values <- c()
  values[1] <- y0
  
  for (i in 2:(n + 1)) {
    values[i] <- (a * values[i - 1] + b) %% m
  }

  normalized_values <- values[-1] / m    # Exclude y0 from the result
  return(normalized_values)
}

# Générer les 20 premiers termes
normalized_values <- generate_LC(20, 121, 567, 10^3, 0)

# Afficher les valeurs du vecteurs
normalized_values


#Second Question:

Khi_Deux_test <- function(random_numbers, num_intervals, alpha) {

  breaks <- seq(0, 1, by = 1/num_intervals)
  observed_frequency <- table(cut(random_numbers, breaks = breaks, include.lowest = TRUE))
  expected_frequency <- rep(length(random_numbers) / num_intervals, num_intervals)
  
  chi_squared_stat <- sum((observed_frequency - expected_frequency)^2 / expected_frequency)
  critical_value <- qchisq(1 - alpha, df = num_intervals - 1)
  
  result <- c(chi_squared_stat, critical_value, ifelse(chi_squared_stat > critical_value, "Rejeter H0", "Ne pas rejeter H0"))

  return(result)
}

# Tester la qualité du générateur avec le test du Khi-deux
test_result <- Khi_Deux_test(normalized_values, num_intervals, alpha)

# Afficher les résultats du test
test_result


#Third Question:

set.seed(123)        #La définition de la graine pour reproduire la même séquence de nombres aléatoires dans différentes exécutions de mon programme.
u <- runif(10^4)     #Génèrer un vecteur u contenant 10^4 nombres aléatoires provenant d'une distribution uniforme sur l'intervalle [0, 1]

# Tracer l'histogramme
#Le histogramme aura des bacs ou classes, et chaque bac représentera une plage spécifique de valeurs pour la variable u. La hauteur de chaque barre de l'histogramme (sur l'axe des y) représentera la fréquence des observations se situant dans cette plage particulière.
hist(u, breaks <- seq(0, 1, by = 1/10), main = "Histogramme de u", xlab = "Valeurs de u", ylab = "Fréquence relative", col = "lightblue", border = "black")

# Tracer la densité d'une loi uniforme sur [0, 1]
curve(dunif(x, min = 0, max = 1), col = "red", lwd = 4, add = TRUE, yaxt = "n")

# Légende
legend("topright", legend = c("Histogramme de u", "Densité Uniforme"), fill = c("lightblue", "red"))


#Fourth Question:

# Générer un vecteur colonne de 20 nombres aléatoires dans [0, 1]
vecteur_colonne <- runif(20)
  #Chaque fois que vous exécutez cette commande, un nouveau vecteur de 20 nombres aléatoires sera généré.
  #Pour obtenir la même séquence de nombres aléatoires à chaque exécution, vous pouvez utiliser set.seed à chaque fois. 


#Fifth Question:

# Calculer la racine carrée de chaque élément du vecteur
racine_carrée <- sqrt(vecteur_colonne)

# Afficher la racine carrée
racine_carrée

#Sixth Question:

generate_LC(20, 121, 567, 10^3, 1)
