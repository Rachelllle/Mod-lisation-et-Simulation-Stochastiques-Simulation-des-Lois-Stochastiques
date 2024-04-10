
#First Question:

loi_logistique <- function(n, alpha, beta) {
   
  echantillon <- numeric(n)

  for (i in 1:n) {
    u <- runif(1)  # Génère un nombre aléatoire uniforme entre 0 et 1
    
    echantillon[i] <- alpha - beta * log(1/u - 1)
  }
  return(echantillon)
}

#Simulation d'un echantillon
echantillon <- loi_logistique (n, alpha, beta)

#Tracer l'histogramme et tester la validité de la loi
hist(echantillon, breaks=10, col='blue', main='Histogramme de la loi logistique',xlab='Valeurs', ylab='Densité de probabilité')





#Second Question:

simulate_erlang <- function(n, ordre, moyenne) {

  # Paramètres de la distribution exponentielle (rate)
  lambda <- ordre / moyenne
  

   echantillon <- numeric(n)

   for (i in 1:n) {

    u <- runif(ordre)
    echantillon[i] <-  sum(-(1/lambda)*log(1-u))

  }
  
  return(echantillon)
}

#Simulation d'un echantillon
echantillon <- simulate_erlang(n, ordre, moyenne)

#Tracer l'histogramme et tester la validité de la loi
hist(echantillon, breaks=10, col='blue', main='Histogramme de la loi Erlang',xlab='Valeurs', ylab='Densité de probabilité')




#Third Question:

f <- function(x) {
  return (x^2 * sqrt(25 - x^2))
}

#Acceptation-rejet méthode

acceptation_rejet <- function(n) {

  echantillon <- numeric(n)

  i <- 1
  a <- 0
  b <- 4
  C <- 48
  while (i <= n) {
    u1 <- runif(1)
    x_candidat <- a + (b - a) * u1
    g_u1 <- f(x_candidat)/ C
    u2 <- runif(1)
    
    if (u2 <= g_u1) {
      echantillon[i] <- x_candidat
      i <- i + 1
    }
  }
  
  return(echantillon)
}

#Simulation d'un echantillon
echantillon <- acceptation_rejet(n)

# Afficher un histogramme des résultats
hist(echantillon, breaks = 10, col = 'blue', main = 'Histogramme des échantillons générés ', xlab='Valeurs', ylab='Densité de probabilité')


#Fourth Question:

simulate_hyperexponentielle <- function(n, lambda1, lambda2, p) {
  
  echantillon <- numeric(n)
  
  for (i in 1:n) {
    u1 <- runif(1)
    u2 <- runif(1)
    
    if ( u1 <= p) {
      echantillon[i] <- -(1 / lambda1) * log(1 - u2)
    } else {
      echantillon[i] <- -(1 / lambda2) * log(1 - u2)
    }
  }
  
  return(echantillon)
}

#Simulation d'un echantillon
echantillon <- simulate_hyperexponentielle(n, lambda1, lambda2, p)

# Afficher un histogramme des résultats
hist(echantillon, breaks=10, col='blue', main='Histogramme de la loi Hyperexponentielle',xlab='Valeurs', ylab='Densité de probabilité')


#Fifth Question:

simulate_binomial <- function(n, p, N) {

  echantillon <- numeric(N)

  for (j in 1:N) {
    B <- 0
    for (i in 1:n) {
     u <- runif(1)  
     if (u < p) {
       B <- B + 1
     }
    }
    echantillon[j] <- B
  }
  return(echantillon)
}

#Simulation d'un echantillon
echantillon <- simulate_binomial(n, k, p)

# Afficher un histogramme des résultats
hist(echantillon, breaks=10, col='blue', main='Histogramme de la loi Binomiale',xlab='Valeurs', ylab='Densité de probabilité')


#Sixth Question:

simulate_poisson <- function(lambda, n) {

  echantillon <- numeric(n)
  
  for (i in 1:n) {
    k <- 0
    p <- exp(-lambda)
    F <- p
    u <- runif(1)
    
    while (u > F) {
      k <- k + 1
      p <- (lambda / (k + 1)) * p
      F <- F + p
    }
    
    echantillon[i] <- k
  }
  
  return(echantillon)
}

#Simulation d'un echantillon
echantillon <- simulate_poisson(lambda, n)

# Afficher un histogramme des résultats
hist(echantillon, breaks=10, col='blue', main='Histogramme de la loi Poisson',xlab='Valeurs', ylab='Densité de probabilité')



#Seventh Question:

simulate_normal <- function(mean, variance, n) {

  echantillon <- numeric(n)

  std_dev <- sqrt(variance)
  
  for (i in 1:n) {

    u1 <- runif(1)
    u2 <- runif(1)

    # Transformation de Box-Muller 
    z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)

    echantillon[i] <- std_dev * z1 + mean
  }

  return(echantillon)
}


#Simulation d'un echantillon
echantillon <- simulate_normal (mean, variance, n)

# Afficher un histogramme des résultats
hist(echantillon, breaks=10, col='blue', main='Histogramme de la loi Normal',xlab='Valeurs', ylab='Densité de probabilité')





