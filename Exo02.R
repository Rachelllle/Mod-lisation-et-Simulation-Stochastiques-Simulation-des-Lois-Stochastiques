
#First Question:

#Ecrire un programme de simulation du jeu de Pile ou Face avec une piéce parfaite.

coin_flips <- function(n, p) {

 u <- runif(n);

 #On connait la probabilité p = P(Pile)

 print(paste("Number of piles:", sum(u <= p), "Number of faces:", sum(u > p))) 

}


#Simuler plusieurs jeux en faisant varier le nombre díexpÈriences.
outcomes <- coin_flips(n, p)

#Afficher le résultat.
outcomes

#Second Question:
simulate_game <- function(p, gain, perte, fortune_initiale, objectif, ruine) {
  fortune <- fortune_initiale
  nombre_lancers <- 0

  while (fortune > ruine && fortune < objectif) {
    u <- runif(1)
    if (u <= p) {
      fortune <- fortune + gain
    } else {
      fortune <- fortune - perte
    }
    nombre_lancers <- nombre_lancers + 1
  }==

  return(nombre_lancers)
}

# Scénario a
scenario_a <- simulate_game(3/4, 10, 10, 40, 80, 0)
print(paste("Scénario a) Nombre de lancers:", scenario_a))

# Scénario b
scenario_b <- simulate_game(3/4, 10, 20, 40, 80, 0)
print(paste("Scénario b) Nombre de lancers:", scenario_b))

# Scénario c
scenario_c <- simulate_game(3/4, 20, 10, 40, 80, 0)
print(paste("Scénario c) Nombre de lancers:", scenario_c))


# Scénario a)
scenario_a <- simulate_game(1/2, 10, 10, 40, 80, 0)
print(paste("Scénario a) Nombre de lancers:", scenario_a))

# Scénario b)
scenario_b <- simulate_game(1/2, 10, 20, 40, 80, 0)
print(paste("Scénario b) Nombre de lancers:", scenario_b))

# Scénario c)
scenario_c <- simulate_game(1/2, 20, 10, 40, 80, 0)
print(paste("Scénario c) Nombre de lancers:", scenario_c))




