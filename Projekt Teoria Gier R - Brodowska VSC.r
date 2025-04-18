# Projekt Teoria Gier - Równowaga Nasha w strategiach czystych

# Autor: [Martyna Brodowska, Numer Indeksu]

getAllPureStrategyNE <- function(game) {
  # Liczba graczy
  num_players <- length(game)
  
  # Liczba akcji dla każdego gracza
  num_actions <- sapply(game, function(x) dim(x)[1])
  
  # Wartości maksymalne dla każdego wymiaru
  max_values <- sapply(num_actions, max)
  
  # Lista przechowująca równowagi Nasha
  equilibria <- list()
  
  # Pętla po możliwych profilach akcji
  if(num_players == 2) {
    for (i in 1:max_values[1]) {
      for (j in 1:max_values[2]) {
        # Przypisanie akcji dla graczy
        actions <- c(i, j)
        
        # Sprawdzenie czy jest równowaga Nasha
        is_nash_equilibrium <- TRUE
        
        # Pętla po graczach
        for (player in 1:num_players) {
          # Wypłata dla aktualnego gracza
          payoff <- game[[player]][i, j]
          
          # Pętla po możliwych akcjach dla danego gracza
          for (m in 1:num_actions[player]) {
            # Sprawdzenie czy zmiana akcji zwiększa wypłatę
            if (game[[player]][m, j] > payoff && m != i) {
              is_nash_equilibrium <- FALSE
              break
            }
          }
          # Warunek sprawdzający czy inna akcja gracza zwiększa wypłatę
          if (!is_nash_equilibrium) break
        }
        
        # Dodanie równowagi Nasha do listy, jeśli istnieje
        if (is_nash_equilibrium) {
          key <- paste(i, j, sep = "")
          equilibria[[key]] <- actions
        }
      }
    }
  } else {
    # Pętla po możliwych profilach akcji
    for (i in 1:max_values[1]) {
      for (j in 1:max_values[2]) {
        if (num_players > 2) {
          for (k in 1:max_values[3]) {
            if (num_players > 3) {
              for (l in 1:max_values[4]) {
                # Przypisanie akcji dla graczy
                actions <- c(i, j, k, l)
                
                # Sprawdzenie czy jest równowaga Nasha
                is_nash_equilibrium <- TRUE
                
                # Pętla po graczach
                for (player in 1:num_players) {
                  # Wypłata dla aktualnego gracza
                  payoff <- game[[player]][i, j, k, l]
                  
                  # Pętla po możliwych akcjach dla danego gracza
                  for (m in 1:num_actions[player]) {
                    # Sprawdzenie czy zmiana akcji zwiększa wypłatę
                    if (game[[player]][m, j, k, l] > payoff && m != i) {
                      is_nash_equilibrium <- FALSE
                      break
                    }
                  }
                  # Warunek sprawdzający czy inna akcja gracza zwiększa wypłatę
                  if (!is_nash_equilibrium) break
                }
                
                # Dodanie równowagi Nasha do listy, jeśli istnieje
                if (is_nash_equilibrium) {
                  key <- paste(i, j, k, l, sep = "")
                  equilibria[[key]] <- actions
                }
              }
            } else {
              # Przypisanie akcji dla graczy
              actions <- c(i, j, k)
              
              # Sprawdzenie czy jest równowaga Nasha
              is_nash_equilibrium <- TRUE
              
              # Pętla po graczach
              for (player in 1:num_players) {
                # Wypłata dla aktualnego gracza
                payoff <- game[[player]][i, j, k]
                
                # Pętla po możliwych akcjach dla danego gracza
                for (m in 1:num_actions[player]) {
                  # Sprawdzenie czy zmiana akcji zwiększa wypłatę
                  if (game[[player]][m, j, k] > payoff && m != i) {
                    is_nash_equilibrium <- FALSE
                    break
                  }
                }
                # Warunek sprawdzający czy inna akcja gracza zwiększa wypłatę
                if (!is_nash_equilibrium) break
              }
              
              # Dodanie równowagi Nasha do listy, jeśli istnieje
              if (is_nash_equilibrium) {
                key <- paste(i, j, k, sep = "")
                equilibria[[key]] <- actions
              }
            }
          }
        } else {
          # Przypisanie akcji dla graczy
          actions <- c(i, j)
          
          # Sprawdzenie czy jest równowaga Nasha
          is_nash_equilibrium <- TRUE
          
          # Pętla po graczach
          for (player in 1:num_players) {
            # Wypłata dla aktualnego gracza
            payoff <- game[[player]][i, j]
            
            # Pętla po możliwych akcjach dla danego gracza
            for (m in 1:num_actions[player]) {
              # Sprawdzenie czy zmiana akcji zwiększa wypłatę
              if (game[[player]][m, j] > payoff && m != i) {
                is_nash_equilibrium <- FALSE
                break
              }
            }
            # Warunek sprawdzający czy inna akcja gracza zwiększa wypłatę
            if (!is_nash_equilibrium) break
          }
          
          # Dodanie równowagi Nasha do listy, jeśli istnieje
          if (is_nash_equilibrium) {
            key <- paste(i, j, sep = "")
            equilibria[[key]] <- actions
          }
        }
      }
    }
  }
  
  return(equilibria)
}
# Przykłady zastosowania funkcji getAllPureStrategyNE

# Example (coordination game for two players)
game <- list(
  "player1" = array(c(1, 0, 0, 1), dim = c(2, 2)),
  "player2" = array(c(1, 0, 0, 1), dim = c(2, 2))
)
getAllPureStrategyNE(game)

# Example (prisoners' dilemma for two players)
game <- list(
  "player1" = array(c(5, 10, 1, 2), dim = c(2, 2)), 
  "player2" = array(c(5, 1, 10, 2), dim = c(2, 2))
)
getAllPureStrategyNE(game)

# Example (stag-hare game for two players)
game <- list(
  "player1" = array(c(3, 2, 0, 2), dim = c(2, 2)),
  "player2" = array(c(3, 0, 2, 2), dim = c(2, 2))
)
getAllPureStrategyNE(game)

# Example (anti-coordination game for two players)
game <- list(
  "player1" = array(c(0, 1, 1), dim = c(2, 2)),
  "player2" = array(c(0, 1, 1), dim = c(2, 2))
)
getAllPureStrategyNE(game)

# Example (anti-coordination game for three players)
game <- list(
  "player1" = array(c(0, 1, 1,1,1,1,1,0), dim = c(2, 2,2)),
  "player2" = array(c(0, 1, 1,1,1,1,1,0), dim = c(2, 2,2)),
  "player3" = array(c(0, 1, 1,1,1,1,1,0), dim = c(2, 2,2))
)
getAllPureStrategyNE(game)

# Przykład 7: Gra koordynacyjna dla czterech graczy 
game <- list(
  "player1" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)),
  "player2" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)),
  "player3" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2)),
  "player4" = array(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(2, 2, 2, 2))
)
getAllPureStrategyNE(game)