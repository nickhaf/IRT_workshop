## 2PL Funktion
calc_2pl <- function(a, theta, xi) {
  p <- (exp(a * (theta - xi))) / (1 + exp(a * (theta - xi)))
  return(p)
}

## For a category
calc_pcm <- function(theta, x, delta){

  m <- length(delta)

  zaehler <- exp(sum(

    sapply(0:x, function(j){

      if(j == 0){
        diff_td <- 0
      }else{
        diff_td <- theta-delta[j]
      }
      return(diff_td)
    }
    )

  ))

  nenner <- sum(sapply(0:m, function(r){
    exp(sum(sapply(0:r, function(j){
      if(j == 0){
        diff_td <- 0
      }else{
        diff_td <- theta-delta[j]
      }
      return(diff_td)
    })))
  }))

  return(zaehler/nenner)

}


## Over all categories for a theta.This is the expected score for a person with this theta
calc_pcm_t <- function(theta, delta){

  m <- length(delta)  # Anzahl der Kategorien (m + 1 Kategorien)
  probs <- numeric(m + 1)  # Speicher für die Wahrscheinlichkeiten

  # Berechnung des Nenners (dieser ist für alle x gleich)
  nenner <- sum(sapply(0:m, function(r){
    exp(sum(sapply(0:r, function(j){
      if(j == 0){
        diff_td <- 0
      }else{
        diff_td <- theta - delta[j]
      }
      return(diff_td)
    })))
  }))

  # Berechne die Wahrscheinlichkeiten für alle Kategorien x = 0 bis m
  for (x in 0:m) {
    zaehler <- exp(sum(
      sapply(0:x, function(j){
        if(j == 0){
          diff_td <- 0
        }else{
          diff_td <- theta - delta[j]
        }
        return(diff_td)
      })
    ))
    probs[x + 1] <- zaehler / nenner  # Wahrscheinlichkeit für Kategorie x
  }

  # Erwartungswert E(X) berechnen
  E_X <- sum(0:m * probs)

  return(list(probs = probs, E_X = E_X))
}

# Funktion zur Berechnung des erwarteten Scores (E_score) für verschiedene Theta-Werte
compute_E_score <- function(theta_vals, delta_1, delta_2) {
  results <- data.frame(theta = theta_vals, E_score = numeric(length(theta_vals)))

  for (i in 1:length(theta_vals)) {
    theta <- theta_vals[i]
    result_1 <- calc_pcm_t(theta = theta, delta = delta_1)
    result_2 <- calc_pcm_t(theta = theta, delta = delta_2)

    # Berechne den erwarteten Score E_score für dieses theta
    E_score <- result_1$E_X + result_2$E_X
    results$E_score[i] <- E_score
  }

  return(results)
}


