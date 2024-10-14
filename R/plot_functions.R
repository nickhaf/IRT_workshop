
theme_bg <- function() {
  theme_minimal() +
    theme(panel.background = element_rect(fill = '#F8F8F8', colour = '#F8F8F8'),
          plot.background = element_rect(fill = '#F8F8F8', colour = '#01364C'),
          text = element_text(colour = '#01364C'))
}

plot_pcm <- function(delta){

  # Schwellenparameter (delta)
  theta_vals <- seq(-3, 3, length.out = 100)  # Werte für theta
  x_max <- length(delta)  # Maximale Kategorie, hier nur 2 Schwellen -> 3 Kategorien

  # Leere Liste, um die Wahrscheinlichkeiten zu speichern
  results <- data.frame()

  # Berechne die Wahrscheinlichkeiten für jedes theta und jede Kategorie x
  for (theta in theta_vals) {
    for (x in 0:x_max) {
      prob <- calc_pcm(theta, x, delta)
      results <- rbind(results, data.frame(theta = theta, x = x, prob = prob))
    }
  }

  # Finde den minimalen Wert der y-Achse (für das Enden der Linien)
  min_prob <- min(results$prob)

  # Nächste theta-Werte finden, die den delta-Werten am nächsten sind
  delta_theta1 <- theta_vals[which.min(abs(theta_vals - delta[1]))]
  delta_theta2 <- theta_vals[which.min(abs(theta_vals - delta[2]))]

  # Plotte die Wahrscheinlichkeiten für jede Kategorie x
  ggplot(results, aes(x = theta, y = prob, color = factor(x))) +
    geom_line(linewidth = 1) +

    # Dezente Linien nur bis zum Kreuzungspunkt (max Wahrscheinlichkeiten bei den Näherungen von delta)
    annotate("segment", x = delta[1], xend = delta[1],
             y = min_prob, yend = max(results$prob[results$theta == delta_theta1]),
             color = "grey60", linetype = "dashed", size = 0.5) +
    annotate("segment", x = delta[2], xend = delta[2],
             y = min_prob, yend = max(results$prob[results$theta == delta_theta2]),
             color = "grey60", linetype = "dashed", size = 0.5) +
    labs(title = "Wahrscheinlichkeitskurven für das Partial Credit Model",
         x = "Theta",
         y = "Wahrscheinlichkeit",
         color = "Kategorie") +
    theme_bg()

}

set_colour_scheme <- function(color_scheme = c("#F4BA02", "#9B1B34", "#472120")){
  scale_colour_manual(values = color_scheme)
}


## Und weil ich das gleich noch ein paar mal brauche bastel ich mir mal eine Funktion draus:
plot_group_pars <- function(dat, x, y) {
  ylab_char <- gsub("_2", "", deparse(substitute(y))) ## Automatically produce ylabel

  ggplot(data = dat, aes(x = {{ x }}, y = {{ y }})) +
    geom_point(colour =  "#9B1B34") +
    geom_abline(intercept = 0, slope = 1, colour = "#01364C") +
    xlim(-4, 4) +
    ylim(-4, 4) +
    theme_bg() +
    set_colour_scheme() +
    xlab(TeX(paste0("\\hat{\\", deparse(substitute(x)), "}_1"))) +
    ylab(TeX(paste0("\\hat{\\", ylab_char, "}_2")))
}
