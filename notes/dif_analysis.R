
## Diese Plots sind super, die nehem ich jetzt einfach!



## 2 ganz interessant, weil kein DIF so wirklich. Item 3 ist hiuer noch mit am auffälligsten, und war auch im Paper von
## STrobl auffällig (aber in Interaktion mit age, und sie hatten nur studierende aus bayern genommen).
## 5 zeigt ein bisschen was Richtung Bestimmung von Vögeln, wie z.B. im anderen Paper.
## 3 hat glaube ich die Frage mit dem Autochef, ansonsten grünes Biosiegel.
## 4 hat wieder nichts

qa_answers_na <- qa_dat_wide %>%
  drop_na(gender)

library(eRm)

m1 <- RM(select(qa_answers_na, -gender))
m1_na <- RM(select(qa_answers_na, -gender, -ID))

summary(m1_na)



gender_diffs <- Waldtest(m1_na, splitcr = qa_answers_na$gender)

## Sooooo, wurde denn vorher gelinkt? Hat das Programm das under the hood direkt gemacht?
## Oder wie genau funktioniert das? Weil ich da ja so ein großes aufheben drum mache.
## Jaa, steht irgendwo, die Funktion übernimmt das.


## Very Big, make a lesson about sample sizes and p values out of it.

gender_1_diffs <- gender_diffs$betapar1
gender_2_diffs <- gender_diffs$betapar2

comparisons <- as.data.frame(gender_diffs$coef.table)


m1_f <- RM(select(qa_dat_f, -gender, -ID))
m1_m <- RM(select(qa_dat_m, -gender, -ID))

## Okay, irgendwie würde ich erwarten, dass die Itemparameter sich in etwa so unterscheiden, wie in dem Plot weiter unten. Dem ist aber überhaupt nicht so.
# Die sind ja auch noch gar nicht auf der gleichen Skala! Müßsten also gelinkt werden vorher!

graphics.off()
min.y <- ifelse(ceiling(min(comparisons$`z-statistic`)) > -3, -3,
                ceiling(min(comparisons$`z-statistic`))
)

max.y <- ifelse(ceiling(max(comparisons$`z-statistic`)) < 3, 3,
                ceiling(max(comparisons$`z-statistic`))
)

plot(comparisons$`z-statistic`,
     ylim = c(min.y, max.y),
     ylab = "Z", xlab = "Item", main = "Test Statistics for Item Comparisons \nbetween gender 1 and gender 2"
)
abline(h = 2, col = "red", lty = 2)
abline(h = -2, col = "red", lty = 2)

legend("topright", c("Z Statistic", "Boundaries for Significant Difference"),
       pch = c(1, NA), lty = c(NA, 2), col = c("black", "red"), cex = .7
)



### DIF Scatterplots:

## First, calculate values for constructing the confidence bands:

mean.1.2 <- ((gender_1_diffs - mean(gender_1_diffs)) / 2 * sd(gender_1_diffs) +
               (gender_2_diffs - mean(gender_2_diffs)) / 2 * sd(gender_2_diffs))

joint.se <- sqrt((gender_diffs$se.beta1^2 / sd(gender_1_diffs)) +
                   (gender_diffs$se.beta2^2 / sd(gender_2_diffs)))


upper.group.1 <- mean(gender_1_diffs) + ((mean.1.2 - joint.se) * sd(gender_1_diffs))
upper.group.2 <- mean(gender_2_diffs) + ((mean.1.2 + joint.se) * sd(gender_2_diffs))

lower.group.1 <- mean(gender_1_diffs) + ((mean.1.2 + joint.se) * sd(gender_1_diffs))
lower.group.2 <- mean(gender_2_diffs) + ((mean.1.2 - joint.se) * sd(gender_2_diffs))


upper <- cbind.data.frame(upper.group.1, upper.group.2)
upper <- upper[order(upper$upper.group.1, decreasing = FALSE), ]


lower <- cbind.data.frame(lower.group.1, lower.group.2)
lower <- lower[order(lower$lower.group.1, decreasing = FALSE), ]

## make the scatterplot:

plot(gender_1_diffs, gender_2_diffs,
     xlim = c(-2, 2), ylim = c(-2, 2),
     xlab = "Group 1", ylab = "Group 2", main = "Group 1 Measures \n plotted against \n Group 2 Measures"
)
abline(a = 0, b = 1, col = "purple")

par(new = T)

lines(upper$upper.group.1, upper$upper.group.2, lty = 2, col = "red")

lines(lower$lower.group.1, lower$lower.group.2, lty = 2, col = "red")

legend("bottomright", c("Item Location", "Identity Line", "95% Confidence Band"),
       pch = c(1, NA, NA), lty = c(NA, 1, 2), col = c("black", "purple", "red")
)



### Bar plot of item differences:

# First, calculate difference in difficulty between subgroups
# Note that I multiplied by -1 to reflect item difficulty rather than easiness (eRm quirk):
item_dif <- (gender_1_diffs * -1) - (gender_2_diffs * -1)


# Code to use different colors to highlight items with differences >= .5 logits:
colors <- NULL

for (item.number in 1:30) {
  colors[item.number] <- ifelse(abs(item_dif[item.number]) > .5, "dark blue", "light green")
}

# Bar plot code:
item_dif <- as.vector(item_dif)

x <- barplot(item_dif,
             horiz = TRUE, xlim = c(-2, 2),
             col = colors,
             ylim = c(1, 40),
             xlab = "Logit Difference"
)

# code to add labels to the plot:

dif_labs <- NULL

for (i in 1:length(subgroup_1_diffs)) {
  dif_labs[i] <- ifelse(item_dif[i] < 0, item_dif[i] - .2,
                        item_dif[i] + .2
  )
}

text(dif_labs, x,
     labels = c(1:length(subgroup_1_diffs)),
     xlim = c(-1.5, 1.5), cex = .8
)

# add vertical lines to highlight .5 logit differences:
abline(v = .5, lty = 3)
abline(v = -.5, lty = 3)

# add additional text to help with interpretation:

text(-1, 40, "Easier to Endorse for Group 1", cex = .8)
text(1, 40, "Easier to Endorse for Group 2", cex = .8)

legend("bottomright", c("Diff >= .5 logits", "Diff < .5 logits"),
       pch = 15, col = c("dark blue", "light green"), cex = .7
)




## Wir könnten jetzt das stärkste DIF Item entfernen und die Analyse nochmal durchführen. Ist das wirklich So?! Oder splitted das paket schon nach Items? Lassen wir aber, let's take a quick look at rasch trees!
