
calc_2pl <- function(a, theta, xi){
  psi <- (exp(a*(theta - xi)))/(1 + exp(a*(theta - xi)))
  return(psi)
}


theta_data <- data.frame(
  person = 1:20000,
  theta = c(rnorm(10000, 0, 1), rnorm(10000, 1, 2)),
  group = c(rep("1", 10000), rep("2", 10000))
)

items <- data.frame(
  id = 1:13,
  b = seq(-3, 3, by = 0.5),
  a = seq(0.5, 1.5, length.out = 13),
  c = rep(0, 13)
)

sim_dat <- merge(theta_data, items) %>%
  mutate(prop = calc_2pl(a, theta, b)) %>%
  mutate(Answer = rbinom(n = nrow(.), size = 1, prob = prop))

## could also try sim_irt

## With sim_irt
library(catIrt)

simirt_dat <- simIrt(theta =rnorm(100000, 0, 1), params = as.matrix(items[, c("a", "b", "c")]), mod = "brm")

```


```{r}
#| eval: FALSE

## Analyze with a package to see if everything worked.
library(mirt)
library(TAM)

## Into wide format
sim_dat_wide <- sim_dat %>%
  filter(group == "1") %>%
  select(id, Answer, theta, person) %>%
  pivot_wider(names_from = id, values_from = Answer, id_cols = person)

fit2PL <- tam.mml.2pl(sim_dat_wide %>% select(-person), irtmodel = "2PL")
# saveRDS(fit2PL, file = "./slides/Linking/dat/fit2PL.RDS")
fit2PL <- readRDS(file = "./slides/Linking/dat/fit2PL.RDS")

apply(fit2PL$item_irt[, c("alpha", "beta")], 2, round, 2)
# Actually worked!


fit2PL_2 <- tam.mml.2pl(as.data.frame(simirt_dat$resp), irtmodel = "2PL")
# saveRDS(fit2PL_2, file = "./slides/Linking/dat/fit2PL_simfun.RDS")
fit2PL_2 <- readRDS(file = "./slides/Linking/dat/fit2PL_simfun.RDS")

apply(fit2PL_2$item_irt[, c("alpha", "beta")], 2, round, 2)
# This worked fairly well

fit2PL <- mirt(data = sim_dat_wide %>% select(-person),
               itemtype = "2PL",
               verbose = FALSE)

params2PL <- coef(fit2PL, IRTpars = TRUE, simplify = TRUE)
```


## Kalibrierte Parameter für 2 GLEICHE Gruppen

```{r}
#| eval: FALSE
#| message: FALSE

library(TAM)

calc_2pl <- function(a, theta, xi){
  psi <- (exp(a*(theta - xi)))/(1 + exp(a*(theta - xi)))
  return(psi)
}

items <- data.frame(
  id = 1:13,
  b = seq(-3, 3, by = 0.5),
  a = seq(0.5, 3.5, by = 0.25),
  c = rep(0, 13)
)

theta_data <- data.frame(
  person = 1:20000,
  theta = c(rnorm(10000, 0, 1), rnorm(10000, 0, 1)),
  group = c(rep("1", 10000), rep("2", 10000))
)

sim_dat <- merge(theta_data, items) %>%
  mutate(prop = calc_2pl(a, theta, b)) %>%
  mutate(Answer = rbinom(n = nrow(.), size = 1, prob = prop))

sim_dat_wide <- sim_dat %>%
  select(id, Answer, theta, person, group) %>%
  pivot_wider(names_from = id, values_from = Answer, id_cols = c("person", "group"))


sim_data_1 <- sim_dat_wide %>% filter(group == "1") %>% select(-group)
sim_data_2 <- sim_dat_wide %>% filter(group == "2") %>% select(-group)

fit2PL_1 <- tam.mml.2pl(sim_data_1 %>% select(-person), irtmodel = "2PL")
fit2PL_2 <- tam.mml.2pl(sim_data_2 %>% select(-person), irtmodel = "2PL")



```


## Plot
```{r}
#| eval: FALSE
par_1 <- as.data.frame(apply(fit2PL_1$item_irt[, c("alpha", "beta")], 2, round, 2))
par_2 <- as.data.frame(apply(fit2PL_2$item_irt[, c("alpha", "beta")], 2, round, 2))
colnames(par_2) <- c("alpha_2", "beta_2")

parameters <- cbind(par_1, par_2)

ggplot(data = parameters, aes(x = beta, y = beta_2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Group 1") +
  ylab("Group 2") +
  xlim(-4, 4) +
  ylim(-4, 4)


## Nice, this worked!


```


## Kalibrierte Parameter für 2 versch. Gruppen
```{r}
#| eval: FALSE
#| message: FALSE

library(TAM)

calc_2pl <- function(a, theta, xi){
  psi <- (exp(a*(theta - xi)))/(1 + exp(a*(theta - xi)))
  return(psi)
}

items <- data.frame(
  id = 1:13,
  b = seq(-3, 3, by = 0.5),
  a = seq(0.5, 3.5, by = 0.25),
  c = rep(0, 13)
)

theta_data <- data.frame(
  person = 1:20000,
  theta = c(rnorm(10000, 0, 1), rnorm(10000, 1, 1.3)),
  group = c(rep("1", 10000), rep("2", 10000))
)

sim_dat <- merge(theta_data, items) %>%
  mutate(prop = calc_2pl(a, theta, b)) %>%
  mutate(Answer = rbinom(n = nrow(.), size = 1, prob = prop))

sim_dat_wide <- sim_dat %>%
  select(id, Answer, theta, person, group) %>%
  pivot_wider(names_from = id, values_from = Answer, id_cols = c("person", "group"))


sim_data_1 <- sim_dat_wide %>% filter(group == "1") %>% select(-group)
sim_data_2 <- sim_dat_wide %>% filter(group == "2") %>% select(-group)

fit2PL_1 <- tam.mml.2pl(sim_data_1 %>% select(-person), irtmodel = "2PL")
fit2PL_2 <- tam.mml.2pl(sim_data_2 %>% select(-person), irtmodel = "2PL")



```


## Plot
```{r}
#| eval: FALSE
par_1 <- as.data.frame(apply(fit2PL_1$item_irt[, c("alpha", "beta")], 2, round, 2))
par_2 <- as.data.frame(apply(fit2PL_2$item_irt[, c("alpha", "beta")], 2, round, 2))
colnames(par_2) <- c("alpha_2", "beta_2")

parameters <- cbind(par_1, par_2)

ggplot(data = parameters, aes(x = beta, y = beta_2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Group 1") +
  ylab("Group 2") +
  xlim(-4, 4) +
  ylim(-4, 4)

## Looks good. Possibly some interaction with the discrimination parameter, look that up.


ggplot(data = parameters, aes(x = alpha, y = alpha_2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  xlab("Group 1") +
  ylab("Group 2") +
  xlim(0, 4) +
  ylim(0, 4)

## Also looks great!
