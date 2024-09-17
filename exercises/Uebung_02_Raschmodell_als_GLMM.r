# 0. Vorbereitung: benoetigte Pakete installieren
#################################################

# schauen, ob das Paket installiert ist
# dazu erstmal alle installierten pakete auflisten
allPackages <- installed.packages()

# welche der benoetigten Pakete sind in welcher Version vorhanden?
allPackages[grep(c("TAM|lme4|mirt|eatModel"),allPackages[,"Package"]),c("Package", "Version")]

# das fehlende bei Bedarf installieren
install.packages("TAM")
install.packages("lme4")
install.packages("mirt")
remotes::install_github("weirichs/eatModel", upgrade= "never")


# 1. Wiederholung: dichotomes Raschmodell
#########################################

# Beispieldaten aus dem TAM-Paket
library(eatModel)
data(data.sim.rasch)
dim(data.sim.rasch) # 2000 Personen (Zeilen); 40 Items (Spalten)

# Ergaenzen einer Personen-ID
dat <- data.frame ( person = 1:nrow(data.sim.rasch), data.sim.rasch)

# erster Schritt: Modelldefinition
def <- defineModel(dat = dat, id = "person", items = 2:41, software="tam")

# zweiter Schritt: Modellschaetzung
run <- runModel(def)

# results einlesen
results <- getResults(run)

# itemparameter ausgeben (= fixed effects)
items <- itemFromRes(results)

# als GLMM modellieren ... Datensatz ins Langformat
datLong <- reshape2::melt(dat, id.vars = "person", measure.vars = grep("^I", colnames(dat), value=TRUE), na.rm=TRUE)

# itemspalte sollte character oder factor sein
sapply(datLong, class)

# erstmal nur als logistisches Regressionsmodell
mod1 <- glm(value ~ variable, data = datLong, family = binomial(link="logit"))
summary(mod1)

# jetzt als GLMM, random effect der Personen dazu, Paket lme4 (nAGQ = 0 fuer schnellere Berechnung)
mod2 <- glmer(value ~ variable + (1|person), data = datLong, family = binomial(link="logit"), nAGQ =0)

# itemeffekte gegen die aus dem raschmodell vergleichen
itemsGlm <- fixef(mod2)
itemsGlm <- data.frame(item = gsub("variable", "", names(itemsGlm)), estGlmm =itemsGlm)

# vergleich
vgl <- merge(items[,c("item", "est")], itemsGlm, by="item", all=TRUE)
vgl[,"estGlmm"] <- (-1) * vgl[,"estGlmm"]
vgl[,"estGlmm"] <- vgl[,"estGlmm"] + vgl[1,"estGlmm"]

# random effects
random <- ranef(mod2)
str(random)
round(mean(random$person[,1]), digits = 4)
round(sd(random$person[,1]), digits = 4)


