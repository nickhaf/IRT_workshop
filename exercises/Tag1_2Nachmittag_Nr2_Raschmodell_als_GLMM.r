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

# personen- UND itemeffekte als zufaellig modelliert
mod3 <- glmer(value ~ (1|variable) + (1|person), data = datLong, family = binomial(link="logit"))


# 2. Modellieren und Pruefen statistischer Hypothesen
#####################################################

### Fragestellung: haengt die reading competence im Jahr 2010 vom sozio-oekonomischen Status und vom geschlecht ab?

# empirischen Uebungsdatensatz aufbereiten
data(trends)

# personendatensatz im wideformat
dat_wide <- reshape2::dcast(subset(trends, year == 2010 & domain == "reading"), idstud+sex+ses+country~item, value.var = "value")

# Geschlecht als kategorielle Variable mit zwei Faktorstufen
# SES als quasi-metrische Variable
descr(dat_wide[,"ses"])

# ses z-standardisieren
dat_wide[,"ses_std"] <- weights::stdz(dat_wide[,"ses"])
descr(dat_wide[,"ses_std"])

# Raschmodell mit Hintergrundmodell (conditioning model)
defHGM <- defineModel(dat = dat_wide, id = "idstud", items = grep("^T", colnames(dat_wide), value=TRUE), software="tam", HG.var = c("sex", "ses_std"))
runHGM <- runModel(defHGM)
resHGM <- getResults(runHGM)

# koeffizienten der latenten Regression
regcoefFromRes(resHGM, digits = 3)

# dasselbe Modell in lme4
# Datensatz bleibt im Langformat, standardisierte geschlechtsvariable wieder dranmergen
datL   <- subset(trends, year == 2010 & domain == "reading")
datL   <- merge(datL, dat_wide[,c("idstud", "ses_std")], by = "idstud")
modHGM <- glmer(value ~ sex + ses_std + (1|item) + (1|idstud), data = datL, family = binomial(link="logit"))
summary(modHGM)
          save.lmer.effects(lmerObj= modHGM, fileName="z:/test", standardized = FALSE, quick=TRUE)

# zusaetzliche Praediktoren aus der Itemseite: Itemformat
table(datL[,"format"])
modHGM1<- glmer(value ~ sex + ses_std + format + (1|item) + (1|idstud), data = datL, family = binomial(link="logit"))
summary(modHGM1)
          save.lmer.effects(lmerObj= modHGM1, fileName="z:/test1", standardized = FALSE, quick=TRUE)

# mit interaktion
modHGM2<- glmer(value ~ ses_std + sex * format + (1|item) + (1|idstud), data = datL, family = binomial(link="logit"))
summary(modHGM2)
          save.lmer.effects(lmerObj= modHGM2, fileName="z:/test2", standardized = FALSE, quick=TRUE)

