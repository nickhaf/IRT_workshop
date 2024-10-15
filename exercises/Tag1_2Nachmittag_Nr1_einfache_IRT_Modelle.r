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


library(eatTools)
library(eatModel)
library(lme4)


# 1. dichotomes Raschmodell
###########################

# Beispieldaten aus dem TAM-Paket
library(TAM)
data(data.sim.rasch)
dim(data.sim.rasch) # 2000 Personen (Zeilen); 40 Items (Spalten)

# ist alles dichotom?
table(unlist(data.sim.rasch), useNA="if") # ja, und es kommen auch keine missings vor

# descriptives: Loesungshaeufigkeiten
colMeans(data.sim.rasch)  # Item "I1" ist leicht (hohe Loesungshaeufigkeit), sollte also geringe Schwierigkeit haben
                          # Item "I40" ist schwer (geringe Loesungshaeufigkeit), sollte also hohe Schwierigkeit haben
View(rowMeans(data.sim.rasch)) # Person 1997 hat 90% der Items richtig geloest (sollte hohe Personenfaehigkeit haben)
                               # Person 459 hat nur 10% der Items richtig geloest (sollte geringe Personenfaehigkeit haben)

# modell mit TAM
modTAM <- tam.mml(resp=data.sim.rasch)

# personenfaehigkeitswerte (weighted likelihood estimates, WLE)
wle <- tam.wle(modTAM)

# dasselbe Modell jetzt in eatModel
library(eatModel)

# hierfuer ist eine Personen-ID notwendig, die es bislang noch nicht gibt
dat <- data.frame ( person = 1:nrow(data.sim.rasch), data.sim.rasch)

# erster Schritt: Modelldefinition
def <- defineModel(dat = dat, id = "person", items = 2:41, software="tam")

# zweiter Schritt: Modellschaetzung
run <- runModel(def)

# das zurueckgegebene Objekt entspricht dem von TAM erzeugten Objekt 'modTAM'
wle2 <- tam.wle(run)

# Ergebnisse in wle und wle2 sind nicht exakt identisch!


# 2. Itemfit/Infit
##################

# aus der eatModel-Analyse saemtliche Modellergebnisse einlesen
results <- getResults(run)

# itemparameter aus diesem results-Objekt extrahieren
item <- itemFromRes(results)

# item response kurven als pdf erzeugen lassen
plotICC(resultsObj=results, defineModelObj = def, pdfFolder = file.path(tempdir(), "iccs.pdf"))


# 3. Raschmodell (1pl) vs. Birnbaum-Modell (2pl)
################################################

# 2pl Modell definieren und schaetzen
def2pl <- defineModel(dat = dat, id = "person", items = 2:41, software="tam", irtmodel = "2PL")
run2pl <- runModel(def2pl)

# modellvergleich
anova(run, run2pl)


# 4. Lokale stochastische Unabhaengigkeit
#########################################

# geprueft ueber die Q3-Statistik (Yen, 1984, 1993)
q3  <- q3FromRes(results, out = "wide", triangular = TRUE)
write.csv2(q3[[1]], file.path(tempdir(), "q3.csv"), na="", row.names=FALSE)


# 5. Reliabilitaet
##################

# koennen aus dem "results"-Objekt (dem Objekt, das die getResults()-Funktion zurueckgibt, extrahiert werden)
eapRelFromRes(results)
wleRelFromRes(results)


# 6. Eigene Uebung fuer Teilnehmer:innen
########################################

# empirischen Uebungsdatensatz aufbereiten
data(trends)

# personendatensatz fuer das Jahr 2010 im wideformat
dat_wide  <- tidyr::pivot_wider(subset(trends, year == 2010),
             id_cols =c("idstud","sex", "ses", "country"),
             names_from = "item", values_from = "value") %>% as.data.frame()

# iteminformationen
item_info <- unique(subset(trends, year == 2010)[,c("item", "domain", "format")])

# 'item_info' enthaelt zuordnung der Items zu Dimensionen
qmat <- data.frame(item_info, model.matrix(~domain-1, data = item_info))


###########
# Loesung #
###########

def1pl <- defineModel(dat = dat_wide, id = "idstud", items = grep("^T", colnames(dat_wide), value=TRUE), software="tam")
run1pl <- runModel(def1pl)
res1pl <- getResults(run1pl)
item1pl<- itemFromRes(res1pl)

plotICC(resultsObj=res1pl, defineModelObj = def1pl, pdfFolder = file.path(tempdir(), "iccsLanguage.pdf"))

def2pl <- defineModel(dat = dat_wide, id = "idstud", items = grep("^T", colnames(dat_wide), value=TRUE), software="tam", irtmodel="2PL")
run2pl <- runModel(def2pl)
anova(run1pl, run2pl)

q3.jpn  <- q3FromRes(res1pl, out = "wide", triangular = TRUE)
write.csv2(q3.jpn[[1]], file.path(tempdir(), "q3_Language.csv"), na="", row.names=FALSE)


####################
# Ende der Loesung #
####################


# 7. Differential Item Functioning
##################################

# reading und listening sind zwei domaenen; wir betrachten hier deshalb nur reading

table(dat_wide[,"sex"])
dat_wide[,"sexnum"] <- car::recode(dat_wide[,"sex"], "'male'=0; 'female'=1", as.factor=FALSE)

defDif <- defineModel(dat = dat_wide, id = "idstud", items = subset(qmat, domainreading == 1)[,"item"], software="tam", DIF.var = "sexnum", progress=TRUE)
runDif <- runModel(defDif)

# Konvergenzprobleme
plotDevianceTAM(run1pl)

# empfehlungen fuer convergence trouble
?tam.mml

# zweiter Versuch
defDif2 <- defineModel(dat = dat_wide, id = "idstud", items = subset(qmat, domainreading == 1)[,"item"], software="tam", DIF.var = "sexnum", nodes = 40, increment.factor=1.1, fac.oldxsi=.8, Msteps = 25, progress=TRUE)
runDif2 <- runModel(defDif2)

# sieht etwas besser aus
plotDevianceTAM(runDif2)

# einsammeln der Ergebnisse aus runDif2
resultsDif2 <- getResults(runDif2)
itemsDif2   <- itemFromRes(resultsDif2)

# vergleich
resultsDif3 <- getResults(runDif3)
itemsDif3   <- itemFromRes(resultsDif3)


# 8. Konfirmatorische Pruefung der Eindimensionalitaet
######################################################

# analog zur 1pl vs. 2pl Logik: ein- vs. zweidimensionales Modell
# Voraussetzung: theoretische Annahme, welche Itemeigenschaften Mehrdimensionalitaet bedingen koennten

# Annahme: reading und listening sind zwei Facetten englischen Sprachverstehens

# 'item_info' enthaelt zuordnung der Items zu Dimensionen
qmat <- data.frame(item_info, model.matrix(~domain-1, data = item_info))

# zweidimensionales raschmodell
def2dim <- defineModel(dat = dat_wide, id = "idstud", items = grep("^T", colnames(dat_wide), value=TRUE), software="tam", qMatrix = qmat[,c("item","domainlistening","domainreading")])
run2dim <- runModel(def2dim)
plotDevianceTAM(run2dim)

# ergebnisse einlesen
results2dim <- getResults(run2dim)

# korrelation der beiden Facetten
correlationFromRes(results2dim, digits = 3)

# gegen eindimensionales 1pl modell vergleichen
anova(run1pl, run2dim)
