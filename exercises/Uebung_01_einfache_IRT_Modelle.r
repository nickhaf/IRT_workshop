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


# 3. Raschmodll (1pl) vs. Birnbaum-Modell (2pl)
###############################################

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


# 5. Eigene Uebung fuer Teilnehmer:innen
########################################

data(data.fims.Aus.Jpn.scored)

###########
# Loesung #
###########

jpn <- data.frame ( person = 1:nrow(data.fims.Aus.Jpn.scored),data.fims.Aus.Jpn.scored)
def1pl <- defineModel(dat = jpn, id = "person", items = grep("^M1", colnames(jpn), value=TRUE), software="tam")
run1pl <- runModel(def1pl)
res1pl <- getResults(run1pl)
item1pl<- itemFromRes(res1pl)

plotICC(resultsObj=res1pl, defineModelObj = def1pl, pdfFolder = file.path(tempdir(), "iccsJapan.pdf"))

def2pl <- defineModel(dat = jpn, id = "person", items = grep("^M1", colnames(jpn), value=TRUE), software="tam", irtmodel="2PL")
run2pl <- runModel(def2pl)
anova(run1pl, run2pl)

q3.jpn  <- q3FromRes(res1pl, out = "wide", triangular = TRUE)
write.csv2(q3.jpn[[1]], file.path(tempdir(), "q3_japan.csv"), na="", row.names=FALSE)

####################
# Ende der Loesung #
####################


# 6. Differential Item Functioning
##################################

table(jpn[,"SEX"])
jpn[,"SEX"] <- jpn[,"SEX"] - 1

defDif <- defineModel(dat = jpn, id = "person", items = grep("^M1", colnames(jpn), value=TRUE), software="tam", DIF.var = "SEX", progress=TRUE)
runDif <- runModel(defDif)

# Konvergenzprobleme
plotDevianceTAM(run1pl, omitUntil = 15)

# empfehlungen fuer convergence trouble
?tam.mml

# zweiter Versuch
defDif2 <- defineModel(dat = jpn, id = "person", items = grep("^M1", colnames(jpn), value=TRUE), software="tam", DIF.var = "SEX", increment.factor=1.03, fac.oldxsi=.4, progress=TRUE)
runDif2 <- runModel(defDif2)

# sieht gut aus!
plotDevianceTAM(runDif2)

# manchmal hilft aber auch das nicht. Alternative: auf software conquest ausweichen
defDif3 <- defineModel(dat = jpn, id = "person", items = grep("^M1", colnames(jpn), value=TRUE), software="conquest", DIF.var = "SEX", dir = tempdir(), analysis.name = "DIF")
runDif3 <- runModel(defDif3)
plotDevianceConquest(file.path(tempdir(), "DIF.log"))

# einsammeln der Ergebnisse aus runDif2
resultsDif2 <- getResults(runDif2)
itemsDif2   <- itemFromRes(resultsDif2)

# vergleich
resultsDif3 <- getResults(runDif3)
itemsDif3   <- itemFromRes(resultsDif3)

