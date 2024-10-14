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


# 1. Laengsschnittliche Modellierung
####################################

datL <- readRDS("c:/Users/weirichs/Repositories/IRT_workshop/slides/Tag2_1Vormittag_Nr2_Testdesigns/longitudinal.rds")

# gibt es einen linearen Haupteffekt der Zeit?
mod1 <- glmer(value ~ mzp + (1|item) + (1|person), data = datL, family = binomial(link="logit"))
summary(mod1)

# nonlineare Modellierung
mod2 <- glmer(value ~ factor(mzp) + (1|item) + (1|person), data = datL, family = binomial(link="logit"))
mod2m<- glmer(value ~ factor(mzp) + (1|item) + (-1+factor(mzp)|person), data = datL, family = binomial(link="logit"))
anova(mod1, mod2)

# unterscheidet sich der Lernzuwachs zwischen Jungen und Maedchen?
mod3 <- glmer(value ~ factor(mzp) * sex + (1|item) + (1|person), data = datL, family = binomial(link="logit"))

# Voraussetzungen pruefen: ist das Messmodell invariant ueber die Zeit?
# fuer alle drei Zeitpunkte kalibrieren und miteinander verlinken
kalib <- by ( data = datL, INDICES = datL[,"mzp"], FUN = function (mzp) {
         dat_wide <- tidyr::pivot_wider(mzp, id_cols ="person",
                 names_from = "item", values_from = "value") %>% as.data.frame()
         def <- defineModel(dat = dat_wide, id="person", items = -1, software="tam")
         run <- runModel(def)
         res <- getResults(run)
         prm <- itemFromRes(res)
         return(list(results = res, prm = prm))})
         
# verlinken: mzp1 vs. mzp2
# itemparameter von mzp2 gegen itemparameter von mzp1 vergleichen
mzp2  <- kalib[[2]][["prm"]][,c("item", "est")]
mzp1  <- kalib[[1]][["prm"]][,c("item", "est")]
vgl   <- mergeAttr(mzp1, mzp2, by="item", all=FALSE, setAttr=FALSE, suffixes = c("_mzp1", "_mzp2"), xName = "mzp1", yName="mzp2")
diff  <- mean(vgl[,"est_mzp1"] - vgl[,"est_mzp2"])

diff # 0.3379

# wenn die Items keinen DIF haben, sollte diese Differenz (ungefaehr) gleich fuer alle Items sein
vgl[,"item_difference"] <- vgl[,"est_mzp1"] - vgl[,"est_mzp2"]
vgl[,"DIF"] <- vgl[,"item_difference"] - diff

length(which(abs(vgl[,"DIF"]) > .64)) # mzp1 vs. mzp2: 6 Items mit |DIF| > .64

eq1.vs.2 <- equat1pl(results = kalib[[2]][["results"]], prmNorm = kalib[[1]][["prm"]][,c("item", "est")], difBound = 0.64, iterativ = TRUE)
eq2.vs.3 <- equat1pl(results = kalib[[3]][["results"]], prmNorm = kalib[[2]][["prm"]][,c("item", "est")], difBound = 0.64, iterativ = TRUE)
eq1.vs.3 <- equat1pl(results = kalib[[3]][["results"]], prmNorm = kalib[[1]][["prm"]][,c("item", "est")], difBound = 0.64, iterativ = TRUE)

# A282?
subset(kalib[[1]][["prm"]], item=="A282") # mzp1: nicht vorhanden
roundDF(subset(kalib[[2]][["prm"]], item=="A282"), digits=3) # mzp2: item extrem schwer
roundDF(subset(kalib[[3]][["prm"]], item=="A282"), digits=3) # mzp3: item extrem schwer

# A282 entfernen
datL.sel <- datL[-which(datL[,"item"] == "A282"),]

# DIF oder kein DIF? --> partielle Messinvarianz
# DIF mzp1 vs. mzp2: messzeitpunktspezifische Itemparameter
# Item-IDs zu mzp1 umbenennen
mzp1 <- subset(datL.sel, mzp==1)
alt  <- setdiff(eq1.vs.2[["items"]][[1]][["Dim1"]][["info"]][,"itemExcluded"], "")
altneu <- data.frame ( alt = alt, neu = paste(alt, "mzp1", sep="_"))
mzp1[,"item"] <- recodeLookup(mzp1[,"item"], altneu)

# DIF mzp1 vs. mzp3: messzeitpunktspezifische Itemparameter
# Item-IDs zu mzp3 umbenennen
mzp3 <- subset(datL.sel, mzp==3)
alt  <- setdiff(eq1.vs.3[["items"]][[1]][["Dim1"]][["info"]][,"itemExcluded"], "")
altneu <- data.frame ( alt = alt, neu = paste(alt, "mzp3", sep="_"))
mzp3[,"item"] <- recodeLookup(mzp3[,"item"], altneu)

# DIF mzp2 vs. mzp3: messzeitpunktspezifische Itemparameter
# Item-IDs zu mzp2 umbenennen
mzp2 <- subset(datL.sel, mzp==2)
alt  <- setdiff(eq2.vs.3[["items"]][[1]][["Dim1"]][["info"]][,"itemExcluded"], "")
altneu <- data.frame ( alt = alt, neu = paste(alt, "mzp2", sep="_"))
mzp2[,"item"] <- recodeLookup(mzp2[,"item"], altneu)

# gesamtdatensatz
datL.partInvariance <- rbind(mzp1, mzp2, mzp3)

# nonlineare Modellierung (unidimensional und multidimensional)
mod2.pi <- glmer(value ~ factor(mzp) + (1|item) + (1|person), data = datL.partInvariance, family = binomial(link="logit"))
mod2.piM<- glmer(value ~ factor(mzp) + (1|item) + (-1+factor(mzp)|person), data = datL.partInvariance, family = binomial(link="logit"))

# dropout?
personen <- by(data = datL.partInvariance, INDICES = datL.partInvariance[,"mzp"], FUN = function (x) {unique(x[,"person"])})

# nur die personen beibehalten, die zu allen messzeitpunkten teilgenommen haben
commonPersons <- intersect(intersect(personen[[1]], personen[[2]]), personen[[3]])

# teildatensdatz dieser personen
dat.common.pers <- subset(datL.partInvariance, person %in% commonPersons)

mod2.cp <- glmer(value ~ factor(mzp) + (1|item) + (1|person), data = dat.common.pers, family = binomial(link="logit"))
mod2.cpM<- glmer(value ~ factor(mzp) + (1|item) + (-1+factor(mzp)|person), data = dat.common.pers, family = binomial(link="logit"))


# 2. Laengsschnittliche Modellierung mithilfe plausible values
##############################################################

# Datensatz fuer alle drei Messzeitpunkte mit Geschlecht als Hintergrundvariable kalibrieren
# und PVs ziehen

# Schritt 1: Referenzzeitpunkt festlegen, um die Metrik zu definieren (Messzeitpunkt 1)
# Daten zu Messzeitpunkt 1 kalibrieren, um Refrenzitemparameter zu gewinnen, die dann zu
# den anderen Messzeitpunkten fixiert werden
datWide_mzp1 <- subset(datL.partInvariance, mzp==1) %>%
                tidyr::pivot_wider(id_cols ="person",
                   names_from = "item", values_from = "value") %>% as.data.frame()
def <- defineModel(dat = datWide_mzp1, id="person", items = grep("^A", colnames(datWide_mzp1), value=TRUE), software="tam")
run <- runModel(def)
res <- getResults(run)
items_mzp1 <- itemFromRes(res)

# PVs ziehen fuer alle drei Messzeitpunkte mit verankerten Itemparametern
kalib <- by ( data = datL.partInvariance, INDICES = datL.partInvariance[,"mzp"], FUN = function (mzp) {
         datWide <- tidyr::pivot_wider(mzp, id_cols =c("person", "sex"),
                   names_from = "item", values_from = "value") %>% as.data.frame()
         def <- defineModel(dat = datWide, id="person", HG.var = "sex", anchor = items_mzp1[,c("item", "est")], items = grep("^A", colnames(datWide), value=TRUE), software="tam", n.plausible = 10)
         run <- runModel(def)
         res <- getResults(run, ntheta = 20000, theta.model = FALSE)
         return(res)})

# PVs aus dem Rueckgabeobjekt extrahieren
# dazu die Funktion pvFromRes() fuer alle drei MZP ausfuehren
PVs  <- lapply(kalib, pvFromRes, toWideFormat = FALSE)
PV_dat <- do_call_rbind_withName(df_list = PVs, colName="mzp")

# Daten sind imputiert, man muss die Analysen zehnmal ausfuehren und
# hinterher poolen
mods  <- by(data = PV_dat, INDICES = PV_dat[,"imp"], FUN = function (impdata) {lmer(value~factor(mzp) + (1|person), REML = FALSE, data = impdata)})
pool1 <- miceadds::lmer_pool(mods)
txt1  <- capture.output(summ1 <- summary(pool1))

PV_dat_common <- subset(PV_dat, person %in% commonPersons)
mods2 <- by(data = PV_dat_common, INDICES = PV_dat_common[,"imp"], FUN = function (impdata) {lmer(value~factor(mzp) + (1|person), REML = FALSE, data = impdata)})
pool2 <- miceadds::lmer_pool(mods2)
txt2  <- capture.output(summ2 <- summary(pool2))
