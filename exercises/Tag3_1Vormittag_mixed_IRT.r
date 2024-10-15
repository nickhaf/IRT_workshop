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
library(mirt)
library(tidyverse)


# 1. Mixed IRT model
####################

# pirls Datensatz laden
pirls <- readRDS("c:/Users/weirichs/Repositories/IRT_workshop/slides/Tag2_1Vormittag_Nr2_Testdesigns/pirls.rds")

# itemformate und modellierungstyp
table(pirls[,c("format", "type")])

# items sind nicht laenger dichotom
table(pirls[,"value"])

# datensatz ins wideformat
dat_wide <- tidyr::pivot_wider(pirls, id_cols =c("IDSTUD_FDZ","deu_note"),
                 names_from = "item", values_from = "value") %>% as.data.frame()

# unique itemliste erzeugen
unilist  <- unique(pirls[,c("item", "format", "type", "slope", "beta", "guess", "step1", "step2", "step3")])

# itemparameter frei schaetzen
mod1     <- mirt(data = dat_wide[,unilist[,"item"]],  # Reihenfolge der Items ist hier wichtig!!
            model = 1,                                # number of factors (1 = unidimensional)
            itemtype = unilist[,"type"],              # definiert, welches item zu welchem messmodell "gehoert"
            SE = TRUE,  verbose = TRUE)

# itemparameter als liste
coefs1   <- coef(mod1, IRTpars = TRUE)
coefs1[["R011C10C"]] # andere parametrisierung als bei PIRLS
coefs1[["R011C04M"]]

# plots
itemplot(mod1, "R011C04M") # dichotom, 3pl
itemplot(mod1, "R021U11C") # polytom, gpcm
itemplot(mod1, "R021S15C") # Extrembeispiel, gpcm unnoetig

# testinformationsfunktion plotten
plot(mod1, type = "info")


# 2. Verankern auf originale PIRLS-Werte
########################################

# die Items auf die Werte verankern, die bei PIRLS genommen wurden
# skeleton definieren, ein "leerer" data.frame, in den dann die Verankerungswerte eingetragen werden
skeleton <- mirt(data = dat_wide[,unilist[,"item"]], model = 1, D = 1, itemtype = unilist[,"type"], SE = TRUE,  verbose = FALSE, pars = "values" )

### jetzt die Pirls-Ankerwerte in Skeleton uebertragen und est auf FALSE setzen, damit es nicht als Startwert fuer die Schaetzung verwendet wird, sondern als fester fixierter Wert
skeleton1<- do.call("rbind", by(data =skeleton, INDICES = skeleton[,"item"], FUN = function (i) {
            if ( i[1,"item"] == "GROUP") {i[,"est"] <- TRUE; return(i)}         ### MW und SD sollen ja jetzt geschaetzt werden
            quelle <- unilist[which(unilist[,"item"] == i[1,"item"]),]
            i[which(i[,"name"] == "a1"),"value"] <- quelle[["slope"]]
            i[which(i[,"name"] == "a1"),"est"]   <- FALSE                       ### b = -d/a; https://groups.google.com/g/mirt-package/c/C1E_IL9LRbM
            if(i[1,"class"] == "dich") {                                        ### d = (-1) * b*a
               i[which(i[,"name"] == "d"),"value"] <- (-1) * quelle[["beta"]] * quelle[["slope"]]
               i[which(i[,"name"] == "d"),"est"]   <- FALSE
               if ( quelle[["type"]] == "3PL") {
                   i[which(i[,"name"] == "g"),"value"] <- quelle[["guess"]]
                   i[which(i[,"name"] == "g"),"est"]   <- FALSE
               }
            }
            if(i[1,"class"] == "gpcmIRT") {
               i[grep("^b", i[,"name"]),"value"] <- quelle[["beta"]] - na.omit(unlist(quelle[grep("^step", names(quelle))]))
               i[grep("^b", i[,"name"]),"est"]   <- FALSE
            }
            return(i) }))
skeleton1<- data.frame(skeleton1[sort(skeleton1[,"parnum"],decreasing=FALSE,index.return=TRUE)$ix,], stringsAsFactors=FALSE)

### jetzt mit diesen verankerten Werten schaetzen lassen
modAnk   <- mirt(data = dat_wide[,setdiff(unique(skeleton1[,"item"]), "GROUP")], model = 1, D = 1, itemtype = unilist[,"type"], SE = TRUE,  verbose = FALSE, pars = skeleton1)
coefsAnk <- coef(modAnk, IRTpars = TRUE)
coefsAnk[["R011C04M"]]    # erhaelt man die Original PIRLS-Werte? ... ja
coefs1[["R011C04M"]]      # weichen aber ziemlich gravierend von den frei geschaetzten werten ab
                          # die nationalen Parameter unterscheiden sich teils von den internationalen

# mit latentem Hintergrundmodel
modHGM   <- mirt(data = dat_wide[,setdiff(unique(skeleton1[,"item"]), "GROUP")], model = 1, D = 1, itemtype = unilist[,"type"],
            covdata = dat_wide[,"deu_note", drop=FALSE], formula = ~ deu_note, SE = TRUE,  verbose = FALSE, pars = skeleton1)

# Fehlermeldung, aber warum?
modHGM   <- mirt(data = dat_wide[,setdiff(unique(skeleton1[,"item"]), "GROUP")], model = 1, D = 1, itemtype = unilist[,"type"],
            covdata = dat_wide[,c("IDSTUD_FDZ", "deu_note")], formula = ~ deu_note, SE = TRUE,  verbose = FALSE, pars = skeleton1)

# HGM muss in skeleton mit aufgenommen werden
skeleton2<- mirt(data = dat_wide[,unilist[,"item"]], model = 1, D = 1, itemtype = unilist[,"type"], SE = TRUE,  verbose = FALSE, pars = "values" , covdata = dat_wide[,c("IDSTUD_FDZ", "deu_note")], formula = ~ deu_note)

View(skeleton2)

# ACHTUNG, verankerte Parameter! Intercept darf nicht auf 0 gesetzt werden!
skeleton2[grep("BETA", skeleton2[,"item"]), "est"] <- TRUE
skeleton3<- do.call("rbind", by(data =skeleton2, INDICES = skeleton2[,"item"], FUN = function (i) {
            if ( i[1,"item"] == "GROUP") {i[,"est"] <- TRUE; return(i)}         ### MW und SD sollen ja jetzt geschaetzt werden
            quelle <- unilist[which(unilist[,"item"] == i[1,"item"]),]
            i[which(i[,"name"] == "a1"),"value"] <- quelle[["slope"]]
            i[which(i[,"name"] == "a1"),"est"]   <- FALSE                       ### b = -d/a; https://groups.google.com/g/mirt-package/c/C1E_IL9LRbM
            if(i[1,"class"] == "dich") {                                        ### d = (-1) * b*a
               i[which(i[,"name"] == "d"),"value"] <- (-1) * quelle[["beta"]] * quelle[["slope"]]
               i[which(i[,"name"] == "d"),"est"]   <- FALSE
               if ( quelle[["type"]] == "3PL") {
                   i[which(i[,"name"] == "g"),"value"] <- quelle[["guess"]]
                   i[which(i[,"name"] == "g"),"est"]   <- FALSE
               }
            }
            if(i[1,"class"] == "gpcmIRT") {
               i[grep("^b", i[,"name"]),"value"] <- quelle[["beta"]] - na.omit(unlist(quelle[grep("^step", names(quelle))]))
               i[grep("^b", i[,"name"]),"est"]   <- FALSE
            }
            return(i) }))
skeleton3<- data.frame(skeleton3[sort(skeleton3[,"parnum"],decreasing=FALSE,index.return=TRUE)$ix,], stringsAsFactors=FALSE)

# geht immer noch nicht!?!
modHGM   <- mirt(data = dat_wide[,setdiff(unique(skeleton3[,"item"]), c("GROUP", "BETA"))], model = 1, D = 1, itemtype = unilist[,"type"],
            covdata = dat_wide[,c("IDSTUD_FDZ", "deu_note")], formula = ~ deu_note, SE = TRUE,  verbose = FALSE, pars = skeleton3)

# SE.type
modHGM   <- mirt(data = dat_wide[,setdiff(unique(skeleton3[,"item"]), c("GROUP", "BETA"))], model = 1, D = 1, itemtype = unilist[,"type"],
            covdata = dat_wide[,c("IDSTUD_FDZ", "deu_note")], formula = ~ deu_note, SE = TRUE,  SE.type = "Richardson", verbose = FALSE, pars = skeleton3)
coefs1   <- coef(modHGM, IRTpars = TRUE)
coefs1[["lr.betas"]]

### PVs ziehen
pvs    <- fscores(modHGM, plausible.draws = 15)


# 3. Reliabilitaet in Abhaengigkeit der Anzahl verwendeter Items
################################################################

data(trends)

# personendatensatz fuer das Jahr 2010 im wideformat
dat_wide  <- tidyr::pivot_wider(subset(trends, year == 2010),
             id_cols =c("idstud","sex", "ses", "country"),
             names_from = "item", values_from = "value") %>% as.data.frame()

# iteminformationen
item_info <- unique(subset(trends, year == 2010)[,c("item", "domain", "format")])

# 'item_info' enthaelt zuordnung der Items zu Dimensionen
qmat <- data.frame(item_info, model.matrix(~domain-1, data = item_info))

# jeweils eindimensionale modellierung beider facetten
splittedModels <- splitModels(qMatrix = qmat[,c("item", "domainlistening", "domainreading")], nCores = 1)
def  <- defineModel(dat_wide, id="idstud", splittedModels = splittedModels, software="tam")
run  <- runModel(def)
res  <- getResults(run)
roundDF(eapRelFromRes(res), digits = 3)
roundDF(wleRelFromRes(res), digits = 3)


# zweidimensionale Modellierung beider Facetten
def2d  <- defineModel(dat_wide, items = qmat[,"item"], id="idstud", qMatrix = qmat[,c("item", "domainlistening", "domainreading")], software="tam")
run2d  <- runModel(def2d)
res2d  <- getResults(run2d)
correlationFromRes(res2d, digits = 3)
roundDF(eapRelFromRes(res2d), digits = 3)
roundDF(wleRelFromRes(res2d), digits = 3)


# 4. Posterior predictive mean check (PPMC)
###########################################

# hier eine sehr (!) stark vereinfachte Demonstration des Prinzips;
# fuer vertieftere Darstellung siehe Gelman, Meng & Stern, 1996; Rubin, 1984; Sinharay, 2005; Sinharay and Johnson, 2003

# man tut so, als waere das gewuenschte Modell (Raschmodell) korrekt
# hier am Beispiel lesen
data(trends)

# Lesedatensatz fuer das Jahr 2010 im wideformat
dat_read  <- subset(trends, year == 2010 & domain == "reading")
dat_wide  <- tidyr::pivot_wider(dat_read, id_cols ="idstud",
             names_from = "item", values_from = "value") %>% as.data.frame()
def       <- defineModel(dat_wide, id="idstud", items = -1, software="tam")
run  <- runModel(def)
res  <- getResults(run)
itempars <- itemFromRes(res)

personpars <- wleFromRes(res)

# itemparameter an datensatz mergen
dat_read <- merge(dat_read, itempars [,c("item", "est")], by = "item")

# personenparameter an datensatz mergen
dat_read <- merge(dat_read, personpars [,c("idstud", "wle_est")], by = "idstud")

### nun mithilfe der Item- und Personenparameter wiederholt item responses simulieren
sim_data <- lapply(1:1000, FUN = function (rep) {
            sim_dat_rep <- dat_read[,c("idstud", "item", "est", "wle_est")]
            sim_dat_rep[,"eta"] <- sim_dat_rep[,"wle_est"] - sim_dat_rep[,"est"]
            sim_dat_rep[,"value_sim"] <- item.logit ( z = sim_dat_rep[,"eta"], slope = 1, thr = 0)$x
            sim_dat_rep[,"item_viewed"] <- 1
            sim_wide <- tidyr::pivot_wider(sim_dat_rep, id_cols ="idstud",  names_from = "item", values_from = "value_sim") |> as.data.frame()
            sim_wide[,"PersonScore"] <- rowSums(sim_wide[,-1], na.rm=TRUE)
            viewed <- tidyr::pivot_wider(sim_dat_rep, id_cols ="idstud",  names_from = "item", values_from = "item_viewed") |> as.data.frame()
            sim_wide[,"PersonMax"]   <- rowSums(viewed[,-1], na.rm=TRUE)
            return(sim_wide[,c("idstud", "PersonScores", "PersonMax")])})
            
# absolute summenwerte empirisch
sumscores <- tam.wle(run)
sumscores[,"relative"] <- sumscores[,"PersonScores"] / sumscores[,"PersonMax"]

# plotten
# scores which occur empirically
library(ggplot2)
empScores <- data.frame ( table(sumscores[,"PersonScores"]))
colnames(empScores)[1] <- "score"
class(empScores[,"score"])  # factor

empScores[,"score"] <- as.numeric(as.character(empScores[,"score"]))

# schritt 1
empScores |> ggplot(aes(x=score, y=Freq)) + geom_line(color ="black")


# minimale/maximale belegung
freqTabs <- lapply(sim_data, FUN = function (x) {
    frqs <- data.frame ( table(x[,"PersonScores"]))
    colnames(frqs)[1] <- "score"
    frqs[,"score"] <- as.numeric(as.character(frqs[,"score"]))
    return(frqs)})

merged <- freqTabs[[1]]
for ( i in 2:length(freqTabs)) {
    colnames(freqTabs[[i]])[2] <- paste0(colnames(freqTabs[[i]])[2], i)
    merged <- merge(merged,freqTabs[[i]], by = "score", all=TRUE)}

merged[is.na(merged)] <- 0
merged[,"min"] <- apply(merged[,grep("^Freq", colnames(merged))], 1, FUN = min)
merged[,"max"] <- apply(merged[,grep("^Freq", colnames(merged))], 1, FUN = max)

# combined
comb <- rbind(data.frame ( group = "empirical", empScores),
        data.frame ( group = "sim.min", score = merged[,"score"], Freq = merged[,"min"]),
        data.frame ( group = "sim.max", score = merged[,"score"], Freq = merged[,"max"]))
        
# schritt 2
comb |> ggplot(aes(x=score, y=Freq)) + geom_line(aes(colour = factor(group)))

