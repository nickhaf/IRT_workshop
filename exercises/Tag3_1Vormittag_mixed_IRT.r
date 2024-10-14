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


# 1. Mixed IRT model
####################

# pirls Datensatz laden
pirls <- readRDS("c:/Users/weirichs/Repositories/IRT_workshop/slides/Tag2_1Vormittag_Nr2_Testdesigns/pirls.rds")

# itemformate und modellierungstyp
table(pirls[,c("format", "type")])

# items sind nicht laenger dichotom
table(pirls[,"value"])

# datensatz ins wideformat
dat_wide <- tidyr::pivot_wider(pirls, id_cols ="IDSTUD_FDZ",
                 names_from = "item", values_from = "value") %>% as.data.frame()

# unique itemliste erzeugen
unilist  <- unique(pirls[,c("item", "format", "type", "slope", "beta", "guess", "step1", "step2", "step3")])

# itemparameter frei schaetzen
mod1     <- mirt(data = dat_wide[,unilist[,"item"]],  # Reihenfolge der Items ist hier wichtig!!
            model = 1, # number of factors (1 = unidimensional)
            itemtype = unilist[,"type"], # definiert, welches item zu welchem messmodell "gehoert"
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
            quelle <- unilist[which(unilist[,"varName"] == i[1,"item"]),]
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
modAnk   <- mirt(data = dat_wide[,setdiff(unique(skeleton1[,"item"]), "GROUP")], model = 1, D = 1, itemtype = unilist[,"type"], SE = TRUE,  verbose = FALSE, pars = skeleton1  )
coefsAnk <- coef(modAnk, IRTpars = TRUE)
coefsAnk[["R011C04M"]]    # erhaelt man die Original PIRLS-Werte? ... ja
coefs1[["R011C04M"]]      # weichen aber ziemlich gravierend von den frei geschaetzten werten ab

