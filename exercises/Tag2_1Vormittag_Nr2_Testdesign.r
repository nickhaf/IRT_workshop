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


# 1. Pruefung der Designfaktoren
################################

data(trends)

# iteminformationen
item_info <- unique(subset(trends, year == 2010)[,c("item", "domain", "format")])

# 'item_info' enthaelt zuordnung der Items zu Dimensionen
qmat <- data.frame(item_info, model.matrix(~domain-1, data = item_info))

# kreuztabulieren
# beide Kompetenzbereiche wurden in allen drei Laendern zu allen drei Zeitpunkten erhoben
table(trends[,c("country", "domain")])
table(trends[,c("country", "domain", "year")])

# manche Leseaufgaben wurden erst spaeter entwickelt
table(subset(trends, domain == "reading")[,c("year", "task")])

# manche Zuhoeraufgaben wurden erst spaeter entwickelt, eine Zuhoeraufgabe (T16)
# wurde nach 2010 aus der "Testbatterie" entfernt
table(subset(trends, domain == "listening")[,c("year", "task")])

# Items in Aufgaben genestet
table(trends[,c("task", "item")])
table(subset(trends, year==2010 & domain == "reading")[,c("task", "item")])
lme4::isNested(trends[,"item"], trends[,"task"])

# Aber: Bloecke sind nicht in Testheften genestet
lme4::isNested(trends[,"block"], trends[,"booklet"])
table(subset(trends, year==2010 & domain == "reading")[,c("booklet", "block")])

# Personen zu kreuztabulieren wuerde den Platz der Konsole sprengen ...
# Laengsschnitt oder kein Laengsschnitt?
# wenn Personen in Zeitpunkten genestet, ist es KEIN echter Laengsschnitt, sondern Kohortenvergleich
lme4::isNested(trends[,"idstud"], trends[,"year"])
lme4::isNested(trends[,"idclass"], trends[,"year"])


# Verlinkung innerhalb eines Messzeitpunkts pruefen und einer Domaene pruefen
# personendatensatz im wideformat, fuer eine Beispieldomaene und ein Beispieljahr
dat_wide  <- subset(trends, year == 2015 & domain == "reading") %>%
             tidyr::pivot_wider(id_cols =c("idstud","sex", "ses", "country"),
                 names_from = "item", values_from = "value") %>% as.data.frame()

# Modelldefinition
def <- defineModel(dat = dat_wide, id = "idstud", items = grep("^T", colnames(dat_wide), value=TRUE), software="tam")

# etwas eleganter/uebersichtlicher
dat_wide_all <- trends %>% tidyr::pivot_wider(id_cols =c("idstud","sex", "ses", "country", "year"),
                 names_from = "item", values_from = "value") %>% as.data.frame()

allModels <- splitModels(qMatrix = qmat[,c("item", "domainlistening", "domainreading")], person.groups = dat_wide_all[,c("idstud", "year")], all.persons = FALSE, nCores = 1)
defAll    <- defineModel(dat = dat_wide_all, id = "idstud", software="tam", splittedModels = allModels)


### 2. Repraesentation des Designs im Datensatz
###############################################

datL <- readRDS("c:/Users/weirichs/Repositories/IRT_workshop/slides/Tag2_1Vormittag_Nr2_Testdesigns/longitudinal.rds")
datWideMzp1 <- subset(datL, mzp == 1) %>% tidyr::pivot_wider(id_cols =c("person", "TH"),
                 names_from = "item", values_from = "value") %>% as.data.frame()
