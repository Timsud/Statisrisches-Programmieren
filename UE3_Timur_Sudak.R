load("school.RData")
load("teach.RData")
load("student.RData")
load("teach.agg.RData")
ls(school)
ls(student)
ls(teach)
ls(teach.agg)


#1
str(school)
str(student)
str(teach)
class(school)
class(student)
class(teach)
class(teach.agg)
# alle drei Dateien sind data frames

#2
school
str(unclass(school)) # alle variablen sind als Faktor gegeben -> was liegt dahinter?

schl <- sapply(school, unclass)
schl
bool_num_schl <- apply(schl, 2, is.numeric) # Pruefen, ob alle variablen von schl numerisch sind.
anz_schl <- sum(bool_num_schl) #Summe aller numerischen Variablen in school
anz_schl
# In school sind alle Variablen numerisch
std <- sapply(student, unclass)
std
bool_num_std <-  apply(std, 2, is.numeric)
anz_std <- sum(bool_num_std)        #Summe aller numerischen Variablen in student
# In student sind alle Variablen numerisch
tch <- sapply(teach, unclass)
tch
bool_num_tch <-  apply(tch, 2, is.numeric) 
anz_tch <- sum(bool_num_tch)
# In teach sind alle Variablen numerisch
summ_num <- sum(anz_schl, anz_std, anz_tch)     #Summe aller numerischen Variablen in teach
summ_num
sum_char <- sum(sum(apply(schl, 2, is.character)), sum(apply(std, 2, is.character)),sum(apply(tch, 2, is.character)))         #Gesamtzahl der Zeichenketten-variablen
sum_char # Summe aller char Variablen.
sum_log <- sum(sum(apply(schl, 2, is.logical)),sum(apply(std, 2, is.logical)), sum(apply(tch, 2, is.logical)))             #Gesamtzahl der logischen Variablen
sum_log # Summe aller logischen Variablen.
tabelle <- c("numerisch" =summ_num, "logisch" = sum_char , "Zeichenkette" = sum_log)
tabelle
#########Zweite Variante##########
table(sapply(school, class))
table(sapply(teach, class))
table(sapply(student, class))
#3
ls(school)
schl_bool2 <- gregexpr("PCBG[0-9]+", ls(school)) > 0   # alle Fragen auswählen, wo PCBG vorkommt
schl_bool2 
schl2 <- ls(school)[schl_bool2]       # diese Fragen in eine neue Variable einsetzen 
schl2
IDSCHOOL <- schl2
IDSCHOOL
IDSCHOOL <- sub("PCBG", "F", IDSCHOOL)          #Ersetzen von PCBG durch F in neuer Variable
IDSCHOOL



#4
wrt_sch_dts <- lapply(school, unique, na.rm = TRUE)     #Unique werte jeder Variable von dem Schuldatensatz.
wrt_sch_dts 
anz_wrt <- sapply(wrt_sch_dts, length)     #Anzahl von den werte in jeder Variable von dem Schuldatensatz.
anz_wrt

#5

ch_NUMM <-  apply(as.matrix(school), 2, as.numeric)
ch_NUMM <-  school[as.vector(apply(!apply( ch_NUMM,2,is.na),2,max) == 1)]
sapply(ch_NUMM, as.numeric)

#6
#PCBG04 uebersezen
unique(school$PCBG04)
antwrt <- c("Mehr als 90%", "76% bis 90%", "51% bis 75%", "26% bis 50%", "25% oder weniger")
levels(school$PCBG04) <- antwrt
levels(school$PCBG04)
school$PCBG04

#Umbenennung.
levels(school$PCBG04)[levels(school$PCBG04) == "51% bis 75%"] <- "0 bis 75%"
levels(school$PCBG04)[levels(school$PCBG04) == "26% bis 50%"] <- "0 bis 75%"
levels(school$PCBG04)[levels(school$PCBG04) == "25% oder weniger"] <- "0 bis 75%"
levels(school$PCBG04)
school$PCBG04
#oder eine andere Variante.
levels(school$PCBG04)[3:5] <- "0 bis 75%"


#7
tbl_voll <- rep(0, 366)
tbl_voll
zlg <- c(0:365)
zlg 
names(tbl_voll) <- zlg 
tbl_voll
tbl_voll[names(tbl_voll) %in% names(table(school$PCBG07A))] <- table(school$PCBG07A)
tbl_voll              #Fehlende werte müssen eingefügt werden
anz_NA <- length(school$PCBG07A[is.na(school$PCBG07A)])
names(anz_NA) <- "NA"
tbl_voll <- c(tbl_voll, anz_NA)
tbl_voll


#8
table(school$PCBG07A)
#Kategorien bilden
breaks = c(0, 160, 220, 260, 300)
labels = c("selten", "durchschnittlich", "oft", "sehr oft")
ktgr <- cut(as.numeric(as.character(school$PCBG07A)), breaks = breaks, labels = labels, ordered = TRUE)
school$PCBG07AA <- ktgr
school$PCBG07AA


#9
#Zusammenhang zwischen Anzahl der Tage wann die Schule offen ist (7c) und Größe der Ortes der Schule (5A)
schl_9<- na.omit(school)
schl.ofn <- (school1$PCBG07C)
schl.ofn
schl.grs <- school1$PCBG05A
schl.grs

krz_tbl <- table(schl.ofn,schl.grs)
krz_tbl
prz_tbl <- prop.table(table(schl.ofn, schl.grs)) * 100   #prozent
prz_tbl
#Fuegen colsum und rowsum hinzu.
prz_tbl <- rbind(prz_tbl,"Colsum"=colSums(prz_tbl))
prz_tbl <- cbind(prz_tbl, "Rowsum"=rowSums(prz_tbl))
prz_tbl

Splt_prz <- 100 * prop.table(krz_tbl , 2) 
Splt_prz      #Spaltenprozent
Zl_prz <- 100 * prop.table(krz_tbl , 1)
Zl_prz       #Zeilenprozent

#10
which(sapply(teach, is.numeric))	#numerisch
#Zusammenhang zwischen Physikunterricht pro Woche (16 - numerisch) und Alter (3 - kategoriell)
tch <-na.omit(teach)
tch
tch_physics <- tch$PTBP16     #numerische Variable
tch_age <- tch$PTBG03      #kategorielle Variable
table(tch_physics,tch_age)
grp_mitt <- tapply(tch_physics , tch_age, FUN = mean)    #Gruppenmittelwerte bilden
stnd_abw <- tapply(tch_physics, tch_age, FUN = sd)
n <- tapply(tch_physics, tch_age, FUN = length)
n
snd_fhl <-stnd_abw/sqrt(n)
snd_fhl
#In Dataframe zusammenfassen
zus_gep <- cbind(grp_mitt, snd_fhl)
zus_gep <- as.data.frame(zus_gep)
zus_gep # Eine tabelle mit statistischen dateien.


#11
ktg_3<- as.numeric(tch_age) ==3
ktg_4<- as.numeric(tch_age) ==4
met_3 <- na.omit(tch_physics[ktg_3])
met_4 <- na.omit(tch_physics[ktg_4])
t.test(met_3, met_4)
#Ergebnis ist  signifikant, p-Wert = 0.03879, d.h. es gibt einen signifikanten Unterschied


#12
num_tch <- sapply(teach, is.numeric)         #numerische Variablen ruassuchen
agg_sex <-  aggregate(teach[sapply(teach, is.numeric)], list(teach$PTBG02), sum) 
agg_sex
agg_schl_ebene <-  aggregate(teach[num_tch], list(teach$IDSCHOOL), mean)     #Durchschnittswerte fuer Maenner und Frauen
names(agg_schl_ebene)[1] <- "IDSCHOOL"
agg_schl_ebene


#13
na.agg <- matrix( rep(NA, 48), ncol=6)
agg_schl_ebene <- as.data.frame(rbind(as.matrix(agg_schl_ebene), na.agg))
agg_schl_ebene <- as.matrix(agg_schl_ebene)
school <- cbind(school, "agg"= agg_schl_ebene) 
school$agg.PTBG12[ which(school$agg.IDSCHOOL==5)] # als beispiel, dass das funktioiert.
#14
sex <-student$PSBG01 == "Male"
phys_lrn <-  as.numeric(as.character(student$PSBP15)) 
aggregate(cbind(sex,phys_lrn),list(student$IDSCHOOL), sum, na.rm = TRUE)
aggregate(cbind(sex,phys_lrn),list(student$IDCLASS), sum, na.rm = TRUE)
#15
#merge über gemeinsame Variable IDSCHOOL (andere gemeinsame Variablen: IDCNTRY, IDGRADE, IDGRADER)
ls(school)
ls(student)
merge(school, student, by = "IDSCHOOL")         #inner join
merge(school, student, all.school=TRUE)         # Left Join
merge(school, student, all.student=TRUE)        # Right Join
merge(school, student, all=TRUE)                #Outer Join