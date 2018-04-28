#####################Abschlussbeispiel#################################
setwd("C:/Users/User/Documents/Daten_prj")
                                                    #### Teil 1####
list.files() # Alle files, die in dem Ordner verfuegbar sind.
bool = regexpr("Laser", list.files())>0 # So schliessen wir die Dateien, die nicht mit Laser verbunden sind, aus.
list.files() = list.files()[bool]
namen_tlm =  rep(NA,length(list.files()))
namen_tlm # Die namen aller Teilnehmer.  
# mit diese Schleife extrahieren wir die Namen alle Teilnehmer.Wir machen aber ein Paar Annahmen,
#dass die Filenames immer in gleicher Art geschrieben wird. Z.B: Die Name kommt nach dem zweiten Zeichen "_".
#und dass es nur ein Punkt "." sein wird namlich nach einer Name. 
for(i in 1:length(list.files())){
     namen_tlm[i] = substring(list.files()[i], gregexpr( "_" ,list.files())[[i]][2]+1, regexpr("[.]" ,list.files()[i])-1 )
     }
namen_tlm = gsub("-", "_",gsub(" ","_" ,namen_tlm))
all_dt= list()
for(i in 1:length(list.files())){
      all_dt= c(all_dt, name = read.table(list.files()[i], header=FALSE))
     }
      names(all_dt)= namen_tlm
all_dt
#############################################################################################################################
                                                      #### Teil 2#####
mtr_wrt = matrix(NA, ncol = 6, nrow= length(list.files())) # Zuerst machen wir ein Matrix mit vorgegebene Gro?e.
colnames(mtr_wrt) =  sort(c("rank", "team", "shots", "accuracy", "powers", "score"))     
rownames(mtr_wrt) = namen_tlm

for(i in 1:length(all_dt)){
       bool1 = gregexpr("<" ,levels(unlist(all_dt[i]))) # Nummer von der Stelle, wo dieser Zeichen vorkommt.
	   target = levels(unlist(all_dt[i]))[which(sapply(bool1, length)==2)] # Finden von allen Angabe fuer jede Name, die Passende fur uns Informationen. In unserem Fall, wo "<" Zeichen zwei mal vorkommt.
       names_trg =  numeric(length(target))
          for(l in 1:length(target)){
                 names_trg[l]=substring(target[l],gregexpr("<" ,target)[[l]][1]+1, gregexpr(">" ,target)[[l]][1]-1) # Mit dieser Schleife will alle namen mit zwei "<" rausnehmen. 
                 }
          bool2 = names_trg %in% colnames(mtr_wrt) # ich nehme nur die werte raus, die ich am anfang vorgegeben habe.
              for(j in 1:length(target[bool2])){
                 mtr_wrt[i,j] = substring(target[bool2][j],gregexpr(">" ,target[bool2])[[j]][1]+1, gregexpr("<" ,target[bool2])[[j]][2]-1) # Und endlich mit dieser Schleife fuelle ich meine Matrize mit Werten.
                 }
     }
mtr_wrt = as.data.frame(mtr_wrt)    # Hier wandle ich mein Matrix in Data Frame um. 
mtr_wrt

####Programmierung von 2b#######
#####Zuerst vorbereiten wir unsere elemente zur Bearbeitung, weil wir ein Element haben, der falsch in R aufgezeichnet war.
 scrs= list()
 scrs 
 for(i in namen_tlm){
         sc =  levels(unlist(all_dt[i]))[!(grepl("<" ,levels(unlist(all_dt[i]))))]
         sc[grep( "^EL$" , sc)-2] = paste0(sc[grep( "^EL$" , sc)], sc[grep( "^EL$" , sc)-2]) # Hier basteln wir EL und Commanda zusammen.
         sc = sc[(!grepl( "^EL$" , sc))] # Ausschliessung von diesem Element von nur ein "EL" steht.
         sc[grep( "^X-BOY$" , sc)+1] = paste0(sc[grep( "^X-BOY$" , sc)], sc[grep( "^X-BOY$" , sc)+1]) # Hier basteln wir x-boy und xtrem zusammen.
         sc = sc[(!grepl( "^X-BOY$" , sc))] # Und hier schlieesen wir element, wo nur ein x-boy geblieben ist, aus.
         sc=sc[(!grepl("player", sc))] 
         scrs[[i]] =  sc #### und hier fuegen wir alles zu einem List.
         }
scrs # Ein list, wo namen richtig geschrieben sind.
#############
###### you_hit ###############
  you_hit = matrix(NA, ncol = length(list.files()), nrow = length(list.files()))    
  colnames(you_hit)= sort(namen_tlm)
  rownames(you_hit)= namen_tlm

  for(i in 1:length(scrs)){
     for(j in 1:length(list.files())){
        you_hit[i,j] = as.numeric(substring(sort(scrs[[i]])[j], gregexpr("[;]", sort(scrs[[i]]))[[j]][1]+1, gregexpr("[;]", sort(scrs[[i]]))[[j]][2]-1))
        }
     }  
you_hit # enthalt die Information, wie oft Spieler i den Spieler j getroffen hat. 
###############################
####### hit_you ###############
  hit_you =  matrix(NA, ncol = length(list.files()), nrow = length(list.files()))
  colnames(hit_you)= sort(namen_tlm)
  rownames(hit_you)= namen_tlm

  for(i in 1:length(scrs)){
     for(j in 1:length(list.files())){
        hit_you[i,j] = as.numeric(substring(sort(scrs[[i]])[j], gregexpr("[;]", sort(scrs[[i]]))[[j]][2]+1,  nchar(sort(scrs[[i]]))[[j]]))
        }
     }  
hit_you #  enthalt die Information, wie oft Spieler i von Spieler j getroffen wurde
rowSums(hit_you, na.rm=TRUE)

###############################
######2c####### 
#### Jetzt wollen wir prufen, ob unsere Matrix you_hit und hit_you mit gleichen werte von dem List all_dt stimmen.
    ##### Pruefen von you_hit ########
    chk_youhit =  matrix(NA, ncol = 1, nrow = length(list.files()))
    rownames(chk_youhit)= namen_tlm
    chk_youhit # Diese Tabelle enthaelt die boolische Werte, ob unsere matrix you_hit mit youhit aus all_dt stimmt.
    for(i in 1:length(list.files())){
         brch_lvl = levels(all_dt[[i]])[grep("youhit" , levels(all_dt[[i]]))]  
         chk_youhit[i] =  rowSums(you_hit, na.rm =TRUE)[i] == as.numeric(substring(brch_lvl, gregexpr(">", brch_lvl)[[1]][1]+1, gregexpr("<", brch_lvl)[[1]][2]-1))
           }
    chk_youhit
    ###################################
    ##### Pruefen von hit_you #########
    chk_hityou = matrix(NA, ncol = 1, nrow = length(list.files()))
    rownames(chk_hityou)= namen_tlm
    chk_hityou # Diese Tabelle enthaelt die boolische Werte, ob unsere matrix you_hit mit youhit aus all_dt stimmt.
    for(i in 1:length(list.files())){
         brch_lvl = levels(all_dt[[i]])[grep("hityou" , levels(all_dt[[i]]))]  
         chk_hityou[i] =  rowSums(hit_you, na.rm =TRUE)[i] == as.numeric(substring(brch_lvl, gregexpr(">", brch_lvl)[[1]][1]+1, gregexpr("<", brch_lvl)[[1]][2]-1))
           }
    chk_hityou
    ###################################
##### Jetzt kontrollieren wir daruber hinaus, ob in Summe genauso viele Treffer erzielt wie eingesteckt wurden.
      rowSums(you_hit[sort(rownames(you_hit)),], na.rm=TRUE) == colSums(hit_you, na.rm=TRUE)
##### Wir konnen jetzt sehen, dass rowsum in you_hit ist dasselbe wie colsum in hit_you.
##### Oder kann man auch schreiben wie.
sum(you_hit, na.rm=TRUE) == sum(hit_you, na.rm=TRUE)
####################################################################
#####2d#######
hit_you_rel =  matrix(NA, ncol = length(list.files()), nrow = length(list.files()))
colnames(hit_you_rel)= sort(namen_tlm)
rownames(hit_you_rel)= namen_tlm
hit_you[which(hit_you %in% NA)] = 0
hit_you ### Hier speziell, um die berechnungen durchzufuhren, aendern wir "NA" auf 0.
  for(i in 1:length(list.files())){
        for(j in 1:length(list.files())){
             hit_you_rel[i,j] = hit_you[i,j]/rowSums(hit_you)[i]
            }
        }

hit_you_rel # Die Wahrscheinlichkeit von jemandem getroffen zu sein. 
rowSums(hit_you_rel) ### Zeielnsumme ist uberall gleich 1.
all(hit_you_rel >=0) ### TRUE, was heisst, dass alle Elemente >= 0 sind.  
###################
#########################################################################################################################################


                             ################ TEIL 3 ########################
team_ghr = rep(NA, length(list.files()))
names(team_ghr) = namen_tlm
for(i in 1:length(list.files())){
       team_ghr[i] = as.character(mtr_wrt[i, "team"])
       }  
team_ghr  # jetzt kennen wer zum welchen Team gehort.
tm = matrix(rep(NA, length(team_ghr)), ncol = length(unique(team_ghr)))
colnames(tm) =  unique(team_ghr)
for(i in 1:ncol(tm)){
     
       tm[,i] = names(team_ghr[team_ghr  %in%   unique(team_ghr)[i]])
    } 

tm # hier haben wir Spalten als Teams und unter die Teilnehmer von diesem Team.
###################### 3a ###############################
####### a) Wie oft hat jemand einen Spieler des gegnerischen Teams getroffen? ########

a_mtr = matrix(NA, nrow = length(list.files()), ncol=1) 
rownames(a_mtr) = namen_tlm
a_mtr
      for(i in 1:length(unique(team_ghr))){
          spl = team_ghr[team_ghr %in% unique(team_ghr)[i]] ### Die Leute, die treffen.
          ggnr = team_ghr[!(team_ghr %in% unique(team_ghr)[i])] ### Die Leute, die getroffen sind.
          a_mtr[names(rowSums(you_hit[names(spl), names(ggnr)])),] = rowSums(you_hit[names(spl), names(ggnr)])
         }
a_mtr  ###einen Spieler des gegnerischen Teams getroffen
#########################################
######## b) Wie oft hat jemand einen Spieler des eigenen Teams getroffen?
b_mtr = matrix(NA, nrow = length(list.files()), ncol=1) 
rownames(b_mtr) = namen_tlm
b_mtr
      for(i in 1:length(unique(team_ghr))){
          spl = team_ghr[team_ghr %in% unique(team_ghr)[i]] ### Die Leute, die treffen.
          ggnr = team_ghr[!(team_ghr %in% unique(team_ghr)[i])] ### Die Leute, die getroffen sind.
          b_mtr[names(rowSums(you_hit[names(spl), names(spl)])),] = rowSums(you_hit[names(spl), names(spl)], na.rm = TRUE)
         }

b_mtr  ###einen Spieler des gegnerischen Teams getroffen

all((b_mtr+a_mtr)==(rowSums(you_hit, na.rm=TRUE))) ### Hier sehen wir, dass die Summe der beiden Tabellen mit rowsums aus you_hit stimmt.
##########################################
######### c) Wie oft wurde jemand von einem Spieler des gegnerischen Teams getroffen?
c_mtr = matrix(NA, nrow = length(list.files()), ncol=1) 
rownames(c_mtr) = namen_tlm
c_mtr
      for(i in 1:length(unique(team_ghr))){
          spl = team_ghr[team_ghr %in% unique(team_ghr)[i]] ### Die Leute, die treffen.
          ggnr = team_ghr[!(team_ghr %in% unique(team_ghr)[i])] ### Die Leute, die getroffen sind.
          c_mtr[names(rowSums(hit_you[names(spl), names(ggnr)])),] = rowSums(hit_you[names(spl), names(ggnr)])
         }
c_mtr  ###einen Spieler des gegnerischen Teams getroffen.
########## d) Wie oft wurde jemand von einem Spieler des eigenen Teams getroffen? 
d_mtr = matrix(NA, nrow = length(list.files()), ncol=1) 
rownames(d_mtr) = namen_tlm
d_mtr
      for(i in 1:length(unique(team_ghr))){
          spl = team_ghr[team_ghr %in% unique(team_ghr)[i]] ### Die Leute, die treffen.
          ggnr = team_ghr[!(team_ghr %in% unique(team_ghr)[i])] ### Die Leute, die getroffen sind.
          d_mtr[names(rowSums(hit_you[names(spl), names(spl)])),] = rowSums(hit_you[names(spl), names(spl)], na.rm = TRUE)
         }
d_mtr  ###einen Spieler des gegnerischen Teams getroffen

all((c_mtr+d_mtr)==rowSums(hit_you, na.rm=TRUE)) ### Hier sehen wir, dass die Summe der beiden Tabellen mit rowsums aus hit_you stimmt.
#############################################
########### y) score abzuglich accuracy und powers.
  y_mtr = matrix(NA, nrow = length(list.files()), ncol = 1)
  rownames(y_mtr) = namen_tlm
  for(i in 1:length(list.files())){
        y_mtr[i,1] = as.numeric(as.character(mtr_wrt[i, "score"]))- as.numeric(as.character(mtr_wrt[i, "accuracy"])) - as.numeric(as.character(mtr_wrt[i, "powers"]))  
        }
  y_mtr ### score abzuglich accuracy und powers.
############
##############################################################################
#################### 3b ######################################################
########### mit Hilfe eines Gleichungssystems ##################
A = rbind(t(a_mtr),t(b_mtr),t(c_mtr),t(d_mtr))
A = t(A)
B = y_mtr
koef = solve(t(A) %*% A)%*% (t(A)%*% B) # Unsere Koeffizienten.
########### mittels Regression auf Basis von lm() #################### 
lm(B~0+A) ### Jetzt rechnen wir das mit der Hilfe von lm. Wir schreiebn 0 zu A, weil wir kein Intercept im unseren Modell haben.
#######################################################
######Jetzt wollen wir unsere Koeffiziente prufen, ob sie stimmen. ###########################
B - A%*% koef # Wir haben mit diesen Methoden Estimators gefunden, und deshalb stimmen sie nicht sondern haben eine bestimmte fehler E.
##############################################################################################
##### Ich habe auch anders mit einem Package "corpcor" gemacht. Das Ergebniss ist dasselbe. Und wenn man dann rechnet, stimmat das nicht ganz mit B, weil ein Bestimmten Stormterm gegeben ist.#######
####
####install.packages("corpcor")
######library("corpcor")
pseudoinverse(A)%*%B
A%*%koef == B   ##### Stimmt nicht####
#####################################
########################################################
#############################  Teil 4 ######################################
################## Ich mache a und b gleich auf einmal #########################################
##### Das ist unsere Funktion fkt, die unsere Spieler auf drei Gruppen teilt.
fkt <- function(x){
   quant <- quantile(x, probs = c(1/3, 2/3))
   quant <- c(-Inf, quant, Inf)
   faktor <- cut(x, breaks = quant, 
                 labels = c("3.Schwache Spieler", "2.Mittlere Spieler", "1.Starke Spieler"))
   return(faktor)
 }
ges_tm = unique(as.character(mtr_wrt[,"team"]))
mtr_wrt1 = mtr_wrt[, !(colnames(mtr_wrt)%in%c("team", "rank","powers"))]
mtr_div = matrix(NA, ncol= 5, nrow= length(list.files()))
colnames(mtr_div) = c("accuracy", "score", "shots",  "hit_you", "you_hit")
 
      for(i in 1:ncol(mtr_wrt1[mtr_wrt[,"team"] ==ges_tm[j],])){
          mtr_div[,i] =  as.character(fkt(as.numeric(as.character(mtr_wrt1[,i]))))
            if(i == 3){
              mtr_div[,i+1] = as.character(fkt(rowSums(hit_you, na.rm=TRUE)))
              mtr_div[,i+2] = as.character(fkt(rowSums(you_hit, na.rm=TRUE)))
              }
        }
        mtr_div = cbind(mtr_div, team = as.character(mtr_wrt[,"team"]))
mtr_div #### Hier haben wir ein Matrix gemacht, wo fur jede variable ein Spieler zu bestimmte Gruppe gehort. 
##### Jetzt wollen wir fur jeden Team und fur jede Variable eine eigene Balkendiagramm zeichnen bezuglich Anzahl von der Leute, die in eine bestimmte Gruppe fallen.
    set.seed(33)   
    col= rainbow(5)[sample(length(rainbow(5)), size = 3,replace=FALSE)]  
    for(i in 1:(ncol(mtr_div)-1)){
            barplot(table( mtr_div[,i],mtr_div[ , ncol(mtr_div)]), beside=TRUE, col=col, ylab = "Anzahl der Spieler")
            if(max(table( mtr_div[,i],mtr_div[ , ncol(mtr_div)]))>= 5){ ### mit dieser Iteration wollte ich den Durschnitt von dem Legend mit meinem Graph wegnehmen.Funktioniert aber nur in einem Fall leider nicht.
               legend(x=4, y=5, col=col, pch=19, legend = rev(c("3.Schwache Spieler", "2.Mittlere Spieler", "1.Starke Spieler")))
               title(main = colnames(mtr_div)[i])
               if(i < 5){
                  dev.new()
                 }
               next
              }
            legend(x=4, y=4, col=col, pch=19, legend = rev(c("3.Schwache Spieler", "2.Mittlere Spieler", "1.Starke Spieler")))
            title(main = colnames(mtr_div)[i])
               if(i < 5){
                 dev.new()
                }
            }
      
############################################################################################################
##################################################################################################
############################# Teil 5 ##############################################################
###################### 5a #######################################
sim_dat = function(a, b, c_s, d, mtr_wrt,hit_you, you_hit, zeit){
                  schritt_2 = list()
                  info = matrix(NA,ncol = 7, nrow = length(list.files()))
                  rownames(info) = namen_tlm 
                  colnames(info) = c("team", "a", "b","c", "d", "shots","score") 
                  hit_you = (hit_you/15)*zeit
                   you_hit = (you_hit/15)*zeit
                   a= (a/15)*zeit
                   info[, "a"] = a   
                   b = (b/15)*zeit
                   info[, "b"] = b
                   c_s = (c_s/15)*zeit
                   info[, "c"] = c_s
                   d = (d/15)*zeit
                   info[, "d"] = d
                   info[, "team"] = as.character(mtr_wrt[, "team"])
                   score = koef[1]*a + koef[3]*c_s
                   info[, "score"] = as.numeric(score)
                   shots = ((as.numeric(as.character(mtr_wrt[,"shots"])))/15)*zeit
                   info[, "shots"] = shots
                   info = as.data.frame(info)
                   schritt_2$info = info 
                   schritt_2$hit_you = hit_you
                   schritt_2$you_hit = you_hit
                   print(schritt_2)
              }
################################################################################
############################## 5b ##############################################			  
 g = sim_dat(a=a_mtr, b=b_mtr,c_s=c_mtr,d=d_mtr, mtr_wrt=mtr_wrt,hit_you = hit_you, you_hit=you_hit, zeit= 60)
################################################################################
############################# Teil 6 ###########################################
for(i in 1:nrow(mtr_wrt)){
       nam = g$info
       v1 = c()
       v2 = c()
       v1 = c(v1,"<head>",sapply(colnames(nam), function(x) v2 = c(v2,paste("<",x,">",sep = ""),as.character(nam[i,x]),paste("</",x,">",sep = ""),"\n")),"</head>")
       v1 = c(v1,"<body>\n")
       hit_dat = data.frame(player = sort(rownames(nam)), you_hit = g[[3]][i,],hit_you = g[[2]][i,])
       file = file(paste("las_sim_",rownames(nam)[i],"_.txt"), open = "w")
       close(file)
       file = file(paste("las_sim_",rownames(nam)[i],"_.txt"))
       write(v1,file,append = TRUE)
       write.table(hit_dat,paste("las_sim_",rownames(nam)[i],"_.txt"), row.names = FALSE, append = TRUE, sep = ";", quote = FALSE)
       file = file(paste("las_sim_",rownames(nam)[i],"_.txt"), open = "a")
       v1 = c("</body>")
       write(v1,file,append = TRUE)
       close(file)
       }
########################################################################
