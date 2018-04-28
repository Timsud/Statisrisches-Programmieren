#######################Hauuebung2###############################################
##################AUFGABE1#########################
##1)##
  nrw <- load("nrw17.RData")
##a)##
   mode(nrw17) # Typ des Objektes.
   length(nrw17) # Die laenge des Objektes.
   str(nrw17)
##b)##
   nrw17$inhalt[1]  # Das 1. Element des Vektors inhalt.

##c)##
   erst_spc <- regexpr( " " ,nrw17$inhalt[1:10]) # Erste SPACE nach einem wort in einem elment Inhalt. 
   ohne_erst <- substring(nrw17$inhalt[1:10], erst_spc+ 1, length(nrw17$inhalt)) # Ohne erster Wort im Inhalt
  zwt_spc <- regexpr( " ", ohne_erst) # Finden wir gibt es zweite Space.
   nr_zw_w <- substring(ohne_erst, 1, zwt_spc-1) # Zeigen wir nur das zweite wort.
   nr_zw_w
##d)## 
    erst_kn_retw <-  nrw17$inhalt[!nrw17$isretweet][1:100] # erste Hundert Elmente, die keine rewtweets sind.
    hnd_erst_spc <- regexpr( " ", erst_kn_retw) # Stelle wo erster SPACE auftrit.
    ers_W_hnd <-  substring( erst_kn_retw, 1 ,   hnd_erst_spc-1) # Das erste Wort der ersten 100 Elemente an, die keine Retweets sind 
    ers_W_hnd 
##e)##
    retw <- nrw17$inhalt[nrw17$isretweet]  # Alle Retweets.
    retw_er_spc <- regexpr( " ", retw )  # Stelle wo erster SPACE auftrit.
    retw_ers_W <-  substring( retw , 1 , retw_er_spc-1) # Das erste Wort der allen Retweets. 
    sum(retw_ers_W != "RT")
##f)##
   tw_vrws <-  sum(regexpr("@" ,nrw17$inhalt)>0)/length(nrw17$inhalt) # Wie viel Prozent der Tweets enthalten einen Verweis auf einen beliebigen User.
   tw_vrws
##g)##
  bool_at <- regexpr("@", nrw17$inhalt) > 0 # Ziegt uns die boolische Werte, wo @ gibt
  num<-as.numeric(bool_at)  
  nul_wrt <- which(num==0) # Die stellen von werten mit dem wert 0.
  anz_von_at <- sapply(gregexpr("@", nrw17$inhalt),length) # length von jeder element
  anz_von_at[ nul_wrt]<- 0 # wo keine @ gibt setzen wir das durch 0.
  anz_von_at
  nrw17 <- c(nrw17,list( Anz_von_ats=anz_von_at)) # Fuegen wir das zu der nrw17.  nrw17
  table(anz_von_at)
  
###########AUFGABE2###############
   ##a##)
      all_wrt_lst <- strsplit(nrw17$inhalt, split = " " )
      all_wrt_vkt <- unlist(all_wrt_lst )
      all_wrt_vkt
   ##b##)
      el_mit_at <-unlist(strsplit(nrw17$inhalt[bool_at], split=" ")) # Alle Elemente 
      el_mit_at
      bool <- grepl("@", el_mit_at) # Boolische werte, wo @ gibt
      wrt_at <-  el_mit_at[bool] # Alle Worter mit @ als vektor geschrieben.
      wrt_at
   ##c##) 
       srt_nm <- sort(table(nrw17$name)) # Sortierte user nach Tweets
       lng_srt_nm <- length(srt_nm)
       ers_zhn_usr <- rev(srt_nm[(lng_srt_nm-10):lng_srt_nm]) # Die 10 User, die die meisten Tweets verfasst haben
       ers_zhn_usr
   ##d)##
    m<-table(nrw17$name) # Erstellen wir table, die uns zeigt die qunatitat von der eingaben, die ein User gemacht hat.
    spr<- c( which(diff(rev(sort(m))) < 0), X= length(m)) # Wir zeigen auf welcher Stelle kommt ein Sprung auf
    dif_spr <- c(spr[1], diff(spr)) # Finden wir heraus, wie viel mal jeder Wert wiederholt ist
    names(dif_spr)<-rev(unique(sort(m))) # Ersetzen wir die Namen und verlieren wir keine Allgemeingueltigkeit damit.
    dif_spr

 ##e)## 
    erste_hndr <-  rev(sort(table(nrw17$name)))[1:100] # Erste TOP 100
    zuf_usr <-   sample(erste_hndr ,1, replace=FALSE) # Waehlen wir zueffalig einen User.
    anz_von_verws <- sum(grepl( paste0("@",names(zuf_usr),separate= ""), nrw17$inhalt[bool_at])) # Anzahl von Referenzen ,  die, auf zufaellig gewaehlte person, gemacht war.
    anz_von_verws


#############AUFGABE3##############
###a)###  
       al_gtr <-  unlist(strsplit(nrw17$inhalt, split="")) # Alle vorhandene Zeichen in allen Tweets 
       tabl <- table(al_gtr) # Die Haeufigkeitstabelle von allen Zeichen in allen Tweets.
       tabl 
###b)###
       ltr <- c(letters, LETTERS) 
       eng_alph <- al_gtr[al_gtr %in% ltr] # Nur die Zeichen aus Tweets, die im englischen Alphabet vorkommen
       hauf_tab <-   table(eng_alph) # die Haeufigkeitstabelle davon.
       hauf_tab
###c)###
       b_wert <-  al_gtr %in% ltr # Boolischer Wert, der vorgibt wo eine grosse oder kleine englische Buchstabe gegeben ist.
       all_zch_NA <-  ifelse( b_wert, al_gtr[al_gtr %in% ltr] , "NA") # Wo FALSE gibt, ersetzen wir durch "NA".
       hfg_tbl_all_zch_NA <- table(all_zch_NA) # Jetzt erstellen wir die Haeufigkeitstabelle, wo alle werte, die nicht englische Buchstaben sind, durch "NA' ersetzt.
       hfg_tbl_all_zch_NA
###d)###
      
    data_test <- nrw17$zeit # Die Zeiten von den Tweets
    repl <- sub(pattern = ":", replacement = ".", data_test) # Zuerst ersetzen wir alle ":" zeichen durch "." .
    repl_num <-  unlist(repl) # jetzt nehmen wir list weg.
    repl_num <- as.numeric(repl_num) # Geben zur zeit numerischer Wert.
    repl_num
    rnd_repl <- round(repl_num) # Runden alle werte, um nur die Stunde zu kriegen.
    stdlch <-   paste(rnd_repl, "00" , sep=":") # Jetzt fuegen wir wieder ":" und "00" hinzu
    tbl_std <- sort(table(stdlch)) # und jetzt zeigen wir die Haeufigkeitstabelle 
    tbl_std
###e)###
   
    chisq.test(tbl_std) # Das Ergebnis ist hochsignifikant, was heisst, dass die Tweet von der Zeit abhaengt.
  
##########AUFGABE4######################


###a)###
   mtrz <-  t(sapply(rep(2, 1000), sample, prob=c(0.3,0.7), replace=TRUE, size = 700)) #Jetzt generieren wir einen Matrix mit gebrauchten Eigenschaften 1000  Zeilen und  700 Spalten. Wo "ja" in 30 prozent von faellen vorkommt.
   mtrz[mtrz==2] <- 0 # ersetzen wir alle 2'er durch 0
   dim(mtrz)
     #### al_umfr <-  ifelse(mtrz, mtrz[mtrz==1] <- "ja", mtrz[mtrz==0] <- "nein") # Ersetzen wir nummer durch character ###
     ### al_umfr ###
###b)###
 
   zuf_NR <- sample ( 1:1000 , 1000 ) # Machen wir zuffaellige Sample
    zuf_zl <-  paste0( "Zeile" , zuf_NR)  # die zeile binden wir zusammen mit den zuffaelligen werten zusammen.
    zuf_zl
    rownames(mtrz) <- zuf_zl    # Jetzt geben wir die zuffaelige werte als namen von zeilen.
    mtrz
###c)### 

   ###mtrz <-  t(sapply(rep(2, 1000), sample, prob=c(0.3,0.7), replace=TRUE, size = 700)) #Jetzt generieren wir einen Matrix mit gebrauchten Eigenschaften 1000  Zeilen und 700 Spalten ###                      700 Spalten. Wo "ja" in 30 prozent von faellen vorkommt.
   ###mtrz[mtrz==2] <- 0 # ersetzen wir alle 2'er durch 0###
   ### dim(mtrz)###

   erwrt_umfr <-  rowMeans(mtrz) #Hier berechnen wir mittelwert zeilenweise.
   erwrt_umfr
###d)###
sort(rowMeans(mtrz))[25] # Wir verwenden nur 0.025 prozent von unserer Stichprobe. 
quantile(rowMeans(mtrz) , probs=0.025) # Es kommen gleiche Werte aus.
sort(rowMeans(mtrz))[975] # Wir verwenden nur 0.975 prozent von unserer Stichprobe. 
quantile(rowMeans(mtrz) , probs=0.975)# Hier stimmt das auch ueberein.

###e)###
   
alpha<- 0.05  
schtz<- erwrt_umfr # Schaetzer fuer jede Umfrage.
n <- 1000
konf_intrv_untr_schr<- schtz-qnorm(1-alpha/2)*sqrt((schtz*(1-schtz))/n) # Konfidenzintervalle untere Schranke
konf_intrv_ober_schr<- schtz+qnorm(1-alpha/2)*sqrt((schtz*(1-schtz))/n)  # Konfidenzintervalle obere Schranke
al_konf_intrv <- paste("[" , paste( konf_intrv_untr_schr ,  konf_intrv_ober_schr, sep= " ; " ) ,"]" , sep= "")
al_konf_intrv[975] # Konfidenzintervalle von 975-te Umfrage. 
quantile(rowMeans(mtrz) , probs=0.975) # quantile mit Wahrscheinlichkeit 0.975.
al_konf_intrv[25]  # Konfidenzintervalle von 25-te Umfrage. 
quantile(rowMeans(mtrz) , probs=0.025)  # quantile mit Wahrscheinlichkeit 0.025.
   
  
###f)###
erwrt_umfr <-  rowMeans(mtrz) #  Hier berechnen wir wieder mittelwert zeilenweise.
erwrt_umfr
n_e_u <-   names(erwrt_umfr) # Nehmen die namen von jeder Erwartungswert
n_e_u
wg_zl <- sub("Zeile", replace= "",  n_e_u) # Nehmen Zeile weg
wg_zl
num_g <- (as.numeric(wg_zl)) # Wandel die werte in  numerische werte um.
num_g
srt_wdr_zl <- paste0("Zeile", sort(num_g), sep="")  # Sortiert wieder mit zeilen zusammenstellen.
srt_wdr_zl
erw_srt_zl <- erwrt_umfr[srt_wdr_zl] # Sortiert die Anteile numerisch nach den zufallig vergebenen Zeilennamen
erw_srt_zl
     
###g)###
   
  g<- sample( 700000, 2000) # Hier generieren wie 2000 zuffaelige werten mit 1 bis 7000000 moeglichen indexewerten. 
  mtrz[g] <- NA # Ersetzen wir diese zuffalswerte durch NA
  mtrz
  mn_zuf_ers <- rowMeans(mtrz, na.rm=TRUE) # Jetzt berechnen wir wieder mittelwert.
  mn_zuf_ers









   
