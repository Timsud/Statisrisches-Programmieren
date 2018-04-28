
########################1.AUFGABE####################################################
    ###a)### 
	   getwd() # Wir pruefen unsere work directory.
       wahl<- load("neuwal.RDATA") # Fuegen unsere Dateien in R ein.
       wahl
	   length(wahl) # Anzahl von den Objekten, die wir installiert haben.
       rm(oevp,gruene,fpoe, n, tag, monat, pilz,spoe) # mit diesem Command loeschen wir die objekte.
	   rm(wahl)
    ###b)###  
	   #Das haben wir schon in a gemacht.

    ###c)####
	   len_von_jeder <-c(length(tag) ,length(monat), length(oevp), length(fpoe), length(spoe), length(neos), length(gruene), length(pilz))
       len_von_jeder
       length(n)== len_von_jeder # vergleichen wir die laenge.
    ###d)###  #Jetzt fuegen wir neue dateien fuer 9-te Oktober.
       oevp<- c(33,oevp)
       fpoe<- c(27,fpoe)
       spoe<- c(23,spoe)
       neos<- c(6,neos)
       gruene<-c(5,gruene)
       pilz<- c(5,pilz)
       n<- c(1000,n)
       tag<- c(9,tag)
       monat<- c(10, monat)
    ###e###) 
	   dkrp_stat_stchprb <- c(summary(n), " Varianz " = var(n) ,  "  Standardabweichung "  =  sd(n)) #sinnvolle deskriptive Statistiken fuer die Stichprobengruppe
       dkrp_stat
    ###f)### 
       pilz[is.na(pilz)]<- 0 # Hier haben wir manche Dateien nicht, deshalb setzen wir 0 statt NA ein.
       ant_and_part<- 100-(oevp+fpoe+spoe+neos+gruene+pilz)   #Anteil der anderen Parteien pro Umfrage.

#######################2.AUFGABE###################################################### 
    ###a)###
      cor(spoe,n, method= "spearman") # Korrelation nach Spearman.
	  #Bei der Korrelation nach Spearman wird die monotone Beziehung zwischen zwei stetigen oder ordinalen Variablen ausgewertet. 
	  #In einer monotonen Beziehung andern sich die Variablen tendenziell gemeinsam, jedoch nicht zwangslaufig mit einer konstanten Rate.
	  #Der Korrelationskoeffizient nach Spearman basiert auf den nach Rang geordneten Werten fur die einzelnen Variablen anstelle der Rohdaten.
	  
	  cor(spoe,n, method= "pearson") # Korrelation nach Pearson.
	  #Bei der Korrelation nach Pearson wird die lineare Beziehung zwischen zwei stetigen Variablen untersucht.
	  #Eine Beziehung ist linear, wenn eine Anderung einer Variablen gemeinsam mit einer proportionalen Anderung 
	  #einer anderen Variablen auftritt.
	  
	  ### Korrelation nach Pearson ist groesser als Korrelation nach Spearman . ###
	  
    ###b)###
      erst_quart<- quantile(n, probs=0.25) # Erste Quartile.
      erst_quart
      drt_quart<- quantile(n, probs=0.75)  # Dritte Quartile.
      drt_quart
      mean(spoe[ n < erst_quart]) # Mittelwert von spoe fuer kleine Stichproben.
      mean(spoe[ n > drt_quart])  # Mittelwert von spoe  fuer groesse Stichporben. 
    ###c)### 
	  cum_mean_oevp <- cumsum(rev(oevp))/(1:length(oevp))  # Kumukative Mittelwert.	  	
	  cum_mean_oevp
	  wie_oft_wng <-   sum(mean(oevp)- cum_mean_oevp < 0) # Wie oft ist  cummean weniger.
	  wie_oft_wng
	 # 0 mal weniger >= cummean ist immer groesser.
	 
  ###d)### 
     sum(fpoe> spoe)/ length(fpoe> spoe) # rechnet ab ,in wie viel Prozent der Umfragen liegt die FPOE vor der SPOE.
     # In 42% liegt fpoe vor spoe
	 sum(fpoe[monat>=9] > spoe[monat>=9])/ length(fpoe[monat>=9] > spoe[monat>=9]) # rechnet ab ,in wie viel Prozent der Umfragen  ab September liegt die FPOE vor der SPOE.
	 # In 50% liegt fpoe vor spoe seit dem September.
  ###e)### 
     max(diff(spoe)) # die Staerkste positive = 6.
     min(diff(spoe)) # die Staerkste negative = -8.
     ifelse(max(diff(spoe))-abs( min(diff(spoe))) > 0, print( "Die staerkste Veranderung ist  positiv"), print ("Die staerkste Veranderung ist  negativ")) # Gibt aus, ob unsere staerkste Veraenderung Positiv oder negativ.
  ###f)###
     zw_strk_vern<- min(diff(spoe)[-which(diff(spoe)==min(diff(spoe)))])
     zw_strk_vern # die zweitstaerkste ist -7.
  ###g)### 
     tag[which(neos > gruene)]
     monat[which(neos > gruene)]
  ###h)### 
     paste(tag[which(neos > gruene)],monat[which(neos > gruene)],sep = ".")
  ###i)### 
    # 0's fuer tage
     tag[which(neos > gruene)][tag[which(neos > gruene)]<10] <- paste(0,tag[which(neos > gruene)][tag[which(neos > gruene)]<10],sep="")
     tag[which(neos > gruene)]
     # 0's fuer monate
     monat[which(neos > gruene)][monat[which(neos > gruene)]<10] <- paste(0,monat[which(neos > gruene)][monat[which(neos > gruene)]<10],sep="")
     monat[which(neos > gruene)]
     paste(tag[which(neos > gruene)],monat[which(neos > gruene)],sep = ".")

######################3.AUFGABE#################################################################
 ###a)###
    alpha<- 0.01  
    schtz<- oevp/n # Schaetzer fuer OEVP fuer jede Umfrage.
    konf_intrv_untr_schr<- schtz-qnorm(1-alpha/2)*sqrt((schtz*(1-schtz))/n) # Konfidenzintervalle untere Schranke
    konf_intrv_ober_schr<- schtz+qnorm(1-alpha/2)*sqrt((schtz*(1-schtz))/n)  # Konfidenzintervalle obere Schranke
	paste("[" , paste( round(100*konf_intrv_untr_schr) , round( 100* konf_intrv_ober_schr), sep= " ; " ) ,"]" , sep= "")




  ###b)### 
     stndrd_fhlr <- sd(oevp)/sqrt(n*0.8) # Der Standardfehler.

  ###c)###
  sd(oevp[monat>7]) # Standardabweichung seit August.
  sd(oevp) # Die Ergebnisse aus neuen Umfragen bei oevp zeigen klenere Standardabweichung im vergleich mit anderen, die bis August gemacht waren.
  #Das koente man auch  mit diff funktion anschaeun
  diff(oevp) # Die Ergebnisse vor dem August  zeigen eine grosse Volatilitaet.
  # Oder so diff(abs(mean(oevp)-oevp)) #
  # Vergleich von mittleren Standardfehlern. 
  mean(sd(oevp[monat>7])/sqrt(n[monat>7]*0.8))# mittlerrer Standardfehler seit August
  mean(sd(oevp)/sqrt(n*0.8)) # mittlerrer Standardfehler seit DEM ANFANG
  mean(sd(oevp[monat<=7])/sqrt(n[monat<=7]*0.8)) # mittlerrer Standardfehler BIS August
  # Sie unterscheiden sich auch ziemlich.Seit august sieht man , dass mittleren Standardfehler kleiner ist. Der Grund dafur ist wieder in Ergebnissen von den Umfragen.Weil seit August Ergebnisse konstanter sind(nicht so volatil)

 ###d)###
 ########### Algemeine Beispiel aus 3 c)##############
  name_vn_part<- oevp
  mean(sd(name_vn_part[monat > 7] )/sqrt(n[monat > 7]*0.8))
  sd( name_vn_part [monat > 7]) 

  ###e)###
i<- 1:50
glech.h<- monat[i]==monat[i+1]
glech.h[length(glech.h)]<- FALSE
df_mnt<- diff(monat)
spr_auf_andr_mnt<- c(which(df_mnt==-1),length(monat)) # Hier finden die Uebergang von einem Monat auf einen anderen, um dann in weiteren Berechnungen zu verwenden.
anz_von_glch_mnt<- c(which(df_mnt==-1)[1], diff(spr_auf_andr_mnt)) # Hier finden wir heraus, wie viel mal pro monat ein Umfrage durchgefuehrt war.
vern_von_n.stimm<- rev(n)*rep(seq(0.25, by=-0.02, length.out=length(n))[1:length(n[!glech.h])], times = rev(anz_von_glch_mnt)) # Hier zeigen wir wie hoch die Anzahl von Unentschiedenen pro Umfrage.
eff_stchprb<- rev(n)-vern_von_n.stimm # Die Anzahl von Leute, die schon entschieden haben oder effizienten Stichprobe.
eff_stchprb



##########################################################4.AUFGABE###################################################################
 ###a)###
    prop.test(x = c(fpoe[1], fpoe[2]), n = c(n[1], n[2])) # Der Uunterschied ist nicht signifikant.
    

 ###b)###
    zuff_part<- sample(1:length(fpoe), 2, replace=FALSE) # So erstellen wir zuffaliegkeit
    prop.test(x = c(fpoe[zuff_part[1]], fpoe[zuff_part[2]]), n = c(n[zuff_part[1]], n[zuff_part[2]])) # Der Untershied ist nicht signifikant.

 ###c)###

  ergeb_von_umfrg_sept <- fpoe[monat==9] # Umfrage fpoe september 
  
  ergeb_von_umfrg_okt <- fpoe[monat==10]  # Umfrage fpoe oktober.
  
  t.test( fpoe[monat==10], fpoe[monat==9])  
  # Die Werte sind hier auch nicht signifikant.
  












