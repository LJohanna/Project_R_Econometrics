library(sas7bdat)
library(SASxport)
library(Hmisc)
library("plm")
library(readr)
library(Ecdat)
library(intReg)
library(xlsx)
library("AER")
library(margins)
library(mfx)
library(aod)
library(stargazer)
library(nnet)

donnee_2008 <-spss.get("/Users/johannalalou/Desktop/ESS4e04_4/ESS4e04_4.por", use.value.labels=TRUE)
donnee_2010 <-spss.get("/Users/johannalalou/Desktop/ESS5e03_3/ESS5e03_3.por", use.value.labels=TRUE)
dfchomage <- read.xlsx("/Users/johannalalou/Desktop/chomage.xls",1)

##################2008#############################

donnees_pays_2008_France=donnee_2008[which(donnee_2008$CNTRY=="France"),]
donnees_pays_2008_Grece=donnee_2008[which(donnee_2008$CNTRY=="Greece"),]
donnees_pays_2008_Espagne=donnee_2008[which(donnee_2008$CNTRY=="Spain"),]
donnees_pays_2008_Danemark=donnee_2008[which(donnee_2008$CNTRY=="Denmark"),]
donnees_pays_2008_Allemagne=donnee_2008[which(donnee_2008$CNTRY=="Germany"),]
donnees_pays_2008_RU=donnee_2008[which(donnee_2008$CNTRY=="United Kingdom"),]
donnees_pays_2008_Irlande=donnee_2008[which(donnee_2008$CNTRY=="Ireland"),]
donnees_pays_2008_Belgique=donnee_2008[which(donnee_2008$CNTRY=="Belgium"),]
donnees_pays_2008_Bulgarie=donnee_2008[which(donnee_2008$CNTRY=="Bulgaria"),]
donnees_pays_2008_Croatie=donnee_2008[which(donnee_2008$CNTRY=="Croatia"),]
donnees_pays_2008_Chypre=donnee_2008[which(donnee_2008$CNTRY=="Cyprus"),]
donnees_pays_2008_RT=donnee_2008[which(donnee_2008$CNTRY=="Czech Republic"),]
donnees_pays_2008_Estonie=donnee_2008[which(donnee_2008$CNTRY=="Estonia"),]
donnees_pays_2008_Finlande=donnee_2008[which(donnee_2008$CNTRY=="Finland"),]
donnees_pays_2008_Hongrie=donnee_2008[which(donnee_2008$CNTRY=="Hungary"),]
donnees_pays_2008_Israel=donnee_2008[which(donnee_2008$CNTRY=="Israel"),]
donnees_pays_2008_PaysBas=donnee_2008[which(donnee_2008$CNTRY=="Netherlands"),]
donnees_pays_2008_Norvege=donnee_2008[which(donnee_2008$CNTRY=="Norway"),]
donnees_pays_2008_Pologne=donnee_2008[which(donnee_2008$CNTRY=="Poland"),]
donnees_pays_2008_Portugal=donnee_2008[which(donnee_2008$CNTRY=="Portugal"),]
donnees_pays_2008_Russie=donnee_2008[which(donnee_2008$CNTRY=="Russian Federation"),]
donnees_pays_2008_Slovaquie=donnee_2008[which(donnee_2008$CNTRY=="Slovakia"),]
donnees_pays_2008_Slovenie=donnee_2008[which(donnee_2008$CNTRY=="Slovenia"),]
donnees_pays_2008_Suede=donnee_2008[which(donnee_2008$CNTRY=="Sweden"),]
donnees_pays_2008_Suisse=donnee_2008[which(donnee_2008$CNTRY=="Switzerland"),]
donnees_pays_2008_Ukraine=donnee_2008[which(donnee_2008$CNTRY=="Ukraine"),]

france_2008=donnees_pays_2008_France[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
grece_2008=donnees_pays_2008_Grece[,c("STFLIFE","STFECO","STFGOV","STFEDU","STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
espagne_2008=donnees_pays_2008_Espagne[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
danemark_2008=donnees_pays_2008_Danemark[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
allemagne_2008=donnees_pays_2008_Allemagne[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
ru_2008=donnees_pays_2008_RU[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
irlande_2008=donnees_pays_2008_Irlande[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
belgique_2008=donnees_pays_2008_Belgique[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
bulgarie_2008=donnees_pays_2008_Bulgarie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
croatie_2008=donnees_pays_2008_Croatie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
chypre_2008=donnees_pays_2008_Chypre[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
rt_2008=donnees_pays_2008_RT[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
estonie_2008=donnees_pays_2008_Estonie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
finlande_2008=donnees_pays_2008_Finlande[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
hongrie_2008=donnees_pays_2008_Hongrie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
israel_2008=donnees_pays_2008_Israel[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
paysbas_2008=donnees_pays_2008_PaysBas[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
norvege_2008=donnees_pays_2008_Norvege[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
pologne_2008=donnees_pays_2008_Pologne[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
portugal_2008=donnees_pays_2008_Portugal[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
russie_2008=donnees_pays_2008_Russie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
slovaquie_2008=donnees_pays_2008_Slovaquie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
slovenie_2008=donnees_pays_2008_Slovenie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
suede_2008=donnees_pays_2008_Suede[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
suisse_2008=donnees_pays_2008_Suisse[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
ukraine_2008=donnees_pays_2008_Ukraine[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]

france_2008=na.exclude(france_2008)
grece_2008=na.exclude(grece_2008)
espagne_2008=na.exclude(espagne_2008)
danemark_2008=na.exclude(danemark_2008)
allemagne_2008=na.exclude(allemagne_2008)
ru_2008=na.exclude(ru_2008)
irlande_2008=na.exclude(irlande_2008)
belgique_2008=na.exclude(belgique_2008)
bulgarie_2008=na.exclude(bulgarie_2008)
croatie_2008=na.exclude(croatie_2008)
chypre_2008=na.exclude(chypre_2008)
rt_2008=na.exclude(rt_2008)
estonie_2008=na.exclude(estonie_2008)
finlande_2008=na.exclude(finlande_2008)
hongrie_2008=na.exclude(hongrie_2008)
israel_2008=na.exclude(israel_2008)
paysbas_2008=na.exclude(paysbas_2008)
norvege_2008=na.exclude(norvege_2008)
pologne_2008=na.exclude(pologne_2008)
portugal_2008=na.exclude(portugal_2008)
russie_2008=na.exclude(russie_2008)
slovaquie_2008=na.exclude(slovaquie_2008)
slovenie_2008=na.exclude(slovenie_2008)
suede_2008=na.exclude(suede_2008)
suisse_2008=na.exclude(suisse_2008)
ukraine_2008=na.exclude(ukraine_2008)

for (i in 1:9) {france_2008[,i]=as.numeric(france_2008[,i])}
for (i in 1:9) {grece_2008[,i]=as.numeric(grece_2008[,i])}
for (i in 1:9) {irlande_2008[,i]=as.numeric(irlande_2008[,i])}
for (i in 1:9) {ru_2008[,i]=as.numeric(ru_2008[,i])}
for (i in 1:9) {espagne_2008[,i]=as.numeric(espagne_2008[,i])}
for (i in 1:9) {danemark_2008[,i]=as.numeric(danemark_2008[,i])}
for (i in 1:9) {allemagne_2008[,i]=as.numeric(allemagne_2008[,i])}
for (i in 1:9) {belgique_2008[,i]=as.numeric(belgique_2008[,i])}
for (i in 1:9) {bulgarie_2008[,i]=as.numeric(bulgarie_2008[,i])}
for (i in 1:9) {croatie_2008[,i]=as.numeric(croatie_2008[,i])}
for (i in 1:9) {chypre_2008[,i]=as.numeric(chypre_2008[,i])}
for (i in 1:9) {rt_2008[,i]=as.numeric(rt_2008[,i])}
for (i in 1:9) {estonie_2008[,i]=as.numeric(estonie_2008[,i])}
for (i in 1:9) {finlande_2008[,i]=as.numeric(finlande_2008[,i])}
for (i in 1:9) {hongrie_2008[,i]=as.numeric(hongrie_2008[,i])}
for (i in 1:9) {israel_2008[,i]=as.numeric(israel_2008[,i])}
for (i in 1:9) {paysbas_2008[,i]=as.numeric(paysbas_2008[,i])}
for (i in 1:9) {norvege_2008[,i]=as.numeric(norvege_2008[,i])}
for (i in 1:9) {pologne_2008[,i]=as.numeric(pologne_2008[,i])}
for (i in 1:9) {portugal_2008[,i]=as.numeric(portugal_2008[,i])}
for (i in 1:9) {russie_2008[,i]=as.numeric(russie_2008[,i])}
for (i in 1:9) {slovaquie_2008[,i]=as.numeric(slovaquie_2008[,i])}
for (i in 1:9) {suede_2008[,i]=as.numeric(suede_2008[,i])}
for (i in 1:9) {suisse_2008[,i]=as.numeric(suisse_2008[,i])}
for (i in 1:9) {ukraine_2008[,i]=as.numeric(ukraine_2008[,i])}
for (i in 1:9) {slovenie_2008[,i]=as.numeric(slovenie_2008[,i])}

#SATIS GVT
#pays=["France"; "Grèce"; "Espagne"; "RU"; "Norvège"; "Suisse"]
boxplot(france_2008$STFGOV,grece_2008$STFGOV,espagne_2008$STFGOV, ru_2008$STFGOV, norvege_2008$STFGOV, suisse_2008$STFGOV, las=2, ylab="Satisfaction par rapport au gouvernement", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))
#France et Suisse: médiane=moyenne
#Grèce, RU, Norvège: médiane>moyenne (plus d'extrêment mécontents)
#Espagne: médiane<moyenne (plus d'extrêmement contents)

#SATIS ECO
boxplot(france_2008$STFECO,grece_2008$STFECO,espagne_2008$STFECO, ru_2008$STFECO, norvege_2008$STFECO, suisse_2008$STFECO,las=2, ylab="Satisfaction par rapport à l'économie", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))
#France, RU et Suisse: médiane=moyenne
#Grèce: médiane<moyenne (plus d'extrêmement contents)
#Espagne, Norvège: médiane >moyenne (plus d'extrêment mécontents)

#SATIS VIE
boxplot(france_2008$STFLIFE,grece_2008$STFLIFE,espagne_2008$STFLIFE, ru_2008$STFLIFE, norvege_2008$STFLIFE, suisse_2008$STFLIFE,las=2, ylab="Satisfaction par rapport à la vie", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))
#Espagne et RU: médiane<moyenne (plus d'extrêmement contents)

#SATIS DEM
boxplot(france_2008$STFDEM,grece_2008$STFDEM,espagne_2008$STFDEM, ru_2008$STFDEM, norvege_2008$STFDEM, suisse_2008$STFDEM,las=2, ylab="Satisfaction par rapport à la démocratie", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))
#France: médiane<moyenne (plus d'extrêmement contents)

#CONF POL , je pense que c'est ce boxplot qu'on va sélectionner et pareil en 2010 sinon ca risque d'en faire trop..
boxplot(france_2008$TRSTPLT,grece_2008$TRSTPLT,espagne_2008$TRSTPLT, ru_2008$TRSTPLT, norvege_2008$TRSTPLT, suisse_2008$TRSTPLT,las=2, ylab="Confiance en les politiciens", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))

####################################2010#################################


donnees_pays_2010_France=donnee_2010[which(donnee_2008$CNTRY=="France"),]
donnees_pays_2010_Grece=donnee_2010[which(donnee_2008$CNTRY=="Greece"),]
donnees_pays_2010_Espagne=donnee_2010[which(donnee_2008$CNTRY=="Spain"),]
donnees_pays_2010_Danemark=donnee_2010[which(donnee_2008$CNTRY=="Denmark"),]
donnees_pays_2010_Allemagne=donnee_2010[which(donnee_2008$CNTRY=="Germany"),]
donnees_pays_2010_RU=donnee_2010[which(donnee_2008$CNTRY=="United Kingdom"),]
donnees_pays_2010_Irlande=donnee_2010[which(donnee_2010$CNTRY=="Ireland"),]
donnees_pays_2010_Belgique=donnee_2010[which(donnee_2010$CNTRY=="Belgium"),]
donnees_pays_2010_Bulgarie=donnee_2010[which(donnee_2010$CNTRY=="Bulgaria"),]
donnees_pays_2010_Croatie=donnee_2010[which(donnee_2010$CNTRY=="Croatia"),]
donnees_pays_2010_Chypre=donnee_2010[which(donnee_2010$CNTRY=="Cyprus"),]
donnees_pays_2010_RT=donnee_2010[which(donnee_2010$CNTRY=="Czech Republic"),]
donnees_pays_2010_Estonie=donnee_2010[which(donnee_2010$CNTRY=="Estonia"),]
donnees_pays_2010_Finlande=donnee_2010[which(donnee_2010$CNTRY=="Finland"),]
donnees_pays_2010_Hongrie=donnee_2010[which(donnee_2010$CNTRY=="Hungary"),]
donnees_pays_2010_Israel=donnee_2010[which(donnee_2010$CNTRY=="Israel"),]
donnees_pays_2010_PaysBas=donnee_2010[which(donnee_2010$CNTRY=="Netherlands"),]
donnees_pays_2010_Norvege=donnee_2010[which(donnee_2010$CNTRY=="Norway"),]
donnees_pays_2010_Pologne=donnee_2010[which(donnee_2010$CNTRY=="Poland"),]
donnees_pays_2010_Portugal=donnee_2010[which(donnee_2010$CNTRY=="Portugal"),]
donnees_pays_2010_Russie=donnee_2010[which(donnee_2010$CNTRY=="Russian Federation"),]
donnees_pays_2010_Slovaquie=donnee_2010[which(donnee_2010$CNTRY=="Slovakia"),]
donnees_pays_2010_Slovenie=donnee_2010[which(donnee_2010$CNTRY=="Slovenia"),]
donnees_pays_2010_Suede=donnee_2010[which(donnee_2010$CNTRY=="Sweden"),]
donnees_pays_2010_Suisse=donnee_2010[which(donnee_2010$CNTRY=="Switzerland"),]
donnees_pays_2010_Ukraine=donnee_2010[which(donnee_2010$CNTRY=="Ukraine"),]

france_2010=donnees_pays_2010_France[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
grece_2010=donnees_pays_2010_Grece[,c("STFLIFE","STFECO","STFGOV","STFEDU","STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
espagne_2010=donnees_pays_2010_Espagne[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
danemark_2010=donnees_pays_2010_Danemark[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
allemagne_2010=donnees_pays_2010_Allemagne[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
ru_2010=donnees_pays_2010_RU[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
irlande_2010=donnees_pays_2010_Irlande[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
belgique_2010=donnees_pays_2010_Belgique[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
bulgarie_2010=donnees_pays_2010_Bulgarie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
croatie_2010=donnees_pays_2010_Croatie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
chypre_2010=donnees_pays_2010_Chypre[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
rt_2010=donnees_pays_2010_RT[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
estonie_2010=donnees_pays_2010_Estonie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
finlande_2010=donnees_pays_2010_Finlande[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
hongrie_2010=donnees_pays_2010_Hongrie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
israel_2010=donnees_pays_2010_Israel[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
paysbas_2010=donnees_pays_2010_PaysBas[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
norvege_2010=donnees_pays_2010_Norvege[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
pologne_2010=donnees_pays_2010_Pologne[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
portugal_2010=donnees_pays_2010_Portugal[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
russie_2010=donnees_pays_2010_Russie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
slovaquie_2010=donnees_pays_2010_Slovaquie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
slovenie_2010=donnees_pays_2010_Slovenie[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
suede_2010=donnees_pays_2010_Suede[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
suisse_2010=donnees_pays_2010_Suisse[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
ukraine_2010=donnees_pays_2010_Ukraine[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]

france_2010=na.exclude(france_2010)
grece_2010=na.exclude(grece_2010)
espagne_2010=na.exclude(espagne_2010)
danemark_2010=na.exclude(danemark_2010)
allemagne_2010=na.exclude(allemagne_2010)
ru_2010=na.exclude(ru_2010)
irlande_2010=na.exclude(irlande_2010)
belgique_2010=na.exclude(belgique_2010)
bulgarie_2010=na.exclude(bulgarie_2010)
croatie_2010=na.exclude(croatie_2010)
chypre_2010=na.exclude(chypre_2010)
rt_2010=na.exclude(rt_2010)
estonie_2010=na.exclude(estonie_2010)
finlande_2010=na.exclude(finlande_2010)
hongrie_2010=na.exclude(hongrie_2010)
israel_2010=na.exclude(israel_2010)
paysbas_2010=na.exclude(paysbas_2010)
norvege_2010=na.exclude(norvege_2010)
pologne_2010=na.exclude(pologne_2010)
portugal_2010=na.exclude(portugal_2010)
russie_2010=na.exclude(russie_2010)
slovaquie_2010=na.exclude(slovaquie_2010)
slovenie_2010=na.exclude(slovenie_2010)
suede_2010=na.exclude(suede_2010)
suisse_2010=na.exclude(suisse_2010)
ukraine_2010=na.exclude(ukraine_2010)

for (i in 1:9) {france_2010[,i]=as.numeric(france_2010[,i])}
for (i in 1:9) {grece_2010[,i]=as.numeric(grece_2010[,i])}
for (i in 1:9) {irlande_2010[,i]=as.numeric(irlande_2010[,i])}
for (i in 1:9) {ru_2010[,i]=as.numeric(ru_2010[,i])}
for (i in 1:9) {espagne_2010[,i]=as.numeric(espagne_2010[,i])}
for (i in 1:9) {danemark_2010[,i]=as.numeric(danemark_2010[,i])}
for (i in 1:9) {allemagne_2010[,i]=as.numeric(allemagne_2010[,i])}
for (i in 1:9) {belgique_2010[,i]=as.numeric(belgique_2010[,i])}
for (i in 1:9) {bulgarie_2010[,i]=as.numeric(bulgarie_2010[,i])}
for (i in 1:9) {croatie_2010[,i]=as.numeric(croatie_2010[,i])}
for (i in 1:9) {chypre_2010[,i]=as.numeric(chypre_2010[,i])}
for (i in 1:9) {rt_2010[,i]=as.numeric(rt_2010[,i])}
for (i in 1:9) {estonie_2010[,i]=as.numeric(estonie_2010[,i])}
for (i in 1:9) {finlande_2010[,i]=as.numeric(finlande_2010[,i])}
for (i in 1:9) {hongrie_2010[,i]=as.numeric(hongrie_2010[,i])}
for (i in 1:9) {israel_2010[,i]=as.numeric(israel_2010[,i])}
for (i in 1:9) {paysbas_2010[,i]=as.numeric(paysbas_2010[,i])}
for (i in 1:9) {norvege_2010[,i]=as.numeric(norvege_2010[,i])}
for (i in 1:9) {pologne_2010[,i]=as.numeric(pologne_2010[,i])}
for (i in 1:9) {portugal_2010[,i]=as.numeric(portugal_2010[,i])}
for (i in 1:9) {russie_2010[,i]=as.numeric(russie_2010[,i])}
for (i in 1:9) {slovaquie_2010[,i]=as.numeric(slovaquie_2010[,i])}
for (i in 1:9) {suede_2010[,i]=as.numeric(suede_2010[,i])}
for (i in 1:9) {suisse_2010[,i]=as.numeric(suisse_2010[,i])}
for (i in 1:9) {ukraine_2010[,i]=as.numeric(ukraine_2010[,i])}
for (i in 1:9) {slovenie_2010[,i]=as.numeric(slovenie_2010[,i])}

#SATIS GVT
boxplot(france_2010$STFGOV,grece_2010$STFGOV,espagne_2010$STFGOV, ru_2010$STFGOV, norvege_2010$STFGOV, suisse_2010$STFGOV, las=2, ylab="Satisfaction par rapport au gouvernement", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))
#RU et Suisse: médiane=moyenne
#Grèce, Espagne, Norvège: médiane>moyenne (plus d'extrêment mécontents)
#France: médiane<moyenne (plus d'extrêmement contents)

#SATIS ECO
boxplot(france_2010$STFECO,grece_2010$STFECO,espagne_2010$STFECO, ru_2010$STFECO, norvege_2010$STFECO, suisse_2010$STFECO,las=2, ylab="Satisfaction par rapport à l'économie", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))
#Grèce, Norvège: médiane=moyenne
#RU et Suisse: médiane<moyenne (plus d'extrêmement contents)
#Espagne, France: médiane >moyenne (plus d'extrêment mécontents)

#SATIS VIE
boxplot(france_2010$STFLIFE,grece_2010$STFLIFE,espagne_2010$STFLIFE, ru_2010$STFLIFE, norvege_2010$STFLIFE, suisse_2010$STFLIFE,las=2, ylab="Satisfaction par rapport à la vie", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))
#Espagne: médiane<moyenne (plus d'extrêmement contents)
#Suisse: médiane >moyenne (plus d'extrêment mécontents)

#SATIS DEM
boxplot(france_2010$STFDEM,grece_2010$STFDEM,espagne_2010$STFDEM, ru_2010$STFDEM, norvege_2010$STFDEM, suisse_2010$STFDEM, las=2, ylab="Satisfaction par rapport à la démocratie", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))
#France: médiane<moyenne (plus d'extrêmement contents)

#CONF POL, on voit que par rapport à 2008 ( début de la crise ) il y a une baisse globale de la confiance, sauf Norvege et Suisse 
boxplot(france_2010$TRSTPLT,grece_2010$TRSTPLT,espagne_2010$TRSTPLT, ru_2010$TRSTPLT, norvege_2010$TRSTPLT, suisse_2010$TRSTPLT,las=2, ylab="Confiance en les politiciens", names=c("France", "Grèce", "Espagne", "RU", "Norvège", "Suisse"))


#COMPARAISON PAR PAYS PAR ANNÉE
boxplot(france_2008$TRSTPLT,france_2010$TRSTPLT,france_2008$STFGOV, france_2010$STFGOV, france_2008$STFECO, france_2010$STFECO, france_2008$STFLIFE, france_2010$STFLIFE, france_2008$STFDEM, france_2010$STFDEM, las=2, ylab="Satisfaction en France", names=c(" Conf_8","Conf_10","Gvt_8", "Gvt_2010", "Eco_8", "Eco_10", "Vie_8", "Vie_10", "Dém_8","Dém_10"))
#France: éco: plus de contents, confiance: plus de mécontents
boxplot(grece_2008$TRSTPLT,grece_2010$TRSTPLT,grece_2008$STFGOV, grece_2010$STFGOV, grece_2008$STFECO, grece_2010$STFECO, grece_2008$STFLIFE, grece_2010$STFLIFE, grece_2008$STFDEM, grece_2010$STFDEM, las=2, ylab="Satisfaction en Grèce", names=c(" Confiance politiciens 2008","Confiance politiciens 2010","Gouvernement 2008", "Gouvernement 2010", "Economie 2008", "Economie 2010", "Vie 2008", "Vie 2010", "Démocratie 2008","Démocratie 2010"))
#Grèce:vie: plus de mécontents, éco:idem,  dém: plus de mécontents
boxplot(espagne_2008$TRSTPLT,espagne_2010$TRSTPLT,espagne_2008$STFGOV, espagne_2010$STFGOV, espagne_2008$STFECO, espagne_2010$STFECO, espagne_2008$STFLIFE, espagne_2010$STFLIFE, espagne_2008$STFDEM, espagne_2010$STFDEM, las=2, ylab="Satisfaction en Espagne", names=c(" Confiance politiciens 2008","Confiance politiciens 2010","Gouvernement 2008", "Gouvernement 2010", "Economie 2008", "Economie 2010", "Vie 2008", "Vie 2010", "Démocratie 2008","Démocratie 2010"))
#Espagne: vie: plus de mécontents, dém= plus de mécontents
boxplot(ru_2008$TRSTPLT,ru_2010$TRSTPLT,ru_2008$STFGOV, ru_2010$STFGOV, ru_2008$STFECO, ru_2010$STFECO, ru_2008$STFLIFE, ru_2010$STFLIFE, ru_2008$STFDEM, ru_2010$STFDEM, las=2, ylab="Satisfaction au Royaume-Uni", names=c(" Conf_8","Conf_10","Gvt_8", "Gvt_2010", "Eco_8", "Eco_10", "Vie_8", "Vie_10", "Dém_8","Dém_10"))
#RU: vie: plus de mécontents, dém= plus de mécontents
boxplot(norvege_2008$TRSTPLT,norvege_2010$TRSTPLT,norvege_2008$STFGOV, norvege_2010$STFGOV, norvege_2008$STFECO, norvege_2010$STFECO, norvege_2008$STFLIFE, norvege_2010$STFLIFE, norvege_2008$STFDEM, norvege_2010$STFDEM, las=2, ylab="Satisfaction en Norvège", names=c(" Confiance politiciens 2008","Confiance politiciens 2010","Gouvernement 2008", "Gouvernement 2010", "Economie 2008", "Economie 2010", "Vie 2008", "Vie 2010", "Démocratie 2008","Démocratie 2010"))
#Norvège: écot: plus de contents
boxplot(suisse_2008$TRSTPLT,suisse_2010$TRSTPLT,suisse_2008$STFGOV, suisse_2010$STFGOV, suisse_2008$STFECO, suisse_2010$STFECO, suisse_2008$STFLIFE, suisse_2010$STFLIFE, suisse_2008$STFDEM, suisse_2010$STFDEM, las=2, ylab="Satisfaction en Suisse", names=c(" Confiance politiciens 2008","Confiance politiciens 2010","Gouvernement 2008", "Gouvernement 2010", "Economie 2008", "Economie 2010", "Vie 2008", "Vie 2010", "Démocratie 2008","Démocratie 2010"))
#Suisse: éco= plus de contents, vie: plus contents



######################AVEC LES MÉDIANES#################################

#medianes=data.frame(rep(1:196), 52)
m <- matrix(0, ncol = 11, nrow = 52)
m <- data.frame(m)
colnames(m)=c("SatisfactionVie", "SatisfactionEconomie", "SatisfactionGouvernement", "SatisfactionEducation","SatisfactionSanté","SatisfactionDémocratie", "Immigrationéconomie", "Immigrationvie", "ConfiancePoliticiens",'year','country')
rownames(m)=c("france_2008", "grece_2008", "irlande_2008", "ru_2008", "espagne_2008","danemark_2008","allemagne_2008","autriche_2008","belgique_2008","bulgarie_2008","croatie_2008","chypre_2008","rt_2008","estonie_2008","finlande_2008","hongrie_2008","israel_2008","paysbas_2008","norvege_2008","pologne_2008","portugal_2008","russie_2008","slovaquie_2008","slovenie_2008","suede_2008","suisse_2008","ukraine_2008","france_2010", "grece_2010", "irlande_2010", "ru_2010", "espagne_2010","danemark_2010","allemagne_2010","autriche_2010","belgique_2010","bulgarie_2010","croatie_2010","chypre_2010","rt_2010","estonie_2010","finlande_2010","hongrie_2010","israel_2010","paysbas_2010","norvege_2010","pologne_2010","portugal_2010","russie_2010","slovaquie_2010","slovenie_2010","suede_2010","suisse_2010","ukraine_2010")
             
m[1,1]=median(france_2008$STFLIFE)
m[2,1]=median(grece_2008$STFLIFE)
m[3,1]=median(irlande_2008$STFLIFE)
m[4,1]=median(ru_2008$STFLIFE)
m[5,1]=median(espagne_2008$STFLIFE)
m[6,1]=median(danemark_2008$STFLIFE)
m[7,1]=median(allemagne_2008$STFLIFE)
m[8,1]=median(belgique_2008$STFLIFE)
m[9,1]=median(bulgarie_2008$STFLIFE)
m[10,1]=median(croatie_2008$STFLIFE)
m[11,1]=median(chypre_2008$STFLIFE)
m[12,1]=median(rt_2008$STFLIFE)
m[13,1]=median(estonie_2008$STFLIFE)
m[14,1]=median(finlande_2008$STFLIFE)
m[15,1]=median(hongrie_2008$STFLIFE)
m[16,1]=median(israel_2008$STFLIFE)
m[17,1]=median(paysbas_2008$STFLIFE)
m[18,1]=median(norvege_2008$STFLIFE)
m[19,1]=median(pologne_2008$STFLIFE)
m[20,1]=median(portugal_2008$STFLIFE)
m[21,1]=median(russie_2008$STFLIFE)
m[22,1]=median(slovaquie_2008$STFLIFE)
m[23,1]=median(slovenie_2008$STFLIFE)
m[24,1]=median(suede_2008$STFLIFE)
m[25,1]=median(suisse_2008$STFLIFE)
m[26,1]=median(ukraine_2008$STFLIFE)
m[27,1]=median(france_2010$STFLIFE)
m[28,1]=median(grece_2010$STFLIFE)
m[29,1]=median(irlande_2010$STFLIFE)
m[30,1]=median(ru_2010$STFLIFE)
m[31,1]=median(espagne_2010$STFLIFE)
m[32,1]=median(danemark_2010$STFLIFE)
m[33,1]=median(allemagne_2010$STFLIFE)
m[34,1]=median(belgique_2010$STFLIFE)
m[35,1]=median(bulgarie_2010$STFLIFE)
m[36,1]=median(croatie_2010$STFLIFE)
m[37,1]=median(chypre_2010$STFLIFE)
m[38,1]=median(rt_2010$STFLIFE)
m[39,1]=median(estonie_2010$STFLIFE)
m[40,1]=median(finlande_2010$STFLIFE)
m[41,1]=median(hongrie_2010$STFLIFE)
m[42,1]=median(israel_2010$STFLIFE)
m[43,1]=median(paysbas_2010$STFLIFE)
m[44,1]=median(norvege_2010$STFLIFE)
m[45,1]=median(pologne_2010$STFLIFE)
m[46,1]=median(portugal_2010$STFLIFE)
m[47,1]=median(russie_2010$STFLIFE)
m[48,1]=median(slovaquie_2010$STFLIFE)
m[49,1]=median(slovenie_2010$STFLIFE)
m[50,1]=median(suede_2010$STFLIFE)
m[51,1]=median(suisse_2010$STFLIFE)
m[52,1]=median(ukraine_2010$STFLIFE)

m[1,2]=median(france_2008$STFECO)
m[2,2]=median(grece_2008$STFECO)
m[3,2]=median(irlande_2008$STFECO)
m[4,2]=median(ru_2008$STFECO)
m[5,2]=median(espagne_2008$STFECO)
m[6,2]=median(danemark_2008$STFECO)
m[7,2]=median(allemagne_2008$STFECO)
m[8,2]=median(belgique_2008$STFECO)
m[9,2]=median(bulgarie_2008$STFECO)
m[10,2]=median(croatie_2008$STFECO)
m[11,2]=median(chypre_2008$STFECO)
m[12,2]=median(rt_2008$STFECO)
m[13,2]=median(estonie_2008$STFECO)
m[14,2]=median(finlande_2008$STFECO)
m[15,2]=median(hongrie_2008$STFECO)
m[16,2]=median(israel_2008$STFECO)
m[17,2]=median(paysbas_2008$STFECO)
m[18,2]=median(norvege_2008$STFECO)
m[19,2]=median(pologne_2008$STFECO)
m[20,2]=median(portugal_2008$STFECO)
m[21,2]=median(russie_2008$STFECO)
m[22,2]=median(slovaquie_2008$STFECO)
m[23,2]=median(slovenie_2008$STFECO)
m[24,2]=median(suede_2008$STFECO)
m[25,2]=median(suisse_2008$STFECO)
m[26,2]=median(ukraine_2008$STFECO)
m[27,2]=median(france_2010$STFECO)
m[28,2]=median(grece_2010$STFECO)
m[29,2]=median(irlande_2010$STFECO)
m[30,2]=median(ru_2010$STFECO)
m[31,2]=median(espagne_2010$STFECO)
m[32,2]=median(danemark_2010$STFECO)
m[33,2]=median(allemagne_2010$STFECO)
m[34,2]=median(belgique_2010$STFECO)
m[35,2]=median(bulgarie_2010$STFECO)
m[36,2]=median(croatie_2010$STFECO)
m[37,2]=median(chypre_2010$STFECO)
m[38,2]=median(rt_2010$STFECO)
m[39,2]=median(estonie_2010$STFECO)
m[40,2]=median(finlande_2010$STFECO)
m[41,2]=median(hongrie_2010$STFECO)
m[42,2]=median(israel_2010$STFECO)
m[43,2]=median(paysbas_2010$STFECO)
m[44,2]=median(norvege_2010$STFECO)
m[45,2]=median(pologne_2010$STFECO)
m[46,2]=median(portugal_2010$STFECO)
m[47,2]=median(russie_2010$STFECO)
m[48,2]=median(slovaquie_2010$STFECO)
m[49,2]=median(slovenie_2010$STFECO)
m[50,2]=median(suede_2010$STFECO)
m[51,2]=median(suisse_2010$STFECO)
m[52,2]=median(ukraine_2010$STFECO) 


m[1,3]=median(france_2008$STFGOV)
m[2,3]=median(grece_2008$STFGOV)
m[3,3]=median(irlande_2008$STFGOV)
m[4,3]=median(ru_2008$STFGOV)
m[5,3]=median(espagne_2008$STFGOV)
m[6,3]=median(danemark_2008$STFGOV)
m[7,3]=median(allemagne_2008$STFGOV)
m[8,3]=median(belgique_2008$STFGOV)
m[9,3]=median(bulgarie_2008$STFGOV)
m[10,3]=median(croatie_2008$STFGOV)
m[11,3]=median(chypre_2008$STFGOV)
m[12,3]=median(rt_2008$STFGOV)
m[13,3]=median(estonie_2008$STFGOV)
m[14,3]=median(finlande_2008$STFGOV)
m[15,3]=median(hongrie_2008$STFGOV)
m[16,3]=median(israel_2008$STFGOV)
m[17,3]=median(paysbas_2008$STFGOV)
m[18,3]=median(norvege_2008$STFGOV)
m[19,3]=median(pologne_2008$STFGOV)
m[20,3]=median(portugal_2008$STFGOV)
m[21,3]=median(russie_2008$STFGOV)
m[22,3]=median(slovaquie_2008$STFGOV)
m[23,3]=median(slovenie_2008$STFGOV)
m[24,3]=median(suede_2008$STFGOV)
m[25,3]=median(suisse_2008$STFGOV)
m[26,3]=median(ukraine_2008$STFGOV)
m[27,3]=median(france_2010$STFGOV)
m[28,3]=median(grece_2010$STFGOV)
m[29,3]=median(irlande_2010$STFGOV)
m[30,3]=median(ru_2010$STFGOV)
m[31,3]=median(espagne_2010$STFGOV)
m[32,3]=median(danemark_2010$STFGOV)
m[33,3]=median(allemagne_2010$STFGOV)
m[34,3]=median(belgique_2010$STFGOV)
m[35,3]=median(bulgarie_2010$STFGOV)
m[36,3]=median(croatie_2010$STFGOV)
m[37,3]=median(chypre_2010$STFGOV)
m[38,3]=median(rt_2010$STFGOV)
m[39,3]=median(estonie_2010$STFGOV)
m[40,3]=median(finlande_2010$STFGOV)
m[41,3]=median(hongrie_2010$STFGOV)
m[42,3]=median(israel_2010$STFGOV)
m[43,3]=median(paysbas_2010$STFGOV)
m[44,3]=median(norvege_2010$STFGOV)
m[45,3]=median(pologne_2010$STFGOV)
m[46,3]=median(portugal_2010$STFGOV)
m[47,3]=median(russie_2010$STFGOV)
m[48,3]=median(slovaquie_2010$STFGOV)
m[49,3]=median(slovenie_2010$STFGOV)
m[50,3]=median(suede_2010$STFGOV)
m[51,3]=median(suisse_2010$STFGOV)
m[52,3]=median(ukraine_2010$STFGOV)

m[1,4]=median(france_2008$STFEDU)
m[2,4]=median(grece_2008$STFEDU)
m[3,4]=median(irlande_2008$STFEDU)
m[4,4]=median(ru_2008$STFEDU)
m[5,4]=median(espagne_2008$STFEDU)
m[6,4]=median(danemark_2008$STFEDU)
m[7,4]=median(allemagne_2008$STFEDU)
m[8,4]=median(belgique_2008$STFEDU)
m[9,4]=median(bulgarie_2008$STFEDU)
m[10,4]=median(croatie_2008$STFEDU)
m[11,4]=median(chypre_2008$STFEDU)
m[12,4]=median(rt_2008$STFEDU)
m[13,4]=median(estonie_2008$STFEDU)
m[14,4]=median(finlande_2008$STFEDU)
m[15,4]=median(hongrie_2008$STFEDU)
m[16,4]=median(israel_2008$STFEDU)
m[17,4]=median(paysbas_2008$STFEDU)
m[18,4]=median(norvege_2008$STFEDU)
m[19,4]=median(pologne_2008$STFEDU)
m[20,4]=median(portugal_2008$STFEDU)
m[21,4]=median(russie_2008$STFEDU)
m[22,4]=median(slovaquie_2008$STFEDU)
m[23,4]=median(slovenie_2008$STFEDU)
m[24,4]=median(suede_2008$STFEDU)
m[25,4]=median(suisse_2008$STFEDU)
m[26,4]=median(ukraine_2008$STFEDU)
m[27,4]=median(france_2010$STFEDU)
m[28,4]=median(grece_2010$STFEDU)
m[29,4]=median(irlande_2010$STFEDU)
m[30,4]=median(ru_2010$STFEDU)
m[31,4]=median(espagne_2010$STFEDU)
m[32,4]=median(danemark_2010$STFEDU)
m[33,4]=median(allemagne_2010$STFEDU)
m[34,4]=median(belgique_2010$STFEDU)
m[35,4]=median(bulgarie_2010$STFEDU)
m[36,4]=median(croatie_2010$STFEDU)
m[37,4]=median(chypre_2010$STFEDU)
m[38,4]=median(rt_2010$STFEDU)
m[39,4]=median(estonie_2010$STFEDU)
m[40,4]=median(finlande_2010$STFEDU)
m[41,4]=median(hongrie_2010$STFEDU)
m[42,4]=median(israel_2010$STFEDU)
m[43,4]=median(paysbas_2010$STFEDU)
m[44,4]=median(norvege_2010$STFEDU)
m[45,4]=median(pologne_2010$STFEDU)
m[46,4]=median(portugal_2010$STFEDU)
m[47,4]=median(russie_2010$STFEDU)
m[48,4]=median(slovaquie_2010$STFEDU)
m[49,4]=median(slovenie_2010$STFEDU)
m[50,4]=median(suede_2010$STFEDU)
m[51,4]=median(suisse_2010$STFEDU)
m[52,4]=median(ukraine_2010$STFEDU) 

m[1,5]=median(france_2008$STFHLTH)
m[2,5]=median(grece_2008$STFHLTH)
m[3,5]=median(irlande_2008$STFHLTH)
m[4,5]=median(ru_2008$STFHLTH)
m[5,5]=median(espagne_2008$STFHLTH)
m[6,5]=median(danemark_2008$STFHLTH)
m[7,5]=median(allemagne_2008$STFHLTH)
m[8,5]=median(belgique_2008$STFHLTH)
m[9,5]=median(bulgarie_2008$STFHLTH)
m[10,5]=median(croatie_2008$STFHLTH)
m[15,5]=median(chypre_2008$STFHLTH)
m[12,5]=median(rt_2008$STFHLTH)
m[13,5]=median(estonie_2008$STFHLTH)
m[14,5]=median(finlande_2008$STFHLTH)
m[15,5]=median(hongrie_2008$STFHLTH)
m[16,5]=median(israel_2008$STFHLTH)
m[17,5]=median(paysbas_2008$STFHLTH)
m[18,5]=median(norvege_2008$STFHLTH)
m[19,5]=median(pologne_2008$STFHLTH)
m[20,5]=median(portugal_2008$STFHLTH)
m[21,5]=median(russie_2008$STFHLTH)
m[22,5]=median(slovaquie_2008$STFHLTH)
m[23,5]=median(slovenie_2008$STFHLTH)
m[24,5]=median(suede_2008$STFHLTH)
m[25,5]=median(suisse_2008$STFHLTH)
m[26,5]=median(ukraine_2008$STFHLTH)
m[27,5]=median(france_2010$STFHLTH)
m[28,5]=median(grece_2010$STFHLTH)
m[29,5]=median(irlande_2010$STFHLTH)
m[30,5]=median(ru_2010$STFHLTH)
m[31,5]=median(espagne_2010$STFHLTH)
m[32,5]=median(danemark_2010$STFHLTH)
m[33,5]=median(allemagne_2010$STFHLTH)
m[34,5]=median(belgique_2010$STFHLTH)
m[35,5]=median(bulgarie_2010$STFHLTH)
m[36,5]=median(croatie_2010$STFHLTH)
m[37,5]=median(chypre_2010$STFHLTH)
m[38,5]=median(rt_2010$STFHLTH)
m[39,5]=median(estonie_2010$STFHLTH)
m[40,5]=median(finlande_2010$STFHLTH)
m[41,5]=median(hongrie_2010$STFHLTH)
m[42,5]=median(israel_2010$STFHLTH)
m[43,5]=median(paysbas_2010$STFHLTH)
m[44,5]=median(norvege_2010$STFHLTH)
m[45,5]=median(pologne_2010$STFHLTH)
m[46,5]=median(portugal_2010$STFHLTH)
m[47,5]=median(russie_2010$STFHLTH)
m[48,5]=median(slovaquie_2010$STFHLTH)
m[49,5]=median(slovenie_2010$STFHLTH)
m[50,5]=median(suede_2010$STFHLTH)
m[51,5]=median(suisse_2010$STFHLTH)
m[52,5]=median(ukraine_2010$STFHLTH) 

m[1,6]=median(france_2008$STFDEM)
m[2,6]=median(grece_2008$STFDEM)
m[3,6]=median(irlande_2008$STFDEM)
m[4,6]=median(ru_2008$STFDEM)
m[5,6]=median(espagne_2008$STFDEM)
m[6,6]=median(danemark_2008$STFDEM)
m[7,6]=median(allemagne_2008$STFDEM)
m[8,6]=median(belgique_2008$STFDEM)
m[9,6]=median(bulgarie_2008$STFDEM)
m[10,6]=median(croatie_2008$STFDEM)
m[11,6]=median(chypre_2008$STFDEM)
m[12,6]=median(rt_2008$STFDEM)
m[13,6]=median(estonie_2008$STFDEM)
m[14,6]=median(finlande_2008$STFDEM)
m[15,6]=median(hongrie_2008$STFDEM)
m[16,6]=median(israel_2008$STFDEM)
m[17,6]=median(paysbas_2008$STFDEM)
m[18,6]=median(norvege_2008$STFDEM)
m[19,6]=median(pologne_2008$STFDEM)
m[20,6]=median(portugal_2008$STFDEM)
m[21,6]=median(russie_2008$STFDEM)
m[22,6]=median(slovaquie_2008$STFDEM)
m[23,6]=median(slovenie_2008$STFDEM)
m[24,6]=median(suede_2008$STFDEM)
m[25,6]=median(suisse_2008$STFDEM)
m[26,6]=median(ukraine_2008$STFDEM)
m[27,6]=median(france_2010$STFDEM)
m[28,6]=median(grece_2010$STFDEM)
m[29,6]=median(irlande_2010$STFDEM)
m[30,6]=median(ru_2010$STFDEM)
m[31,6]=median(espagne_2010$STFDEM)
m[32,6]=median(danemark_2010$STFDEM)
m[33,6]=median(allemagne_2010$STFDEM)
m[34,6]=median(belgique_2010$STFDEM)
m[35,6]=median(bulgarie_2010$STFDEM)
m[36,6]=median(croatie_2010$STFDEM)
m[37,6]=median(chypre_2010$STFDEM)
m[38,6]=median(rt_2010$STFDEM)
m[39,6]=median(estonie_2010$STFDEM)
m[40,6]=median(finlande_2010$STFDEM)
m[41,6]=median(hongrie_2010$STFDEM)
m[42,6]=median(israel_2010$STFDEM)
m[43,6]=median(paysbas_2010$STFDEM)
m[44,6]=median(norvege_2010$STFDEM)
m[45,6]=median(pologne_2010$STFDEM)
m[46,6]=median(portugal_2010$STFDEM)
m[47,6]=median(russie_2010$STFDEM)
m[48,6]=median(slovaquie_2010$STFDEM)
m[49,6]=median(slovenie_2010$STFDEM)
m[50,6]=median(suede_2010$STFDEM)
m[51,6]=median(suisse_2010$STFDEM)
m[52,6]=median(ukraine_2010$STFDEM) 

m[1,7]=median(france_2008$IMBGECO)
m[2,7]=median(grece_2008$IMBGECO)
m[3,7]=median(irlande_2008$IMBGECO)
m[4,7]=median(ru_2008$IMBGECO)
m[5,7]=median(espagne_2008$IMBGECO)
m[6,7]=median(danemark_2008$IMBGECO)
m[7,7]=median(allemagne_2008$IMBGECO)
m[8,7]=median(belgique_2008$IMBGECO)
m[9,7]=median(bulgarie_2008$IMBGECO)
m[10,7]=median(croatie_2008$IMBGECO)
m[11,7]=median(chypre_2008$IMBGECO)
m[12,7]=median(rt_2008$IMBGECO)
m[13,7]=median(estonie_2008$IMBGECO)
m[14,7]=median(finlande_2008$IMBGECO)
m[15,7]=median(hongrie_2008$IMBGECO)
m[16,7]=median(israel_2008$IMBGECO)
m[17,7]=median(paysbas_2008$IMBGECO)
m[18,7]=median(norvege_2008$IMBGECO)
m[19,7]=median(pologne_2008$IMBGECO)
m[20,7]=median(portugal_2008$IMBGECO)
m[21,7]=median(russie_2008$IMBGECO)
m[22,7]=median(slovaquie_2008$IMBGECO)
m[23,7]=median(slovenie_2008$IMBGECO)
m[24,7]=median(suede_2008$IMBGECO)
m[25,7]=median(suisse_2008$IMBGECO)
m[26,7]=median(ukraine_2008$IMBGECO)
m[27,7]=median(france_2010$IMBGECO)
m[28,7]=median(grece_2010$IMBGECO)
m[29,7]=median(irlande_2010$IMBGECO)
m[30,7]=median(ru_2010$IMBGECO)
m[31,7]=median(espagne_2010$IMBGECO)
m[32,7]=median(danemark_2010$IMBGECO)
m[33,7]=median(allemagne_2010$IMBGECO)
m[34,7]=median(belgique_2010$IMBGECO)
m[35,7]=median(bulgarie_2010$IMBGECO)
m[36,7]=median(croatie_2010$IMBGECO)
m[37,7]=median(chypre_2010$IMBGECO)
m[38,7]=median(rt_2010$IMBGECO)
m[39,7]=median(estonie_2010$IMBGECO)
m[40,7]=median(finlande_2010$IMBGECO)
m[41,7]=median(hongrie_2010$IMBGECO)
m[42,7]=median(israel_2010$IMBGECO)
m[43,7]=median(paysbas_2010$IMBGECO)
m[44,7]=median(norvege_2010$IMBGECO)
m[45,7]=median(pologne_2010$IMBGECO)
m[46,7]=median(portugal_2010$IMBGECO)
m[47,7]=median(russie_2010$IMBGECO)
m[48,7]=median(slovaquie_2010$IMBGECO)
m[49,7]=median(slovenie_2010$IMBGECO)
m[50,7]=median(suede_2010$IMBGECO)
m[51,7]=median(suisse_2010$IMBGECO)
m[52,7]=median(ukraine_2010$IMBGECO) 

m[1,8]=median(france_2008$IMWBCNT)
m[2,8]=median(grece_2008$IMWBCNT)
m[3,8]=median(irlande_2008$IMWBCNT)
m[4,8]=median(ru_2008$IMWBCNT)
m[5,8]=median(espagne_2008$IMWBCNT)
m[6,8]=median(danemark_2008$IMWBCNT)
m[7,8]=median(allemagne_2008$IMWBCNT)
m[8,8]=median(belgique_2008$IMWBCNT)
m[9,8]=median(bulgarie_2008$IMWBCNT)
m[10,8]=median(croatie_2008$IMWBCNT)
m[11,8]=median(chypre_2008$IMWBCNT)
m[12,8]=median(rt_2008$IMWBCNT)
m[13,8]=median(estonie_2008$IMWBCNT)
m[14,8]=median(finlande_2008$IMWBCNT)
m[15,8]=median(hongrie_2008$IMWBCNT)
m[16,8]=median(israel_2008$IMWBCNT)
m[17,8]=median(paysbas_2008$IMWBCNT)
m[18,8]=median(norvege_2008$IMWBCNT)
m[19,8]=median(pologne_2008$IMWBCNT)
m[20,8]=median(portugal_2008$IMWBCNT)
m[21,8]=median(russie_2008$IMWBCNT)
m[22,8]=median(slovaquie_2008$IMWBCNT)
m[23,8]=median(slovenie_2008$IMWBCNT)
m[24,8]=median(suede_2008$IMWBCNT)
m[25,8]=median(suisse_2008$IMWBCNT)
m[26,8]=median(ukraine_2008$IMWBCNT)
m[27,8]=median(france_2010$IMWBCNT)
m[28,8]=median(grece_2010$IMWBCNT)
m[29,8]=median(irlande_2010$IMWBCNT)
m[30,8]=median(ru_2010$IMWBCNT)
m[31,8]=median(espagne_2010$IMWBCNT)
m[32,8]=median(danemark_2010$IMWBCNT)
m[33,8]=median(allemagne_2010$IMWBCNT)
m[34,8]=median(belgique_2010$IMWBCNT)
m[35,8]=median(bulgarie_2010$IMWBCNT)
m[36,8]=median(croatie_2010$IMWBCNT)
m[37,8]=median(chypre_2010$IMWBCNT)
m[38,8]=median(rt_2010$IMWBCNT)
m[39,8]=median(estonie_2010$IMWBCNT)
m[40,8]=median(finlande_2010$IMWBCNT)
m[41,8]=median(hongrie_2010$IMWBCNT)
m[42,8]=median(israel_2010$IMWBCNT)
m[43,8]=median(paysbas_2010$IMWBCNT)
m[44,8]=median(norvege_2010$IMWBCNT)
m[45,8]=median(pologne_2010$IMWBCNT)
m[46,8]=median(portugal_2010$IMWBCNT)
m[47,8]=median(russie_2010$IMWBCNT)
m[48,8]=median(slovaquie_2010$IMWBCNT)
m[49,8]=median(slovenie_2010$IMWBCNT)
m[50,8]=median(suede_2010$IMWBCNT)
m[51,8]=median(suisse_2010$IMWBCNT)
m[52,8]=median(ukraine_2010$IMWBCNT) 

m[1,9]=median(france_2008$TRSTPLT)
m[2,9]=median(grece_2008$TRSTPLT)
m[3,9]=median(irlande_2008$TRSTPLT)
m[4,9]=median(ru_2008$TRSTPLT)
m[5,9]=median(espagne_2008$TRSTPLT)
m[6,9]=median(danemark_2008$TRSTPLT)
m[7,9]=median(allemagne_2008$TRSTPLT)
m[8,9]=median(belgique_2008$TRSTPLT)
m[9,9]=median(bulgarie_2008$TRSTPLT)
m[10,9]=median(croatie_2008$TRSTPLT)
m[11,9]=median(chypre_2008$TRSTPLT)
m[12,9]=median(rt_2008$TRSTPLT)
m[13,9]=median(estonie_2008$TRSTPLT)
m[14,9]=median(finlande_2008$TRSTPLT)
m[15,9]=median(hongrie_2008$TRSTPLT)
m[16,9]=median(israel_2008$TRSTPLT)
m[17,9]=median(paysbas_2008$TRSTPLT)
m[18,9]=median(norvege_2008$TRSTPLT)
m[19,9]=median(pologne_2008$TRSTPLT)
m[20,9]=median(portugal_2008$TRSTPLT)
m[21,9]=median(russie_2008$TRSTPLT)
m[22,9]=median(slovaquie_2008$TRSTPLT)
m[23,9]=median(slovenie_2008$TRSTPLT)
m[24,9]=median(suede_2008$TRSTPLT)
m[25,9]=median(suisse_2008$TRSTPLT)
m[26,9]=median(ukraine_2008$TRSTPLT)
m[27,9]=median(france_2010$TRSTPLT)
m[28,9]=median(grece_2010$TRSTPLT)
m[29,9]=median(irlande_2010$TRSTPLT)
m[30,9]=median(ru_2010$TRSTPLT)
m[31,9]=median(espagne_2010$TRSTPLT)
m[32,9]=median(danemark_2010$TRSTPLT)
m[33,9]=median(allemagne_2010$TRSTPLT)
m[34,9]=median(belgique_2010$TRSTPLT)
m[35,9]=median(bulgarie_2010$TRSTPLT)
m[36,9]=median(croatie_2010$TRSTPLT)
m[37,9]=median(chypre_2010$TRSTPLT)
m[38,9]=median(rt_2010$TRSTPLT)
m[39,9]=median(estonie_2010$TRSTPLT)
m[40,9]=median(finlande_2010$TRSTPLT)
m[41,9]=median(hongrie_2010$TRSTPLT)
m[42,9]=median(israel_2010$TRSTPLT)
m[43,9]=median(paysbas_2010$TRSTPLT)
m[44,9]=median(norvege_2010$TRSTPLT)
m[45,9]=median(pologne_2010$TRSTPLT)
m[46,9]=median(portugal_2010$TRSTPLT)
m[47,9]=median(russie_2010$TRSTPLT)
m[48,9]=median(slovaquie_2010$TRSTPLT)
m[49,9]=median(slovenie_2010$TRSTPLT)
m[50,9]=median(suede_2010$TRSTPLT)
m[51,9]=median(suisse_2010$TRSTPLT)
m[52,9]=median(ukraine_2010$TRSTPLT) 



m[1,10]=1
m[1,10]=1
m[2,10]=1
m[3,10]=1
m[4,10]=1
m[5,10]=1
m[6,10]=1
m[7,10]=1
m[8,10]=1
m[9,10]=1
m[10,10]=1
m[11,10]=1
m[12,10]=1
m[13,10]=1
m[14,10]=1
m[15,10]=1
m[16,10]=1
m[17,10]=1
m[18,10]=1
m[19,10]=1
m[20,10]=1
m[21,10]=1
m[22,10]=1
m[23,10]=1
m[24,10]=1
m[25,10]=1
m[26,10]=1
m[27,10]=2
m[28,10]=2
m[29,10]=2
m[30,10]=2
m[31,10]=2
m[32,10]=2
m[33,10]=2
m[34,10]=2
m[35,10]=2
m[36,10]=2
m[37,10]=2
m[38,10]=2
m[39,10]=2
m[40,10]=2
m[41,10]=2
m[42,10]=2
m[43,10]=2
m[44,10]=2
m[45,10]=2
m[46,10]=2
m[47,10]=2
m[48,10]=2
m[49,10]=2
m[50,10]=2
m[51,10]=2
m[52,10]=2


m[1,11]='France'
m[2,11]='Grece'
m[3,11]='Irlande'
m[4,11]='RU'
m[5,11]='Espagne'
m[6,11]='Danemark'
m[7,11]='Allemagne'
m[8,11]='Belgique'
m[9,11]='Bulgarie'
m[10,11]='Croatie'
m[11,11]='Chypre'
m[12,11]='RT'
m[13,11]='Estonie'
m[14,11]='Finlande'
m[15,11]='Hongrie'
m[16,11]='Israel'
m[17,11]='Paysbas'
m[18,11]='Norvege'
m[19,11]='Pologne'
m[20,11]='Portugal'
m[21,11]='Russie'
m[22,11]='Slovaquie'
m[23,11]='Slovenie'
m[24,11]='Suede'
m[25,11]='Suisse'
m[26,11]='Ukraine'
m[27,11]='France'
m[28,11]='Grece'
m[29,11]='Irlande'
m[30,11]='RU'
m[31,11]='Espagne'
m[32,11]='Danemark'
m[33,11]='Allemagne'
m[34,11]='Belgique'
m[35,11]='Bulgarie'
m[36,11]='Croatie'
m[37,11]='Chypre'
m[38,11]='RT'
m[39,11]='Estonie'
m[40,11]='Finlande'
m[41,11]='Hongrie'
m[42,11]='Israel'
m[43,11]='Paysbas'
m[44,11]='Norvege'
m[45,11]='Pologne'
m[46,11]='Portugal'
m[47,11]='Russie'
m[48,11]='Slovaquie'
m[49,11]='Slovenie'
m[50,11]='Suede'
m[51,11]='Suisse'
m[52,11]='Ukraine'

#On ajoute la colonne du chomage
m <- cbind(m,dfchomage['chomage'])
print(m)

# Modèle de panel
# On dit qu'on cherche le "lien entre la satisfaction concernant le fonctionnement du pays et la confiance en les politiciens" pour pas faire "les déterminants de la confiance", surtout que nos variables touchent toutes à la satisfaction sauf le chômage
#Régression des différences premières (avec cluster)
regfd <- plm(ConfiancePoliticiens ~ SatisfactionVie + SatisfactionEconomie + SatisfactionGouvernement + SatisfactionEducation + SatisfactionDémocratie + Immigrationéconomie + Immigrationvie + SatisfactionSanté + chomage, data= m, index = c("country","year"), model = "fd", cluster='country') 
summary(regfd) 
#Pour la variables SatisfactionDémocratie, c'est assez logique car un manque de confiance dans la démocratie est assez liée à un manque de confiance dans le système politique en général.
#### satis dém et chô 

#On fait ici la régression en différences premières à la main ( et évidemment on trouve les mêmes résultats qu'au dessus)
regfd2 <- lm(diff(m$ConfiancePoliticiens,lag=26) ~ diff(m$SatisfactionVie,lag=26) + diff(m$SatisfactionEconomie,lag=26) +diff(m$ SatisfactionGouvernement,lag=26) +diff(m$SatisfactionEducation ,lag=26) + diff(m$Immigrationéconomie,lag=26) +diff(m$Immigrationvie ,lag=26) +diff(m$SatisfactionDémocratie,lag=26) +diff(m$SatisfactionSanté,lag=26)+ diff(m$chomage,lag=26))
summary(regfd2)

#Régression within (avec cluster)
regwithin <- plm(ConfiancePoliticiens ~ SatisfactionVie + SatisfactionEconomie + SatisfactionGouvernement + SatisfactionEducation + SatisfactionDémocratie +Immigrationéconomie + Immigrationvie + SatisfactionSanté + chomage, data= m, index = c("country","year"), model = "within", cluster='country') 
summary(regwithin)
####SatisDem

# On peut supposer la non exogénéité stricte pour SatisfactionDémocratie car la Satisfaction dans le système démocratique en 2010 peut dépendre de la confiance en les politiciens pendant l'année 2008 (variables liées) mais estimateurs within et FD assez proches donc on en doute quand même...
# On vérifie si la variable SatisfactionDémocratie vérifie l'exogénéité stricte ou pas.
# Voir cours, Linear Regression avec les variables en différences premières et les variables en niveau à la date 2. Il faut tester la nullité jointe des coefficients devant les variables en niveau car les coefficients sont égaux à 0 sous exogénéité stricte ( convergence des coefficients ). Sinon, il y a exogénéité faible.
reg <- lm(diff(m$ConfiancePoliticiens,lag=26) ~ diff(m$SatisfactionVie,lag=26) + diff(m$SatisfactionEconomie,lag=26) +diff(m$ SatisfactionGouvernement,lag=26) +diff(m$SatisfactionEducation ,lag=26) +diff(m$SatisfactionDémocratie,lag=26) +diff(m$Immigrationéconomie,lag=26) +diff(m$Immigrationvie ,lag=26)  +diff(m$SatisfactionSanté,lag=26)+ diff(m$chomage,lag=26) + m[m['year']==2,"SatisfactionVie"] +m[m['year']==2,"SatisfactionSanté"]+ m[m['year']==2,"SatisfactionEconomie"]+ m[m['year']==2,"SatisfactionGouvernement"]+m[m['year']==2,"SatisfactionEducation"]+m[m['year']==2,"SatisfactionSanté"]+m[m['year']==2,"SatisfactionDémocratie"]+m[m['year']==2, "Immigrationéconomie"]+m[m['year']==2, "Immigrationvie"] +m[m['year']==2, "chomage"])
summary(reg)
###TESTER QUE CHAQUE ÉLÉMENT DU VECTEUR EST NUL (WALD, SCORE, RAPPORT DE VRAISEMBLANCE, FISHER), mais ca c'est pour montrer qu'il y a exogénéité stricte pour toutes les variables je crois..
###Dans l'exemple du cours, il regarde que le coefficient devant la variable considérée, même si il ajoute les autres variables en niveau dans la régression donc à voir.. 
vcov(reg)
wald.test(Sigma=vcov(reg), b= coef(reg), Terms=1:19)
#p value très faible, donc on rejette l'hypo selon laquelle les coefficients sont tous nuls, donc il n'y a pas exogénéité stricte. 
#####on teste la nullité simultané de q coefficients dans le modèle linéaire multiple à k variables
R2ur <-0.8877
R2r <-0.7443
((R2ur-R2r)/9)/((1-R2ur)/(26-18-1))
#0,99<2,54, donc on ne rejette pas H0 au seuil de 5%.


#On cherche à instrumenter la différence première de la variable SatisfactionDémocratie avec la variable SatisfactionDémocratie en t-1 car non corrélée avec le terme d'erreur si il y a exogénéité faible ( voir Cours )
reg1 <- lm( diff(m$SatisfactionDémocratie,lag=26) ~ m[m['year']==1,'SatisfactionDémocratie'] + diff(m$SatisfactionVie,lag=26) + diff(m$SatisfactionEconomie,lag=26) +diff(m$ SatisfactionGouvernement,lag=26) +diff(m$SatisfactionEducation ,lag=26) +diff(m$Immigrationéconomie,lag=26) +diff(m$Immigrationvie ,lag=26) + diff(m$SatisfactionSanté,lag=26)+ diff(m$chomage,lag=26))
summary(reg1)
#Pas bonne variable instrumentale car vérifie pas la règle du pouce, instrument faible ( et on peut pas utiliser l'autre du cours parce qu'on a deux dates )
#2ème étape de IV
reg2 <- lm(diff(m$ConfiancePoliticiens,lag=26) ~ reg1$fitted.values + diff(m$SatisfactionVie,lag=26) + diff(m$SatisfactionEconomie,lag=26) +diff(m$ SatisfactionGouvernement,lag=26) +diff(m$SatisfactionEducation ,lag=26) +diff(m$Immigrationéconomie,lag=26) +diff(m$Immigrationvie ,lag=26)  +diff(m$SatisfactionSanté,lag=26) +diff(m$chomage,lag=26), data=m)
summary(reg2)
#coefficient incohérent pour et pas significatif pour la différence première de la variable SatisfactionDémocratie instrumentée
#On s'y attendait un peu dans la mesure où l'instrument était faible

###On cherche à instrumenter la différence première de la variable chô avec la variable chô en t-1
regcho <- lm( diff(m$chomage,lag=26) ~ m[m['year']==1,'chomage'] + diff(m$SatisfactionVie,lag=26) + diff(m$SatisfactionEconomie,lag=26) +diff(m$ SatisfactionGouvernement,lag=26) +diff(m$SatisfactionEducation ,lag=26) +diff(m$Immigrationéconomie,lag=26) +diff(m$Immigrationvie ,lag=26) + diff(m$SatisfactionSanté,lag=26)+ diff(m$SatisfactionDémocratie,lag=26))
summary(regcho)
#Pas une bonne variable instrumentable, p-value trop élevée donc on rejette l'hypothèse que le coeffcient est nul, donc exogénéité.
reg3 <- lm(diff(m$ConfiancePoliticiens,lag=26) ~ regcho$fitted.values + diff(m$SatisfactionVie,lag=26) + diff(m$SatisfactionEconomie,lag=26) +diff(m$ SatisfactionGouvernement,lag=26) +diff(m$SatisfactionEducation ,lag=26) +diff(m$Immigrationéconomie,lag=26) +diff(m$Immigrationvie ,lag=26)  +diff(m$SatisfactionSanté,lag=26) +diff(m$SatisfactionDémocratie,lag=26), data=m)
summary(reg3)
#Le coefficient n'est pas significatif

########################################################################################################################

#Modèle Probit
#La montée des extrêmes
#Est ce que ce manque de confiance en les politiciens, conséquence de la non satisfaction des citoyens d'un pays, peut engendrer les votes extrêmes ( comme une alternative?)?
#On se restreint à la France ( c'est lié à l'actualité ) et on reprend les mêmes variables mais en variable expliquée on a une variable binaire:

#donnees <-spss.get("/Users/parvatichauchaix/Documents/ENSAE/2A/S2/Econométrie 2/Projet/ESS_2010/ESS5e03_3.por", use.value.labels=TRUE)
donnees <-spss.get("/Users/johannalalou/Desktop/ESS5e03_3_2/ESS5e03_3.por", use.value.labels=TRUE)
donnees_France=donnees[which(donnees$CNTRY=="France"),]
donnees_France=donnees_France[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT","PRTVTBFR")]
donnees_France=na.exclude(donnees_France)
donnees_France$extreme<-ifelse(donnees_France$PRTVTBFR=='FN (Front national)' |donnees_France$PRTVTBFR=='PC (Parti Communiste)' |donnees_France$PRTVTBFR=='Les Verts'| donnees_France$PRTVTBFR=='LCR (Ligue communiste révolutionnaire)'| donnees_France$PRTVTBFR=='LO (Lutte ouvrière)',1,0)
#On prend à chaque fois la modalité de l'individu moyennement satisfait en modalité de référence
donnees_France$STFLIFE <- relevel(donnees_France$STFLIFE, "5")
donnees_France$STFECO <- relevel(donnees_France$STFECO, "5")
donnees_France$STFGOV <- relevel(donnees_France$STFGOV, "5")
donnees_France$TRSTPLT <- relevel(donnees_France$TRSTPLT, "5")
donnees_France$STFEDU  <- relevel(donnees_France$STFEDU , "5")
donnees_France$STFHLTH <- relevel(donnees_France$STFHLTH, "5")
donnees_France$STFDEM <- relevel(donnees_France$STFDEM, "5")
donnees_France$IMBGECO <- relevel(donnees_France$IMBGECO, "5")
donnees_France$IMWBCNT <- relevel(donnees_France$IMWBCNT, "5")
probit <- glm(donnees_France$extreme ~ donnees_France$STFLIFE + donnees_France$STFECO + donnees_France$STFGOV + donnees_France$STFEDU + donnees_France$STFHLTH + donnees_France$STFDEM + donnees_France$IMBGECO + donnees_France$IMWBCNT + donnees_France$TRSTPLT, family=binomial(link="probit"), data=donnees_France)
summary (probit)
#On peut que interpréter qualitativement, regarder les effets marginaux.
#Variables significatives: SatisVie (1), SatisEco (1,2,3), SatisEduc (0,1,7), ImmigrationVie(1,2,3), COnfPol(1)
#Signe: +,+,+,+,+,+,+,-,-,-,-: c'est cohérent, sauf pour immigration vie et COnfPol.
#On a quand même l'intuition que le passage d'un avis moyen à un avis très négatif sur la satisfaction de vie (modalité 1) pousse à voter extrême (on ne rejette pas la nullité du coefficient)
#De même pour la satisfaction dans l'économie (passages aux modalités 1,2,3) et la satisfaction dans l'education (passages aux modalités Extremely bad et 1)

#donneesnulles=donnees_France[,c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT")]
#extreme=data.frame(c("STFLIFE","STFECO","STFGOV","STFEDU", "STFHLTH","STFDEM","IMBGECO","IMWBCNT","TRSTPLT","PRTVTBFR"))
#dydx(donnees_France, probit,donnees_France$STFLIFE)

#Effets marginaux moyens, moins significatifs (juste Euducation extremely bad reste assez significatif)
probitmfx(donnees_France$extreme ~ donnees_France$STFLIFE + donnees_France$STFECO + donnees_France$STFGOV + donnees_France$STFEDU + donnees_France$STFHLTH + donnees_France$STFDEM + donnees_France$IMBGECO + donnees_France$IMWBCNT + donnees_France$TRSTPLT, data=donnees_France, atmean = FALSE)
#Effets significatifs: SatisVie (7),SatisEco(8,10),SatisGvt(10), SatisEduc(7,9), SatisSanté(1), SatisDem(9), ImmiVie(0,1,2,3), ConfPol(1,2,8,9,10)
#Signes: -,-,-,-,+,+,-,-,-,-,-,-,-,-,-,-,-,
#APrès il faut regarder la valeur du coefficient
#en commun avec précédemment on a:SatisEduc (7), ImmigrationVie (1,2,3), ConfPol(1)


#On se penche sur un modèle qui permet de séparer votes extrème droite/votes extrèmes gauche
#Modèle polytomique non ordonné: -1: extrême gauche, 0: centre 1: extrême droite
#On va pouvoir voir plus d'effets selon le type de vote extrême
donnees_France$extreme2<-ifelse(donnees_France$PRTVTBFR=='PC (Parti Communiste)' |donnees_France$PRTVTBFR=='Les Verts' | donnees_France$PRTVTBFR=='LCR (Ligue communiste révolutionnaire)'| donnees_France$PRTVTBFR=='LO (Lutte ouvrière)',-1,0)
donnees_France$extreme2[donnees_France$PRTVTBFR=='FN (Front National)'] <- 1


#On convertit la variable de sortie en facteurs
donnees_France$extreme2 <- as.factor(donnees_France$extreme2)

donnees_France$extreme2 <- relevel(donnees_France$extreme2, "0")

#modèle polytomique non ordonné (modèle multinomial)

reg = multinom(donnees_France$extreme2 ~ donnees_France$STFLIFE + donnees_France$STFECO + donnees_France$STFGOV + donnees_France$STFEDU + donnees_France$STFHLTH + donnees_France$STFDEM + donnees_France$IMBGECO + donnees_France$IMWBCNT + donnees_France$TRSTPLT)
summary(reg)
coeff <- summary(reg)$coeff
coeff <- t(coeff)
#Statistiques de test
z <- summary(reg)$coeff/summary(reg)$standard.errors
z <- t(z)
#On veut les pvalues, qu'on a pas par défaut
pvaleur  <- 2 * (1 - pnorm(abs(z), 0, 1))
print(pvaleur)
#On affiche le tout
results <- cbind(coeff, pvaleur)
print(results)
stargazer((results))

