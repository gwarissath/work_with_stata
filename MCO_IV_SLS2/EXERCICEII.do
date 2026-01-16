*******Variables instrumentale et Double moindre Carrée**************
*definition du repository
cd "D:\MES CERTIFICATIONS\stata\variable_intrumentale"

*faire appel à la base à utiliser
log using result.smcl
use database_college.dta, clear

*description de la base 
describe //les variables educ et exper n'ont pas de label
label variable educ "Years of education"
label variable exper "Years of work experience"
summarize

*création et mise en forme de variable
gen logwage= log(wage) // création de la variable logarithme du salaire (logwage)
label variable logwage "Log wages" //labelisons cette nouvelle variable créée
drop wage //Supression de l'ancienne variable salaire (wage)


*tableau de statistiques descriptives 
sum educ exper black south married smsa nearc4 logwage

*eportaion du tableau des statistiques de base sous format word(.doc)
asdoc sum educ exper black south married smsa nearc4 logwage, separator(0) ,save(statdes.doc)

/*nuage de points et droite de régression linéaire représentant 
la relation entre le nombre d’années d’études (educ) et 
le logarithme du salaire (logwage)*/
twoway (scatter logwage educ) (lfit logwage educ)

*Regression pas à pas
//regression MCO avec une seule variable eplicative
reg logwage educ
//exporter les resultats
asdoc reg logwage educ, save(reg.doc) 
//résumé des regressions pas à pas dans un tableau exporté en word
asdoc reg logwage educ exper, nested  append
asdoc reg logwage educ exper black, nested append
asdoc reg logwage educ exper black south, nested append
asdoc reg logwage educ exper black south married, nested append
asdoc reg logwage educ exper black south married smsa, nested append


*Test de normalité des residus (Test de Jarque et Bera )
//récupération des résidus
quietly reg logwage educ exper black south married smsa
predict residmco, residuals
//réalisation du test
sktest residmco 		
*Les résultats du test donnent une P-value < 0,10 donc on rejette l'hypothèse de normalité
//représentation des résidus
twoway (hist residmco, percent color(gs12)) ///
       (kdensity residmco, lcolor(red) lwidth(medthick))


*HETEROSCEDASTICITE
qui reg logwage educ exper black south married smsa
//Test Breusch-Pagan, version Koenker
estat hettest, iid
*on obtient une p-value > 0,1 donc on ne rejette pas l'hypotèse d'homoscédasticité.
// Test de White
estat imtest, white
*on obtient une p-value < 0,1 donc on rejette l'hypothèse d'homoscédasticité avec ce test
//Repprenon la regression avec l'option robust pour la correction
reg logwage educ exper black south married smsa, ro

*Prise en compte de l'endogéneité du nombre d'année d'éducation
//estimation avec la méthode des doubles moindre carrée (DMC) 
ivregress 2sls logwage  exper black south married smsa (educ= nearc4), first 
* Résumé des tests d’instrumentation
estat firststage //faiblesse de l'instrument
estat endogenous //verification endogénéité


*Exportons les résultats sous format word
reg logwage educ exper black south married smsa
eststo MCO

ivregress 2sls logwage exper black south married smsa (educ= nearc4), first 
eststo DMC

outreg2  [MCO DMC] using table2.doc, title("Effet de l'éducation sur les salaires") bdec(3) sdec(3) replace

