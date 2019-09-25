/// Elaborado por Sebastián Garrido de Sierra Garrido/// Este código corresponde a parte del análisis presentado en el Capítulo 2 del
/// libro "La reforma definitiva. Por qué y cómo la reforma electoral de 1996 
/// precipitó la caída del régimen de partido dominante priísta" 
/// En particular, con este código se pueden calcular los modelos Weibull 
/// incluidos en dicho capítulo para analizar el potencial efecto del desempeño 
/// de la economía en la supervivencia de un régimen de partido dominante.
/// La base de datos utilizada para este análisis fue previamente procesada y 
/// generada en R. Revisar el archivo cap_02_cargar_unir_datos.R en el folder 
/// 02_codigo.*** -------------------------------------------------------------------------- *
*** Cargar paquetes ***
ssc install estout, replace
*** -------------------------------------------------------------------------- **** Importar datos **** use "../reforma_definitiva/03_datos_generados/sp.dta", clearuse "/Users/segasi 1/Google/R/01 investigacion/2017/reforma_definitiva/03_datos_generados/sp.dta", clear


*** -------------------------------------------------------------------------- **** Definir datos como datos survival **** Al utilizar la función 'stset', Stata agrega cuatro variables a la base de datos:* _t0 y _t registran el período de tiempo que vivió la observación, con t unidades
* para cada observación; _d registra el resultado de cada observación al final del 
* período analizado (1 si la observación termina en fracaso o 0 si no); _st registra
* si la observación es relevante y puede ser incluida en el análisis.stset gwf_duration, failure(gwf_fail==1) id(gwf_casename)* Nota: dada la naturaleza longitudinal de los datos analyzados, donde cada régimen
* tiene múltiples observaciones, id() permite indicarle a Stata qué variable utilizar 
* para no tratar cada observación como si fuera diferente. 

* failure() define la variable y el valor que representan un evento para el análisis; 
* en este caso, la caída de un régimen. *** Descripción breve de la base de datos que fue definidia con stsetstdes*** -------------------------------------------------------------------------- **** Análisis no paramétrico **** Gráfica de Kaplan-Meier de la función de supervivencia s(t), sin variables de* control  sts graph* Gráfica de Kaplan-Meier de la función de supervivencia s(t) por tipo de régimen, 
* sin variables de control  sts graph, by (gwf_regimetype)* Datos utilizados para construir las gráficas de Kaplan-Meier previassts list*** -------------------------------------------------------------------------- **** Modelos **** Nota: Si se incluye 'nohr' al final del código del modelo, Stata reporta* coeficientes, no ratio de riesgo.  Si se incluye 'time' al final del código,* Stata reporta el tiempo acelerado de fracaso o Accelerated Failure Time (AFT). 
* Si se incluye 'tr', Stata reporta el ratio del tiempo acelerado de fracaso o 
* Accelerated Failure Time (AFT), que es lo mismo que exp(coeficiente) del AFT.
** Modelos calculados con las variables de PWT7// Modelo 1 Fin del régimen ~ Crecimiento anual del PIBstreg pwt7_rgdpch_chl1,  dist(weib) cl(gwf_country)estimates store m1_weibull_pwt, title(Modelo 1pwt)// Modelo 2 Fin del régimen ~ Promedio móvil de 5 añosstreg pwt7_fiveyma, dist(weib) cl(gwf_country)estimates store m2_weibull_pwt, title(Modelo 2pwt)// Modelo 3 Fin del régimen ~ Desviación del promedio móvil de siete añosstreg pwt7_devsevenyma, dist(weib) cl(gwf_country)estimates store m3_weibull_pwt, title(Modelo 3pwt)// Modelo 4 Fin del régimen ~ Crecimiento anual del PIB + PIB per cápita + //          Ingreso petrolero per cápita + Régimen impuesto desde el extranjerostreg pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed, dist(weib) cl(gwf_country)estimates store m4_weibull_pwt, title(Modelo 4pwt)// Modelo 5  Fin del régimen ~ Promedio móvil de 5 años+ PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjerastreg pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed, dist(weib) cl(gwf_country)estimates store m5_weibull_pwt, title(Modelo 5pwt)// Modelo 6 Fin del régimen ~ Desviación del promedio móvil de siete años + PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjerastreg pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed, dist(weib) cl(gwf_country)estimates store m6_weibull_pwt, title(Modelo 6pwt)// Modelo 7 Fin del régimen ~ Crecimiento anual del PIB + PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera 
//          + Tipo de régimen + Regionesstreg pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m7_weibull_pwt, title(Modelo 7pwt)

// Modelo 8 Fin del régimen ~ Promedio móvil de 5 años+ PIB per cápita +//          Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera + 
//          Tipo de régimen + Regionesstreg pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt, title(Modelo 8pwt)// Modelo 9 Fin del régimen ~ Desviación del promedio móvil de siete años + 
//          PIB per cápita + Ingreso petrolero per cápita + //          Régimen impuesto por potencia extranjera + Tipo de régimen + Regionesstreg pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib)  cl(gwf_country)estimates store m9_weibull_pwt, title(Modelo 9pwt)*** Estadísticas de todos los modelosestimates stats _all// Tablaestout m1_weibull_pwt m2_weibull_pwt m3_weibull_pwt m4_weibull_pwt ///
m5_weibull_pwt m6_weibull_pwt m7_weibull_pwt m8_weibull_pwt /// 
m9_weibull_pwt, eform varlabels(_cons \_cons) cells(b(star fmt(3)) ///se(par fmt(3))) stats(ll aic N) * Tiempo acelerado de fracaso del modelo 8streg pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country) time** Modelos calculados con las variables de Maddison// Modelo 1 Fin del régimen ~ Crecimiento anual del PIBstreg mad_chgdppcl1,  dist(weib) cl(gwf_country)estimates store m1_weibull_mad, title(Modelo 1mad)// Modelo 2 Fin del régimen ~ Promedio móvil de 5 añosstreg mad_fiveyma, dist(weib) cl(gwf_country)estimates store m2_weibull_mad, title(Modelo 2mad)// Modelo 3 Fin del régimen ~ Desviación del promedio móvil de siete añosstreg mad_devsevenyma, dist(weib) cl(gwf_country)estimates store m3_weibull_mad, title(Modelo 3mad)// Modelo 4 Fin del régimen ~ Crecimiento anual del PIB + PIB per cápita + //          Ingreso petrolero per cápita + Régimen impuesto desde el extranjero
streg mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed, dist(weib) cl(gwf_country)estimates store m4_weibull_mad, title(Modelo 4mad)
// Modelo 5  Fin del régimen ~ Promedio móvil de 5 años+ PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjerastreg mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed, dist(weib) cl(gwf_country)estimates store m5_weibull_mad, title(Modelo 5mad)// Modelo 6 Fin del régimen ~ Desviación del promedio móvil de siete años + PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjerastreg mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed, dist(weib) cl(gwf_country)estimates store m6_weibull_mad, title(Modelo 6mad)// Modelo 7 Fin del régimen ~ Crecimiento anual del PIB + PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera 
//          + Tipo de régimen + Regionesstreg mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m7_weibull_mad, title(Modelo 7mad)// Modelo 8 Fin del régimen ~ Promedio móvil de 5 años+ PIB per cápita +//          Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera + 
//          Tipo de régimen + Regionesstreg mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_mad, title(Modelo 8mad)// Modelo 9 Fin del régimen ~ Desviación del promedio móvil de siete años + 
//          PIB per cápita + Ingreso petrolero per cápita + //          Régimen impuesto por potencia extranjera + Tipo de régimen + Regionesstreg mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)
estimates store m9_weibull_mad, title(Modelo 9mad)*** Estadísticas de todos los modelosestimates stats _all// Tabla
estout m1_weibull_mad m2_weibull_mad m3_weibull_mad m4_weibull_mad ///
m5_weibull_mad m6_weibull_mad m7_weibull_mad m8_weibull_mad m9_weibull_mad, ///
eform varlabels(_cons \_cons) cells(b(star fmt(3)) ///se(par fmt(3))) stats(ll aic N) 


*** Panel A: Gráfica del modelo 8 con pwt7_fiveyma = 3.5, 2.5 y 1.5 + 
***          fimposed = 0 + gwf_cacar = 1

** Usando PWT 7.0

* Modelostreg pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)

* Gráficastcurve, hazard at(pwt7_fiveyma = 3.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(pwt7_fiveyma = 2.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, ///gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(pwt7_fiveyma = 1.5, fimposed = 0, ///gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) ///  ylabel(0(0.01).06, angle(0))  xlabel(0(10)90, angle(0)) plotregion(fcolor(white)) graphregion(fcolor(white))  ///  ylabel(, nogrid) ytitle("Función de riesgo") xtitle("Años") title("Panel (a)") lcolor(black gs50 gs10) lpattern(solid shortdash dash) legend(region(lwidth(none))) ///  legend(region(lcolor(none))) legend(region( fcolor(none))) legend(cols(1) pos(2) ring(0)) ///   legend( order(1 "Prom. móvil de cinco años = 3.5%"  2 "Prom. móvil de cinco años = 2.5%" 3 "Prom. móvil de cinco años = 1.5%")) ///  graphregion(color(white)) bgcolor(white)

predict time, time

predict, hr 

at(pwt7_fiveyma = 3.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0)

margins, at(pwt7_fiveyma = 3.5) 

, fimposed = 1, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(pwt7_fiveyma = 2.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, ///gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(pwt7_fiveyma = 1.5, fimposed = 0, ///gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0)
* 
survci, at(pwt7_fiveyma = 3.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(pwt7_fiveyma = 2.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, ///gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(pwt7_fiveyma = 1.5, fimposed = 0, ///gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0)


** Usando Maddison (no incluida en el capítulo)

* Modelo
streg mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)predict hazard* Gráficastcurve, hazard at(mad_fiveyma = 3.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(mad_fiveyma = 2.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, ///gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(mad_fiveyma = 1.5, fimposed = 0, ///gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) ///  ylabel(0(0.01).06, angle(0))  xlabel(0(10)90, angle(0)) plotregion(fcolor(white)) graphregion(fcolor(white))  ///  ylabel(, nogrid) ytitle("Función de riesgo") xtitle("Años") title("Panel (a)") lcolor(black gs50 gs10) lpattern(solid shortdash dash) legend(region(lwidth(none))) ///  legend(region(lcolor(none))) legend(region( fcolor(none))) legend(cols(1) pos(2) ring(0)) ///   legend( order(1 "Prom. móvil de cinco años = 3.5%"  2 "Prom. móvil de cinco años = 2.5%" 3 "Prom. móvil de cinco años = 1.5%")) ///  graphregion(color(white)) bgcolor(white)



*** Panel B: Gráfica del modelo 8 con pwt7_fiveyma = 6.3, 2.5 y -1.3 + 
***          fimposed = 0 + gwf_cacar = 1

** Usando PWT 7.0

* Modelostreg pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)

* Gráficastcurve, hazard at(pwt7_fiveyma = 6.3, fimposed = 0, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(pwt7_fiveyma = 2.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, ///gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(pwt7_fiveyma = -1.3, fimposed = 0, ///gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) ///  ylabel(0(0.01).09, angle(0))  xlabel(0(10)90, angle(0)) plotregion(fcolor(white)) graphregion(fcolor(white))  ///  ylabel(, nogrid) ytitle("Función de riesgo") xtitle("Años") title("Panel (b)") lcolor(black gs50 gs10) lpattern(solid shortdash dash) legend(region(lwidth(none))) ///  legend(region(lcolor(none))) legend(region( fcolor(none))) legend(cols(1) pos(2) ring(0)) ///   legend( order(1 "Prom. móvil de cinco años = 6.3%"  2 "Prom. móvil de cinco años = 2.5%" 3 "Prom. móvil de cinco años = -1.3%")) ///  graphregion(color(white)) bgcolor(white)

** Usando Maddison (no incluida en el capítulo)

* Modelo
streg mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_mad, title(Modelo 8mad)* Gráficastcurve, hazard at(mad_fiveyma = 6.3, fimposed = 0, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(mad_fiveyma = 2.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, ///gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) at(mad_fiveyma = -1.3, fimposed = 0, ///gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, gwf_nafrica = 0, gwf_ssafrica = 0, gwf_samerica = 0) ///  ylabel(0(0.01).09, angle(0))  xlabel(0(10)90, angle(0)) plotregion(fcolor(white)) graphregion(fcolor(white))  ///  ylabel(, nogrid) ytitle("Función de riesgo") xtitle("Años") title("Panel (b)") lcolor(black gs50 gs10) lpattern(solid shortdash dash) legend(region(lwidth(none))) ///  legend(region(lcolor(none))) legend(region( fcolor(none))) legend(cols(1) pos(2) ring(0)) ///   legend( order(1 "Prom. móvil de cinco años = 6.3%"  2 "Prom. móvil de cinco años = 2.5%" 3 "Prom. móvil de cinco años = -1.3%")) ///  graphregion(color(white)) bgcolor(white)
  


*** -------------------------------------------------------------------------- **** Diferentes versiones del Modelo 8, utilizando diversos promedios móviles

// Con "Promedio móvil de 2 años"streg pwt7_twoyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt7_dos, title(Modelo 8pwt)

// Con "Promedio móvil de 3 años"streg pwt7_threeyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt7_tres, title(Modelo 8pwt)

// Con "Promedio móvil de 4 años"streg pwt7_fouryma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt7_cuatro, title(Modelo 8pwt)

// Con "Promedio móvil de 5 años"streg pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt7_cinco, title(Modelo 8pwt)

// Con "Promedio móvil de 6 años"streg pwt7_sixyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt7_seis, title(Modelo 8pwt)

// Con "Promedio móvil de 7 años"streg pwt7_sevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt7_siete, title(Modelo 8pwt)

// Con "Promedio móvil de 8 años"streg pwt7_eightyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt7_ocho, title(Modelo 8pwt)

// Con "Promedio móvil de 9 años"streg pwt7_nineyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt7_nueve, title(Modelo 8pwt)

// Con "Promedio móvil de 10 años"streg pwt7_tenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party ///gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope  ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar, dist(weib) cl(gwf_country)estimates store m8_weibull_pwt7_diez, title(Modelo 8pwt)




*** Resultados de todas las versiones del modelo 8
// Tabla de Modelos calculados con PWT 7.0estout m8_weibull_pwt7_dos m8_weibull_pwt7_tres m8_weibull_pwt7_cuatro m8_weibull_pwt7_cinco ///
m8_weibull_pwt7_seis m8_weibull_pwt7_siete m8_weibull_pwt7_ocho m8_weibull_pwt7_nueve ///
m8_weibull_pwt7_diez, varlabels(_cons \_cons) cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N)

