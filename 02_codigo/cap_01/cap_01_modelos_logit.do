/// Elaborado por Sebasti�n Garrido de Sierra Garrido/// Este c�digo corresponde a parte del an�lisis presentado en el Cap�tulo 2 del
/// libro "La reforma definitiva. Por qu� y c�mo la reforma electoral de 1996 
/// precipit� la ca�da del r�gimen de partido dominante pri�sta" 
/// En particular, con este c�digo se pueden calcular los modelos Logit y Logit 
/// Rare Events incluidos en dicho cap�tulo para analizar el potencial efecto 
/// del desempe�o de la econom�a en la supervivencia de un r�gimen de partido 
/// dominante.
/// La base de datos utilizada para este an�lisis fue previamente procesada y 
/// generada en R. Revisar el archivo cap_02_cargar_unir_datos.R en el folder 
/// 02_codigo.*** -------------------------------------------------------------------------- *
*** Cargar paquetes ***

* Para usar la funci�n relogit, primero se debe descargar el programa correspondiente 
* de https://gking.harvard.edu/files/gking/files/relogit.zip. Despu�s de decargarlo y
* y descomprimirlo, hay que mover todos los archivos al folder ado/sbtplus para que 
* Stata pueda utilizarlo. Para encontrar la ruta al folder ado/sbtplus hace falta
* usar sysdir.* En mi caso, despu�s de guardar los archivos de relogit en ado/sbtplus, tuve 
* que definir el directorio PLUS con la siguiente l�nea de c�digo:

sysdir set PLUS "/Applications/Stata/ado/stbplus"

* Es probable que necesites hacer algo parecido para poder usar relogit.*** -------------------------------------------------------------------------- **** Importar datos **** use "../reforma_definitiva/03_datos_generados/sp.dta", clearuse "/Users/segasi 1/Google/R/01 investigacion/2017/reforma_definitiva/03_datos_generados/sp.dta", clear*** -------------------------------------------------------------------------- **** Crear variable ***mkspline durations = gwf_duration, cubic nknots(5) displayknotsmat knots = r(knots)*** -------------------------------------------------------------------------- **** Modelos **** En todos los modelos relogit utilizo la opci�n pc(). De acuerdo con la ayuda 
* de relogit, este comando: "corrects for selection on the dependent variable 
* by using the  of prior correction.  This option requires a numeric argument, 
* the proportion of 1's in the population, which should be between 0 and 1, 
* exclusive..."

* Para indicar el valor de pc() en cada modelo, calcul� la proporci�n de 1s
* en gwf_fail en el subuniverso de observaciones que ten�an valores diferentes 
* a . (missing values) en las variables explicativas correspondientes
** Modelos calculados con las variables de PWT7// Modelo 1 Fin del r�gimen ~ Crecimiento econ�mico de corto plazo* C�lculo de pc()tab gwf_fail if pwt7_rgdpch_chl1 != . 

* Modelorelogit gwf_fail pwt7_rgdpch_chl1 durations*, pc(0.0277) cl(gwf_country)
estimates store m1_logit_pwt, title(Modelo 1 logit pwt)
* logit gwf_fail pwt7_rgdpch_chl1 durations*, cl(gwf_country)// Modelo 2 Fin del r�gimen ~ Promedio m�vil de 5 a�os* C�lculo de pc()tab gwf_fail if pwt7_fiveyma != . 

* Modelorelogit gwf_fail pwt7_fiveyma durations*, pc(0.0289) cl(gwf_country)estimates store m2_logit_pwt, title(Modelo 2 logit pwt)
*logit gwf_fail pwt7_fiveyma durations*, cl(gwf_country)// Modelo 3 Fin del r�gimen ~ Desviaci�n del promedio m�vil de siete a�os* C�lculo de pc()tab gwf_fail if pwt7_devsevenyma != .* Modelorelogit gwf_fail pwt7_devsevenyma durations*, pc(0.03) cl(gwf_country)
estimates store m3_logit_pwt, title(Modelo 3 logit pwt)
*logit gwf_fail pwt7_devsevenyma durations*, cl(gwf_country)// Modelo 4 Fin del r�gimen ~ Crecimiento anual del PIB + PIB per c�pita + //          Ingreso petrolero per c�pita + R�gimen impuesto desde el extranjero* C�lculo de pc()tab gwf_fail if pwt7_rgdpch_chl1 != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & fimposed != .
* Con pwt7relogit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, pc(0.029) cl(gwf_country)
estimates store m4_logit_pwt, title(Modelo 4 logit pwt)
*logit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)// Modelo 5  Fin del r�gimen ~ Promedio m�vil de 5 a�os+ PIB per c�pita //          + Ingreso petrolero per c�pita + R�gimen impuesto por potencia extranjera* C�lculo de pc()tab gwf_fail if pwt7_fiveyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & fimposed != .
* Modelorelogit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, ///pc(0.0302) cl(gwf_country)estimates store m5_logit_pwt, title(Modelo 5 logit pwt)*logit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)// Modelo 6 Fin del r�gimen ~ Desviaci�n del promedio m�vil de siete a�os + PIB per c�pita //          + Ingreso petrolero per c�pita + R�gimen impuesto por potencia extranjera* C�lculo de pc()tab gwf_fail if pwt7_devsevenyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & fimposed != .* Modelorelogit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, ///pc(0.0317) cl(gwf_country)
estimates store m6_logit_pwt, title(Modelo 6 logit pwt)
*logit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)// Modelo 7 Fin del r�gimen ~ Crecimiento anual del PIB + PIB per c�pita //          + Ingreso petrolero per c�pita + R�gimen impuesto por potencia extranjera 
//          + Tipo de r�gimen + Regiones* C�lculo de pc()tab gwf_fail if pwt7_rgdpch_chl1 != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0354) cl(gwf_country)
estimates store m7_logit_pwt, title(Modelo 7 logit pwt)
*logit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, cl(gwf_country)// Modelo 8 Fin del r�gimen ~ Promedio m�vil de 5 a�os+ PIB per c�pita +//          Ingreso petrolero per c�pita + R�gimen impuesto por potencia extranjera + 
//          Tipo de r�gimen + Regiones* C�lculo de pc()tab gwf_fail if pwt7_fiveyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)
estimates store m8_logit_pwt, title(Modelo 8 logit pwt)*logit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)// Modelo 9 Fin del r�gimen ~ Desviaci�n del promedio m�vil de siete a�os + 
//          PIB per c�pita + Ingreso petrolero per c�pita + //          R�gimen impuesto por potencia extranjera + Tipo de r�gimen + Regiones* C�lculo de pc()tab gwf_fail if pwt7_devsevenyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelosrelogit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0381) cl(gwf_country)
estimates store m9_logit_pwt, title(Modelo 9 logit pwt)*logit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)** Modelos calculados con las variables de Maddison// Modelo 1 Fin del r�gimen ~ Crecimiento econ�mico de corto plazo* C�lculo de pc()tab gwf_fail if mad_chgdppcl1 != .

* Modelo relogit gwf_fail mad_chgdppcl1 durations*, pc(0.0249) cl(gwf_country)estimates store m1_logit_mad, title(Modelo 1 logit mad)// Modelo 2 Fin del r�gimen ~ Promedio m�vil de 5 a�os* C�lculo de pc()
tab gwf_fail if mad_fiveyma != . 

* Modelorelogit gwf_fail mad_fiveyma durations*, pc(0.026) cl(gwf_country)estimates store m2_logit_mad, title(Modelo 2 logit mad)*logit gwf_fail mad_fiveyma durations*, cl(gwf_country)
// Modelo 3 Fin del r�gimen ~ Desviaci�n del promedio m�vil de siete a�os

* C�lculo de pc()
tab gwf_fail if mad_devsevenyma != .
* Modelorelogit gwf_fail mad_devsevenyma durations*, pc(0.0266) cl(gwf_country)estimates store m3_logit_mad, title(Modelo 3 logit mad)
*logit gwf_fail mad_devsevenyma durations*, cl(gwf_country)// Modelo 4 Fin del r�gimen ~ Crecimiento anual del PIB + PIB per c�pita + //          Ingreso petrolero per c�pita + R�gimen impuesto desde el extranjero* C�lculo de pc()
tab gwf_fail if mad_chgdppcl1 != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .* Modelorelogit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed durations*, ///
pc(0.0273) cl(gwf_country)estimates store m4_logit_mad, title(Modelo 4 logit mad)
*logit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)// Modelo 5  Fin del r�gimen ~ Promedio m�vil de 5 a�os+ PIB per c�pita //          + Ingreso petrolero per c�pita + R�gimen impuesto por potencia extranjera* C�lculo de pc()
tab gwf_fail if mad_fiveyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .* Modelorelogit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, ///pc(0.0277) cl(gwf_country)estimates store m5_logit_mad, title(Modelo 5 logit mad)
*logit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
// Modelo 6 Fin del r�gimen ~ Desviaci�n del promedio m�vil de siete a�os + PIB per c�pita //          + Ingreso petrolero per c�pita + R�gimen impuesto por potencia extranjera* C�lculo de pc()
tab gwf_fail if mad_devsevenyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .* Modelorelogit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, ///pc(0.028) cl(gwf_country)
estimates store m6_logit_mad, title(Modelo 6 logit mad)
*logit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
// Modelo 7 Fin del r�gimen ~ Crecimiento anual del PIB + PIB per c�pita //          + Ingreso petrolero per c�pita + R�gimen impuesto por potencia extranjera 
//          + Tipo de r�gimen + Regiones* C�lculo de pc()
tab gwf_fail if mad_chgdppcl1 != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0323) cl(gwf_country)
estimates store m7_logit_mad, title(Modelo 7 logit mad)
*logit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, cl(gwf_country)// Modelo 8 Fin del r�gimen ~ Promedio m�vil de 5 a�os+ PIB per c�pita +//          Ingreso petrolero per c�pita + R�gimen impuesto por potencia extranjera + 
//          Tipo de r�gimen + Regiones* C�lculo de pc()
tab gwf_fail if mad_fiveyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar* Modelorelogit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0325) cl(gwf_country)
estimates store m8_logit_mad, title(Modelo 8 logit mad)
*logit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0325) cl(gwf_country)// Modelo 9 Fin del r�gimen ~ Desviaci�n del promedio m�vil de siete a�os + 
//          PIB per c�pita + Ingreso petrolero per c�pita + //          R�gimen impuesto por potencia extranjera + Tipo de r�gimen + Regiones* C�lculo de pc()
tab gwf_fail if mad_devsevenyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar* Modelorelogit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0328) cl(gwf_country)
estimates store m9_logit_mad, title(Modelo 9 logit mad)
*logit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
*** Estad�sticas de todos los modelosestimates stats _all*** Resultados de todos los modelos
// Tabla de Modelos calculados con PWT 7.0estout m1_logit_pwt m2_logit_pwt m3_logit_pwt m4_logit_pwt m5_logit_pwt ///
m6_logit_pwt m7_logit_pwt m8_logit_pwt m9_logit_pwt, varlabels(_cons \_cons) ///
cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N) // Tabla de Modelos calculados con Maddisonestout m1_logit_mad m2_logit_mad m3_logit_mad m4_logit_mad m5_logit_mad ///
m6_logit_mad m7_logit_mad m8_logit_mad m9_logit_mad, varlabels(_cons \_cons) ///
cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N)*** Panel B: Gr�fica del modelo 8 

** Usando PWT 7.0

* Modelorelogit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)* C�lculo de probabilidad predichapredxcon gwf_fail, xvar(pwt7_fiveyma) from(-9) to(9) adjust(pwt7_rgdpchl1_log=mean ///
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///gwf_nafrica=0 gwf_ssafrica=0 gwf_samerica=0 durations1 durations2 durations3 durations4) 

* C�lculo de probabilidad predicha para comparar cambio de pwt7_fiveyma = 2.5% y 1.5%predxcon gwf_fail, xvar(pwt7_fiveyma) from(-9.5) to(9.5) adjust(pwt7_rgdpchl1_log=mean ///
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///gwf_nafrica=0 gwf_ssafrica=0 gwf_samerica=0 durations1 durations2 durations3 durations4) 

* C�lculo de probabilidad predicha para comparar cambio de pwt7_fiveyma = -1.3%predxcon gwf_fail, xvar(pwt7_fiveyma) from(-9.3) to(9.3) adjust(pwt7_rgdpchl1_log=mean ///
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///gwf_nafrica=0 gwf_ssafrica=0 gwf_samerica=0 durations1 durations2 durations3 durations4) 


* Utilizo los datos de este c�lculo para hacer la gr�fica en R. El archivo utilizado 
* se llama datos_grafica_2.6.csv** Usando Maddison
* Modelorelogit gwf_fail mad_fiveyma  mad_gdppcl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)* C�lculo de probabilidad predichapredxcon gwf_fail, xvar(mad_fiveyma) from(-9) to(9) adjust( mad_gdppcl1_log=mean ///
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///gwf_nafrica=0 gwf_ssafrica=0 gwf_samerica=0 durations1 durations2 durations3 durations4) 
** Diferentes versiones del Modelo 8, utilizando diversos promedios m�viles

// Con "Promedio m�vil de 2 a�os"* C�lculo de pc()tab gwf_fail if pwt7_twoyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_twoyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0354) cl(gwf_country)
estimates store m8_logit_pwt_dos, title(Modelo 8 logit pwt)// Con "Promedio m�vil de 3 a�os"* C�lculo de pc()tab gwf_fail if pwt7_threeyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_threeyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0357) cl(gwf_country)
estimates store m8_logit_pwt_tres, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 4 a�os"* C�lculo de pc()tab gwf_fail if pwt7_fouryma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_fouryma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.036) cl(gwf_country)
estimates store m8_logit_pwt_cuatro, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 5 a�os"* C�lculo de pc()tab gwf_fail if pwt7_fiveyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)
estimates store m8_logit_pwt_cinco, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 6 a�os"* C�lculo de pc()tab gwf_fail if pwt7_sixyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_sixyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0374) cl(gwf_country)
estimates store m8_logit_pwt_seis, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 7 a�os"* C�lculo de pc()tab gwf_fail if pwt7_sevenyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_sevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0381) cl(gwf_country)
estimates store m8_logit_pwt_siete, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 8 a�os"* C�lculo de pc()tab gwf_fail if pwt7_eightyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_eightyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0385) cl(gwf_country)
estimates store m8_logit_pwt_ocho, title(Modelo 8 logit pwt)// Con "Promedio m�vil de 9 a�os"* C�lculo de pc()tab gwf_fail if pwt7_nineyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_nineyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0388) cl(gwf_country)
estimates store m8_logit_pwt_nueve, title(Modelo 8 logit pwt)// Con "Promedio m�vil de 10 a�os"* C�lculo de pc()tab gwf_fail if pwt7_tenyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_tenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0396) cl(gwf_country)
estimates store m8_logit_pwt_diez, title(Modelo 8 logit pwt)*** Resultados de todas las versiones del modelo 8
// Tabla de Modelos calculados con PWT 7.0estout m8_logit_pwt_dos m8_logit_pwt_tres m8_logit_pwt_cuatro m8_logit_pwt_cinco ///
m8_logit_pwt_seis m8_logit_pwt_siete m8_logit_pwt_ocho m8_logit_pwt_nueve ///
m8_logit_pwt_diez, varlabels(_cons \_cons) cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N)
