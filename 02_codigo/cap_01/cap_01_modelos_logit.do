/// Elaborado por Sebastián Garrido de Sierra Garrido/// Este código corresponde a parte del análisis presentado en el Capítulo 2 del
/// libro "La reforma definitiva. Por qué y cómo la reforma electoral de 1996 
/// precipitó la caída del régimen de partido dominante priísta" 
/// En particular, con este código se pueden calcular los modelos Logit y Logit 
/// Rare Events incluidos en dicho capítulo para analizar el potencial efecto 
/// del desempeño de la economía en la supervivencia de un régimen de partido 
/// dominante.
/// La base de datos utilizada para este análisis fue previamente procesada y 
/// generada en R. Revisar el archivo cap_02_cargar_unir_datos.R en el folder 
/// 02_codigo.*** -------------------------------------------------------------------------- *
*** Cargar paquetes ***

* Para usar la función relogit, primero se debe descargar el programa correspondiente 
* de https://gking.harvard.edu/files/gking/files/relogit.zip. Después de decargarlo y
* y descomprimirlo, hay que mover todos los archivos al folder ado/sbtplus para que 
* Stata pueda utilizarlo. Para encontrar la ruta al folder ado/sbtplus hace falta
* usar sysdir.* En mi caso, después de guardar los archivos de relogit en ado/sbtplus, tuve 
* que definir el directorio PLUS con la siguiente línea de código:

sysdir set PLUS "/Applications/Stata/ado/stbplus"

* Es probable que necesites hacer algo parecido para poder usar relogit.*** -------------------------------------------------------------------------- **** Importar datos **** use "../reforma_definitiva/03_datos_generados/sp.dta", clearuse "/Users/segasi 1/Google/R/01 investigacion/2017/reforma_definitiva/03_datos_generados/sp.dta", clear*** -------------------------------------------------------------------------- **** Crear variable ***mkspline durations = gwf_duration, cubic nknots(5) displayknotsmat knots = r(knots)*** -------------------------------------------------------------------------- **** Modelos **** En todos los modelos relogit utilizo la opción pc(). De acuerdo con la ayuda 
* de relogit, este comando: "corrects for selection on the dependent variable 
* by using the  of prior correction.  This option requires a numeric argument, 
* the proportion of 1's in the population, which should be between 0 and 1, 
* exclusive..."

* Para indicar el valor de pc() en cada modelo, calculé la proporción de 1s
* en gwf_fail en el subuniverso de observaciones que tenían valores diferentes 
* a . (missing values) en las variables explicativas correspondientes
** Modelos calculados con las variables de PWT7// Modelo 1 Fin del régimen ~ Crecimiento económico de corto plazo* Cálculo de pc()tab gwf_fail if pwt7_rgdpch_chl1 != . 

* Modelorelogit gwf_fail pwt7_rgdpch_chl1 durations*, pc(0.0277) cl(gwf_country)
estimates store m1_logit_pwt, title(Modelo 1 logit pwt)
* logit gwf_fail pwt7_rgdpch_chl1 durations*, cl(gwf_country)// Modelo 2 Fin del régimen ~ Promedio móvil de 5 años* Cálculo de pc()tab gwf_fail if pwt7_fiveyma != . 

* Modelorelogit gwf_fail pwt7_fiveyma durations*, pc(0.0289) cl(gwf_country)estimates store m2_logit_pwt, title(Modelo 2 logit pwt)
*logit gwf_fail pwt7_fiveyma durations*, cl(gwf_country)// Modelo 3 Fin del régimen ~ Desviación del promedio móvil de siete años* Cálculo de pc()tab gwf_fail if pwt7_devsevenyma != .* Modelorelogit gwf_fail pwt7_devsevenyma durations*, pc(0.03) cl(gwf_country)
estimates store m3_logit_pwt, title(Modelo 3 logit pwt)
*logit gwf_fail pwt7_devsevenyma durations*, cl(gwf_country)// Modelo 4 Fin del régimen ~ Crecimiento anual del PIB + PIB per cápita + //          Ingreso petrolero per cápita + Régimen impuesto desde el extranjero* Cálculo de pc()tab gwf_fail if pwt7_rgdpch_chl1 != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & fimposed != .
* Con pwt7relogit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, pc(0.029) cl(gwf_country)
estimates store m4_logit_pwt, title(Modelo 4 logit pwt)
*logit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)// Modelo 5  Fin del régimen ~ Promedio móvil de 5 años+ PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera* Cálculo de pc()tab gwf_fail if pwt7_fiveyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & fimposed != .
* Modelorelogit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, ///pc(0.0302) cl(gwf_country)estimates store m5_logit_pwt, title(Modelo 5 logit pwt)*logit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)// Modelo 6 Fin del régimen ~ Desviación del promedio móvil de siete años + PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera* Cálculo de pc()tab gwf_fail if pwt7_devsevenyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & fimposed != .* Modelorelogit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, ///pc(0.0317) cl(gwf_country)
estimates store m6_logit_pwt, title(Modelo 6 logit pwt)
*logit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)// Modelo 7 Fin del régimen ~ Crecimiento anual del PIB + PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera 
//          + Tipo de régimen + Regiones* Cálculo de pc()tab gwf_fail if pwt7_rgdpch_chl1 != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0354) cl(gwf_country)
estimates store m7_logit_pwt, title(Modelo 7 logit pwt)
*logit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, cl(gwf_country)// Modelo 8 Fin del régimen ~ Promedio móvil de 5 años+ PIB per cápita +//          Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera + 
//          Tipo de régimen + Regiones* Cálculo de pc()tab gwf_fail if pwt7_fiveyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)
estimates store m8_logit_pwt, title(Modelo 8 logit pwt)*logit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)// Modelo 9 Fin del régimen ~ Desviación del promedio móvil de siete años + 
//          PIB per cápita + Ingreso petrolero per cápita + //          Régimen impuesto por potencia extranjera + Tipo de régimen + Regiones* Cálculo de pc()tab gwf_fail if pwt7_devsevenyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelosrelogit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0381) cl(gwf_country)
estimates store m9_logit_pwt, title(Modelo 9 logit pwt)*logit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)** Modelos calculados con las variables de Maddison// Modelo 1 Fin del régimen ~ Crecimiento económico de corto plazo* Cálculo de pc()tab gwf_fail if mad_chgdppcl1 != .

* Modelo relogit gwf_fail mad_chgdppcl1 durations*, pc(0.0249) cl(gwf_country)estimates store m1_logit_mad, title(Modelo 1 logit mad)// Modelo 2 Fin del régimen ~ Promedio móvil de 5 años* Cálculo de pc()
tab gwf_fail if mad_fiveyma != . 

* Modelorelogit gwf_fail mad_fiveyma durations*, pc(0.026) cl(gwf_country)estimates store m2_logit_mad, title(Modelo 2 logit mad)*logit gwf_fail mad_fiveyma durations*, cl(gwf_country)
// Modelo 3 Fin del régimen ~ Desviación del promedio móvil de siete años

* Cálculo de pc()
tab gwf_fail if mad_devsevenyma != .
* Modelorelogit gwf_fail mad_devsevenyma durations*, pc(0.0266) cl(gwf_country)estimates store m3_logit_mad, title(Modelo 3 logit mad)
*logit gwf_fail mad_devsevenyma durations*, cl(gwf_country)// Modelo 4 Fin del régimen ~ Crecimiento anual del PIB + PIB per cápita + //          Ingreso petrolero per cápita + Régimen impuesto desde el extranjero* Cálculo de pc()
tab gwf_fail if mad_chgdppcl1 != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .* Modelorelogit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed durations*, ///
pc(0.0273) cl(gwf_country)estimates store m4_logit_mad, title(Modelo 4 logit mad)
*logit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)// Modelo 5  Fin del régimen ~ Promedio móvil de 5 años+ PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera* Cálculo de pc()
tab gwf_fail if mad_fiveyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .* Modelorelogit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, ///pc(0.0277) cl(gwf_country)estimates store m5_logit_mad, title(Modelo 5 logit mad)
*logit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
// Modelo 6 Fin del régimen ~ Desviación del promedio móvil de siete años + PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera* Cálculo de pc()
tab gwf_fail if mad_devsevenyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .* Modelorelogit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, ///pc(0.028) cl(gwf_country)
estimates store m6_logit_mad, title(Modelo 6 logit mad)
*logit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
// Modelo 7 Fin del régimen ~ Crecimiento anual del PIB + PIB per cápita //          + Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera 
//          + Tipo de régimen + Regiones* Cálculo de pc()
tab gwf_fail if mad_chgdppcl1 != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0323) cl(gwf_country)
estimates store m7_logit_mad, title(Modelo 7 logit mad)
*logit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, cl(gwf_country)// Modelo 8 Fin del régimen ~ Promedio móvil de 5 años+ PIB per cápita +//          Ingreso petrolero per cápita + Régimen impuesto por potencia extranjera + 
//          Tipo de régimen + Regiones* Cálculo de pc()
tab gwf_fail if mad_fiveyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar* Modelorelogit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0325) cl(gwf_country)
estimates store m8_logit_mad, title(Modelo 8 logit mad)
*logit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0325) cl(gwf_country)// Modelo 9 Fin del régimen ~ Desviación del promedio móvil de siete años + 
//          PIB per cápita + Ingreso petrolero per cápita + //          Régimen impuesto por potencia extranjera + Tipo de régimen + Regiones* Cálculo de pc()
tab gwf_fail if mad_devsevenyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar* Modelorelogit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0328) cl(gwf_country)
estimates store m9_logit_mad, title(Modelo 9 logit mad)
*logit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
*** Estadísticas de todos los modelosestimates stats _all*** Resultados de todos los modelos
// Tabla de Modelos calculados con PWT 7.0estout m1_logit_pwt m2_logit_pwt m3_logit_pwt m4_logit_pwt m5_logit_pwt ///
m6_logit_pwt m7_logit_pwt m8_logit_pwt m9_logit_pwt, varlabels(_cons \_cons) ///
cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N) // Tabla de Modelos calculados con Maddisonestout m1_logit_mad m2_logit_mad m3_logit_mad m4_logit_mad m5_logit_mad ///
m6_logit_mad m7_logit_mad m8_logit_mad m9_logit_mad, varlabels(_cons \_cons) ///
cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N)*** Panel B: Gráfica del modelo 8 

** Usando PWT 7.0

* Modelorelogit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)* Cálculo de probabilidad predichapredxcon gwf_fail, xvar(pwt7_fiveyma) from(-9) to(9) adjust(pwt7_rgdpchl1_log=mean ///
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///gwf_nafrica=0 gwf_ssafrica=0 gwf_samerica=0 durations1 durations2 durations3 durations4) 

* Cálculo de probabilidad predicha para comparar cambio de pwt7_fiveyma = 2.5% y 1.5%predxcon gwf_fail, xvar(pwt7_fiveyma) from(-9.5) to(9.5) adjust(pwt7_rgdpchl1_log=mean ///
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///gwf_nafrica=0 gwf_ssafrica=0 gwf_samerica=0 durations1 durations2 durations3 durations4) 

* Cálculo de probabilidad predicha para comparar cambio de pwt7_fiveyma = -1.3%predxcon gwf_fail, xvar(pwt7_fiveyma) from(-9.3) to(9.3) adjust(pwt7_rgdpchl1_log=mean ///
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///gwf_nafrica=0 gwf_ssafrica=0 gwf_samerica=0 durations1 durations2 durations3 durations4) 


* Utilizo los datos de este cálculo para hacer la gráfica en R. El archivo utilizado 
* se llama datos_grafica_2.6.csv** Usando Maddison
* Modelorelogit gwf_fail mad_fiveyma  mad_gdppcl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)* Cálculo de probabilidad predichapredxcon gwf_fail, xvar(mad_fiveyma) from(-9) to(9) adjust( mad_gdppcl1_log=mean ///
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///gwf_nafrica=0 gwf_ssafrica=0 gwf_samerica=0 durations1 durations2 durations3 durations4) 
** Diferentes versiones del Modelo 8, utilizando diversos promedios móviles

// Con "Promedio móvil de 2 años"* Cálculo de pc()tab gwf_fail if pwt7_twoyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_twoyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0354) cl(gwf_country)
estimates store m8_logit_pwt_dos, title(Modelo 8 logit pwt)// Con "Promedio móvil de 3 años"* Cálculo de pc()tab gwf_fail if pwt7_threeyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_threeyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0357) cl(gwf_country)
estimates store m8_logit_pwt_tres, title(Modelo 8 logit pwt)

// Con "Promedio móvil de 4 años"* Cálculo de pc()tab gwf_fail if pwt7_fouryma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_fouryma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.036) cl(gwf_country)
estimates store m8_logit_pwt_cuatro, title(Modelo 8 logit pwt)

// Con "Promedio móvil de 5 años"* Cálculo de pc()tab gwf_fail if pwt7_fiveyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_fiveyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.037) cl(gwf_country)
estimates store m8_logit_pwt_cinco, title(Modelo 8 logit pwt)

// Con "Promedio móvil de 6 años"* Cálculo de pc()tab gwf_fail if pwt7_sixyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_sixyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0374) cl(gwf_country)
estimates store m8_logit_pwt_seis, title(Modelo 8 logit pwt)

// Con "Promedio móvil de 7 años"* Cálculo de pc()tab gwf_fail if pwt7_sevenyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_sevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0381) cl(gwf_country)
estimates store m8_logit_pwt_siete, title(Modelo 8 logit pwt)

// Con "Promedio móvil de 8 años"* Cálculo de pc()tab gwf_fail if pwt7_eightyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_eightyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0385) cl(gwf_country)
estimates store m8_logit_pwt_ocho, title(Modelo 8 logit pwt)// Con "Promedio móvil de 9 años"* Cálculo de pc()tab gwf_fail if pwt7_nineyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_nineyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0388) cl(gwf_country)
estimates store m8_logit_pwt_nueve, title(Modelo 8 logit pwt)// Con "Promedio móvil de 10 años"* Cálculo de pc()tab gwf_fail if pwt7_tenyma != . & pwt7_rgdpchl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
* Modelorelogit gwf_fail pwt7_tenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed ///gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope ///gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0396) cl(gwf_country)
estimates store m8_logit_pwt_diez, title(Modelo 8 logit pwt)*** Resultados de todas las versiones del modelo 8
// Tabla de Modelos calculados con PWT 7.0estout m8_logit_pwt_dos m8_logit_pwt_tres m8_logit_pwt_cuatro m8_logit_pwt_cinco ///
m8_logit_pwt_seis m8_logit_pwt_siete m8_logit_pwt_ocho m8_logit_pwt_nueve ///
m8_logit_pwt_diez, varlabels(_cons \_cons) cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N)
