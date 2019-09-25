/// Elaborado por Sebasti�n Garrido de Sierra Garrido
/// libro "La reforma definitiva. Por qu� y c�mo la reforma electoral de 1996 
/// precipit� la ca�da del r�gimen de partido dominante pri�sta" 

/// Rare Events incluidos en dicho cap�tulo para analizar el potencial efecto 
/// del desempe�o de la econom�a en la supervivencia de un r�gimen de partido 
/// dominante.
/// La base de datos utilizada para este an�lisis fue previamente procesada y 
/// generada en R. Revisar el archivo cap_02_cargar_unir_datos.R en el folder 
/// 02_codigo.
*** Cargar paquetes ***

* Para usar la funci�n relogit, primero se debe descargar el programa correspondiente 
* de https://gking.harvard.edu/files/gking/files/relogit.zip. Despu�s de decargarlo y
* y descomprimirlo, hay que mover todos los archivos al folder ado/sbtplus para que 
* Stata pueda utilizarlo. Para encontrar la ruta al folder ado/sbtplus hace falta
* usar sysdir.
* que definir el directorio PLUS con la siguiente l�nea de c�digo:

sysdir set PLUS "/Applications/Stata/ado/stbplus"

* Es probable que necesites hacer algo parecido para poder usar relogit.
* de relogit, este comando: "corrects for selection on the dependent variable 
* by using the  of prior correction.  This option requires a numeric argument, 
* the proportion of 1's in the population, which should be between 0 and 1, 
* exclusive..."

* Para indicar el valor de pc() en cada modelo, calcul� la proporci�n de 1s
* en gwf_fail en el subuniverso de observaciones que ten�an valores diferentes 
* a . (missing values) en las variables explicativas correspondientes


* Modelo
estimates store m1_logit_pwt, title(Modelo 1 logit pwt)
* logit gwf_fail pwt7_rgdpch_chl1 durations*, cl(gwf_country)

* Modelo
*logit gwf_fail pwt7_fiveyma durations*, cl(gwf_country)
estimates store m3_logit_pwt, title(Modelo 3 logit pwt)
*logit gwf_fail pwt7_devsevenyma durations*, cl(gwf_country)

estimates store m4_logit_pwt, title(Modelo 4 logit pwt)
*logit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)

estimates store m6_logit_pwt, title(Modelo 6 logit pwt)
*logit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
//          + Tipo de r�gimen + Regiones
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m7_logit_pwt, title(Modelo 7 logit pwt)
*logit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, cl(gwf_country)
//          Tipo de r�gimen + Regiones
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt, title(Modelo 8 logit pwt)
//          PIB per c�pita + Ingreso petrolero per c�pita + 
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m9_logit_pwt, title(Modelo 9 logit pwt)

* Modelo 
tab gwf_fail if mad_fiveyma != . 

* Modelo
// Modelo 3 Fin del r�gimen ~ Desviaci�n del promedio m�vil de siete a�os

* C�lculo de pc()
tab gwf_fail if mad_devsevenyma != .
* Modelo
*logit gwf_fail mad_devsevenyma durations*, cl(gwf_country)
tab gwf_fail if mad_chgdppcl1 != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .
pc(0.0273) cl(gwf_country)
*logit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
tab gwf_fail if mad_fiveyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .
*logit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)

tab gwf_fail if mad_devsevenyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .
estimates store m6_logit_mad, title(Modelo 6 logit mad)
*logit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)

//          + Tipo de r�gimen + Regiones
tab gwf_fail if mad_chgdppcl1 != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m7_logit_mad, title(Modelo 7 logit mad)
*logit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, cl(gwf_country)
//          Tipo de r�gimen + Regiones
tab gwf_fail if mad_fiveyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
estimates store m8_logit_mad, title(Modelo 8 logit mad)
*logit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0325) cl(gwf_country)
//          PIB per c�pita + Ingreso petrolero per c�pita + 
tab gwf_fail if mad_devsevenyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
estimates store m9_logit_mad, title(Modelo 9 logit mad)
*logit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)


m6_logit_pwt m7_logit_pwt m8_logit_pwt m9_logit_pwt, varlabels(_cons \_cons) ///
cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N) 
m6_logit_mad m7_logit_mad m8_logit_mad m9_logit_mad, varlabels(_cons \_cons) ///
cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N)

** Usando PWT 7.0

* Modelo
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///

* C�lculo de probabilidad predicha para comparar cambio de pwt7_fiveyma = 2.5% y 1.5%
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///

* C�lculo de probabilidad predicha para comparar cambio de pwt7_fiveyma = -1.3%
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///


* Utilizo los datos de este c�lculo para hacer la gr�fica en R. El archivo utilizado 
* se llama datos_grafica_2.6.csv
* Modelo
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///


// Con "Promedio m�vil de 2 a�os"
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt_dos, title(Modelo 8 logit pwt)
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt_tres, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 4 a�os"
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt_cuatro, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 5 a�os"
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt_cinco, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 6 a�os"
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt_seis, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 7 a�os"
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt_siete, title(Modelo 8 logit pwt)

// Con "Promedio m�vil de 8 a�os"
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt_ocho, title(Modelo 8 logit pwt)
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt_nueve, title(Modelo 8 logit pwt)
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m8_logit_pwt_diez, title(Modelo 8 logit pwt)

m8_logit_pwt_seis m8_logit_pwt_siete m8_logit_pwt_ocho m8_logit_pwt_nueve ///
m8_logit_pwt_diez, varlabels(_cons \_cons) cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N)