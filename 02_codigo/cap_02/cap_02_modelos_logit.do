/// Elaborado por Sebasti�n Garrido de Sierra Garrido
/// libro "La reforma definitiva. Por qu� y c�mo la reforma electoral de 1996 
/// precipit� la ca�da del r�gimen de partido dominante pri�sta 

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

* sysdir set PLUS "/Applications/Stata/ado/stbplus"

* Es probable que necesites hacer algo parecido para poder usar relogit.
* de relogit, este comando: "corrects for selection on the dependent variable 
* by using the  of prior correction.  This option requires a numeric argument, 
* the proportion of 1's in the population, which should be between 0 and 1, 
* exclusive..."

* Para indicar el valor de pc() en cada modelo, calcul� la proporci�n de 1s
* en gwf_fail en el subuniverso de observaciones que ten�an valores diferentes 
* a . (missing values) en las variables explicativas correspondientes

tab gwf_fail if mad_chgdppcl1 != . 
relogit gwf_fail pwt7_rgdpch_chl1 durations*, pc(0.0272) cl(gwf_country)
estimates store m1_logit_pwt, title(Modelo 1 logit pwt)

* logit gwf_fail pwt7_rgdpch_chl1 durations*, cl(gwf_country)
tab gwf_fail if mad_fiveyma != . 
relogit gwf_fail pwt7_fiveyma durations*, pc(0.0284) cl(gwf_country)
*logit gwf_fail pwt7_fiveyma durations*, cl(gwf_country)
tab gwf_fail if mad_devsevenyma != .
estimates store m3_logit_pwt, title(Modelo 3 logit pwt)
*logit gwf_fail pwt7_devsevenyma durations*, cl(gwf_country)

* Con mad
*logit gwf_fail mad_devsevenyma durations*, cl(gwf_country)
tab gwf_fail if mad_chgdppcl1 != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .
estimates store m4_logit_pwt, title(Modelo 4 logit pwt)
*logit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
pc(0.0275) cl(gwf_country)
*logit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
tab gwf_fail if mad_fiveyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .
*logit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)


tab gwf_fail if mad_devsevenyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & fimposed != .
estimates store m6_logit_pwt, title(Modelo 6 logit pwt)
*logit gwf_fail pwt7_devsevenyma pwt7_rgdpchl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)
estimates store m6_logit_mad, title(Modelo 6 logit mad)
*logit gwf_fail mad_devsevenyma mad_gdppcl1_log ross_preciol1_log fimposed durations*, cl(gwf_country)



//          + Tipo de r�gimen + Regiones
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

tab gwf_fail if mad_chgdppcl1 != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

estimates store m7_logit_pwt, title(Modelo 7 logit pwt)
*logit gwf_fail pwt7_rgdpch_chl1 pwt7_rgdpchl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, cl(gwf_country)
estimates store m7_logit_mad, title(Modelo 7 logit mad)
*logit gwf_fail mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, cl(gwf_country)
//          Tipo de r�gimen + Regiones
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

tab gwf_fail if mad_fiveyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
estimates store m8_logit_pwt, title(Modelo 8 logit pwt)
estimates store m8_logit_mad, title(Modelo 8 logit mad)
*logit gwf_fail mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party gwf_partymil gwf_partymilper gwf_casia gwf_easia gwf_ceeurope gwf_nafrica gwf_ssafrica gwf_samerica gwf_cacar durations*, pc(0.0325) cl(gwf_country)
//          PIB per c�pita + Ingreso petrolero per c�pita + 
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar

tab gwf_fail if mad_devsevenyma != . & mad_gdppcl1_log != . & ross_preciol1_log != . & ///
fimposed != . & gwf_party != . & gwf_partymil != . & gwf_partymilper != . & gwf_casia != . & ///
gwf_easia != . & gwf_ceeurope != . & gwf_nafrica != . & gwf_ssafrica != . & gwf_samerica != . & gwf_cacar
estimates store m9_logit_pwt, title(Modelo 9 logit pwt)
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

* Gr�fica
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///
ylabel(0(0.02).2) xlabel(-9(3)9) xtitle("Cambio porcentual") ///
ytitle("Probabilidad predicha") legend(cols(1) pos(2) ring(0)) ///
legend(region(lcolor(none))) title("") legend(region( fcolor(none))) ///
graphregion(color(white)) bgcolor(white) plotregion(fcolor(white)) ///
* Modelo
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///

* Gr�fica
ross_preciol1_log=mean fimposed=0 gwf_party=mean gwf_partymil=mean ///
gwf_partymilper=mean gwf_casia=0 gwf_easia=0 gwf_ceeurope=0 gwf_cacar=1 ///
ylabel(0(0.02).2) xlabel(-9(3)9) xtitle("Cambio porcentual") ///
ytitle("Probabilidad predicha") legend(cols(1) pos(2) ring(0)) ///
legend(region(lcolor(none))) title("") legend(region( fcolor(none))) ///
graphregion(color(white)) bgcolor(white) plotregion(fcolor(white)) ///