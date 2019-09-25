/// Elaborado por Sebasti�n Garrido de Sierra Garrido
/// libro "La reforma definitiva. Por qu� y c�mo la reforma electoral de 1996 
/// precipit� la ca�da del r�gimen de partido dominante pri�sta" 

/// incluidos en dicho cap�tulo para analizar el potencial efecto del desempe�o 
/// de la econom�a en la supervivencia de un r�gimen de partido dominante.
/// La base de datos utilizada para este an�lisis fue previamente procesada y 
/// generada en R. Revisar el archivo cap_02_cargar_unir_datos.R en el folder 
/// 02_codigo.
*** Cargar paquetes ***
ssc install estout, replace



*** -------------------------------------------------------------------------- *
* para cada observaci�n; _d registra el resultado de cada observaci�n al final del 
* per�odo analizado (1 si la observaci�n termina en fracaso o 0 si no); _st registra
* si la observaci�n es relevante y puede ser incluida en el an�lisis.
* tiene m�ltiples observaciones, id() permite indicarle a Stata qu� variable utilizar 
* para no tratar cada observaci�n como si fuera diferente. 

* failure() define la variable y el valor que representan un evento para el an�lisis; 
* en este caso, la ca�da de un r�gimen. 
* sin variables de control  
* Si se incluye 'tr', Stata reporta el ratio del tiempo acelerado de fracaso o 
* Accelerated Failure Time (AFT), que es lo mismo que exp(coeficiente) del AFT.

//          + Tipo de r�gimen + Regiones

// Modelo 8 Fin del r�gimen ~ Promedio m�vil de 5 a�os+ PIB per c�pita +
//          Tipo de r�gimen + Regiones
//          PIB per c�pita + Ingreso petrolero per c�pita + 
m5_weibull_pwt m6_weibull_pwt m7_weibull_pwt m8_weibull_pwt /// 
m9_weibull_pwt, eform varlabels(_cons \_cons) cells(b(star fmt(3)) ///
streg mad_chgdppcl1 mad_gdppcl1_log ross_preciol1_log fimposed, dist(weib) cl(gwf_country)

//          + Tipo de r�gimen + Regiones
//          Tipo de r�gimen + Regiones
//          PIB per c�pita + Ingreso petrolero per c�pita + 
estimates store m9_weibull_mad, title(Modelo 9mad)
estout m1_weibull_mad m2_weibull_mad m3_weibull_mad m4_weibull_mad ///
m5_weibull_mad m6_weibull_mad m7_weibull_mad m8_weibull_mad m9_weibull_mad, ///
eform varlabels(_cons \_cons) cells(b(star fmt(3)) ///


*** Panel A: Gr�fica del modelo 8 con pwt7_fiveyma = 3.5, 2.5 y 1.5 + 
***          fimposed = 0 + gwf_cacar = 1

** Usando PWT 7.0

* Modelo

* Gr�fica

predict time, time

predict, hr 

at(pwt7_fiveyma = 3.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///

margins, at(pwt7_fiveyma = 3.5) 

, fimposed = 1, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///
* 
survci, at(pwt7_fiveyma = 3.5, fimposed = 0, gwf_casia = 0, gwf_easia = 0, gwf_ceeurope = 0, gwf_cacar = 1, ///


** Usando Maddison (no incluida en el cap�tulo)

* Modelo
streg mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party ///



*** Panel B: Gr�fica del modelo 8 con pwt7_fiveyma = 6.3, 2.5 y -1.3 + 
***          fimposed = 0 + gwf_cacar = 1

** Usando PWT 7.0

* Modelo

* Gr�fica

** Usando Maddison (no incluida en el cap�tulo)

* Modelo
streg mad_fiveyma mad_gdppcl1_log ross_preciol1_log fimposed gwf_party ///
  


*** -------------------------------------------------------------------------- *

// Con "Promedio m�vil de 2 a�os"

// Con "Promedio m�vil de 3 a�os"

// Con "Promedio m�vil de 4 a�os"

// Con "Promedio m�vil de 5 a�os"

// Con "Promedio m�vil de 6 a�os"

// Con "Promedio m�vil de 7 a�os"

// Con "Promedio m�vil de 8 a�os"

// Con "Promedio m�vil de 9 a�os"

// Con "Promedio m�vil de 10 a�os"




*** Resultados de todas las versiones del modelo 8

m8_weibull_pwt7_seis m8_weibull_pwt7_siete m8_weibull_pwt7_ocho m8_weibull_pwt7_nueve ///
m8_weibull_pwt7_diez, varlabels(_cons \_cons) cells(b(star fmt(3)) se(par fmt(3))) stats(ll aic N)
