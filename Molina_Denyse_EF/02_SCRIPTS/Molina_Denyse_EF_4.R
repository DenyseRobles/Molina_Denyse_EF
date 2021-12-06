#Molina_Denyse_EF_4.R
#INSTRUCCIONES
#Dadas dos secuencias del mismo tamaño encontrar un "score de alineamiento" entre ellas, por ejemplo, si coinciden dos nucleótidos entre ellas sumar un punto al score y si difieren restar -1, por ejemplo, con 
#GAGCCTACTAACGGGAT
#CATCGTAATGACGGCCT
#El score es -1+1-1+1 + ...1 = 10

#CARGA LA LIBRERIA PARA PODER SUBIR LAS SECUENCIAS 
library(Biostrings)
library(msa)

#SUBIR LAS SECUENCIAS. LA FUNCION DNAString NOS PERMITE LEER LA SECUENCIA COMO DNA. 
seq1sc <- c("GAGCCTACTAACGGGAT")
seq2sc <- c("CATCGTAATGACGGCCT")

#SCORE
#PREMIAR LA COINCIDENCIA +1
#PENALIZAR LA DIFERENCIA -1

#COMPARAR LAS SECUENCIAS
compareStrings(seq1sc,seq2sc)
#TIENE 7 DIFERENCIAS
#EN DIFERENCIA SE COLOCA EL NUMERO OBTENIDO DE AQUI

width(seq1sc)
width(seq2sc)
#NOS DICE QUE LAS SECUENCIAS SON DE 17 NUCLEOTIDOS

#PARA OBTENER LAS COINCIDENCIAS LE RESTAMOS LAS DIFERENCIAS AL TOTAL.
coincidencias<-17-7
coincidencias

#FUNCION DEFINIMOS Scoreuwu COMO LA FUNCION QUE DARA EL SCORE.
#diferencia DARA EL VALOR PARA EL SCORE YA PENALIZANDO LAS DIFERENCIAS.
#coincidencia DARA EL VALOR PARA EL SCORE YA PREMIANDO LAS DIFECENCIAS.
#Score DARÁ EL RESULTADO DEL SCORE, SUMANDO EL VALOR DE LAS DIFERENCIAS CON EL DE LAS COINCIDENCIAS. 

Scoreuwu<-function(){
  diferencia<-(7)*(-1)
  coincidencia<-(coincidencias)*(1)
  Score<-diferencia+coincidencia
  return(print(paste("EL SCORE DE LAS SECUENCIAS ES DE", Score)))
}

Scoreuwu()
