#Molina_Denyse_EF_2.R
#INSTRUCCIONES
#Escribe un programa en R que contenga una función que, a partir de dos secuencias FASTA, determine lo siguiente. En cada uno de los casos debe mandar un mensaje.
#1. Si las secuencias tienen o no el mismo tamaño
#2. El porcentaje de GC de cada una de ellas y las compare.
#3. La distancia de Hamming entre las primeras N letras en donde N es la longitud de la secuencia más pequeña. 

#CARGA LA LIBRERIA PARA PODER SUBIR LAS SECUENCIAS 
library(Biostrings)
library(msa)

#SUBIR LAS SECUENCIAS Y CONCATENAR LAS SECUENCIAS EN UN SOLO VECTOR
coronavirus229e<-readDNAStringSet("https://ftp.ncbi.nlm.nih.gov/genomes/Viruses/human_coronavirus_229e_uid14913/NC_002645.fna")
coronavirusn163<-readDNAStringSet("https://ftp.ncbi.nlm.nih.gov/genomes/Viruses/human_coronavirus_nl63_uid14960/NC_005831.fna")
covishos<-c(coronavirus229e,coronavirusn163)

#1- DETERMINAR SI LAS SECUENCIAS TIENEN O NO EL MISMO TAMANO
#DEFINIMOS LA FUNCION LENGTHSEC (QUE NOS DARA SI LAS SECUENCIAS TIENEN EN MISMO TAMANO). "width" NOS DIRA EL TAMANO DE LA SECUENCIA 1 (seq1) Y DE LA SECUENCIA 2 (seq2).
#CON EL "IF" INDICAMOS QUE ARROJE EL MENSAJE SI LAS SECUENCIAS SON IGUALES (CUANDO CUMPLE LA CONDICION DE seq1==seq2). SI NO CUMPLE LA CONDICION, EL MENSAJE QUE ARROJARA SERA "LAS SECUENCIAS NO SON IGUALES". 
lengthsec<-function(){
  seq1<-width(coronavirus229e)
  seq2<-width(coronavirusn163)
  if(seq1==seq2){
    print("LAS SECUENCIAS SON IGUALES")
  }else{print("LAS SECUENCIAS NO SON IGUALES")}
}

lengthsec()

#2- DETERMINAR EL CONTENIDO DE GC DE CADA SECUENCIA Y COMPARARLAS.
#DEFINIMOS LA FUNCION (CONTGC) QUE NOS DARA EL CONTENIDO DE GC PARA CADA SECUENCIA. seq1gc DARA EL CONTENIDO DE GC PARA LA PRIMERA SECUENCIA. seq1gc DARA EL CONTENIDO DE GC DE LA SEGUNDA SECUENCIA.
#LETTERFREQUENCY NOS SACARA LA FRECUENCIA DE LAS LETRAS G Y C EN LAS SECUENCIAS, EL AS. PROB ES PARA QUE LO REPRESENTE COMO PROBABILIDAD. 
#IF ARROJARA EL MENSAJE DE SI EL CONTENIDO DE GC ES IGUAL O NO PARA LAS SECUENCIAS COMPARADAS. 
#EL RETURN NOS ENVIARA EL MENSAJE DE CUAL ES EL CONTENIDO DE GC PARA CADA SECUENCIA, EN EL ORDEN  SECUECNIA 1 Y SECUENCIA 2. 
contgc<-function(){
  seq1gc<-letterFrequency(coronavirus229e, c("CG"), as.prob = TRUE)
  seq2gc<-letterFrequency(coronavirusn163, c("CG"), as.prob = TRUE)
  if(seq1gc==seq2gc){
    print("EL CONTENIDO DE GC ES IGUAL EN LAS SECUENCIAS")
  }else{print("EL CONTENIDO DE GC ES DIFERENTE")}
  return(print(paste("EL CONTENIDO DE GC EN LAS SECUENCIAS ES", seq1gc, "Y", seq2gc, "RESPECTIVAMENTE")))
}

contgc ()
#3.- DETERMINAR LA DISTANCIA DE HAMMING. SACAR DISTANCIA DE HAMMING (n/N) ENTRE LAS PRIMERAS LETRAS, N ES LA LONGITUD DE LA SECUENCIA MAS PEQUENA.
#DEFINIMOS LA FUNCION Hammingdistance QUE NOS SACARA LA DISTANCIA DE HAMMING ENTRE DOS SECUENCIAS. 
#CREAMOS LA VARIABLE diferencia, PARA COMPARAR  LA SECUENCIA 1 Y 2 Y SACAR LAS DIFERENCIAS. SERIA UN EQUIVALENTE A (x!=y). 
#CREAMOS LA VARIABLE  Hammingvalue PARA SACAR LA DISTANCIA DE HAMMING, LA FORMULA ES n/N. SIENDO N LA SECUENCIA MAS PEQUENA Y n LA DIFERENCIA ENTRE LAS SECUENCIAS. 

Hammingdistance<-function(){
  diferencia<-(width(coronavirus229e)!=width(coronavirusn163))
  width(coronavirus229e) #C
  width(coronavirusn163) #
  Hammingvalue<-diferencia/width(coronavirus229e)
  return(print(paste("LAS DISTANCIA DE HAMMING ES", Hammingvalue)))
}

Hammingdistance()
