#Molina_Denyse_EF_3.R
#INSTRUCCIONES
#Usa funciones y ciclo while para pedirle al usuario una secuencia de tres nucleótidos y que genere
#1. El aminoácido correspondiente
#2. La secuencia complementaria

#DEFINIR LA FUNCION CODON QUE NOS DARA EL AMINOACIDO DE LA SECUENCIA METIDA. codonusu LE PIDE AL USUARIO LA SECUENCIA. EL IF SE UTILIZA PARA QUE LE ARROJE AL USUARIO EL AMINOACIDO AL QUE CODIFICA SU SECUENCIA.
#as.character NOS PERMITE LEER LA SECUENICA INGRESADA COMO UN CARACTER (PATRON DE LETRAS)


codon<-function(){
  codonusu<-readline(prompt = paste("INGRESA UNA SECUENCIA DE 3 NUCLEOTIDOS  : "))
  AAcodif <- as.character(codonusu)
  if (AAcodif =="GGG" | AAcodif =="GGT" | AAcodif =="GGA" | AAcodif =="GGG"){
    print(paste("EL AMINOACIDO ES G"))
  }else if (AAcodif == "TTT" | AAcodif =="TTC"){
    print(paste("EL AMINOACIDO ES F"))
  }else if(AAcodif =="TTA" | AAcodif =="TTG" | AAcodif =="CTT" | AAcodif =="CTC" |
           AAcodif =="CTA" | AAcodif =="CTG"){
    print(paste("EL AMINOACIDO ES L"))
  }else if(AAcodif =="TCT" | AAcodif =="TCC" |AAcodif =="TCA" | AAcodif =="TCG" |
           AAcodif =="AGT" | AAcodif =="AGC"){
    print(paste("EL AMINOACIDO ES S"))
  }else if(AAcodif =="TAT" | AAcodif =="TAC" ){
    print(paste("EL AMINOACIDO ES Y"))
  }else if(AAcodif =="TGT" | AAcodif =="TGC"){
    print(paste("EL AMINOACIDO ES C"))
  }else if(AAcodif =="TGG"){
    print(paste("EL AMINOACIDO ES W"))
  }else if(AAcodif =="CCT" | AAcodif =="CCC" | AAcodif =="CCA"){
    print(paste("EL AMINOACIDO ES P"))
  }else if(AAcodif =="CAT" | AAcodif =="CAC"){
    print(paste("EL AMINOACIDO ES H"))
  }else if(AAcodif =="CAA" | AAcodif =="CAG"){
    print(paste("EL AMINOACIDO ES Q"))
  }else if(AAcodif =="CGT" | AAcodif =="CGC" | AAcodif=="CGA" | AAcodif=="CGG" | 
           AAcodif =="AGA" | AAcodif =="AGG"){
    print(paste("EL AMINOACIDO ES R"))
  }else if(AAcodif =="ATT" | AAcodif =="ATC"  | AAcodif =="ATA" ){
    print(paste("EL AMINOACIDO ES I"))
  }else if(AAcodif =="ATG"){
    print(paste("EL AMINOACIDO ES M"))
  }else if(AAcodif =="ACT"| AAcodif =="ACC"  | AAcodif =="ACA" | AAcodif =="ACG" ){
    print(paste("EL AMINOACIDO ES T"))
  }else if(AAcodif =="AAT" | AAcodif =="AAC" ){
    print(paste("EL AMINOACIDO ES N"))
  }else if(AAcodif =="AAA" | AAcodif =="AAG" ){
    print(paste("EL AMINOACIDO ES K"))
  }else if(AAcodif =="GTT" | AAcodif =="GTC"  | AAcodif =="GTA" | AAcodif =="GTG" ){
    print(paste("EL AMINOACIDO ES V"))
  }else if(AAcodif =="GCT" | AAcodif =="GCC" | AAcodif =="GCA" | AAcodif =="GCG" ){
    print(paste("EL AMINOACIDO ES A"))
  }else if(AAcodif =="GAT" | AAcodif =="GAC" ){
    print(paste("EL AMINOACIDO ES D"))
  }else if(AAcodif =="GAA" | AAcodif =="GAG" ){
    print(paste("EL AMINOACIDO ES E"))
  }else if(AAcodif =="TAA" | AAcodif =="TAG" | AAcodif =="TAG"){
    print(paste("EL AMINOACIDO ES *"))
  }
}


# DEFINIMOS LA FUNCION COMPLEMENTO QUE LE ARROJARA AL USUARIO LA SECUENCIA COMPLEMENTO DE SU SECUENCIA QUE INGRESO. codonusu1 LE PIDE AL USUARIO NUEVAMENTE SU SECUENCIA. DNAString LEE LA SECUENICA COMO DNA.
complementoaa<-function(){
  codonusu1<-readline(prompt = paste("INGRESA UNA SECUENCIA DE 3 NUCLEOTIDOS  : "))
  dnacadena<- DNAString(codonusu1)}

