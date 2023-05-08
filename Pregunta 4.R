#Primer examen parcial 
#Curso de programación
#Examen 1
#Cinthya Marín
#2023

#file.choose()
#Nota para el profesor: Utilizar su dirección generada por el file.choose del punto anterior()
baseDatosCelulas=read.csv("[DIRECCION DEL ARCHIVO]")


#Calculo volumen promedio

calcula.radio.promedio <- function(radio_X, radio_Y, radio_Z){
  radio.promedio <- mean(c(radio_X, radio_Y, radio_Z))
  
  return(radio.promedio)
  
}

calcula.volumen.cel <- function(radio){
  volumen.cel <- (4/3) * pi * radio^3
  return(volumen.cel)
}

promedio.radios <- function() {
  for (i in 1:nrow(baseDatosCelulas)){
    radioX <- baseDatosCelulas[i,"DIM_RAD_NM_X"]
    radioY <- baseDatosCelulas[i,"DIM_RAD_NM_Y"]
    radioZ <- baseDatosCelulas[i,"DIM_RAD_NM_Z"]
    radioPromedio <- calcula.radio.promedio(radioX, radioY, radioZ)
    volumenPromedio <- calcula.volumen.cel(radioPromedio)
    baseDatosCelulas[i,"volumenPromedio"] <- volumenPromedio
  }
  return(baseDatosCelulas)
}

baseDatosCelulas <- promedio.radios()






#PREGUNTA 4


#Se genera un nuevo conjunto de datos, que incluye COD_PACIENTE, VOL_MED_CEL, EDAD_PACIENTE_AÑOS

calcularEdadAnos <- function(edad){
  meses <- edad %% 12
  edadAnos <- (edad - meses) / 12
  return(edadAnos)
}

generar.nuevo.conjunto <- function (){
  edad.anos <- calcularEdadAnos(baseDatosCelulas$EDAD_PACIENTE)
  conjunto.nuevo <- data.frame(COD_PACIENTE=baseDatosCelulas$COD_PACIENTE, VOL_MED_CEL=baseDatosCelulas$volumenPromedio, EDAD_PACIENTE_ANOS=edad.anos)
}

#Se genera una función llamada NUEVO_CONJUNTO en la que se almacena los datos obtenidos en la función generar.nuevo.conjunto:
NUEVO_CONJUNTO <- generar.nuevo.conjunto()

print(NUEVO_CONJUNTO)