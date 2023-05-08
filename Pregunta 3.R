#Primer examen parcial 
#Curso de programación
#Examen 1
#Cinthya Marín
#2023

#file.choose()
#Nota para el profesor: Utilizar su dirección generada por el file.choose del punto anterior()
baseDatosCelulas=read.csv("[DIRECCION DEL ARCHIVO]")


#PREGUNTA 3

calcula.radio.promedio <- function(radio_X, radio_Y, radio_Z){
  radio.promedio <- mean(c(radio_X, radio_Y, radio_Z))
  
  return(radio.promedio)
  
}

# Función para calcular volumen de célula esférica
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

data.frame(baseDatosCelulas)


# Volumen promedio de las células que le tocaron a cada paciente

promedios_pacientes_volumen <- aggregate(baseDatosCelulas$volumenPromedio, 
                                         by = list(baseDatosCelulas$COD_PACIENTE), 
                                         FUN = mean)
print (promedios_pacientes_volumen)