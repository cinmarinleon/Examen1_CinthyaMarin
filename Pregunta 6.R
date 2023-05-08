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





#PREGUNTA 6

# Volumen del equipo 

agregar_vol_equipo <- function() {
  for (i in 1:nrow(baseDatosCelulas)){
    volumenEquipo <- calcula.volumen.cel(runif(1,1,10))
    baseDatosCelulas[i,"VOL_EQUIPO"] <- volumenEquipo
  }
  return(baseDatosCelulas)
}

# Agregar la columna VOL_EQUIPO a la base de datos

baseDatosCelulas <- agregar_vol_equipo()

print(baseDatosCelulas)



#VARIANZA (Pregunta a) #

calcular_varianza <- function(baseDatosCelulas) {
  
  # Calcular la varianza de las medidas volumen.promedio y VOL_EQUIPO para cada fila
  varianza <- apply(baseDatosCelulas[, c("volumenPromedio", "VOL_EQUIPO")], 1, var)
  
  # Agregar la nueva columna VARIANZA al data frame con los valores de la varianza calculada
  baseDatosCelulas$VARIANZA <- varianza
  
  return(baseDatosCelulas)
}

baseDatosCelulas <- calcular_varianza(baseDatosCelulas)

print(baseDatosCelulas)


#VARIANZA (Pregunta b) #

# Filtra los datos únicos de la columna COD_ANALISTA y calcula el promedio de VARIANZA
promedio_varianza <- aggregate(VARIANZA ~ COD_ANALISTA, data = baseDatosCelulas, FUN = mean)

# Crea una nueva columna en la base de datos baseDatosCelula llamada PRECISION_ANALISTA
baseDatosCelulas$PRECISION_ANALISTA <- NA

# Asigna los valores de promedio_varianza a la columna PRECISION_ANALISTA 
baseDatosCelulas[baseDatosCelulas$COD_ANALISTA %in% promedio_varianza$COD_ANALISTA, "PRECISION_ANALISTA"] <- promedio_varianza$VARIANZA



#Analista con menor varianza


obtener_analista_menor_varianza <- function() {
  min_row <- which.min(promedio_varianza$VARIANZA)
  mejorAnalista <- promedio_varianza$COD_ANALISTA[min_row]
  
  return(mejorAnalista)
}

mejorAnalista <- obtener_analista_menor_varianza() 

print(mejorAnalista)
