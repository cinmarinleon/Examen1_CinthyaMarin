#Primer examen parcial 
#Curso de programación
#Examen 1
#Cinthya Marín
#2023

file.choose()
#Nota para el profesor: Utilizar su dirección generada por el file.choose del punto anterior()
baseDatosCelulas=read.csv("[DIRECCION DEL ARCHIVO]")
View(baseDatosCelulas$ID_CELULA)
names(baseDatosCelulas)
data.frame(baseDatosCelulas)

#PREGUNTA 5

#Se genera una lista de valores aleatorios con la misma longitud que la cantidad de celulas en la base de datos

generar.esfericidad <- function() {
  for (i in 1:nrow(baseDatosCelulas)){
    esfericidad <- runif(1,0, 1)
    baseDatosCelulas[i,"ESFERICIDAD"] <- esfericidad
  }
  return(baseDatosCelulas)
}

baseDatosCelulas <- generar.esfericidad()


print(baseDatosCelulas)