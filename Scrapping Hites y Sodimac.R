################################################################################
############ Proyecto final de Big Data: Srcapping Hites y Sodimac #############
################################################################################

#Se proceden a instalar y ejecutar paquetes de apoyo para el proyecto
install.packages("rvest")
library("rvest")

install.packages("dplyr")
library("dplyr")

install.packages("gdata")
library("gdata")

install.packages("curl")
library("curl")

install.packages("xml2")
library("xml2")


########################## Primera pagina: Hites ###############################

#Nota: La ultima vizualizacion de las paginas se realizo el 29-07-2021

#Pagina inicial de donde por medio de las categorias dentro de ella se hara el web scarping
Hites <-read_html("https://www.hites.com/")
print(html_text(Hites))


#La primera categoria sera sobre a seccion de sofas ordenados por "recomendado" 
#dentro de la pagina de Hites 

#Se define la variable a trabajar
livingHites <- read_html("https://www.hites.com/muebles/living/sofa/")

#Se define la variable asociada a la class de la pagina
sofasH <- html_nodes(livingHites, css=".tile-section")
print(html_text(sofasH))


#Se defienen 3 variables para sacar Nombre del producto, con su precio original y con rebaja
SH1 <- html_nodes(sofasH, css = ".link.product-name")
print(html_text(SH1))
SH2 <- html_nodes(sofasH, css = ".price-item.list.strike-through")
print(html_text(SH2))
SH3 <- html_nodes(sofasH, css = ".price-item.sales")
print(html_text(SH3))


#Se limpia la seccion para obtener el Nombe producto
SH1 <- html_nodes(sofasH, css = ".link.product-name")
print(html_text(SH1))
Txsh1 <- html_text(SH1)
Txsh1 <- gsub("/","",Txsh1)
Txsh1 <- gsub("3","",Txsh1)
Txsh1 <- gsub("Cuerpos","",Txsh1)
Txsh1 <- gsub("2","",Txsh1)
Txsh1 <- gsub("Butacas","",Txsh1)
Txsh1 <- gsub("Poltronas","",Txsh1)
Txsh1 <- gsub("[+]","",Txsh1)
print(Txsh1)


#Se limpia la seccion para obtener el Precio normal
SH2 <- html_nodes(sofasH, css = ".price-item.list.strike-through")
print(html_text(SH2))
Txsh2 <- html_text(SH2)
Txsh2 <- gsub("\n","",Txsh2)
Txsh2 <- gsub("Price","",Txsh2)
Txsh2 <- gsub("reduced","",Txsh2)
Txsh2 <- gsub("from","",Txsh2)
Txsh2 <- gsub("Normalto","",Txsh2)
Txsh2 <- gsub("Normal","",Txsh2)
Txsh2 <- gsub("[$]","",Txsh2)
Txsh2 <- gsub("[.]","",Txsh2)
Txsh2 <- gsub("to","",Txsh2)
print(Txsh2)


#Se limpia la seccion para obtener el Precio con rebaja por oferta
SH3 <- html_nodes(sofasH, css = ".price-item.sales")
print(html_text(SH3))
Txsh3 <- html_text(SH3)
Txsh3 <- gsub("\n","",Txsh3)
Txsh3 <- gsub("Oferta","",Txsh3)
Txsh3 <- gsub("[$]","",Txsh3)
Txsh3 <- gsub("[.]","",Txsh3)
print(Txsh3)


#Se finaliza el limpiado de las 3 variables a usar mas adelante


#La segunda categoria sera sobre a seccion de mesas de centro y relacionados 
#ordenados por "recomendado" dentro de la pagina de Hites

#Se define la variable a trabajar
livingHites2 <- read_html("https://www.hites.com/muebles/living/mesas-de-centro-arrimo-y-laterales/")
#Se define la variable asociada a la class de la pagina
mesasH <- html_nodes(livingHites2, css=".tile-section")
print(html_text(mesasH))


#Se defienen 3 variables para sacar Nombre del producto, con su precio original y con rebaja
MH1 <- html_nodes(mesasH, css = ".link.product-name")
print(html_text(MH1))
MH2 <- html_nodes(mesasH, css = ".price-item.list.strike-through")
print(html_text(MH2))
MH3 <- html_nodes(mesasH, css = ".price-item.sales")
print(html_text(MH3))


#Se limpia la seccion para obtener el Nombre producto
MH1 <- html_nodes(mesasH, css = ".link.product-name")
print(html_text(MH1))
Txmh1 <- html_text(MH1)
print(Txmh1)
#nota: Al mostrar el nombre del producto de inmediato, no hay necesidad de lipieza


#Se limpia la seccion para obtener el Precio normal
MH2 <- html_nodes(mesasH, css = ".price-item.list.strike-through")
print(html_text(MH2))
Txmh2 <- html_text(MH2)
print(Txmh2)
Txmh2 <- gsub("\n","",Txmh2)
Txmh2 <- gsub("Price","",Txmh2)
Txmh2 <- gsub("reduced","",Txmh2)
Txmh2 <- gsub("from","",Txmh2)
Txmh2 <- gsub("Normalto","",Txmh2)
Txmh2 <- gsub("Normal to","",Txmh2)
Txmh2 <- gsub("[$]","",Txmh2)
Txmh2 <- gsub("[.]","",Txmh2)
print(Txmh2)


#Se limpia la seccion para obtener el Precio con rebaja por oferta
MH3 <- html_nodes(mesasH, css = ".price-item.sales")
print(html_text(MH3))
Txmh3 <- html_text(MH3)
print(Txmh3)
Txmh3 <- gsub("\n","",Txmh3)
Txmh3 <- gsub("Oferta","",Txmh3)
Txmh3 <- gsub("[$]","",Txmh3)
Txmh3 <- gsub("[.]","",Txmh3)
print(Txmh3)

#Se finaliza el limpiado de las 3 variables a usar mas adelante


############### Se comienza la creacion de las tabla para Hites ################

# Creando data frame que almacene las listas relacionadas a las mesas de Hites
NameMesaHites <- data.frame(Txmh1)
PriceMesaHites <- data.frame(Txmh2)
SaleMesaHites <- data.frame(Txmh3)

# Uniendo data frames en una sola tabla
HITESMesaTabla <- cbind(NameMesaHites,PriceMesaHites,SaleMesaHites)

# Cambiando los nombres de las columnas
HITESMesaTabla <- rename(HITESMesaTabla, "Nombre producto" = "Txmh1", "Precio normal CLP" = "Txmh2", "Precio oferta CLP" = "Txmh3")

# Transformando los precios a valores numericos
HITESMesaTabla[,2] <- as.numeric(HITESMesaTabla[,2])
typeof(HITESMesaTabla[,2])
HITESMesaTabla[,3] <- as.numeric(HITESMesaTabla[,3])
typeof(HITESMesaTabla[,3])

# Creando data frame que almacene las listas relacionadas a los sofas de Hites
NameSofaHites <- data.frame(Txsh1)
PriceSofaHites <- data.frame(Txsh2)
SaleSofaHites <- data.frame(Txsh3)

# Uniendo data frames en una sola tabla
HITESSofaTabla <- cbind(NameSofaHites,PriceSofaHites,SaleSofaHites)

# Cambiando los nombres de las columnas
HITESSofaTabla <- rename(HITESSofaTabla, "Nombre producto" = "Txsh1", "Precio normal CLP" = "Txsh2", "Precio oferta CLP" = "Txsh3")

# Transformando los precios a valores numericos
HITESSofaTabla[,2] <- as.numeric(HITESSofaTabla[,2])
HITESSofaTabla[,3] <- as.numeric(HITESSofaTabla[,3])
typeof(HITESSofaTabla[,2])
typeof(HITESSofaTabla[,3])





################################################################################
######################## Segunda pagina: Sodimac ###############################

#Pagina inicial de donde por medio de las categorias dentro de ella se hara el web scarping
Sodimac <-read_html("https://www.sodimac.cl/sodimac-cl/")
print(html_text(Sodimac))

#La primera categoria sera sobre a seccion de sofas ordenados por "recomendado"
#dentro de la pagina de Sodimac

#Se define la variable a trabajar
livingSodimac <- read_html("https://www.sodimac.cl/sodimac-cl/category/cat1140004/")
#Se define la variable asociada a la class de la pagina
sofasS <- html_nodes(livingSodimac, css=".jsx-3663142191")
print(html_text(sofasS))

#Se defienen 2 variables para sacar Nombre del producto y su precio original
SS1 <- html_nodes(sofasS, css = ".jsx-411745769.product-title")
print(html_text(SS1))
SS2 <- html_nodes(sofasS, css = ".jsx-585964327.main.gridView")
print(html_text(SS2))

#Se limpia la seccion para obtener el Nombe producto
SS1 <- html_nodes(sofasS, css = ".jsx-411745769.product-title")
print(html_text(SS1))
Txss1 <- html_text(SS1)
Txss1 <- gsub("210x85x85 cm","",Txss1)
Txss1 <- gsub("158x85x85 cm","",Txss1)
Txss1 <- gsub("180x85x75 cm","",Txss1)
Txss1 <- gsub("188x77x76 cm","",Txss1)
Txss1 <- gsub("137x180x67 cm","",Txss1)
Txss1 <- gsub("188x136x77 cm","",Txss1)
Txss1 <- gsub("177x75x71 cm","",Txss1)
Txss1 <- gsub("186x90x85 cm","",Txss1)
Txss1 <- gsub("200x80x85 cm","",Txss1)
Txss1 <- gsub("130x70x78 cm","",Txss1)
Txss1 <- gsub("180x137x67 cm","",Txss1)
Txss1 <- gsub("220x65x85","",Txss1)
Txss1 <- gsub("260x172x78 cm","",Txss1)
Txss1 <- gsub("220x70x90 cm","",Txss1)
Txss1 <- gsub("210x80x80 cm","",Txss1)
print(Txss1)


#Se limpia la seccion para obtener el Precio normal
SS2 <- html_nodes(sofasS, css = ".jsx-411745769.desktop-price-cart-btn")
print(html_text(SS2))
Txss2 <- html_text(SS2)
Txss2 <- substring(Txss2, first = 3)
Txss2 <- gsub(")","",Txss2)
Txss2 <- gsub("C/UDisponible para despachoNo disponible para retiroAgregar al carro","",Txss2)
Txss2 <- gsub("[$]","",Txss2)
Txss2 <- gsub("[.]","",Txss2)
print(Txss2)

#nota: No cuenta con precios con rebaja por oferta


#La segunda categoria sera sobre a seccion de mesas de centro y relacionados 
#ordenados por "recomendado" dentro de la pagina de Sodimac

#Se define la variable a trabajar
livingSodimac2 <- read_html("https://www.sodimac.cl/sodimac-cl/category/scat100551/Mesas-de-Centro,-Laterales-y-Arrimos")
#Se define la variable asociada a la class de la pagina
mesasS <- html_nodes(livingSodimac2, css=".jsx-3663142191")
print(html_text(mesasS))


#Se defienen 2 variables para sacar Nombre del producto y su precio original
MS1 <- html_nodes(mesasS, css = ".jsx-411745769.product-title")
print(html_text(MS1))
MS2 <- html_nodes(mesasS, css = ".jsx-585964327.main.gridView")
print(html_text(MS2))


#Se limpia la seccion para obtener el Nombe producto
MS1 <- html_nodes(mesasS, css = ".jsx-411745769.product-title")
print(html_text(MS1))
Txms1 <- html_text(MS1)
Txms1 <- gsub("91x60x40 cm","",Txms1)
Txms1 <- gsub("98x60x43 cm","",Txms1)
Txms1 <- gsub("100x30x80 cm","",Txms1)
Txms1 <- gsub("90x77x36 cm","",Txms1)
Txms1 <- gsub("170x40x76 cm","",Txms1)
Txms1 <- gsub("155x73x47 cm","",Txms1)
Txms1 <- gsub("88x54x29,5 cm","",Txms1)
Txms1 <- gsub("100x30x80 cm","",Txms1)
Txms1 <- gsub("90x54x36 cm","",Txms1)
Txms1 <- gsub("45x110x60 cm","",Txms1)
Txms1 <- gsub("30x51 cm","",Txms1)
Txms1 <- gsub("44x35x73 cm","",Txms1)
Txms1 <- gsub("31,6x60x135 cm","",Txms1)
Txms1 <- gsub("60x90x46 cm","",Txms1)
Txms1 <- gsub("38x120x60 cm","",Txms1)
Txms1 <- gsub("107x60x33 cm","",Txms1)
Txms1 <- gsub("63x63x33 cm","",Txms1)
Txms1 <- gsub("48x44x44 cm","",Txms1)
Txms1 <- gsub("120,3x32,6x82 cm","",Txms1)
Txms1 <- gsub("45x45x45 cm","",Txms1)
Txms1 <- gsub("129x79x30 cm","",Txms1)
Txms1 <- gsub("90x40x76 cm","",Txms1)
Txms1 <- gsub("36,5x36,5x75 cm","",Txms1)
print(Txms1)


#Se limpia la seccion para obtener el Precio normal
MS2 <- html_nodes(mesasS, css = ".jsx-411745769.desktop-price-cart-btn")
print(html_text(MS2))
Txms2 <- html_text(MS2)
Txms2 <- substring(Txms2, first = 4)
Txms2 <- gsub(")","",Txms2)
Txms2 <- gsub("C/UDisponible para despachoNo disponible para retiroAgregar al carro","",Txms2)
Txms2 <- gsub("C/UDisponible para despachoDisponible para retiroAgregar al carro","",Txms2)
Txms2 <- gsub("C/UNormal[:][$]139.990","",Txms2)
Txms2 <- gsub("[$]","",Txms2)
Txms2 <- gsub("[.]","",Txms2)
print(Txms2)

#nota: al haber solo un producto con rebajas, se decidio no considerarlo al ser poco relevante


############### Se comienza la creacion de las tabla para Sodimac ################

# Creando data frame que almacene las listas relacionadas a las mesas de Sodimac
NameMesaSodi <- data.frame(Txms1)
PriceMesaSodi <- data.frame(Txms2)

# Uniendo data frames en una sola tabla
SODIMACMesaTabla <- cbind(NameMesaSodi,PriceMesaSodi)

# Cambiando los nombres de las columnas
SODIMACMesaTabla <- rename(SODIMACMesaTabla, "Nombre del producto" = "Txms1", "Precio normal CLP" = "Txms2")

# Transformando precios a valores numericos
SODIMACMesaTabla[,2] <- as.numeric(SODIMACMesaTabla[,2])
typeof(SODIMACMesaTabla[,2])

# Creando data frame que almacene las listas relacionadas a los sofas de Sodimac
NameSofaSodi <- data.frame(Txss1)
PriceSofaSodi <- data.frame(Txss2)

# Uniendo data frames en una sola tabla
SODIMACSofaTabla <- cbind(NameSofaSodi,PriceSofaSodi)

# Cambiando los nombres de las columnas
SODIMACSofaTabla <- rename(SODIMACSofaTabla, "Nombre del producto" = "Txss1", "Precio normal CLP" = "Txss2")

# Tranformando precios a valores numericos
SODIMACSofaTabla[,2] <- as.numeric(SODIMACSofaTabla[,2])
typeof(SODIMACSofaTabla[,2])





################################################################################
################### DISEÑO DEL PROYECTO UNIENDO AMBAS PAGINAS ################## 

# Creando vectores que tendran el nombre de cada tienda

HITES <- rep("Hites",24)
print(HITES)

SODIMAC <- rep("Sodimac",28)
print(SODIMAC)

# Integrando nuevas variables a las tablas correspondientes

HITESMesaTabla <- cbind(HITESMesaTabla,HITES)
HITESSofaTabla <- cbind(HITESSofaTabla, HITES)

SODIMACMesaTabla <- cbind(SODIMACMesaTabla, SODIMAC)
SODIMACSofaTabla <- cbind(SODIMACSofaTabla, SODIMAC)

# Extrayendo columna que no se utilizara de las tablas
HITESMesaTabla <- HITESMesaTabla[,-3]
HITESSofaTabla <- HITESSofaTabla[,-3]

# Cambiando nombre de columnas con la tienda perteneciente
HITESSofaTabla <- rename(HITESSofaTabla, "Tienda" = "HITES")
HITESMesaTabla <- rename(HITESMesaTabla, "Tienda" = "HITES")

SODIMACMesaTabla <- rename(SODIMACMesaTabla, "Tienda" = "SODIMAC")
SODIMACSofaTabla <- rename(SODIMACSofaTabla, "Tienda" = "SODIMAC")

#######Corrigiendo error de sintaxis en las tablas de hites
HITESMesaTabla <- rename(HITESMesaTabla, "Nombre del producto" = "Nombre producto")
HITESSofaTabla <- rename(HITESSofaTabla, "Nombre del producto" = "Nombre producto")

#Uniendo data frames acorde al producto
SILLONES <- rbind(HITESSofaTabla,SODIMACSofaTabla)
MESAS <- rbind(HITESMesaTabla,SODIMACMesaTabla)

# # # # # # # # # # # # # # CÁLCULO ESTADÍSTICO # # # # # # # # # # # # # # # #

# Valor promedio de los sofas de Hites
PromedioSillonHites <- mean(HITESSofaTabla[,2])
print(paste("El promedio de los sillones en Hites es de", "$",PromedioSillonHites))

# Desviacion de los sofas de Hites
DevSillonHites <- sd(HITESSofaTabla[,2])
print(paste("La desviación estandar de los sofas de Hites es de",DevSillonHites))

# Valor promedio de las mesas de Hites
PromedioMesasHites <- mean(HITESMesaTabla [,2])
print(paste("El promedio de los mesas en Hites es de", "$",PromedioMesasHites))

# Desviacion de los mesas de Hites
DevMesasHites <- sd(HITESMesaTabla[,2])
print(paste("La desviación estandar de los mesas de Hites es de",DevMesasHites))

# Valor promedio de los sofas de Sodimac
PromediosSillonesSodimac <- mean(SODIMACSofaTabla[,2])
print(paste("El promedio de los sillones en Sodimac es de", "$",PromediosSillonesSodimac))

# Desviacion de los sofas de Sodimac
DevSillonSodimac <- sd(SODIMACSofaTabla[,2])
print(paste("La desviación estandar de los sofas de Sodimac es de",DevSillonSodimac))

# Valor promedio de las mesas de Sodimac
PromediosMesasSodimac <- mean(SODIMACMesaTabla[,2])
print(paste("El promedio de los mesas en Sodimac es de", "$",PromediosMesasSodimac))

# Desviacion de los mesas de Sodimac
DevMesasSodimac <- sd(SODIMACMesaTabla[,2])
print(paste("La desviación estandar de los mesas de Sodimac es de",DevMesasSodimac))


# Encontrar que tienda tiene el sillon mas caro y el más barato con sus valores

SillonCaroBarato <- function(b){
  #Calculando el valor maximo
  Maximo <- max(SILLONES[,2])
  #Calculando el valor minimo
  Minimo <- min(SILLONES[,2])
  #Creando variable que almacenara el nombre de la tienda
  TiendaCara <- c()
  TiendaBarata <- c()
  #Creando LOOP que buscara la tienda corresdiente al precio mas alto
  for (z in 1:nrow(SILLONES)) {
    if(SILLONES[z,2] == Maximo){
      TiendaCara <- c(TiendaCara,SILLONES[z,3])
    }
  }
  #Creando LOOP que buscara la tienda correspondiente al precio mas bajo
  for (y in 1:nrow(SILLONES)) {
    if(SILLONES[y,2] == Minimo){
      TiendaBarata <- c(TiendaBarata,SILLONES[y,3])
    }
  }
  print(paste("El sofa más caro en venta en",TiendaCara,"tiene un precio de",Maximo,"CLP"))
  print(paste("El sofa más barato en venta en",TiendaBarata,"tiene un precio de",Minimo,"CLP"))
}

#Probando la funcion
SillonCaroBarato(SILLONES)

# Encontrar que tienda tiene la mesa mas cara y su valor
MesaCaraBarata <- function(c){
  #Calculando el valor maximo
  Maximo <- max(MESAS[,2])
  #Calculando el valor minimo
  Minimo <- min(MESAS[,2])
  #Creando variable que almacenara el nombre de la tienda
  TiendaCara <- c()
  TiendaBarata <- c()
  #Creando LOOP que buscara la tienda correspondiente al precio mas alto
  for (z in 1:nrow(MESAS)) {
    if(MESAS[z,2] == Maximo){
      TiendaCara <- c(TiendaCara,MESAS[z,3])
    }
  }
  #Creando LOOP que buscara la tienda correspondiente al precio mas bajo
  for (y in 1:nrow(MESAS)) {
    if(MESAS[y,2] == Minimo){
      TiendaBarata <- c(TiendaBarata,MESAS[y,3])
    }
  }
  print(paste("La mesa más cara en venta en",TiendaCara,"tiene un precio de",Maximo,"CLP"))
  print(paste("La mesa más barata en venta en",TiendaBarata,"tiene un precio de",Minimo,"CLP"))
}

# Probando la función
MesaCaraBarata(MESAS)

# Calculo de la desviación estándar de los valores de los sillones y mesas

## Desviación estandar de valor sillones

DevSillon <- sd(SILLONES[,2])
print(paste("La desviación estandar de los sofas es de",DevSillon))

## Desviación estandar de valor de mesas)

DevMesas <- sd(MESAS[,2])
print(paste("La desviación estandar de las mesas es de",DevMesas))