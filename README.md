
# Proyecto final de Big Data: Scrapping Hites y Sodimac

Integrantes: Camila Campos Salinas 
             Andr�s Jelves 
          
Docente: Amaru Dur�n 

## Introducci�n
##### En el presente informe, se presentar� un an�lisis comparativo entre las paginas web de Hites y Homecenter mediante los precios de los sof�s y mesas de centro recomendadas por las mismas p�ginas, haciendo uso de las herramientas entregadas por R. Esto con el objetivo de guiar al consumidor en una mejor toma de decisi�n al momento de compra, pudiendo satisfacer sus necesidades en base a sus condiciones de preferencia personal.
##### La elecci�n de ambas p�ginas, mencionadas anteriormente, nace de comparar una empresa asociada a la venta de art�culos de moda y tecnolog�a con una relacionada al mundo de la construcci�n y al hogar, de esta forma se comprender� el comportamiento de los precios de ambas.
##### Los an�lisis comparativos ser�n hechos por medio de 2 tablas, una correspondiente a la informaci�n de los precios de los sof�s y otra con los precios de las mesas, las cuales tendr�n integrada las informaciones de ambas p�ginas, donde se desprender� informaci�n tan apreciativa como por medio de estad�sticas.
##### Finalmente se entregar�n como resultado final, el promedio de los precios de cada p�gina seg�n su producto, como la desviaci�n est�ndar presente en esta. A su vez se se�alar�n en cu�l de las 2 p�ginas est� presente el precio mas bajo y mas alto de ambos productos a trabajar.

## Desarrollo
##### Como se comentaba anteriormente, el estudio se realizar� con dos empresas respectivas al mundo del retail como Sodimac que distribuye tanto elementos de construcci�n, como para decoraci�n y amoblar el hogar, e Hites una empresa tambi�n es del �rea del retail, que no solo se concentra en elementos decorativos, adem�s, para el hogar y bienes para los inmuebles, e incluso ofrece productos como vestimenta y si bien, se ofrecen todos este tipo de elementos dentro de ambas tiendas, pero el objetivo central de esto, es el an�lisis comparativo de las caracter�sticas de los productos respectivos a muebles y elementos asociados a esto, con ello se quiere lograr encontrar una alternativa m�s accesible y acorde a un bajo presupuesto. 
##### En primer lugar se proceder� a la instalaci�n de paquetes de apoyo para la programaci�n, as� hacer m�s f�cil la interacci�n con las respectivas p�ginas a utilizar correspondientes a Sodimac e Hites.
 
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

##### Por consiguiente, se da el pie de ingreso de la primera p�gina seleccionada en este caso Hites, para tener una mejor vista de las distintas categor�as que se buscan estudiar.

Hites <-read_html("https://www.hites.com/")
print(html_text(Hites))

##### Ante la selecci�n e introducci�n del sitio web para trabajar, se da a seleccionar una categor�a, para comenzar con el an�lisis, es por ello, que la categor�a a elegir es "Decoraci�n y Hogar", m�s espec�ficamente, se ver�n los productos respectivos como "sof�" que ya est� ordenado, respectivamente como la p�gina inicial, como "recomendado". 
##### Se es definida la variable a trabajar, como "livingHites, ante el elemento "sofa", respetando la ubicaci�n de la p�gina. 

livingHites <- read_html("https://www.hites.com/muebles/living/sofa/")

##### Luego de eso, es definida el producto objetivo, en este caso "sof�", asociado a la class de la p�gina respectiva. 

sofasH <- html_nodes(livingHites, css=".tile-section")
print(html_text(sofasH))

##### Despu�s de la definici�n de variables a introducir para su an�lisis, se definen tres variables m�s, que puedan ayudar a posicionar por separado, tanto el Nombre del producto, precio original del articulo y si este contiene una rebaja respectiva. 

SH1 <- html_nodes(sofasH, css = ".link.product-name")
print(html_text(SH1))
SH2 <- html_nodes(sofasH, css = ".price-item.list.strike-through")
print(html_text(SH2))
SH3 <- html_nodes(sofasH, css = ".price-item.sales")
print(html_text(SH3))

##### Con los tres elementos ya ingresados, dependiendo de las categorizaciones anteriores, se hace una limpieza para facilitar el uso de las secciones de mejor forma m�s adelante, es por ello, que se otorga la limpieza de este elemento de acuerdo con el Nombre del producto. 

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

##### Por consiguiente, se hace una limpieza del siguiente elemento, correspondiente al Precio Normal 

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

##### Finalmente, se limpia el �ltimo elemento de la secci�n del Precio con rebaja por oferta

SH3 <- html_nodes(sofasH, css = ".price-item.sales")
print(html_text(SH3))
Txsh3 <- html_text(SH3)
Txsh3 <- gsub("\n","",Txsh3)
Txsh3 <- gsub("Oferta","",Txsh3)
Txsh3 <- gsub("[$]","",Txsh3)
Txsh3 <- gsub("[.]","",Txsh3)
print(Txsh3)

##### Siguiendo la l�nea de las categor�as, se selecciona otra conteniendo el elemento de Mesas de centro y productos relacionados a este tipo de variable. Estos est�n ordenados categ�ricamente por "recomendado" dentro del sitio web. 
##### Se define la variable correspondiente a esto, para trabajar indicando su ubicaci�n en la p�gina web. 

livingHites2 <- read_html("https://www.hites.com/muebles/living/mesas-de-centro-arrimo-y-laterales/")

##### Se indica el elemento a trabajar correspondiente a las mesas y los elementos asociados para el estudio correspondiente a aplicar.

mesasH <- html_nodes(livingHites2, css=".tile-section")
print(html_text(mesasH))

##### Nuevamente se definen las variables a trabajar, dependiendo y separando respectivamente por Nombre del producto, precio original del producto y si este ostenta rebaja u oferta. 

MH1 <- html_nodes(mesasH, css = ".link.product-name")
print(html_text(MH1))
MH2 <- html_nodes(mesasH, css = ".price-item.list.strike-through")
print(html_text(MH2))
MH3 <- html_nodes(mesasH, css = ".price-item.sales")
print(html_text(MH3))

##### Se procede a una limpieza respectiva de los elementos anteriormente mencionados para su uso de an�lisis en profundidad m�s adelante, se da una limpieza a la preferencia, para obtener la categor�a de Nombre Producto. 

MH1 <- html_nodes(mesasH, css = ".link.product-name")
print(html_text(MH1))
Txmh1 <- html_text(MH1)
print(Txmh1)

##### Si bien, se procede a una limpieza del elemento a seleccionar se da una notoriedad que este se presenta en las condiciones adecuadas y se comprueba que no hab�a necesidad de otorgar limpieza a este tema. 
##### Se selecciona el siguiente elemento llamado Precio Normal para proceder con la limpieza respectiva de sus datos. 

MH2 <- html_nodes(mesasH, css = ".price-item.list.strike-through")
print(html_text(MH2))
Txmh2 <- html_text(MH2)
print(Txmh2)
Txmh2 <- gsub("\n","",Txmh2)
Txmh2 <- gsub("Price","",Txmh2)
Txmh2 <- gsub("reduced","",Txmh2)
Txmh2 <- gsub("from","",Txmh2)
Txmh2 <- gsub("Normalto","",Txmh2)
Txmh2 <- gsub("[$]","",Txmh2)
Txmh2 <- gsub("[.]","",Txmh2)
print(Txmh2)

##### Finalmente se le da una limpieza a para tener un elemento adecuado a Precio con rebaja por oferta 

MH3 <- html_nodes(mesasH, css = ".price-item.sales")
print(html_text(MH3))
Txmh3 <- html_text(MH3)
print(Txmh3)
Txmh3 <- gsub("\n","",Txmh3)
Txmh3 <- gsub("Oferta","",Txmh3)
Txmh3 <- gsub("[$]","",Txmh3)
Txmh3 <- gsub("[.]","",Txmh3)
print(Txmh3)

##### Ya finalizado la presentaci�n de elementos correspondientes a la tienda Hites, donde se muestran y se hace una limpieza de las variables para un m�s apto estudio de las secciones elegidas, se comienza a trabajar con la siguiente p�gina, correspondiente a Sodimac. 
##### En primera parte se da por iniciada el registro de la p�gina a trabajar, como anteriormente fue mencionado, como Sodimac.

Sodimac <-read_html("https://www.sodimac.cl/sodimac-cl/")
print(html_text(Sodimac))

##### Siguiendo la l�nea de lo estipulado con la p�gina anterior, se define el objetivo general de estudio, como "decoraci�n y hogar", seleccionando y trabajando con la secci�n de sof�, la cual esta ordenada por la variable "recomendado". 

livingSodimac <- read_html("https://www.sodimac.cl/sodimac-cl/category/cat1140004/")
#Se define la variable asociada a la class de la pagina
sofasS <- html_nodes(livingSodimac, css=".jsx-3663142191")
print(html_text(sofasS))

##### Se hace la definici�n del elemento sof� de acuerdo con Nombre del producto y Precio Original.

SS1 <- html_nodes(sofasS, css = ".jsx-411745769.product-title")
print(html_text(SS1))
SS2 <- html_nodes(sofasS, css = ".jsx-585964327.main.gridView")
print(html_text(SS2))

##### Ante la definici�n de las secciones que se quieren representar, se tiene que hacer una limpieza de las variables, para obtener informaci�n m�s clara de lo que se quiere estudiar, por ello es por lo que primero se comenzara con Nombre Producto.

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

##### Siguiendo la misma instrucci�n anterior, se da por tomada la variable respectiva del Precio normal para su limpieza para la obtenci�n de los datos correspondientes y de una manera que la informaci�n sea clara respecto a esta categorizaci�n. 

SS2 <- html_nodes(sofasS, css = ".jsx-411745769.desktop-price-cart-btn")
print(html_text(SS2))
Txss2 <- html_text(SS2)
Txss2 <- substring(Txss2, first = 3)
Txss2 <- gsub(")","",Txss2)
Txss2 <- gsub("C/UDisponible para despachoNo disponible para retiroAgregar al carro","",Txss2)
Txss2 <- gsub("[$]","",Txss2)
Txss2 <- gsub("[.]","",Txss2)
print(Txss2)

##### Cabe destacar que el elemento faltante para la limpieza es la parte de Rebaja por oferta, que no pudo ser tomada, ya que la pagina referida a Sodimac no contaba con esa secci�n para poder otorgar la revisi�n correspondiente. 
##### Se sigue la elecci�n de la misma categor�a ya mencionada en la primera p�gina, siendo el siguiente paso el an�lisis correspondiente a la secci�n de mesas de centro e insumos relacionados. Se define, primeramente, la variable a trabajar de acuerdo con su ubicaci�n del sitio web. 

livingSodimac2 <- read_html("https://www.sodimac.cl/sodimac-cl/category/scat100551/Mesas-de-Centro,-Laterales-y-Arrimos")

##### Luego se define cual es el elemento a trabajar de acuerdo con la secci�n establecida dentro de la p�gina, en este segmento se refiere a las mesas.

mesasS <- html_nodes(livingSodimac2, css=".jsx-3663142191")
print(html_text(mesasS))

##### Por consiguiente se definen las variables clasificatorias del elemento mesas, acorde al Nombre del producto y el precio original de este. 

MS1 <- html_nodes(mesasS, css = ".jsx-411745769.product-title")
print(html_text(MS1))
MS2 <- html_nodes(mesasS, css = ".jsx-585964327.main.gridView")
print(html_text(MS2))

##### Como en las funciones anteriores, se espera hacer una limpieza de los datos, para as� tener una visualizaci�n m�s clara de los elementos a trabajar y una clasificaci�n respectiva. El primer elemento a limpiar ser� Nombre Producto. 

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

##### El segundo elemento para la limpieza y se muestren datos m�s claros para el an�lisis correspondiente que se har� m�s adelante, se centra en el Precio Normal.

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

##### Ante el an�lisis de los elementos mostrados en la p�gina de Sodimac referente a Mesas, se mostr� solo un elemento de estudio a una secci�n referente y ya nombrada anteriormente como Producto en rebaja, pero al tener solo un resultado, se decide no considerarlo por la poca relevancia que este mostrar�a en el estudio. 
##### En consecuencia con la obtenci�n de datos m�s claros correspondiente a las variables que se decidieron estudiar, de acuerdo con el objetivo predeterminado, se desea crear un almacenamiento de los datos seleccionados anteriormente para su mejor comparaci�n. Es por eso por lo que los datos se dividir�n en tablas que contengan la informaci�n necesaria para su an�lisis. 
##### Se crea una data frame correspondiente al almacenamiento de la variable mesas respecto a la p�gina de Sodimac, la cual contendr� Nombre correspondiente al producto, y su precio oficial. Cabe destacar que la transformaci�n de los datos a una tabla se har� acorde a que aparezca la informaci�n de forma exacta a sus lineamientos, ya sea nombre y precios en cifras. 

NameMesaSodi <- data.frame(Txms1)
PriceMesaSodi <- data.frame(Txms2)

SODIMACMesaTabla <- cbind(NameMesaSodi,PriceMesaSodi)

SODIMACMesaTabla <- rename(SODIMACMesaTabla, "Nombre del producto" = "Txms1", "Precio normal CLP" = "Txms2")

SODIMACMesaTabla[,2] <- as.numeric(SODIMACMesaTabla[,2])
typeof(SODIMACMesaTabla[,2])

##### Se seleccionan los elementos asociados a la variable Sofas correspondientes a la p�gina web de Sodimac, para la concentraci�n de los datos en limpio, demostrando sus lineamientos correspondientes de Nombre de Productos y el precio acordado al producto. 

NameSofaSodi <- data.frame(Txss1)
PriceSofaSodi <- data.frame(Txss2)

SODIMACSofaTabla <- cbind(NameSofaSodi,PriceSofaSodi)

SODIMACSofaTabla <- rename(SODIMACSofaTabla, "Nombre del producto" = "Txss1", "Precio normal CLP" = "Txss2")

SODIMACSofaTabla[,2] <- as.numeric(SODIMACSofaTabla[,2])
typeof(SODIMACSofaTabla[,2]



## An�lisis completo de datos
##### Correspondiente al objetivo que fue nombrado al principio del informe, se har� la comparativa, respecto a los productos tanto de la p�gina Sodimac, como con la de Hites, por consiguiente, para un an�lisis m�s adecuado, se contemplar� ambas variables en formato de vectores para una m�s f�cil medici�n de los elementos antes nombrados. 

HITES <- rep("Hites",24)
print(HITES)

SODIMAC <- rep("Sodimac",28)
print(SODIMAC)

##### Se da el paso para integrar las funciones respetivas a las tablas tanto de la tienda Hites y la tienda Sodimac, correspondientes a los elementos Mesa y Sof� de ambos sitios web, no obstante, tambi�n se da una transformaci�n de sus variables para un m�s adecuado funcionamiento.

HITESMesaTabla <- cbind(HITESMesaTabla,HITES)
HITESSofaTabla <- cbind(HITESSofaTabla, HITES)

SODIMACMesaTabla <- cbind(SODIMACMesaTabla, SODIMAC)
SODIMACSofaTabla <- cbind(SODIMACSofaTabla, SODIMAC)

HITESMesaTabla <- HITESMesaTabla[,-3]
HITESSofaTabla <- HITESSofaTabla[,-3]

HITESSofaTabla <- rename(HITESSofaTabla, "Tienda" = "HITES")
HITESMesaTabla <- rename(HITESMesaTabla, "Tienda" = "HITES")

SODIMACMesaTabla <- rename(SODIMACMesaTabla, "Tienda" = "SODIMAC")
SODIMACSofaTabla <- rename(SODIMACSofaTabla, "Tienda" = "SODIMAC")


##### Finalmente, se corrigen los errores demostrados en la tabla para una igualaci�n de condiciones de trabajo y as� sea m�s f�cil la extracci�n y an�lisis de datos.

HITESMesaTabla <- rename(HITESMesaTabla, "Nombre del producto" = "Nombre producto")
HITESSofaTabla <- rename(HITESSofaTabla, "Nombre del producto" = "Nombre producto")

SILLONES <- rbind(HITESSofaTabla,SODIMACSofaTabla)
MESAS <- rbind(HITESMesaTabla,SODIMACMesaTabla)

## An�lisis estad�stico correspondiente
##### Se forma el an�lisis finalmente de los datos estad�sticos de acuerdo con Hites e Sodimac, cabe destacar que se har� una comparativa entre los productos, tanto de sof�s y Mesas, visualizando si los precios que estos imponen son m�s accesibles en sus valores, es decir, si son caros o baratos. 
### Sof�s
##### Se har� un promedio de valores de cada uno de los elementos en este caso de los sof�s (Sillones), correspondientes a ambas p�ginas. 

#### Promedio sillones de Hites 
##### De acuerdo al producto de Hites correspondiente a los sof�s (sillones), se selecciona la variable para poder tomar la totalidad de los datos y tomar un promedio de estos, para la observaci�n de sus precios finales. 
PromedioSillonHites <- mean(HITESSofaTabla[,2])
print(paste("El promedio de los sillones en Hites es de", "$",PromedioSillonHites))

##### Junto a la evaluaci�n del promedio de los sof�s, se demuestra que la media de todos los elementos a estudiar corresponde a la empresa del retail Hites es de $829.156 pesos chilenos. 

#### Promedio sillones de Sodimac 
##### Correspondiente al producto sof�s(sillones) en Sodimac, se contempla un promedio de todos los elementos asociados al producto seleccionado. 
PromediosSillonesSodimac <- mean(SODIMACSofaTabla[,2])
print(paste("El promedio de los sillones en Sodimac es de", "$",PromediosSillonesSodimac))

##### En el presente an�lisis dependiendo del promedio de la lista de sof�s (sillones) de la tienda Sodimac, se pudo recabar que su valor con respecto a la media es de $292.130 pesos chilenos. 

##### Por consiguiente se encontrar� que tienda contiene la valoraci�n m�s cara o la valoraci�n m�s barata acorde a los productos de sof�, perteneciente a cada una de las tiendas de estudio. 

SillonCaroBarato <- function(b){
  ##### Calculando el valor maximo
  Maximo <- max(SILLONES[,2])
  #Calculando el valor minimo
  Minimo <- min(SILLONES[,2])
  ##### Creando variable que almacenara el nombre de la tienda
  TiendaCara <- c()
  TiendaBarata <- c()
  ##### Creando LOOP que buscara la tienda correspondiente al precio m�s alto
  for (z in 1:nrow(SILLONES)) {
    if(SILLONES[z,2] == Maximo){
      TiendaCara <- c(TiendaCara,SILLONES[z,3])
    }
  }
## Conclusi�n
#####