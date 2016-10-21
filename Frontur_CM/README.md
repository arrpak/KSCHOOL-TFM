# VISUALIZACION DE DATOS FRONTUR 

Cuadro de mandos para visualizar los datos de frontur.
A traves del framework shiny para R y la librería para pintar datos en mapas leaflet, se desarrolla 
este cuadro de mandos. 

COMO USARLO
 - Ejecutar el fichero CM.R.

DATOS.
 - Los datos se encuentran dentro de la ruta Frontur_CM/DATA. Los datos que se visualizan por defecto 
 son los del año 2015. Si se desea cargar datos de otro año sería bajarse de frontur los datos y guardalos 
 en este fichero con el nombre de Frontur
 

OPTIMIZACIÓN EN EL RENDIMIENTO. 
Se han sacado del programa principal el tratamiento y formateo del fichero de carga para así optimizar 
los tiempos de carga a la hora de visualizar. Entonces en caso de querer cargar nuevos datos, a parte 
de cargar los datos en la tabla data, es necesario ir a la carpeta scripts y ejecutar el programa 
Script_tratar_data.R, que se encargará de leer el fichero principal y crear un fichero ya mapeado y que 
guardará en la misma carpeta de data con el nombre de Frontur_CM.

