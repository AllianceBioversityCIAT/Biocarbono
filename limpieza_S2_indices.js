// ============================================================================================================================================================ //
// Fuente:
// - GEE: https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_S2_CLOUD_PROBABILITY
// Editado: Jorge Andres Perez Escobar
// Contacto: j.perez@cgiar.org
// Objetivo: Limpieza de nubes a traves de banda de colección de probabilidad de nubes y GCLM de Sentine-1
// - NOTAS:
//  * Línea 38 y 39: selecionar el municipio a procesar (observar líneas 12 - 16)
//  * Línea 190 y 191 : seleccinar las listas de acuerdo con el municipio que va a procesar (listas en líneas 169 - 187)
// ============================================================================================================================================================ //

var LaMacarena = ee.FeatureCollection('users/terraiciat/Biocarbono/la_macarena')
var LaPrimavera = ee.FeatureCollection('users/terraiciat/Biocarbono/la_primavera')
var PazdeAriporo = ee.FeatureCollection('users/terraiciat/Biocarbono/paz_de_ariporo')
var PuertoLopez = ee.FeatureCollection('users/terraiciat/Biocarbono/puerto_lopez')
var Arauca = ee.FeatureCollection('users/terraiciat/Biocarbono/arauca')

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Llamado de colección de imágenes
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Colección de imágenes Sentinel-2 Nivel 2A
var s2Sr = ee.ImageCollection('COPERNICUS/S2_SR');
// Colección de imágenes de probabilidad de nubes
var s2Clouds = ee.ImageCollection('COPERNICUS/S2_CLOUD_PROBABILITY');

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Parámetros de entrada
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Fecha de inicio de la colección de imágenes
var START_DATE = ee.Date('2020-01-01');
// Fecha de fin de la colección de imágenes
var END_DATE = ee.Date('2020-12-31');

// Máxima probabilidad de nubes
// Escriba el municipio a procesar Arauca, La Macarena, La primavera
// Paz de Ariporo, Puerto Lopez
var MAX_CLOUD_PROBABILITY = 20; 
var roi = Arauca;
var roi_string = 'Arauca';
Map.centerObject(roi,8);

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Limpieza por banda SCL
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
function SCLFilter(image){
  var scl = image.select('SCL'); 
  var NoDefective = scl.neq(1);
  var imagen = image.updateMask(NoDefective);
  var Dark_Area_Pixels = scl.neq(2);
  imagen = imagen.updateMask(Dark_Area_Pixels);
  var Cloud_Shadows = scl.neq(3);
  imagen = imagen.updateMask(Cloud_Shadows);
  var Clouds_Low_Probability = scl.neq(7);
  imagen = imagen.updateMask(Clouds_Low_Probability);
  var Clouds_Medium_Probability = scl.neq(8);
  imagen = imagen.updateMask(Clouds_Medium_Probability);
  var Clouds_high_Probability = scl.neq(9);
  imagen = imagen.updateMask(Clouds_high_Probability);
  var Cirrus = scl.neq(10);
  imagen = imagen.updateMask(Cirrus);
  return imagen;}

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Función para enmascarar nubes a través la colección s2Clouds
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
function maskClouds(img) {
  var clouds = ee.Image(img.get('cloud_mask')).select('probability');
  var isNotCloud = clouds.lt(MAX_CLOUD_PROBABILITY);
  return img.updateMask(isNotCloud)}

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Función de enmascarado de bordes de nubes
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Las máscaras para las bandas de 10 m a veces no excluyen los datos incorrectos en los bordes
// de la escena, por lo que también aplicamos máscaras de las bandas de 20 my 60 m
function maskEdges(s2_img) {
  return s2_img.updateMask(
      s2_img.select('B8A').mask().updateMask(s2_img.select('B9').mask()));}

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Filtro de las colecciones de entrada por región y rango de datos deseados
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Criterios de filtro por fecha y región de interés.
var criteria = ee.Filter.and(
    ee.Filter.bounds(roi), ee.Filter.date(START_DATE, END_DATE));
// Filtrado de colección S2_SR por criterios de filtro y aplicación de la función maskEdges
s2Sr = s2Sr.filter(criteria).map(maskEdges);
// Filtrado de la colección S2_CLOUD_PROBABILITY por criterios de filtro
s2Clouds = s2Clouds.filter(criteria);

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Fusión de colección S2_SR con colección de probabilidad de nube para añadir a la función de
// enmascarado de nubes
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
var s2SrWithCloudMask = ee.Join.saveFirst('cloud_mask').apply({
  primary: s2Sr,
  secondary: s2Clouds,
  condition:
      ee.Filter.equals({leftField: 'system:index', rightField: 'system:index'})})

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Escalado de bandas espectrales
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
function escalado(image){
  var imagen = image.multiply(0.0001)
  return imagen}

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Función NDVI
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
function NDVI (image){
  var ndvi_function = image.expression("(B8 - B4) / (B8 + B4)", {
    B8: image.select("B8"),
    B4: image.select("B4")
  }).rename("NDVI");
  return image.addBands(ndvi_function)}

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// TASSELED CAP SENTINEL2
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
function TC (image){
  var brillo = ((ee.Image(0.0822).multiply(image.select("B2"))).add(ee.Image(0.1360).multiply(image.select("B3")))
                                 .add(ee.Image(0.2611).multiply(image.select("B4"))).add(ee.Image(0.3895).multiply(image.select("B8")))
                                 .add(ee.Image(0.3882).multiply(image.select("B11"))).add(ee.Image(0.1366).multiply(image.select("B12"))))
                                 .rename("Brillo");
  var verdor = ((ee.Image(-0.1128).multiply(image.select("B2"))).add(ee.Image(-0.1680).multiply(image.select("B3")))
                                  .add(ee.Image(-0.3480).multiply(image.select("B4"))).add(ee.Image(0.3165).multiply(image.select("B8")))
                                  .add(ee.Image(-0.4578).multiply(image.select("B11"))).add(ee.Image(-0.4064).multiply(image.select("B12"))))
                                  .rename("Verdor");
  var humedad = ((ee.Image(0.1363).multiply(image.select("B2"))).add(ee.Image(0.2802).multiply(image.select("B3")))
                                  .add(ee.Image(0.3072).multiply(image.select("B4"))).add(ee.Image(-0.0807).multiply(image.select("B8")))
                                  .add(ee.Image(-0.4064).multiply(image.select("B11"))).add(ee.Image(-0.5602).multiply(image.select("B12"))))
                                  .rename("Humedad")
  return image.addBands(brillo).addBands(verdor).addBands(humedad)}

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Aplicación de enmascarado de nubes
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Enmascarado de nubes, selección de bandas y cálculo del NDVI
var s2CloudMasked =
    ee.ImageCollection(s2SrWithCloudMask).map(maskClouds).map(SCLFilter)
    .select('B2','B3','B4','B8','B11','B12').map(escalado).map(NDVI).map(TC);
print("Colección filtrada",s2CloudMasked)
//Map.addLayer(s2CloudMasked.median().clip(roi), {} ,"sin normalizar")

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Reducción de la colección a la media y desviación estandar y normalización
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
var media_sin_escalar = (s2CloudMasked.reduce(ee.Reducer.mean())).reduceRegion({
  reducer: ee.Reducer.mean(),
  geometry: roi,
  scale: 10,
  bestEffort: true,
  tileScale: 2
});
print("media_sin_escalar",media_sin_escalar);
  
var stdDev_sin_escalar = (s2CloudMasked.reduce(ee.Reducer.stdDev())).reduceRegion({
  reducer:ee.Reducer.mean(),
  geometry: roi,
  scale: 10,
  bestEffort: true,
  tileScale: 2});
print("stdDev_sin_escalar",stdDev_sin_escalar);




var list_mean_LaPrimavera = [0.054495659864286006, 0.0767186925608137, 0.08117650561461623, 0.21907441232134547, 0.2715287687074941, 0.1724687505313119,
                             0.45344547280583686, 0.25040465397908307, -0.17234539376730565, -0.17078383210484646];
var list_stdDev_LaPrimavera = [0.01784087964258244, 0.016967401913001258, 0.020886220214862476, 0.03463223423095687, 0.042630232111401954, 0.04003784521011827,
                               0.10271107782333225, 0.03597267222607455, 0.042285156927790585, 0.030120989816899753];
var list_mean_LaMacarena = [0.04719591155982795, 0.06968986126837538, 0.05154804370110776, 0.299317215469735, 0.20621903002656047, 0.10530814016101724,
                            0.7066178861320025, 0.23783989410084444, -0.07744071613644687, -0.12516047231141955];
var list_stdDev_LaMacarena = [0.019500671117409822, 0.01920066009048521, 0.01900816972017611, 0.05108167120000748, 0.03773117106931169, 0.02429789535277558,
                              0.07589976597952347, 0.04044949981160544, 0.027128595266847803, 0.027268215318347108];
var list_mean_PazDeAriporo = [0.056614847386029364,0.08001465787605773,0.08078388559140114,0.24136527518808562,0.2690673492934881,0.170969939255078,
                              0.4964134787809343,0.25844661983792755,-0.16421051571692366,-0.16965098794198213];
var list_stdDev_PazDeAriporo = [0.020360580713967846,0.0192488703855632,0.027178044094341578,0.04136888160727375,0.0642881923130713,0.0555403076857571,
                                0.12260754629084578,0.04928754389767053,0.06184996322448091,0.044934228152509956];
var list_mean_PuertoLopez = [0.05665213135384243,0.08159659308140112,0.07933772376080843,0.2643102542424535,0.2669640242081025,0.16033009574005352,
                             0.5364206077277829,0.2649543908334178,-0.15142820164664025,-0.16468333696485898];
var list_stdDev_PuertoLopez = [0.018664428371511,0.018431943822718535,0.023870876797207487,0.04257598904158034,0.048259709937014485,0.040307986989942875,
                               0.10387451296397818,0.04064214504549553,0.04619304050194809,0.03381123149245584];
var list_mean_Arauca = [0.05926072221842559,0.08315346111543893,0.08321580294010349,0.25927933590953917,0.278189299137733,0.1738371324497958,
                        0.5166551564657276,0.2706362877803924,-0.16555405231427273,-0.17442280426973344];
var list_stdDev_Arauca = [0.023350523726944246,0.021812297735230086,0.03370664497960523,0.049017028009829974,0.07794416983335206,0.06599097116205334,
                          0.1488552993089101,0.057630871059275945,0.07712343432820808,0.05360887281053252];

var list_mean = list_mean_Arauca; //Cambiar lista de acuerdo con el municipio que va a procesar
var list_stdDev = list_stdDev_Arauca; //Cambiar lista de acuerdo con el municipio que va a procesar

var normalize = function(image, mean_var, stdDev_var){
  var image2 = image;
  image = image.expression("((image-m)/(3*sd)+1)/2", {
    'image': image2,
    'm': mean_var,
    'sd': stdDev_var,
  }).set('system:time_start', image2.get('system:time_start'));
  image = (image.multiply(ee.Image(image.gte(0)))).set('system:time_start', image2.get('system:time_start'));
  image = (image.multiply(ee.Image(image.lt(1))).add(ee.Image(image.gte(1)))).set('system:time_start', image2.get('system:time_start'));
  return image.float();};

var normalizeS2 = function(image){
  return normalize(image,list_mean,list_stdDev);};

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Reducción de la colección a la media y desviación estandar y normalización
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
s2CloudMasked = s2CloudMasked.map(normalizeS2)
var mean_img = s2CloudMasked.reduce(ee.Reducer.mean())
var median_img = s2CloudMasked.reduce(ee.Reducer.median())
var max_img = s2CloudMasked.reduce(ee.Reducer.max())
var min_img = s2CloudMasked.reduce(ee.Reducer.min())
var stdDev_img = s2CloudMasked.reduce(ee.Reducer.stdDev())

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Unión de imágenes para realizar una composición 
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
var imagen = ee.Image([mean_img, median_img, max_img, min_img, stdDev_img]).float()
               .clip(roi)
print("imagen",imagen)

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Agregar imágen corregida y centrado del mapa de acuerdo a la región de interes 
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Impresión de la imágen corregida sobre el mapa
Map.addLayer(imagen, {}, 'Imagen corregida');

// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
// Exportar imagen
// ----------------------------------------------------------------------------------------------------------------------------------------------------------- //
Export.image.toDrive({
  image: imagen,
  description: roi_string + "_2020_normalizada" ,
  region:roi,
  folder: "biocarbono",
  scale: 10,
  maxPixels: 10000000000000
})

