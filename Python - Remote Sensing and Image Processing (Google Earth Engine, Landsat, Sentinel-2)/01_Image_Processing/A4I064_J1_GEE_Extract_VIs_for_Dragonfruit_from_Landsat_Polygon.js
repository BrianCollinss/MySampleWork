// This script extracts the band values from Landsat and calculates the 14 vegetation indices for the field polygons for dragon fruit using Google Earth Engine. It reads "dragonpolygon" as the assets in which "dragonpolygon" is uploaded as the field polygon shape file. It creates two csv files as the output in the Google Drive which has been linked to GEE:1) "VIs_Landsat_Dragonfruit_Polygons_MinMaxMean.csv" which includes the minimum, maximum and average values of the VIs within the polygon and 2) "VIs_Landsat_Dragonfruit_Polygons.csv" which includes the average values of VIs within the polygon. The second file is the one that is used for further analysis. Both the files also report the centroid latitudes and longitudes.   

// Load crop region
var crop_region = ee.FeatureCollection("users/brcollins2020/DragonfruitPolygon");

// Center the map
Map.centerObject(crop_region, 7.5);
Map.addLayer(crop_region, {}, "Rice Region");

// Clean crop region
var crop_region_clean = crop_region;

// --- Cloud mask for Landsat 8/9 ---
function maskL8sr(image) {
  var qaPixel = image.select('QA_PIXEL');
  var cloudShadowBitMask = 1 << 3;
  var cloudsBitMask = 1 << 5;
  var mask = qaPixel.bitwiseAnd(cloudShadowBitMask).eq(0)
                    .and(qaPixel.bitwiseAnd(cloudsBitMask).eq(0));
  return image.updateMask(mask)
              .copyProperties(image, ['system:time_start']);
}

// --- Load and scale Landsat 8/9 SR data ---
var image = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
  .merge(ee.ImageCollection('LANDSAT/LC09/C02/T1_L2'))
  .filterDate('2024-01-01', '2025-05-31')
  .filterBounds(crop_region)
  .filter(ee.Filter.lt('CLOUD_COVER', 20))
  .map(maskL8sr)
  .map(function(img) {
    return img.select(['SR_B2','SR_B3','SR_B4','SR_B5','SR_B6'])
              .multiply(0.0000275)
              .add(-0.2)
              .rename(['B2','B3','B4','B5','B6'])
              .copyProperties(img, ['system:time_start']);
  });

// Vegetation index functions (using Landsat bands)
var addNDVI = function(image) {
  return image.addBands(image.normalizedDifference(['B5', 'B4']).rename('NDVI'));
};

var addEVI = function(image) {
  return image.addBands(image.expression(
    '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
    {
      'NIR': image.select('B5'),
      'RED': image.select('B4'),
      'BLUE': image.select('B2')
    }).rename('EVI'));
};

var addSAVI = function(image) {
  var L = 0.5;
  var nir = image.select('B5');
  var red = image.select('B4');
  return image.addBands(nir.subtract(red).multiply(1 + L)
    .divide(nir.add(red).add(L)).rename('SAVI'));
};

var addGNDVI = function(image) {
  return image.addBands(image.normalizedDifference(['B5', 'B3']).rename('GNDVI'));
};

var addNDWI = function(image) {
  return image.addBands(image.normalizedDifference(['B3', 'B5']).rename('NDWI'));
};

var addCIG = function(image) {
  return image.addBands(image.select('B5').divide(image.select('B3')).subtract(1).rename('CIG'));
};

var addDVI = function(image) {
  return image.addBands(image.select('B5').subtract(image.select('B4')).rename('DVI'));
};

var addTVI = function(image) {
  var ndvi = image.select('NDVI');
  return image.addBands(ndvi.add(0.5).max(0).sqrt().rename('TVI'));
};

var addRVI = function(image) {
  return image.addBands(image.select('B5').divide(image.select('B4')).rename('RVI'));
};

var addMSAVI = function(image) {
  var nir = image.select('B5');
  var red = image.select('B4');
  var part1 = nir.multiply(2).add(1);
  var part2 = part1.multiply(part1).subtract(nir.subtract(red).multiply(8)).sqrt();
  return image.addBands(part1.subtract(part2).divide(2).rename('MSAVI'));
};

var addPRI = function(image) {
  var green = image.select('B3');
  var red = image.select('B4');
  var denominator = green.add(red);
  return image.addBands(green.subtract(red).divide(denominator).rename('PRI'));
};

var addWDRVI = function(image) {
  var nir = image.select('B5');
  var red = image.select('B4');
  return image.addBands(nir.multiply(0.1).subtract(red)
    .divide(nir.multiply(0.1).add(red)).rename('WDRVI'));
};

var addVARI = function(image) {
  var green = image.select('B3');
  var red = image.select('B4');
  var blue = image.select('B2');
  return image.addBands(green.subtract(red).divide(green.add(red).subtract(blue)).rename('VARI'));
};

var addARI = function(image) {
  var green = image.select('B3');
  var red = image.select('B4');
  return image.addBands(ee.Image(1).divide(green).subtract(ee.Image(1).divide(red)).rename('ARI'));
};

// Apply all index functions
var image_addband = image
  .map(addNDVI)
  .map(addEVI)
  .map(addSAVI)
  .map(addGNDVI)
  .map(addNDWI)
  .map(addCIG)
  .map(addDVI)
  .map(addTVI)
  .map(addRVI)
  .map(addMSAVI)
  .map(addPRI)
  .map(addWDRVI)
  .map(addVARI)
  .map(addARI);


// --- Selected bands ---
var selectedBands = ['ARI','CIG','DVI','EVI','GNDVI','MSAVI','NDVI','NDWI','PRI','RVI','SAVI','TVI','VARI','WDRVI'];

// --- Sampling at polygons ---
var sampled_polygons = image_addband.map(function(img){
  var sampled = img.select(selectedBands).reduceRegions({
    collection: crop_region_clean,
    reducer: ee.Reducer.mean().combine({reducer2: ee.Reducer.min(), sharedInputs:true}).combine({reducer2: ee.Reducer.max(), sharedInputs:true}),
    scale:30
  });
  
  return sampled.map(function(f){
    return f.set({'Plot_ID': f.get('Name'),'system:time_start': img.get('system:time_start')});
  });
});

// Flatten
var flattened = sampled_polygons.flatten();

// --- MEAN ONLY CLEAN WITH CENTROIDS + DATE ---
var meanOnlyClean = flattened.map(function(f){
  var centroid = f.geometry().centroid();
  var coords = centroid.coordinates();
  
  var props = {
    'Plot_ID': f.get('Plot_ID'),
    'Date': ee.Date(f.get('system:time_start')).format('dd/MM/YYYY'),  // <-- formatted date
    'longitude': coords.get(0),   // <-- renamed for clarity
    'latitude': coords.get(1)     // <-- renamed for clarity
  };
  
  selectedBands.forEach(function(b){
    props[b] = f.get(b + '_mean');
  });
  
  return ee.Feature(centroid, props);  // place feature at centroid
});

// --- MIN–MAX–MEAN CLEAN WITH CENTROIDS + DATE ---
var MinMaxMeanClean = flattened.map(function(f){
  var centroid = f.geometry().centroid();
  var coords = centroid.coordinates();
  
  var props = {
    'Plot_ID': f.get('Plot_ID'),
    'Date': ee.Date(f.get('system:time_start')).format('dd/MM/YYYY'),
    'longitude': coords.get(0),
    'latitude': coords.get(1)
  };
  
  selectedBands.forEach(function(b){
    props['Mean' + b] = f.get(b + '_mean');
    props['Min'  + b] = f.get(b + '_min');
    props['Max'  + b] = f.get(b + '_max');
  });
  
  return ee.Feature(centroid, props);
});

// --- EXPORT CSVs ---
Export.table.toDrive({collection: meanOnlyClean, description:'VIs_Landsat_Dragonfruit_Polygons', fileFormat:'CSV', folder: 'GEE_VIs'});
Export.table.toDrive({collection: MinMaxMeanClean, description:'VIs_Landsat_Dragonfruit_Polygons_MinMaxMean', fileFormat:'CSV', folder: 'GEE_VIs'});
