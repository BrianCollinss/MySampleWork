// This script extracts the band values from Sentinel-2 and calculates the 14 vegetation indices for the field polygons for dragon fruit using Google Earth Engine. It reads "dragonpolygon" as the assets in which "dragonpolygon" is uploaded as the field polygon shape file. It creates two csv files as the output in the Google Drive which has been linked to GEE:1) "VIs_Sentinel2_Dragonfruit_Polygons_MinMaxMean.csv" which includes the minimum, maximum and average values of the VIs within the polygon and 2) "VIs_Sentinel2_Dragonfruit_Polygons.csv" which includes the average values of VIs within the polygon. The second file is the one that is used for further analysis. Both the files also report the centroid latitudes and longitudes.

var crop_region = ee.FeatureCollection("users/brcollins2020/DragonfruitPolygon");

//Center the map
Map.centerObject(crop_region, 7.5); //7.5 is a zoom level
Map.addLayer(crop_region, {}, "My Crop Region");
print(crop_region.limit(5));


// The input region is read from a single file
var crop_region_clean = crop_region

// Function to mask clouds using SCL band (Sentinel-2 SR)
function maskS2clouds(image) {
  var scl = image.select('SCL');

  // Keep pixels that are not cloud or shadow
  var mask = scl.neq(3) // cloud shadows
              .and(scl.neq(8)) // medium probability clouds
              .and(scl.neq(9)) // high probability clouds
              .and(scl.neq(10)) // thin cirrus
              .and(scl.neq(11)); // snow or ice

  return image.updateMask(mask)
            .divide(10000)
            .copyProperties(image, ['system:time_start']);
}

// Map the function over one year of data and take the median
// Load Sentinel-2 reflectance data
var image = ee.ImageCollection('COPERNICUS/S2_SR')
                  .filterDate('2024-01-01', '2025-05-31')
                  .filterBounds(crop_region)
                  // Pre-filter to get less cloudy granules.
                  .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
                  .map(maskS2clouds)
                  .select(['B2', 'B3', 'B4', 'B8']);

// Function to add an VI band
var addNDVI = function(image) {
  return image
    .addBands(image.normalizedDifference(['B8', 'B3'])
    .rename('NDVI'))
    .copyProperties(image, ['system:time_start']);
    //.float();
};

// Add EVI using an expression.
var addEVI=function(image){
var EVI = image.expression(
      '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))', {
      'NIR' : image.select('B8'),
      'RED' : image.select('B4'),
      'BLUE': image.select('B2')}).rename('EVI')
      .copyProperties(image, ['system:time_start']);
      return image.addBands(EVI);
};

// Function to compute SAVI
var addSAVI = function(image) {
  var L = 0.5;  // Soil brightness correction factor
  var nir = image.select('B8');   // NIR (Band 8)
  var red = image.select('B4');   // Red (Band 4)

  var numerator = nir.subtract(red).multiply(1 + L);
  var denominator = nir.add(red).add(L);

  var savi = numerator.divide(denominator)
                      .rename('SAVI')
                      .copyProperties(image, ['system:time_start']);

  return image.addBands(savi);
};

// Function to compute GNDVI
var addGNDVI = function(image) {
  var nir = image.select('B8');    // NIR (Band 8 for Sentinel-2)
  var green = image.select('B3');  // Green (Band 3)

  var numerator = nir.subtract(green);
  var denominator = nir.add(green);

  var gndvi = numerator.divide(denominator)
                       .rename('GNDVI')
                       .copyProperties(image, ['system:time_start']);

  return image.addBands(gndvi);
};

// Function to compute NDWI (McFeeters)
var addNDWI = function(image) {
  var green = image.select('B3');  // Green (Band 3 for Sentinel-2)
  var nir = image.select('B8');    // NIR (Band 8)

  var numerator = green.subtract(nir);
  var denominator = green.add(nir);

  var ndwi = numerator.divide(denominator)
                      .rename('NDWI')
                      .copyProperties(image, ['system:time_start']);

  return image.addBands(ndwi);
};

// Function to compute CIG (Chlorophyll Index - Green)
var addCIG = function(image) {
  var nir = image.select('B8');    // NIR (Band 8 for Sentinel-2)
  var green = image.select('B3');  // Green (Band 3)

  var cig = nir.divide(green)
               .subtract(1)
               .rename('CIG')
               .copyProperties(image, ['system:time_start']);

  return image.addBands(cig);
};

// Function to compute DVI (Difference Vegetation Index)
var addDVI = function(image) {
  var nir = image.select('B8');   // NIR band (Band 8)
  var red = image.select('B4');   // Red band (Band 4)

  var dvi = nir.subtract(red)
               .rename('DVI')
               .copyProperties(image, ['system:time_start']);

  return image.addBands(dvi);
};

// Function to compute TVI (Transformed Vegetation Index)
var addTVI = function(image) {
  var ndvi = image.select('NDVI');
  
  // TVI = sqrt(max(NDVI + 0.5, 0))
  var tvi = ndvi.add(0.5).max(0).sqrt()
                .rename('TVI')
                .copyProperties(image, ['system:time_start']);
  
  return image.addBands(tvi);
};

// Function to compute RVI (Ratio Vegetation Index)
var addRVI = function(image) {
  var nir = image.select('B8');   // NIR band (Band 8)
  var red = image.select('B4');   // Red band (Band 4)

  var rvi = nir.divide(red)
               .rename('RVI')
               .copyProperties(image, ['system:time_start']);

  return image.addBands(rvi);
};

// Function to compute MSAVI (Modified Soil-Adjusted Vegetation Index)
var addMSAVI = function(image) {
  var nir = image.select('B8');   // NIR band
  var red = image.select('B4');   // Red band

  var twoNIRPlusOne = nir.multiply(2).add(1);
  var inner = twoNIRPlusOne.multiply(twoNIRPlusOne)
               .subtract(nir.subtract(red).multiply(8));

  var msavi = twoNIRPlusOne
                .subtract(inner.sqrt())
                .divide(2)
                .rename('MSAVI')
                .copyProperties(image, ['system:time_start']);

  return image.addBands(msavi);
};

// Function to compute PRI (Photochemical Reflectance Index)
var addPRI = function(image) {
  var green = image.select('B3');  // Green band
  var red = image.select('B4');    // Red band

  var pri_numerator = green.subtract(red);
  var pri_denominator = green.add(red);
  var pri_mask = pri_denominator.neq(0);

  var pri = ee.Image(0)
               .where(pri_mask, pri_numerator.divide(pri_denominator))
               .rename('PRI')
               .copyProperties(image, ['system:time_start']);

  return image.addBands(pri);
};

// Function to compute WDRVI (Wide Dynamic Range Vegetation Index)
var addWDRVI = function(image) {
  var nir = image.select('B8');  // NIR band
  var red = image.select('B4');  // Red band

  var wdrvi_numerator = nir.multiply(0.1).subtract(red);
  var wdrvi_denominator = nir.multiply(0.1).add(red);
  var wdrvi_mask = wdrvi_denominator.neq(0);

  var wdrvi = ee.Image(0)
                .where(wdrvi_mask, wdrvi_numerator.divide(wdrvi_denominator))
                .rename('WDRVI')
                .copyProperties(image, ['system:time_start']);

  return image.addBands(wdrvi);
};

// Function to compute VARI (Visible Atmospherically Resistant Index)
var addVARI = function(image) {
  var green = image.select('B3');  // Green band
  var red = image.select('B4');    // Red band
  var blue = image.select('B2');   // Blue band

  var vari_numerator = green.subtract(red);
  var vari_denominator = green.add(red).subtract(blue);
  var vari_mask = vari_denominator.neq(0);

  var vari = ee.Image(0)
                .where(vari_mask, vari_numerator.divide(vari_denominator))
                .rename('VARI')
                .copyProperties(image, ['system:time_start']);

  return image.addBands(vari);
};

// Function to compute ARI (Anthocyanin Reflectance Index)
var addARI = function(image) {
  var green = image.select('B3');  // Green band
  var red = image.select('B4');    // Red band

  var ari_mask = green.neq(0).and(red.neq(0));
  var one = ee.Image.constant(1);

  var green_inv = one.divide(green);
  var red_inv = one.divide(red);

  var ari = ee.Image(0)
               .where(ari_mask, green_inv.subtract(red_inv))
               .rename('ARI')
               .copyProperties(image, ['system:time_start']);

  return image.addBands(ari);
};

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

var selectedBands = [
  'ARI', 'CIG', 'DVI', 'EVI', 'GNDVI', 'MSAVI', 'NDVI', 'NDWI',
  'PRI', 'RVI', 'SAVI', 'TVI', 'VARI', 'WDRVI'
];

// Function to extract mean, min, max for each polygon
var sampled_polygons = image_addband.map(function(img) {
  var reduced = img.select(selectedBands)
    .reduceRegions({
      collection: crop_region,
      reducer: ee.Reducer.mean()
                 .combine({reducer2: ee.Reducer.min(), sharedInputs: true})
                 .combine({reducer2: ee.Reducer.max(), sharedInputs: true}),
      scale: 10,
    });

  // Attach time property and Plot_ID from "Name"
  return reduced.map(function(f) {
    return f.set({
      'system:time_start': img.get('system:time_start'),
      'Plot_ID': f.get('Name')  // <-- take "Name" from shapefile
    });
  });
});

// Flatten the results into one collection
var flattened = sampled_polygons.flatten();

// --- MEAN ONLY CLEAN --- //
var meanOnlyClean = flattened.map(function(feat) {
  var centroid = feat.geometry().centroid();  // compute centroid
  var coords = centroid.coordinates();

  var props = {
    'Plot_ID': feat.get('Plot_ID'),
    'Date': ee.Date(feat.get('system:time_start')).format('dd/MM/YYYY'),  // <-- format date
    'longitude': coords.get(0),  // X
    'latitude': coords.get(1)    // Y
  };
  
  selectedBands.forEach(function(band) {
    props[band] = feat.get(band + '_mean');   // just keep mean
  });
  
  return ee.Feature(centroid, props);  // feature located at centroid
});

// --- MIN–MAX–MEAN CLEAN --- //
var MinMaxMeanClean = flattened.map(function(feat) {
  var centroid = feat.geometry().centroid();  
  var coords = centroid.coordinates();

  var props = {
    'Plot_ID': feat.get('Plot_ID'),
    'Date': ee.Date(feat.get('system:time_start')).format('dd/MM/YYYY'),
    'longitude': coords.get(0),
    'latitude': coords.get(1)
  };
  
  selectedBands.forEach(function(band) {
    props['Mean' + band] = feat.get(band + '_mean');
    props['Min'  + band] = feat.get(band + '_min');
    props['Max'  + band] = feat.get(band + '_max');
  });
  
  return ee.Feature(centroid, props);
});

// --- EXPORT CSVs --- //
Export.table.toDrive({collection: meanOnlyClean, description: 'VIs_Sentinel2_Dragonfruit_Polygons', fileFormat: 'CSV', folder: 'GEE_VIs'});
Export.table.toDrive({collection: MinMaxMeanClean, description: 'VIs_Sentinel2_Dragonfruit_Polygons_MinMaxMean', fileFormat: 'CSV', folder: 'GEE_VIs'});