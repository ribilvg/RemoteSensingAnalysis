
# Loading Libraries -----------------------------------------------------------------
library(sp)
library(terra)
library(rgdal)
library(raster)
library(randomForest)

# Defining Directory Paths ----------------------------------------------------------
dirs = list()
dirs$raw <- "../Input/Raw_data/"
dirs$processed <- "../Input/Processed_data/"
dirs$output <- "../Output/"

# Loading Data ----------------------------------------------------------------------
# Stacked Raster Image
stack_raster <- rast(file.path(dirs$processed,
                                 "Col_2019_20_stacked_clip.tif"))

# Polygon layer from QGIS for training Data
training_data <- readOGR(file.path(dirs$processed, "training.shp"))


# Data Preparation ------------------------------------------------------------------
# set seed for uniform random results
set.seed(1)
# generate point samples from the polygons
ptsamp <- spsample(training_data, 1000, type='random')
# add the land cover class to the points
ptsamp$Notes <- over(ptsamp, training_data)$Notes
# We convert `ptsamp` to `SpatVector`
ptsamp <- vect(ptsamp)
# We use the x-y coordinates to extract the spectral values for the locations
xy <- as.matrix(geom(ptsamp)[,c('x','y')])
df <- extract(stack_raster, xy)
# combine lulc class information with extracted values
sampdata <- data.frame(Notes = ptsamp$Notes, df)


# Modelling -------------------------------------------------------------------------
# Training the Model
rf_model <- randomForest(x=sampdata[, 2:ncol(sampdata)], 
                         y=as.factor(sampdata$Notes),
                         ntree=500, 
                         importance=TRUE)

# Plot mean decrease in accuracy variable importance
varImpPlot(rf_model, type=1)

# Prediction
classified <- predict(stack_raster, 
                      rf_model, 
                      type="response", 
                      index=1, 
                      na.rm=TRUE, 
                      progress="window", 
                      overwrite=TRUE)

classified

# Plotting
plot(classified,
     col = c('green', "black", 'red', 'blue'))

levels(as.factor(sampdata$Notes))
# Levels
# 1 = "NonVeg-Veg"
# 2 = "Unclassified"
# 3 = "Veg-NonVeg"
# 4 = "No Change"(Veg - Veg)

classes <- c("NonVeg-Veg", "Unclassified", "Veg-NonVeg", "No Change")

# Model Evaluation ------------------------------------------------------------------
set.seed(99)
# number of folds
k <- 5
j <- sample(rep(1:k, each = round(nrow(sampdata))/k))

table(j)

x <- list()
for (k in 1:5) {
  train <- sampdata[j!= k, ]
  test <- sampdata[j == k, ]
  rf <- randomForest(x=sampdata[, 2:ncol(sampdata)], 
                       y=as.factor(sampdata$Notes),
                       ntree=500, 
                       importance=TRUE)
  pclass <- predict(rf, test, na.rm = TRUE)
  x[[k]] <- cbind(test$Notes, as.integer(pclass))
}



y <- do.call(rbind, x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
# confusion matrix
conf_matrix <- table(y)
# change the name of the classes
colnames(conf_matrix) <- classes
rownames(conf_matrix) <- classes
print(conf_matrix)

# Total Observations
total <- sum(conf_matrix)
# number of correctly classified cases per class
correct_pred <- diag(conf_matrix)
# Overall Accuracy
Accuracy <- sum(correct_pred) / total

# Exports ---------------------------------------------------------------------------

writeRaster(classified, 
            file.path(dirs$output, "Images/Change_detection.tif"), 
            overwrite=TRUE)

