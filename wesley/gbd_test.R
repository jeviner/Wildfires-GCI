require(rgdal)

# The input file geodatabase
fgdb <- "C:/Users/Owner/Downloads/S_USA.EVMid_R05_NorCoastWest.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="EVMid_R05_NorCoastWest")
fc2 <- readOGR(dsn=fgdb,layer="OpenFileGDB")

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)