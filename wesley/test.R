library(rgdal)

path <- "E:/SynologyDrive/Documents/School/Chapman/Spring 2021/SCI 200/Ecological Dataset Research/ds515/ds515.gdb"
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(path)
print(fc_list)

d <- readOGR(dsn=path,layer="ds515")
summary(d)
head(d)

plot(d)

d2 <- readOGR(dsn=path,layer="OpenFileGDB")
