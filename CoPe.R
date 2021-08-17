## Scripts in support of NSF CoPe application

# Setup -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

#system("sudo apt install make g++ gfortran libgeos-dev libproj-dev libgdal-dev libudunits2-dev libcurl4-openssl-dev -y") # Install linux geospatial dependencies 

# Install/call libraries
install.packages("renv")
#renv::init()

PKG <- c("googledrive","tidyr","purrr", "sf", "tmap", "raster", "rgdal", "exactextractr","ggplot2","gargle", "s2","dplyr")

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}

renv::snapshot()
rm(p,PKG)


# Data download
dir.create(file.path('Data'), recursive = TRUE)
folder_url<-"https://drive.google.com/open?id=15amwG3br43cpU9MS8gghNcYhljHoi52I"
folder<-drive_get(as_id(folder_url))
files<-drive_ls(folder)
dl<-function(files){
  walk(files, ~ drive_download(as_id(.x), overwrite = TRUE))
}
setwd("./Data")
system.time(map(files$id,dl))
system.time(unzip("Data.zip", exdir = "."))
file.remove("Data.zip")
setwd("..")
rm(files, folder, folder_url, dl)

#ESI
ESI<-st_read("Data/ESILRIMA.gpkg")
ESI<-ESI[,c("ESI","geom")]
ESI$length<-st_length(ESI)
ESI<-separate(ESI,ESI,c("I1","I2","I3"),"/",convert = TRUE)
ESI$x1a<-ifelse(ESI$I1 == "1A",ESI$length, ifelse(ESI$I2 == "1A",ESI$length, ifelse(ESI$I3 == "1A",ESI$length,0))) # Create variables for all classes and populate them with relevant length
ESI$x1b<-ifelse(ESI$I1 == "1B",ESI$length, ifelse(ESI$I2 == "1B",ESI$length, ifelse(ESI$I3 == "1B",ESI$length,0)))
ESI$x2a<-ifelse(ESI$I1 == "2A",ESI$length, ifelse(ESI$I2 == "2A",ESI$length, ifelse(ESI$I3 == "2A",ESI$length,0)))
ESI$x3a<-ifelse(ESI$I1 == "3A",ESI$length, ifelse(ESI$I2 == "3A",ESI$length, ifelse(ESI$I3 == "3A",ESI$length,0)))
ESI$x3b<-ifelse(ESI$I1 == "3B",ESI$length, ifelse(ESI$I2 == "3B",ESI$length, ifelse(ESI$I3 == "3B",ESI$length,0)))
ESI$x4<-ifelse(ESI$I1 == "4",ESI$length, ifelse(ESI$I2 == "4", 1,ifelse(ESI$I3 == "4",ESI$length,0)))
ESI$x5<-ifelse(ESI$I1 == "5",ESI$length, ifelse(ESI$I2 == "5", 1,ifelse(ESI$I3 == "5",ESI$length,0)))
ESI$x6a<-ifelse(ESI$I1 == "6A",ESI$length, ifelse(ESI$I2 == "6A",ESI$length, ifelse(ESI$I3 == "6A",ESI$length,0)))
ESI$x7<-ifelse(ESI$I1 == "7",ESI$length, ifelse(ESI$I2 == "7", 1,ifelse(ESI$I3 == "7",ESI$length,0)))
ESI$x6b<-ifelse(ESI$I1 == "6B",ESI$length, ifelse(ESI$I2 == "6B",ESI$length, ifelse(ESI$I3 == "6B",ESI$length,0)))
ESI$x8a<-ifelse(ESI$I1 == "8A",ESI$length, ifelse(ESI$I2 == "8A",ESI$length, ifelse(ESI$I3 == "8A",ESI$length,0)))
ESI$x8b<-ifelse(ESI$I1 == "8B",ESI$length, ifelse(ESI$I2 == "8B",ESI$length, ifelse(ESI$I3 == "8B",ESI$length,0)))
ESI$x8c<-ifelse(ESI$I1 == "8C",ESI$length, ifelse(ESI$I2 == "8C",ESI$length, ifelse(ESI$I3 == "8C",ESI$length,0)))
ESI$x9a<-ifelse(ESI$I1 == "9A",ESI$length, ifelse(ESI$I2 == "9A",ESI$length, ifelse(ESI$I3 == "9A",ESI$length,0)))
ESI$x9b<-ifelse(ESI$I1 == "9B",ESI$length, ifelse(ESI$I2 == "9B",ESI$length, ifelse(ESI$I3 == "9B",ESI$length,0)))
ESI$x10a<-ifelse(ESI$I1 == "10A",ESI$length, ifelse(ESI$I2 == "10A",ESI$length, ifelse(ESI$I3 == "10A",ESI$length,0)))
ESI$xU<-ifelse(ESI$I1 == "U",ESI$length, ifelse(ESI$I2 == "U",ESI$length, ifelse(ESI$I3 == "U",ESI$length,0)))
ESI[is.na(ESI)]<-0 # Replace NAs with zeros

ESI$RockyShore<-ESI$x1a + ESI$x2a + ESI$x6a +ESI$x8a
ESI$SandyBeach<-ESI$x3a + ESI$x3b + ESI$x4 + ESI$x5
ESI$Marshes<-ESI$x9b + ESI$x10a
ESI$Armored<-ESI$x1b + ESI$x6b + ESI$x8b + ESI$x8c
ESI$TidalFlat<-ESI$x7 + ESI$x9a

ESI<-ESI[,c("RockyShore","SandyBeach","Marshes","Armored","TidalFlat","geom")]

muni<-st_read("Data/MAmunicipalities.gpkg")
muni<-st_transform(muni, st_crs(ESI))
muni<-muni[,c("TOWN","geom")]
muni<-slice(muni,-c(17,30,38,111,191)) # "Evaluation error: Found 5 features with invalid spherical geometry."
#muni<-muni %>% dplyr::filter(TOWN %in% c("BOSTON","GLOUCESTER","NEW BEDFORD","NANTUCKET"))

ESI<-st_join(muni,ESI)

ESI$TOWN<-as.factor(ESI$TOWN)

ESIsum<-ESI %>% 
  group_by(TOWN) %>% 
  summarise(sum(RockyShore),sum(SandyBeach),sum(Marshes),sum(Armored),sum(TidalFlat))

ESIsum$total<-ESIsum$`sum(RockyShore)`+ESIsum$`sum(SandyBeach)`+ESIsum$`sum(Marshes)`+ESIsum$`sum(Armored)`+ESIsum$`sum(TidalFlat)`

ESIp<-ESIsum

ESIp$RockyShore<-round((ESIp$`sum(RockyShore)`/ESIp$total)*100,1)
ESIp$SandyBeach<-round((ESIp$`sum(SandyBeach)`/ESIp$total)*100,1)
ESIp$Marshes<-round((ESIp$`sum(Marshes)`/ESIp$total)*100,1)
ESIp$Armored<-round((ESIp$`sum(Armored)`/ESIp$total)*100,1)
ESIp$TidalFlat<-round((ESIp$`sum(TidalFlat)`/ESIp$total)*100,1)

ESIp$totalkm<-round(ESIp$total/1000,1)

ESIp$geom<-NULL
ESIp<-as.data.frame(ESIp)

write.csv(ESIp,"ESIp.csv")
