## Scripts in support of NSF CoPe application

# Setup -------------------------------------------------------------------
rm(list=ls()) # Clears workspace

#system("sudo apt install make g++ gfortran libgeos-dev libproj-dev libgdal-dev libudunits2-dev libcurl4-openssl-dev -y") # Install linux geospatial dependencies 

# Install/call libraries
install.packages("renv")
#renv::init()

PKG <- c("googledrive","tidyr","purrr", "sf", "tmap", "raster", "rgdal", "exactextractr","ggplot2","gargle")

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
