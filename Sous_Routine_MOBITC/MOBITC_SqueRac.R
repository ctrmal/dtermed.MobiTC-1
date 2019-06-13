
#chem_mobitc="C:\\R\\R-3.3.2\\Cerema\\MOBITC"
#chemin_rep_travail="C:\\R\\R-3.3.2\\Cerema\\MOBITC\\Rivages"
#fichier_sque="20180502T124233--Env-C010-T0250-Sque-C001v0_1_2"
#distrac=3000


MOBITC_SqueRac<-function(chem_mobitc,chemin_rep_travail,fichier_sque,distrac)
{
  if(!require(rgdal)){install.packages("rgdal")}
  library(rgdal)
  if(!require(rgeos)){install.packages("rgeos")}
  library(rgeos)

  dsnlayer=chemin_rep_travail
  nomlayersque=fichier_sque
  print(nomlayersque)
  print(dsnlayer)
  Squelette0 = readOGR(dsnlayer,nomlayersque)
  
  #verif pour voir si les bouts sont déjà bien fusionnés
  Sque1=gLineMerge(Squelette0)
  print(length(Sque1))
  Sque1=disaggregate(Sque1)
  print(length(Sque1))
  Squelette1 = SpatialLinesDataFrame(Sque1, data=data.frame(sapply(slot(Sque1, 'lines'), function(x) slot(x, 'ID'))))
  #à virer par la suite
  writeOGR(Squelette1, dsn=dsnlayer, layer=paste(fichier_sque,"_2",sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  isque=1
 
  while (length(Squelette1)>isque)
  { 
    dist=data.frame()
    for (k in 1:length(Squelette1))
    {
      #dist[k,]=cbind(k,999999,999999)
      if (k==isque)
      {
        
      } else {
        Squelette1@lines[[k]]@Lines[[1]]@coords[,]
        length(Squelette1@lines[[k]]@Lines[[1]]@coords[,2])
        dist_temp=data.frame()
        eucdist <- function(v1, v2,i,j) sqrt((abs(v1[i,1] - v2[j,1]))^2+(abs(v1[i,2]-v2[j,2]))^2)
        v1=Squelette1@lines[[k]]@Lines[[1]]@coords
        v2=Squelette1@lines[[isque]]@Lines[[1]]@coords
        dist_temp=rbind(eucdist(v1,v2,1,1),eucdist(v1,v2,1,length(v2[,1])),eucdist(v1,v2,length(v1[,1]),1),eucdist(v1,v2,length(v1[,1]),length(v2[,1])))
      }
    }
  }
  
print("essai")
sortie="calcul fini sque rac"

return(sortie)
}