# 
# chem_mobitc = "C:\\R\\R-3.5.1\\Cerema\\MOBITC"
# 
# chemin_rep_travail = "C:\\R\\R-3.5.1\\Cerema\\MOBITC\\TARAVO4"
# 
# fichier_env="20190218T131915-nvx-Env-C010-T0250"
# distcoupsque=10
# 
MOBITC_Sque<-function(chem_mobitc,chemin_rep_travail,fichier_env,distcoupsque)
{
  if(!require(rgdal)){install.packages("rgdal")}
  library(rgdal)
  if(!require(deldir)){install.packages("deldir")}
  library(deldir)
  if(!require(rgeos)){install.packages("rgeos")}
  library(rgeos)
  if(!require(stplanr)){install.packages("stplanr")}
  library(stplanr)
    
  dsnlayer=chemin_rep_travail
  nomlayerenv=fichier_env
  print(nomlayerenv)
  print(dsnlayer)
  Enveloppe = readOGR(dsnlayer,nomlayerenv)
  
  sauve=0
  pascoup=distcoupsque
  
  nom_exp_sque=paste(fichier_env,"-Sque-C",as.character(formatC(pascoup, width = 3, format = "d", flag = "0")),sep="")
  nomlayersque=nom_exp_sque
  
  #ecriture dans le fichier parametre
  fichier_init=paste(chem_mobitc,"/Init_Routine_MobiTC.txt",sep="")
  fid=file(fichier_init, open = "r+")
  lignes=readLines(fid)
  lignes[8]=nom_exp_sque
  cat(lignes,file=fid,sep="\n")
  close(fid)
  
  #########################################
  rayon=cbind(1,0.9,0.8,0.7,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65)
  npointC=16
  isque=0
  
  for (ienv in 1 : length(Enveloppe)){
    print(paste("Enveloppe n°", as.character(ienv)))
    crds=Enveloppe@polygons[[ienv]]@Polygons[[1]]@coords
    
    #couper l'enveloppe: 1/5 de la résolution donne qqch de bien souvent
    print(paste("coupure Enveloppe n°", as.character(ienv)))
    crdscoup=crds[1,]
    for (itour in 2:dim(crds)[1]){
      print(itour)
      dista=((crds[itour,1]-crds[itour-1,1])^2+(crds[itour,2]-crds[itour-1,2])^2)^0.5
      ncoup=round((dista+0.49*pascoup)/pascoup,0)
      if (ncoup>0)
      {
        sequ=seq(1/(ncoup), 1, length.out = ncoup) 
        crdscoup=rbind(crdscoup,cbind(crds[itour-1,1]+sequ*(crds[itour,1]-crds[itour-1,1]),crds[itour-1,2]+sequ*(crds[itour,2]-crds[itour-1,2])))
      }
    }
    
    # Calculate tessellation and triangulation
    print(paste("Calcul Tesselation Enveloppe n°", as.character(ienv)))
    # Pts=data.frame(x=crdscoup[,1]+runif(dim(crdscoup)[1])*pascoup/10000,y=crdscoup[,2]+runif(dim(crdscoup)[1])*pascoup/10000)
    # vtess=deldir(Pts[1:100,],plot=TRUE)
    # vtess=deldir(Pts,plot=TRUE)
     vtess <- deldir(crdscoup[,1]+runif(dim(crds)[1])*pascoup/10000,crdscoup[,2]+runif(dim(crds)[1])*pascoup/10000)
    
    # Récupération des segments du Voronoi
    print(paste("Récupération Segment Enveloppe n°", as.character(ienv)))
    ligne = vector(mode='list', length=length(vtess$dirsg$x1))
    
    for (i in seq(along=ligne)) {
      pcrds=rbind(cbind(vtess$dirsgs$x1[i],vtess$dirsgs$y1[i]),cbind(vtess$dirsgs$x2[i],vtess$dirsgs$y2[i]))
      ligne[[i]]=Lines(list(Line(pcrds)), ID=as.character(i))
    }
    
    print(paste("Suppression segment hors Enveloppe n", as.character(ienv)))
    SP = SpatialLines(ligne)
    proj4string(SP)=Enveloppe@proj4string
    
    # Travail par enveloppe
    envi=SpatialPolygons (Enveloppe@polygons[ienv])
    proj4string(envi)=Enveloppe@proj4string      
    
    # récupération des segments contenus dans l'enveloppe
    nb=which(gContains(envi,SP,byid=TRUE)==TRUE)
    SP=SP[nb]
    
    # boucle pour supprimer les extrémités par des cercles
    rempli=1
    tour=0
    while (rempli==1) {
      tour=tour+1
      print(paste("Tour de nettoyage n°",as.character(tour)))
      #nettoyer cercle sur extrémité...
      #fusion des lignes  
      print(length(SP))
      if (length(SP)>0){
        SP=gLineMerge(SP)
        #désagrégation
        print(length(SP))
        SP=disaggregate(SP)
        print(length(SP))
        
        if (sauve!=0) {
          voronoiline = SpatialLinesDataFrame(SP, data=data.frame(sapply(slot(SP, 'lines'), function(x) slot(x, 'ID'))))
          print(paste("Export Squelette n° ", as.character(ienv),"tour n°",as.character(tour)))
          writeOGR(voronoiline, dsn=dsnlayer, layer=paste("SqueletteR_",as.character(ienv),"_",as.character(tour),sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE)
        }
      }
      # Bug s'il n'y a qu'un segment!
      rempli=0
      inc=0
      asupp=NULL
      if (length(SP)>1) {
        reseau=SpatialLinesNetwork(SP,tolerance=1)
        
        # Création des mini-cercles
        for (ires in 1:length(reseau@nb))
        {
          # Récupération des axes finaux
          if (length(reseau@nb[[ires]])==1){
            inc=inc+1
            
            ligne=SP[reseau@nb[[ires]]]@lines[[1]]@Lines[[1]]@coords
            Xdeb=ligne[1,1]
            Ydeb=ligne[1,2]
            Xfin=ligne[dim(ligne)[1],1]
            Yfin=ligne[dim(ligne)[1],2]
            dist=((Xfin-Xdeb)^2+(Yfin-Ydeb)^2)^0.5
            coefB=dist/2*rayon[tour]
            crds=cbind((Xdeb+Xfin)/2 + coefB*cos(seq(0,2*pi,length.out = npointC)),(Ydeb+Yfin)/2 + coefB*sin(seq(0,2*pi,length.out = npointC)))
            
            # Création d'un polygones
            Pa <- Polygon (crds)
            Psa <- Polygons (list ( Pa ), inc)
            pola <- SpatialPolygons (list ( Psa ))
            proj4string(pola)=Enveloppe@proj4string 
            tableau <- data.frame(IDENT = inc, row.names = inc)
            
            SPDFa <- SpatialPolygonsDataFrame(pola, tableau)
            
            if (inc==1)
            {
              SPDFb=SPDFa
              SPDF=SPDFa
            }
            if (inc>1)
            {
              SPDF=rbind(SPDFb,SPDFa)
              SPDFb=SPDF
            }
            
            nb=which(gContains(envi,pola,byid=TRUE)==TRUE)
            if (length(nb)>0)
            {
              if (nb==1)
              {
                if (rempli==0){
                  asupp=reseau@nb[[ires]]
                  rempli=1
                }else{
                  asupp=cbind(asupp,reseau@nb[[ires]])
                }
              }
            }
          }
          
        }
        
      }else{
        rempli=0
      }
      
      print("On ne garde que les bons")
      if (length(asupp)>0)
      {SP=SP[-asupp]}
      
      if (sauve!=0) {
        writeOGR(SPDF, dsn=dsnlayer, layer=paste("SqueletteR_",as.character(ienv),"_",as.character(tour),'_Cercle',sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE)
        plot(SP)
      }
    }
    
    #fusion des lignes  
    print(length(SP))
    SP=gLineMerge(SP)
    #désagrégation
    print(length(SP))
    SP=disaggregate(SP)
    print(length(SP))
    
    # Vérification que les squelettes ne soient pas trop petits
    boite=SP@bbox[,2]-SP@bbox[,1]
    if ((boite[1]^2+boite[2]^2)^0.5>pascoup)
    {
      isque=isque+1
      voronoiline = SpatialLinesDataFrame(SP, data=data.frame(sapply(slot(SP, 'lines'), function(x) slot(x, 'ID'))))
      voronoiline@data=cbind(voronoiline@data,ienv)
      colnames(voronoiline@data)=cbind("IDENT","Enveloppe")
      
      
      print(paste("Export Squelette n° ", as.character(ienv),"tour n°",as.character(tour)))
      writeOGR(voronoiline, dsn=dsnlayer, layer=paste("SqueletteR_",as.character(ienv),"_",as.character(tour),sep=""), driver="ESRI Shapefile",overwrite_layer=TRUE)
      
      
      if (isque==1)
      {
        SLDFb=SP
        SLDF=SP
      }
      if (isque>1)
      {
        row.names(SP)=as.character(as.numeric(row.names(SP))+length(SLDF))
        SLDF=rbind(SLDFb,SP)
        SLDFb=SLDF
      }
    }
  }
  print("Export Squelette fINAL")
  SLDF=disaggregate(gLineMerge(SLDF))
  SLDFF = SpatialLinesDataFrame(SLDF, data=data.frame(sapply(slot(SLDF, 'lines'), function(x) slot(x, 'ID'))))
  ###############
  names(SLDFF)[1] <- "NAxe"
  writeOGR(SLDFF, dsn=dsnlayer, layer=nomlayersque, driver="ESRI Shapefile",overwrite_layer=TRUE)
  ####################
  texte="calcul fini"
  
  return(list(texte,Enveloppe,SLDFF))
}