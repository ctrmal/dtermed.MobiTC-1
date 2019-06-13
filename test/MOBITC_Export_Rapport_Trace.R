
chemin_rep_travail="C:\\0_ENCOURS\\TPM\\Erosion\\MobiTC_rivages"
dirr=R.home()
chem_mobitc=paste(dirr,"/Cerema/MOBITC",sep="")

fichier_intersectionv1="20190517T152421-TPM-Sque-cont-Tra-P50-L0100sel-lisse-filtre3-mod-IntersTDC-v1.txt"
fichier_sque="20190517T152421-TPM-Sque-cont"
fichier_trace="20190517T152421-TPM-Sque-cont-Tra-P50-L0100sel-lisse-filtre3-mod"

MOBITC_Export_Rapport_Trace<-function(chem_mobitc,chemin_rep_travail,fichier_sque,fichier_trace,fichier_intersectionv1)
{
  if(!require(rmarkdown)){install.packages("rmarkdown")}
  library(rmarkdown)
  if(!require(flexdashboard)){install.packages("flexdashboard")}
  library(flexdashboard)
  
  # lecture des param?tres
  fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
  fid=file(fichier_init, open = "r+")
  lignes=readLines(fid)
  produc=lignes[14]
  ICtx=lignes[16]
  datedebgraph=lignes[18]
  datefingraph=lignes[20]
  dateprosp=lignes[22]
  close(fid)
  
  #lecture du fichier intersection V1 normalement (limites retenue)
  chem_intersectionv1=paste(chemin_rep_travail,"\\",fichier_intersectionv1,sep="")
  #lecture de toutes les intersections
  fichier_intersectionv0=paste(substr(fichier_intersectionv1,1,nchar(fichier_intersectionv1)-7),"-v0.txt",sep="")
  chem_intersectionv0=paste(chemin_rep_travail,"\\",fichier_intersectionv0,sep="")
  tab00=read.table(chem_intersectionv0,sep="\t",header=TRUE,row.names = NULL)
  tab11=read.table(chem_intersectionv1,sep="\t",header=TRUE,row.names = NULL)
  #modif des dates dans le tableau de d?part
  source(paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Convertdate_2.R",sep=""))	
  tab=MOBITC_Convertdate_2(tab00) #ttes
  #pour AOR
  tab1=MOBITC_Convertdate_2(tab11) #que les retenues
  
  nomdirrapport=paste(chemin_rep_travail,"\\Rapport",sep="")
  if (file.exists(nomdirrapport)=="FALSE"){dir.create(nomdirrapport)}
  
  for (iaxe in 1 : length(unique(tab$NAxe)))
  {
    itemp=which(tab$NAxe ==unique(tab$NAxe)[iaxe])
    #calcul du nombre de trace
    NbTrace=length(unique(tab$NTrace[itemp]))
    for (itr in 1:NbTrace)
    {
      #extrait des valeurs du sque et de la trace
      NAxe=unique(tab$NAxe)[iaxe]
      NTrace=unique(tab$NTrace[itemp])[itr]
      
      extraittab1=tab1[which(tab1$NAxe == NAxe & tab1$NTrace == NTrace),]
      
      if (length(extraittab1$Distance)>1)
      {
        chem_rmd=paste(chem_mobitc,"/Sous_Routine_MOBITC/Rivages_report-6.Rmd",sep="")
        chem_rapport=paste0(chemin_rep_travail,"/Rapport/Rapport-MobiTC-Naxe",NAxe, "-Ntrace", NTrace, ".html")
        rmarkdown::render(chem_rmd, params = list(chemin_rep_travail = chemin_rep_travail,chem_mobitc = chem_mobitc,fichier_sque=fichier_sque,fichier_intersectionv1=fichier_intersectionv1,fichier_trace=fichier_trace,iaxe=iaxe,itr=itr),output_file = chem_rapport )
      }
    }
  }
  textexportrapport="Export fini"
  
  return(list(textexportrapport))
}

      