if(!require(shiny)){install.packages("shiny")}
library(shiny)
if(!require(ggmap)){install.packages("ggmap")}
library(ggmap)
if(!require(leaflet)){install.packages("leaflet")}
library(leaflet)
if(!require(flexdashboard)){install.packages("flexdashboard")}
library(flexdashboard)

shinyServer(function(input, output) {
	dirr=R.home()
	chem_mobitc=paste(dirr,"/Cerema/MOBITC",sep="")

	observeEvent(input$pack,
		{
	  chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_InstallationPack.R",sep="")
		source(chemsource)
	  MOBITC_InstallationPack()
		output$textini<-renderText("Installation terminée")
		})
	
	observeEvent(input$rep,
		{
	  chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_CreationFichierInit.R",sep="")
		source(chemsource)
	  fichier_init=MOBITC_CreationFichierInit(chem_mobitc,isolate(input$chemin_rep_travail),isolate(input$nom_projet))
		output$textrep<-renderText(paste("Le fichier ",fichier_init," a été crée ou mis a jour. Merci de relancer MobiTC pour prendre en compte ces nouvelles entrées.",sep=""))
		})
	
	output$actu_menu_env  <- renderUI({
	  actionButton("actu_menu_env_but", "Actualiser le menu")
	})
	
	observeEvent(input$voirF,
	 {
	  output$tabF<-renderTable({
	  fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	  fid=file(fichier_init, open = "r")
	  lignes <- readLines(fid)
	  close(fid)
	  chemin_rep_travail=lignes[2]
	  chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Fournisseur.R",sep="")
	  source(chemsource)
	  sortieF=MOBITC_Fournisseur(chemin_rep_travail)
	  sortieF[[1]]
	  })

	})
	
	observeEvent(input$rajoutF,
	             {
	               output$tabF<-renderTable({
	                 fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                 fid=file(fichier_init, open = "r")
	                 lignes <- readLines(fid)
	                 close(fid)
	                 chemin_rep_travail=lignes[2]
	                 chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_RajoutFournisseur.R",sep="")
	                 source(chemsource)
	                 sortieF2=MOBITC_RajoutFournisseur(chemin_rep_travail,isolate(input$NCF),isolate(input$NLF))
	                 sortieF2[[1]]
	               })
	               
	             })
	               
	observeEvent(input$voirL,
	             {
	               output$tabL<-renderTable({
	                 fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                 fid=file(fichier_init, open = "r")
	                 lignes <- readLines(fid)
	                 close(fid)
	                 chemin_rep_travail=lignes[2]
	                 chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Limite.R",sep="")
	                 source(chemsource)
	                 sortieL=MOBITC_Limite(chemin_rep_travail)
	                 sortieL[[1]]
	               })
	               
	             })
	
	observeEvent(input$actu_menu_env_but,
	             {
	               output$actu_fich_travail_env <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",14)}
	                 }
	                 textInput(inputId ="chemin_rep_travail_env","Chemin du répertoire de travail",value = lignes[2])})
	             output$actu_nom_projet_env <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",14)}
	                 }
	                 textInput(inputId ="nom_projet","Nom du projet",value = lignes[4])})
	             })
  
	observeEvent(input$env,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Enveloppe.R",sep="")
		source(chemsource)	
		sortieenv=MOBITC_Enveloppe(chem_mobitc,isolate(input$chemin_rep_travail_env),isolate(input$fichier_tdc_env$name),isolate(input$distcoup),isolate(input$disttri),isolate(input$nom_projet),isolate(input$export_tri))
		output$textenv<-renderText(sortieenv[[1]])
		output$mapenv<-renderPlot(
		  {
		    plot(sortieenv[[2]],col="red")
		    box()
		    axis(1)
		    axis(2)
		  })
		})
	
	output$actu_menu_sque  <- renderUI({
	  actionButton("actu_menu_sque_but", "Actualiser le menu")
	})
	
	observeEvent(input$actu_menu_sque_but,
	 {
	  output$actu_fich_travail_sque <- renderUI({
	    {
	      fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	      if (file.exists(fichier_init)==TRUE) {
	        fid=file(fichier_init, open = "r")
	        lignes <- readLines(fid)
	        close(fid)} else {lignes=rep("",10)}
	    }
	    textInput(inputId ="chemin_rep_travail_sque","Chemin du répertoire de travail",value = lignes[2])})
	  
	  output$actu_fich_env_sque <- renderUI({
	    {
	      fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	      if (file.exists(fichier_init)==TRUE) {
	        fid=file(fichier_init, open = "r")
	        lignes <- readLines(fid)
	        close(fid)} else {lignes=rep("",10)}
	    }
	    textInput(inputId="fichier_env_sque", "Nom du fichier de l'enveloppe (sans l'extension)", value = lignes[6])})
	 })
	
	observeEvent(input$sque,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Sque.R",sep="")
		source(chemsource)		
		sortiesque=MOBITC_Sque(chem_mobitc,isolate(input$chemin_rep_travail_sque),isolate(input$fichier_env_sque),isolate(input$distcoupsque))
		output$textsque<-renderText(sortiesque[[1]])
		output$mapsque<-renderPlot(
		  {
		    maxXY <- pmax(bbox(sortiesque[[2]])[, 2], bbox(sortiesque[[3]])[, 2])
		    minXY <- pmin(bbox(sortiesque[[2]])[, 1], bbox(sortiesque[[3]])[, 1])
		    plot(sortiesque[[2]],col="red",xlim = c(minXY[1], maxXY[1]), ylim = c(minXY[2], maxXY[2]))
		    plot(sortiesque[[3]],add=TRUE)
		    box()
		    axis(1)
		    axis(2)
		  })
		})
	
	observeEvent(input$squerac,
		{
		print(input$fichier_sque_rac$name)
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_SqueRac.R",sep="")
		source(chemsource)		
		finsquerac=MOBITC_SqueRac(chem_mobitc,isolate(input$fichier_sque_rac),isolate(input$distrac))
		#output$textsquerac<-renderText(input$fichier_sque_rac)
		})
	
	observeEvent(input$squeorr,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_SqueOrr.R",sep="")
		source(chemsource)		
		finsqueorr=MOBITC_SqueOrr(chem_mobitc,isolate(input$fichier_sque2),isolate(input$axesens))
		output$textsqueorr<-renderText(finsqueorr)
		})
	
	observeEvent(input$squelisse,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_SqueLisse.R",sep="")
		source(chemsource)		
		finsquelisse=MOBITC_SqueLisse(chem_mobitc)
		output$textsquelisse<-renderText(finsquelisse)
		})
	
	output$actu_menu_trace  <- renderUI({
	  actionButton("actu_menu_trace_but", "Actualiser le menu")
	})
	
	observeEvent(input$actu_menu_trace_but,
	             {
	               output$actu_fich_travail_tr <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",14)}
	                 }
	                 textInput(inputId ="chemin_rep_travail_tr","Chemin du répertoire de travail",value = lignes[2])})
	               
	               output$actu_fich_sque_tr <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",14)}
	                 }
	                 textInput(inputId="fichier_sque_tr", "Nom du fichier de la ligne de base (sans l'extension)", value = lignes[8])})
	 })
	
	observeEvent(input$trace,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Trace.R",sep="")
		source(chemsource)		
		sortietrace=MOBITC_Trace(chem_mobitc,isolate(input$chemin_rep_travail_tr),isolate(input$fichier_sque_tr),isolate(input$disttrace),isolate(input$longtrace))
		output$texttrace<-renderText(sortietrace[[1]])
		# output$maptrace<-renderPlot(
		#   {
		#     maxXY <- pmax(bbox(sortietrace[[2]])[, 2], bbox(sortietrace[[3]])[, 2])
		#     minXY <- pmin(bbox(sortietrace[[2]])[, 1], bbox(sortietrace[[3]])[, 1])
		#     plot(sortietrace[[2]],col="red",xlim = c(minXY[1], maxXY[1]), ylim = c(minXY[2], maxXY[2]))
		#     plot(sortietrace[[3]],add=TRUE)
		#     for (j in 1:length(sortietrace[[3]]))
		#     {
		#     points(sortietrace[[3]]@lines[[j]]@Lines[[1]]@coords[1,1],sortietrace[[3]]@lines[[j]]@Lines[[1]]@coords[1,2],pch=19,col="green")
		#     points(sortietrace[[3]]@lines[[j]]@Lines[[1]]@coords[2,1],sortietrace[[3]]@lines[[j]]@Lines[[1]]@coords[2,2],pch=19,col="red")  
		#     }
		#   title("Traces")
		#   box()
		#   axis(1)
		#   axis(2)
		#   })
		
		output$maptrace<-renderLeaflet(
			{
			  l=leaflet()%>%addPolylines(data=sortietrace[[3]],color = "#5F04B4",group = "Trace")%>%addCircles(data=sortietrace[[4]],color = "#00FF00",group = "Sens_Trace")%>%addCircles(data=sortietrace[[5]],color = "#FF0000",group = "Sens_Trace")%>%addPolylines(data=sortietrace[[2]],color = "#FF0000",group="Ligne de base")%>%addMeasure(primaryLengthUnit = "meters")
				esri <- grep("^Esri", providers, value = TRUE)
				esri=esri[c(5,2,4,9)]
				for (provider in esri) {
				  l <- l %>% addProviderTiles(provider, group = provider)
				}
				l %>%
				  addLayersControl(baseGroups = names(esri),overlayGroups = c("Ligne de base","Trace","Sens_Trace"),
								   options = layersControlOptions(collapsed = FALSE)) %>%
				  addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
							 position = "bottomleft") %>%
				  htmlwidgets::onRender("
										function(el, x) {
										var myMap = this;
										myMap.on('baselayerchange',
										function (e) {
										myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
										})
										}")								
			  })
		})
	
	output$actu_menu_trace_lisse  <- renderUI({
	  actionButton("actu_menu_trace_lisse_but", "Actualiser le menu")
	})
	
	observeEvent(input$actu_menu_trace_lisse_but,
	             {
	               output$actu_fich_travail_tr_lisse <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",14)}
	                 }
	                 textInput(inputId ="chemin_rep_travail_tr_lisse","Chemin du répertoire de travail",value = lignes[2])})
	               
				   output$actu_fich_trace_tr_lisse <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",14)}
	                 }
	                 textInput(inputId="fichier_trace_tr_lisse", "Nom du fichier des traces (sans l'extension)", value = lignes[10])})
	 })
	
	observeEvent(input$tracelisse,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_TraceLisse.R",sep="")
		source(chemsource)		
		sortietracelisse=MOBITC_TraceLisse(chem_mobitc,isolate(input$chemin_rep_travail_tr_lisse),isolate(input$fichier_trace_tr_lisse),isolate(input$gauss))
		output$texttracelisse<-renderText(sortietracelisse[[1]])
		output$maptracelisse<-renderLeaflet(
		  {
		  l=leaflet()%>%addPolylines(data=sortietracelisse[[2]],color = "#5F04B4",group = "Trace")%>%addCircles(data=sortietracelisse[[3]],color = "#00FF00",group = "Sens_Trace")%>%addCircles(data=sortietracelisse[[4]],color = "#FF0000",group = "Sens_Trace")%>%addMeasure(primaryLengthUnit = "meters")
		    esri <- grep("^Esri", providers, value = TRUE)
			esri=esri[c(5,2,4,9)]
			for (provider in esri) {
			  l <- l %>% addProviderTiles(provider, group = provider)
			}
			l %>%
			  addLayersControl(baseGroups = names(esri),overlayGroups = c("Trace","Sens_Trace"),
							   options = layersControlOptions(collapsed = FALSE)) %>%
			  addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
						 position = "bottomleft") %>%
			  htmlwidgets::onRender("
									function(el, x) {
									var myMap = this;
									myMap.on('baselayerchange',
									function (e) {
									myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
									})
									}")								
		  })
		})
	
	output$actu_menu_interponc  <- renderUI({
	  actionButton("actu_menu_interponc_but", "Actualiser le menu")
	})
	
	observeEvent(input$actu_menu_interponc_but,
	             {
	               output$actu_fich_travail_ip <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",10)}
	                 }
	                 textInput(inputId ="chemin_rep_travail_ip","Chemin du répertoire de travail",value = lignes[2])})
	               
	               output$actu_fich_env_ip <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",10)}
	                 }
	                 textInput(inputId="fichier_env_ip", "Nom du fichier de l'enveloppe (sans l'extension)", value = lignes[6])})
	               
	               output$actu_fich_sque_ip <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",10)}
	                 }
	                 textInput(inputId="fichier_sque_ip", "Nom du fichier du squelette (sans l'extension)", value = lignes[8])})
	               
	               output$actu_fich_trace_ip <- renderUI({
	                 {
	                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
	                   if (file.exists(fichier_init)==TRUE) {
	                     fid=file(fichier_init, open = "r")
	                     lignes <- readLines(fid)
	                     close(fid)} else {lignes=rep("",10)}
	                 }
	                 textInput(inputId="fichier_trace_ip", "Nom du fichier des traces (sans l'extension)", value = lignes[10])})
	             })
	
	observeEvent(input$interponc,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_IntersectionPonc_2.R",sep="")
		source(chemsource)		
		sortieip=MOBITC_IntersectionPonc_2(chem_mobitc,isolate(input$chemin_rep_travail_ip),isolate(input$fichier_env_ip),isolate(input$fichier_sque_ip),isolate(input$fichier_trace_ip),isolate(input$fichier_tdc_ip$name),isolate(input$methode_ip),isolate(input$methode_ip_lim))
		output$textip<-renderText(sortieip[[1]])
		output$tableip <- renderTable({sortieip[[2]]})
		})
 
observeEvent(input$intersurf,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_IntersectionSurf.R",sep="")
		source(chemsource)		
		finintersurf=MOBITC_IntersectionSurf(chem_mobitc,isolate(input$fichier_tracef2),isolate(input$fichier_tdc3))
		output$textintersurf<-renderText(finintersurf)
		}
	)

output$actu_menu_evol  <- renderUI({
  actionButton("actu_menu_evol_but", "Actualiser le menu")
})

observeEvent(input$actu_menu_evol_but,
             {
               output$actu_fich_travail_evol <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",10)}
                 }
                 textInput(inputId ="chemin_rep_travail_evol","Chemin du répertoire de travail",value = lignes[2])})
				 
				output$actu_fich_trace_evol <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",10)}
                 }
                 textInput(inputId="fichier_trace_evol", "Nom du fichier des traces", value = lignes[10])}) 
               
               output$actu_fich_intersection_evol <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",10)}
                 }
                 textInput(inputId="fichier_intersection_evol", "Nom du fichier des intersections", value = lignes[12])})
             })

observeEvent(input$evol,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Evolution.R",sep="")
		source(chemsource)		
		sortieevol=MOBITC_Evolution(chem_mobitc,isolate(input$chemin_rep_travail_evol),isolate(input$fichier_intersection_evol),isolate(input$produc),isolate(input$IC),isolate(input$datedebgraph),isolate(input$datefingraph),isolate(input$dateprosp))
		output$textevol<-renderText(sortieevol[[1]])
		output$tableevol <- renderTable(sortieevol[[2]])	
    })

output$actu_menu_graph  <- renderUI({
  actionButton("actu_menu_graph_but", "Actualiser le menu")
})

observeEvent(input$actu_menu_graph_but,
             {
               output$actu_fich_travail_graph <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",14)}
                 }
                 textInput(inputId ="chemin_rep_travail_graph","Chemin du répertoire de travail",value = lignes[2])})
               
               output$actu_fich_trace_graph <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",10)}
                 }
                 textInput(inputId="fichier_trace_graph", "Nom du fichier des traces", value = lignes[10])}) 
               
               output$actu_fich_intersection_graph <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",10)}
                 }
                 textInput(inputId="fichier_intersection_graph", "Nom du fichier des intersections", value = lignes[12])})
             })

observeEvent(input$export_graphb,
             {
               chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Export_Graph_2.R",sep="")
               source(chemsource)		
               sortiegraph=MOBITC_Export_Graph_2(chem_mobitc,isolate(input$chemin_rep_travail_graph),isolate(input$fichier_trace_graph),isolate(input$fichier_intersection_graph),isolate(input$fichier_evolution_graph$name))
               output$textexportgraph<-renderText(sortiegraph)
             })

output$actu_menu_rapport  <- renderUI({
  actionButton("actu_menu_rapport_but", "Actualiser le menu")
})

observeEvent(input$actu_menu_rapport_but,
             {
               output$actu_fich_travail_rapport <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",14)}
                 }
                 textInput(inputId ="chemin_rep_travail_rapport","Chemin du répertoire de travail",value = lignes[2])})
               
               output$actu_fich_sque_rapport <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",10)}
                 }
                 textInput(inputId="fichier_sque_rapport", "Nom du fichier du squelette", value = lignes[8])})
               
               output$actu_fich_trace_rapport <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",10)}
                 }
                 textInput(inputId="fichier_trace_rapport", "Nom du fichier des traces", value = lignes[10])}) 
               
               output$actu_fich_intersection_rapport <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",10)}
                 }
                 textInput(inputId="fichier_intersection_rapport", "Nom du fichier des intersections", value = lignes[12])})
             })

observeEvent(input$export_rapport,
             {
               chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Export_Rapport_Trace.R",sep="")
               source(chemsource)		
               sortierapport=MOBITC_Export_Rapport_Trace(chem_mobitc,isolate(input$chemin_rep_travail_rapport),isolate(input$fichier_sque_rapport),isolate(input$fichier_trace_rapport),isolate(input$fichier_intersection_rapport))
               output$textexportrapport<-renderText(sortierapport)
             })

# output$choix_axe <- renderUI({
#   chem_tabres=paste(input$chemin_rep_travail_graph,"\\",input$fichier_mobitc_graph,sep="")
#   tabres=read.table(chem_tabres,sep="\t",header=TRUE,row.names = NULL)
#   selectInput("naxe", "Choix de l'axe", as.list(unique(tabres$NAxe)))})
# 
# output$choix_trace <- renderUI({
#   chem_tabres=paste(input$chemin_rep_travail_graph,"\\",input$fichier_mobitc_graph,sep="")
#   tabres=read.table(chem_tabres,sep="\t",header=TRUE,row.names = NULL)
#   tabresextrait=tabres[which(tabres$NAxe == input$naxe),]
#   selectInput("ntrace", "Choix de la trace", as.list(unique(tabresextrait$NTrace)))})

output$actu_menu_histo  <- renderUI({
  actionButton("actu_menu_histo_but", "Actualiser le menu")
})

observeEvent(input$actu_menu_histo_but,
             {
               output$actu_fich_travail_histo <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",14)}
                 }
                 textInput(inputId ="chemin_rep_travail_histo","Chemin du répertoire de travail",value = lignes[2])})
				
				output$actu_fich_trace_histo <- renderUI({
                 {
                   fichier_init=paste(chem_mobitc,"\\Init_Routine_MobiTC.txt",sep="")
                   if (file.exists(fichier_init)==TRUE) {
                     fid=file(fichier_init, open = "r")
                     lignes <- readLines(fid)
                     close(fid)} else {lignes=rep("",14)}
                 }
                 textInput(inputId="fichier_trace_histo", "Nom du fichier des traces", value = lignes[10])}) 

             })
			 
		observeEvent(input$histo,
		{
		chemsource=paste(chem_mobitc,"/Sous_Routine_MOBITC/MOBITC_Export_Histo.R",sep="")
		source(chemsource)		
		sortiehisto=MOBITC_Export_Histo(chem_mobitc,isolate(input$chemin_rep_travail_histo),isolate(input$fichier_trace_histo),isolate(input$fichier_evolution_hist),isolate(input$largeur_histo),isolate(input$longueur_histo),isolate(input$tronqu_histo),isolate(input$taux_histo))
		output$texthisto<-renderText(sortiehisto[[1]])	
    })


observeEvent(input$quit,
             {
               stopApp(returnValue = invisible())
             })

})