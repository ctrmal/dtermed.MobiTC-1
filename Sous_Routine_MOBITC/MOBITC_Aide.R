MOBITC_Aide<-function(chem_mobitc)
{
  aide_pres=div(h3("Bienvenue dans MobiTC"),
				p("Le logiciel MobiTC (Mobilit� du Trait de C�te) est un outil automatis� de traitement des traits de c�te 
					historiques, d�velopp� au Cerema M�diterran�e. Il se base sur le traitement fortement automatis� des 
					traits de c�te pour effectuer les principaux calculs utilis�s en ing�nierie et en recherche."),
				p("La premi�re �tape consiste � cr�er une ligne de base issue du squelette des traits de c�te (algorithme de 
				Vorono�). Des traces perpendiculaires � cette ligne de base sont ensuite cr��es et leurs intersections 
				avec les traits de c�te font l'objet de traitements statistiques simples comme le calcul d�une r�gression 
				lin�aire et de ces intervalles de confiance."),
				p("Les r�sultats sont ensuite convertibles sous forme SIG afin de r�pondre � une visualisation simple de la 
				mobilit� du trait de c�te. La projection des traits de c�te � �ch�ance est aussi disponible en prenant � la 
				fois en compte la projection de la r�gression lin�aire et ses intervalles de confiance. "))
  
  aide_init=div(h3("Objectifs :"),
                p(paste("Dans ce menu, un fichier d'initialisation est cr�� ou mis � jour dans le r�pertoire de travail pour stocker le chemin du r�pertoire de travail et les principaux noms des fichiers cr��s par MobiTC.")),
                p("Ce fichier est mis � jour au fur et � mesure que les diff�rentes �tapes de MobiTC ont �t� lanc�es."),
                p("Ce fichier permet la mise � jour des noms de fichier dans les diff�rents menus de MobiTC. Lorsque les boutons",
                  em("Actualiser le menu"),
                  "sont click�s, ce fichier est lu."),
                p("Ce fichier peut aussi �tre modifi� par un �diteur de texte."),
                h3("Entr�es :"),
                h4("- Chemin du r�pertoire de travail"),
                h4("- Nom du projet"))
  
  aide_fourn=div(h4("Objectifs"),
                 p(paste("Dans ce menu, une liste de fournisseurs de trait de c�te est mise � jour. Elle est stock�e dans le r�pertoire de travail"))
                 )
  
  aide_liste_tdc=div(h4("Objectifs"),
                     p(paste("Dans ce menu, une liste de trait de c�te est mise � jour."))
  )
  
  aide_env=div(h4("Objectifs"),
				p("Ce menu est la premi�re �tape pour la r�alisation de la ligne de base."),
				p("Pour rappel, la ligne de base est une sorte de ligne moyenne g�n�r�e � partir des TDC disponibles qui 
va permettre de d�finir des lignes perpendiculaires, appel�es traces, o� l'�volution du TDC sera calcul�e. 
Cette ligne de base sera donc le � point 0 �, � partir duquel la distance aux diff�rents TDC sera calcul�e 
et transform�e en �volution du trait de c�te. "),
				p("Pour avoir une ligne de base coh�rente avec les TDC, il faut que celle-ci soit d'une forme repr�sentative 
des diff�rents TDC. C'est pourquoi, dans MobiTC, il a �t� choisi de r�aliser de fa�on automatique une 
ligne  de  base,  �  partir  de  l'ensemble  des  diff�rents  TDC  disponibles,  en  repr�sentant  une  sorte  de 
m�diane. Pour cela le principe de la squelettisation a �t� retenu. "),
				p("Dans ce menu, l'enveloppe contenant tous les TDC est trac�e. Au menu Ligne de base, le squelette de cette enveloppe est r�alis�."),
				h4("Bon � savoir"),
				p("Il est possible de s'affranchir des �tapes de r�alisation de l'enveloppe et de la ligne de base lorsque les calculs sont trop longs.
				Il est possible de passer directement au menu Traces avec une ligne de base provenant d'une autre source. Il faut que cette ligne de base soit tout de m�me correctement orient�e.")
  )
  
  aide_sque=div(h4("Objectif"),
               p(paste("Dans ce menu, squelette")))
  
  aide_squerac=div(h4("Objectifs"),
                   p(paste("Dans ce menu, squelette rac")))
  
  aide_squeorr=div(h4("Objectifs"),
                   p(paste("Dans ce menu, squelette orr")))
  
  aide_squelisse=div(h4("Objectif"),
                     p(paste("Dans ce menu, squelette lisse")))
  
  aide_trace=div(h4("Objectifs"),
                  p(paste("Dans ce menu, trace")))
  
  aide_tracelisse=div(h4("Objectifs"),
                      p(paste("Dans ce menu, trace lisse")))
  
  aide_ip=div(h4("Objectifs"),
              p(paste("Dans ce menu, intersection ponctuelle")))
  
  aide_intersuf=div(h4("Objectifs"),
                    p(paste("Dans ce menu, intersection surfacique")))
  
  aide_evol=div(h4("Objectifs"),
                p(paste("Dans ce menu, �volution")))
				  
  aide_graph=div(h4("Objectifs"),
                      p(paste("Dans ce menu, graph")))
					  
  aide_rapport=div(h4("Objectifs"),
                      p(paste("Dans ce menu, rapport")))				  
  
  aide_histo=div(h4("Objectifs"),
                  p(paste("Dans ce menu, histogramme")))
  
  sortie=list(aide_pres,aide_init,aide_fourn,aide_liste_tdc,aide_env,
              aide_sque,aide_squerac,aide_squeorr,aide_squelisse,
              aide_trace,aide_tracelisse,
              aide_ip,aide_intersuf,
              aide_evol,aide_graph,aide_rapport,aide_histo)
  
  names(sortie) <- c("aide_pres","aide_init", "aide_fourn", "aide_liste_tdc","aide_env",
                     "aide_sque","aide_squerac","aide_squeorr","aide_squelisse",
                     "aide_trace","aide_tracelisse",
                     "aide_ip","aide_intersuf",
                     "aide_evol","aide_graph","aide_rapport","aide_histo")
  
  return(sortie)
}