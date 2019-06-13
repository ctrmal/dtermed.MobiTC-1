MOBITC_Aide<-function(chem_mobitc)
{
  aide_pres=div(h3("Bienvenue dans MobiTC"),
				p("Le logiciel MobiTC (Mobilité du Trait de Côte) est un outil automatisé de traitement des traits de côte 
					historiques, développé au Cerema Méditerranée. Il se base sur le traitement fortement automatisé des 
					traits de côte pour effectuer les principaux calculs utilisés en ingénierie et en recherche."),
				p("La première étape consiste à créer une ligne de base issue du squelette des traits de côte (algorithme de 
				Voronoï). Des traces perpendiculaires à cette ligne de base sont ensuite créées et leurs intersections 
				avec les traits de côte font l'objet de traitements statistiques simples comme le calcul d’une régression 
				linéaire et de ces intervalles de confiance."),
				p("Les résultats sont ensuite convertibles sous forme SIG afin de répondre à une visualisation simple de la 
				mobilité du trait de côte. La projection des traits de côte à échéance est aussi disponible en prenant à la 
				fois en compte la projection de la régression linéaire et ses intervalles de confiance. "))
  
  aide_init=div(h3("Objectifs :"),
                p(paste("Dans ce menu, un fichier d'initialisation est créé ou mis à jour dans le répertoire de travail pour stocker le chemin du répertoire de travail et les principaux noms des fichiers créés par MobiTC.")),
                p("Ce fichier est mis à jour au fur et à mesure que les différentes étapes de MobiTC ont été lancées."),
                p("Ce fichier permet la mise à jour des noms de fichier dans les différents menus de MobiTC. Lorsque les boutons",
                  em("Actualiser le menu"),
                  "sont clickés, ce fichier est lu."),
                p("Ce fichier peut aussi être modifié par un éditeur de texte."),
                h3("Entrées :"),
                h4("- Chemin du répertoire de travail"),
                h4("- Nom du projet"))
  
  aide_fourn=div(h4("Objectifs"),
                 p(paste("Dans ce menu, une liste de fournisseurs de trait de côte est mise à jour. Elle est stockée dans le répertoire de travail"))
                 )
  
  aide_liste_tdc=div(h4("Objectifs"),
                     p(paste("Dans ce menu, une liste de trait de côte est mise à jour."))
  )
  
  aide_env=div(h4("Objectifs"),
				p("Ce menu est la première étape pour la réalisation de la ligne de base."),
				p("Pour rappel, la ligne de base est une sorte de ligne moyenne générée à partir des TDC disponibles qui 
va permettre de définir des lignes perpendiculaires, appelées traces, où l'évolution du TDC sera calculée. 
Cette ligne de base sera donc le « point 0 », à partir duquel la distance aux différents TDC sera calculée 
et transformée en évolution du trait de côte. "),
				p("Pour avoir une ligne de base cohérente avec les TDC, il faut que celle-ci soit d'une forme représentative 
des différents TDC. C'est pourquoi, dans MobiTC, il a été choisi de réaliser de façon automatique une 
ligne  de  base,  à  partir  de  l'ensemble  des  différents  TDC  disponibles,  en  représentant  une  sorte  de 
médiane. Pour cela le principe de la squelettisation a été retenu. "),
				p("Dans ce menu, l'enveloppe contenant tous les TDC est tracée. Au menu Ligne de base, le squelette de cette enveloppe est réalisé."),
				h4("Bon à savoir"),
				p("Il est possible de s'affranchir des étapes de réalisation de l'enveloppe et de la ligne de base lorsque les calculs sont trop longs.
				Il est possible de passer directement au menu Traces avec une ligne de base provenant d'une autre source. Il faut que cette ligne de base soit tout de même correctement orientée.")
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
                p(paste("Dans ce menu, évolution")))
				  
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