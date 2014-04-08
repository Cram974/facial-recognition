#Chargement des bibliotheques
library(pixmap)
library(FactoMineR)

#Definitions des constantes
data_folder = "referenceFaceData"
test_folder = "testFaceData"
load_face = function(file){
    picture = read.pnm(file)
    mat = picture@grey[22:112, 0:90]
    vect = as.vector(mat)
    return(matrix(vect,1))
}

ind_names = NULL

#Boucle de chargement des images constituants 
#les données de référence.

ref_data = NULL
for(i in list.files(data_folder)){
    file = paste0(data_folder, "/", i)
    ref_data = rbind(ref_data, load_face(file))
    ind_names = rbind(ind_names, i)
}

#Boucle de chargement des images constituants 
#les données de test.
test_data = NULL
for(i in list.files(test_folder)){
    file = paste0(test_folder, "/", i)
    test_data = rbind(test_data, load_face(file))
    ind_names = rbind(ind_names, i)
}

#Creation du set de données
data = rbind(ref_data, test_data)
rownames(data) <- ind_names

############################################
#Estimation du nombre d'axes
############################################

#generalized cross validation approximation
e1 = estim_ncp(ref_data, ncp.min = 0, ncp.max = NULL, scale = FALSE, method = "GCV")
e1

#smoothing method
e2 = estim_ncp(ref_data, ncp.min = 0, ncp.max = NULL, scale = FALSE, method = "Smooth")
e2

#Réalise l'ACP
#ici on choisi pour nombre d'axes e1$ncp
ind_sup = (dim(ref_data)[1]+1):dim(data)[1]
res = PCA(data, ind.sup=ind_sup, scale.unit=FALSE, ncp=e1$ncp, graph=F)

#Pourcentage d'inertie par axe
barplot(res$eig[,2], main = "Pourcentage Inertie", names.arg = paste("Dim",1:nrow(res$eig), sep=""))

#Representation des individus sur le premier plan
plot.PCA(res, choix="ind", col.ind.sup="red")

#Pourcentage d'inertie cumulée
res$eig[,3]

#Individus reférence
#res$ind

#Individus test
#res$ind.sup

#Qualité de représentation des individus test 
#angle de deformation 
#1-> excellent 
#0-> très mauvais
res$ind.sup$cos2   
 
#On réalise la reconnaissance
for(i in 1:dim(test_data)[1]){
    i_data = dim(ref_data)[1] + i
    
    #On récupères les coordonnées de l'individu test
    p = res$ind.sup$coord[i,]
    
    #On recupère le plan optimale pour ce dernier
    a1 = which.max(res$ind.sup$cos2[i,])
    a2 = which.max((res$ind.sup$cos2[i,])[-a1])
    if(a2>=a1){
        a2 = a2+1
    }
    
    #On séléctionne ses coordonées dans ce plan
    p = c(p[a1], p[a2])
    
    #On calcule la distance entre p et les projection des visage de reference
    #et on retient le minimum
    min = NULL
    dmin = 999999999
    #Pour chaque visage de référence
    for( j in 1:(dim(ref_data)[1])){
        #on recupere les coordonnées de référence sur le plan (a1,a2)
        r = res$ind$coord[j,]
        r = c(r[a1], r[a2])
        
        #On calcule la distance euclidienne
        v1 = r[1]-p[1]
        v2 = r[2]-p[2]
        d = (v1*v1) + (v2*v2)
        
        #On la compare à la distance minimum
        if(d < dmin){
            #Si elle est plus petite elle devient notre nouvelle
            #distance minimum
            dmin = d
            #Notre visage identifié devient alors le j
            min = j
        }
    }
    
    
    title = paste(rownames(data)[i_data], " id-> " ,rownames(data)[min])
    s = c(rownames(data)[1:dim(ref_data)[1]], rownames(data)[i_data])
    
    plot.PCA(res, col.ind.sup="red", select=s, axes=c(a1,a2), title=title)
    
    #On affiche le visage de test à gauche du visage identifié
    test = matrix(test_data[i,], 91, 91, FALSE)
    id = matrix(ref_data[min,], 91, 91, FALSE)
    plot(pixmapGrey(cbind(test, id), 91, 182), main=title)
}
