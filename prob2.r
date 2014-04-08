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


############################################
#CAH
############################################

h_meth = "ward"
d_meth = "euclidean"

hc = hclust(dist(res$ind$coord, method = d_meth), h_meth)
title = paste0("Dendrogram: d_meth: ", d_meth,  " h_meth: ", h_meth, " ncp: ", e1$ncp)
plot(hc, hang=-1, main=title)

#On réalise la reconnaissance
for(i in 1:dim(test_data)[1]){
    i_data = dim(ref_data)[1] + i
    
    #On recupère le plan optimale pour ce dernier
    a1 = which.max(res$ind.sup$cos2[i,])
    a2 = which.max((res$ind.sup$cos2[i,])[-a1])
    if(a2>=a1){
        a2 = a2+1
    }
    
    ind = res$ind$coord[,c(a1,a2)]
    ind_sup = c(res$ind.sup$coord[i,c(a1,a2)])
    coords = rbind(ind,ind_sup)
    rownames(coords)[dim(coords)[1]] = rownames(data)[i_data]
    label = rownames(data)[i_data]
    
    hc = hclust(dist(coords, method = d_meth), h_meth)
    title = paste0("Dendrogram: d_meth: ", d_meth,  " h_meth: ", h_meth, " ncp: ", e1$ncp, " ", label)
    plot(hc, hang=-1, main=title)
    
    
    cl = NULL
    #Pour chaque aggregation
    for( h in hc$height){
        #on recupère les classes
        tree = cutree(hc, h = h)

        n = 0
        cl = NULL
        #pour chaque elements de l'etape
        for( j in names(tree) ){
            if(tree[j] == tree[label]){
                cl = cbind(cl, j)
                n = n + 1
            }
        }
        if(n > 1) {
            break
        }
    }
    m = NULL
    for( e in cl){     
        #On affiche le visage de test à gauche du visage identifié
        m = cbind(m, matrix(data[e,], 91, 91, FALSE))
    }
    plot(pixmapGrey(m, dim(m)[1], dim(m)[2]))
}

