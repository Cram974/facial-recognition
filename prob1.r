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
#Fonction de distance: Euclidienne
squaredDistance = function(a, b){
    v = a-b
    d = 0
    for(i in 1:dim(v)[2]){
        d = d + v[i]*v[i]
    }
    return(d)
}

#Boucle de chargement des images constituants 
#les données de référence.

in_data = NULL
for(i in list.files(data_folder)){
    file = paste0(data_folder, "/", i)
    in_data = rbind(in_data, load_face(file))
}
#Estimation du nombre de composante a garder

#generalized cross validation approximation
e1 = estim_ncp(in_data, ncp.min = 0, ncp.max = NULL, scale = FALSE, method = "GCV")
#smoothing method
e2 = estim_ncp(in_data, ncp.min = 0, ncp.max = NULL, scale = FALSE, method = "Smooth")

e1
e2


#Réalise l'analyse
#ici on choisi e1
res = PCA(in_data, scale.unit=FALSE, ncp=e1$ncp, graph=F)

plot(res)

for(file in list.files(test_folder)){
    #On charge l'image de test
    face = load_face(paste0(test_folder, "/", file))

    #On recentre sa matrice
    phi = face - res$call$centre

    #On projette sur les axes de l'ACP
    p = phi %*% res$svd$V

    #On calcule la distance entre p et les projection des visage de reference
    #et on retient le minimum
    min = NULL
    dmin = 999999999
    for( i in 1:(dim(res$ind$coord)[1])){
        d = squaredDistance(res$ind$coord[i,], p)
        if(d < dmin){
            dmin = d
            min = i
        }
    }

    test = matrix(face, 91, 91, FALSE)
    id = matrix(in_data[min,], 91, 91, FALSE)
    plot(pixmapGrey(cbind(test, id), 91, 182))
}
