options(digits=5)
################################################################################################
#           Certificat Sciences des Donn?es et Big Data - Toulouse Tech
#                             ann?e 2020-2021
#                         MODULE SENSIBILISATION
#         Section 1: Introduction ? R et au calcul statistique
################################################################################################
# Auteur: Florent Bourgeois, INP-ENSIACET, florent.bourgeois@toulouse-inp.fr
# M?J: 02/10/2020
################################################################################################
{# Soit X1 et X2 deux variables al?atoires ind?pendantes qui suivent toutes les deux
# une loi normale ou une loi uniforme, au choix.
# ?crivez en langage R les 6 fonctions (p, d, q, r, e et v)CertificatBigData,
# dans un m?me script R, qui permettent de calculer/g?n?rer:
# - la fonction de r?partition CDF (Cumulative Distribution Function): pCertificatBigData()
# - la densit? de probabilit? PDF (Probability Density Function): dCertificatBigData()
# - un quartile: qCertificatBigData()
# - des nombres al?atoires: rCertificatBigData()
# - la moyenne: eCertificatBigData()
# - la variance: vCertificatBigData()
# des variables Y = X1+X2, Z = X1*X2 et Z = X1?X2.
# Ces fonctions devront produire, au choix de l'utilisateur,
# des graphes pertinents qui permettent de visualiser les r?sultats.
#
# Au terme de cet exercice, vous r?pondrez gr?ce ? votre code de calcul
# ? la question suivante : quelle est la distribution statistique de la
# somme de 2 variables al?atoires uniformes ?
# La r?ponse ? cette question sera discut?e durant la s?ance en pr?sentiel
# associ?e ? ce module du certificat.
################################################################################################
} # Enonc? du probl?me

# fonction rCertificatBigData()

#' Title
#'
#' @param n a number vkfjedgofih.
#' @param paramX1_1 a number.
#' @param paramX1_2 a number.
#' @param paramX2_1 a number.
#' @param paramX2_2 a number.
#' @param loi a number.
#' @param operation a number.
#' @param graphics a number.
#'
#' @return a number.
#' @export
#'
#' @examples
#' rCertificatBigData(10,0,1,0,1)
rCertificatBigData <- function(n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics = FALSE){
{
# fonction qui ?chantillonne la somme, produit et ratio de 2 v.a. uniformes ou normales
# entr?es:
#   n : nombre de valeurs renvoy?es
#   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
#   paramX1_2: ?cart-type (loi normale) ou max (loi uniforme) de X1
#   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
#   paramX2_2: ?cart-type (loi normale) ou max (loi uniforme) de X2
#   loi: 'uniforme' ou 'normale'
#   operation: 'somme', 'produit' ou 'ratio'
#   graphics: TRUE ou FALSE
# sorties: data.frame qui contient:
#   sortie$X1 : vecteur de n valeurs de X1
#   sortie$X2 : vecteur de n valeurs de X2
#   sortie$Z : vecteur de n valeurs de Z
#
# exemple d'appel: rCertificatBigData(n=10000, paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, loi='uniforme', operation='produit', graphics=TRUE)
}# SIGNATURE
print(paste(operation," de X1 et X2, v.a. ",loi,"s",sep=""))

## G?n?ration de X1, X2 et Z = f(X1,X2)
  switch (loi,
          'uniforme' =
            {
              print("loi uniforme s?lectionn?e")
              X1 <- runif(n,min=paramX1_1, max=max(paramX1_2, paramX1_1+0.1))
              X2 <- runif(n,min=paramX2_1, max=max(paramX2_2, paramX2_1+0.1))
            },
          'normale' =
            {
              print("loi normale s?lectionn?e")
              X1 <- rnorm(n,mean=paramX1_1, sd=paramX1_2)
              X2 <- rnorm(n,mean=paramX2_1, sd=paramX2_2)
            },
            # autre
            {
              print("loi normale par d?faut")
              X1 <- rnorm(n,mean=paramX1_1, sd=paramX1_2)
              X2 <- rnorm(n,mean=paramX2_1, sd=paramX2_2)
            }
      )
  switch (operation,
          'somme' =
          {
            Z <- X1 + X2
            label_Z <- "Z = X1+X2"
          },
          'produit' =
          {
            Z <- X1 * X2
            label_Z <- "Z = X1*X2"
          },
          'ratio' =
          {
            Z <- X1 / X2
            label_Z <- "Z = X1/X2"
          },
          # autre
          {
            operation = 'somme'
            Z <- X1 + X2
            label_Z <- "Z = X1+X2"
          }
  )

  if (graphics == TRUE) {
    plot(Z, ylab=label_Z)
    plot(density(X1)$x,density(X1)$y/max(density(X1)$y),
         xlim=c(min(density(X1)$x, density(X2)$x), max(density(X1)$x, density(X2)$x)),
         ylim=c(0,1), type="l", col="blue", xlab="X1, X2", ylab="Densit? de probabilit?")
    lines(density(X2)$x,density(X2)$y/max(density(X2)$y), lty="solid", col="red")

  }

  # variable de sortie
  sortie <- list(X1,X2,Z,label_Z)
  names(sortie) <- c("X1","X2","Z","label_Z")
  return(sortie) # variable de sortie
}

# fonction eCertificatBigData()
#' Petite fonction Sympa
#'
#' @param n a number vkfjedgofih.
#' @param paramX1_1 a number.
#' @param paramX1_2 a number.
#' @param paramX2_1 a number.
#' @param paramX2_2 a number.
#' @param loi a number.
#' @param operation a number.
#' @param graphics a number.
#'
#' @return a number.
#' @export
#'
#' @examples
#' eCertificatBigData(10,0,1,0,1)
eCertificatBigData <- function(n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics = FALSE){
  {
    # fonction qui calcule la moyenne de la somme, produit et ratio de 2 v.a. uniformes ou normales
    # entr?es:
    #   n : nombre de valeurs renvoy?es
    #   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
    #   paramX1_2: ?cart-type (loi normale) ou max (loi uniforme) de X1
    #   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
    #   paramX2_2: ?cart-type (loi normale) ou max (loi uniforme) de X2
    #   loi: 'uniforme' ou 'normale'
    #   operation: 'somme', 'produit' ou 'ratio'
    #   graphics: TRUE ou FALSE
    # sorties:
    #   e : vmoyenne des n valeurs
    # exemple d'appel: eCertificatBigData(paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='uniforme', operation='somme', graphics=TRUE)
  }# SIGNATURE
  # ?chantillonne n valeurs de z
  z <- rCertificatBigData(n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics=FALSE)$Z
  # calcul de la moyenne
  e <- mean(z)

  if (graphics == TRUE) {
    pCertificatBigData(e,n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics)
  } # Trac? de la densit? de probabilit? de Z

  print(paste("la moyenne de", operation,"vaut: ", round(e,3)))
  return(invisible(e)) # variable de sortie
}

# fonction vCertificatBigData()
#' fonction qui sert à faire des statistique sur le risque en entreprise
#'
#' @param n a number vkfjedgofih.
#' @param paramX1_1 a number.
#' @param paramX1_2 a number.
#' @param paramX2_1 a number.
#' @param paramX2_2 a number.
#' @param loi a number.
#' @param operation a number.
#' @param graphics a number.
#'
#' @return a number.
#' @export
#'
#' @examples
#' vCertificatBigData(10,0,1,0,1)
vCertificatBigData <- function(n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics=FALSE){
  {
    # fonction qui calcule la variance de la somme, produit et ratio de 2 v.a. uniformes ou normales
    # entr?es:
    #   n : nombre de valeurs renvoy?es
    #   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
    #   paramX1_2: ?cart-type (loi normale) ou max (loi uniforme) de X1
    #   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
    #   paramX2_2: ?cart-type (loi normale) ou max (loi uniforme) de X2
    #   loi: 'uniforme' ou 'normale'
    #   operation: 'somme', 'produit' ou 'ratio'
    #   graphics: TRUE ou FALSE
    # sorties:
    #   v : vmoyenne des n valeurs
    # exemple d'appel: vCertificatBigData(paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='uniforme', operation='somme')
  }# SIGNATURE
  # ?chantillonne n valeurs de z
  z <- rCertificatBigData(n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics)$Z
  # calcul de la variance
  v <- var(z)
  print(paste("la variance de", operation,"vaut: ", round(v,3)))
  return(v) # variable de sortie

}


# fonction qCertificatBigData()
#' Title
#'
#' @param n a number vkfjedgofih.
#' @param paramX1_1 a number.
#' @param paramX1_2 a number.
#' @param paramX2_1 a number.
#' @param paramX2_2 a number.
#' @param loi a number.
#' @param operation a number.
#' @param graphics a number.
#'
#' @return a number.
#' @export
#'
#' @examples
#' qCertificatBigData(1,2)
qCertificatBigData <- function(p,n,paramX1_1=0,paramX1_2=1,
               paramX2_1=0,paramX2_2=1,
               loi='uniforme',
               operation='somme',
               graphics=FALSE){
  {
    # fonction qui renvoie le quartile q de la somme, produit et ratio de 2 v.a. uniformes ou normales
    # entr?es:
    #   p : valeur de la probabilit?
    #   n : nombre de valeurs renvoy?es
    #   paramX1_1: moyenne (loi normale) ou min (loi uniforme) de X1
    #   paramX1_2: ?cart-type (loi normale) ou max (loi uniforme) de X1
    #   paramX2_1: moyenne (loi normale) ou min (loi uniforme) de X2
    #   paramX2_2: ?cart-type (loi normale) ou max (loi uniforme) de X2
    #   loi: 'uniforme' ou 'normale'
    #   operation: 'somme', 'produit' ou 'ratio'
    #   graphics: TRUE ou FALSE
    # sorties:
    #   q : valeur du quartile q tel que P(Z <= q)= p
    # exemple d'appel: qCertificatBigData(p=0.5, paramX1_1=0,paramX1_2=4,paramX2_1=5,paramX2_2=6, n=100000, loi='normale', operation='somme', graphics=TRUE)
  }# SIGNATURE
  # ?chantillonne n valeurs de z
  z <- rCertificatBigData(n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics=FALSE)$Z
  # calcul du quartile
  q <- quantile(ecdf(z),p)

  if (graphics == TRUE) {
    pCertificatBigData(q,n,paramX1_1,paramX1_2,paramX2_1,paramX2_2,loi,operation,graphics=FALSE)
  } # Trac? de la densit? de probabilit? de Z

  print(paste("le quartile q tel que P(Z <= q)=", p," vaut: ",round(q,5),sep=""))
  return(q) # variable de sortie
}


# # Quelques exemples d'appel
# ###########################
# p11 <- 0; p12 <- 4; p21 <- 10; p22 <- 20; npoints = 100000; distr = 'normale'; op = 'produit';
# quartile <- 16; proba <- 0.95
# Z <- rCertificatBigData(n=npoints, paramX1_1=p11,paramX1_2=p12,paramX2_1=p21,paramX2_2=p22, loi=distr, operation=op, graphics=TRUE)
# Z <- dCertificatBigData(q=quartile, paramX1_1=p11,paramX1_2=p12,paramX2_1=p21,paramX2_2=p22, n=npoints, loi=distr, operation=op, graphics=TRUE)
# Z <- pCertificatBigData(q=quartile, paramX1_1=p11,paramX1_2=p12,paramX2_1=p21,paramX2_2=p22, n=npoints, loi=distr, operation=op, graphics=TRUE)
# Z <- qCertificatBigData(p=proba, paramX1_1=p11,paramX1_2=p12,paramX2_1=p21,paramX2_2=p22, n=npoints, loi=distr, operation=op, graphics=TRUE)
# Z <- pCertificatBigData(q=qCertificatBigData(p=proba, paramX1_1=p11,paramX1_2=p12,paramX2_1=p21,paramX2_2=p22, n=npoints, loi=distr, operation=op, graphics=TRUE),
#                         paramX1_1=p11,paramX1_2=p12,paramX2_1=p21,paramX2_2=p22, n=npoints, loi=distr, operation=op, graphics=TRUE)
# mean_Z <- eCertificatBigData(paramX1_1=p11,paramX1_2=p12,paramX2_1=p21,paramX2_2=p22, n=npoints, loi=distr, operation=op, graphics=TRUE)
# varZ <- vCertificatBigData(paramX1_1=p11,paramX1_2=p12,paramX2_1=p21,paramX2_2=p22, n=npoints, loi=distr, operation=op)
