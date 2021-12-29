#' Check interval values
#'
#' @param data tableau
#' @param noms_colonnes les nom des colonnes dont on doit verifier le type
#' @param table_orig nom du tableau
#'
#' @return
#' @export
#' @import magrittr
#'
#' @examples
fcot_value_interval <- function(data,noms_colonnes,mini,maxi,table_orig){
  ##
  test <- NULL
  ##
  verif <- sapply(noms_colonnes,
                  function(i)
                    (
                      sum(dplyr::between(data %>%
                                           dplyr::pull(i),
                                         left = mini,
                                         right = maxi),
                          na.rm=TRUE) == nrow(data)
                    )
  )
  ##
  if(sum(verif) == length(noms_colonnes)){
    test <- tibble::tibble(test="2.3",
                   valeur_test="oui",
                   nom_test="colnames_value",
                   table_orig=table_orig,
                   # sortie=data.frame(
                   test_precis="2.3.1",
                   nom_test_precis="colnames_value_interval",
                   # test="format_data.frame",
                   message=paste0("")
                   # )
    )
  }else{
    for(i in noms_colonnes[verif==FALSE]){
      verif <- which(
        (dplyr::between(data %>%
                          dplyr::pull(i),
                        left = mini,
                        right = maxi) %in% c(NA,FALSE))
      )

      ##
      test <- dplyr::bind_rows(test,
                        tibble::tibble(test="2.3",
                               valeur_test="non",
                               nom_test="colnames_value",
                               table_orig=table_orig,
                               # sortie=data.frame(
                               test_precis=paste0("2.3.1_",i),
                               # test="format_data.frame",
                               nom_test_precis="colnames_value_interval",
                               message=paste0("La variable suivante ",
                                              i,
                                              " possède ",
                                              length(verif),
                                              " valeur(s) non comprises dans l'intervalle [",
                                              mini,
                                              ";",
                                              maxi,
                                              "].\n Lignes :",
                                              paste0(verif,collapse = ", "))
                        )
      )
    }
  }
  return(test)
}
