#' Check empty values
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
fcot_value_empty <- function(data,noms_colonnes,table_orig){
  ##
  test <- NULL
  ##
  verif <- sapply(noms_colonnes,
                  function(i)
                    (
                      sum(!(data %>% dplyr::pull(i) %in% c(NA,""))) == nrow(data)
                    )
  )
  ##
  if(sum(verif) == length(noms_colonnes)){
    test <- tibble::tibble(test="2.2",
                   valeur_test="oui",
                   nom_test="colnames_value",
                   table_orig=table_orig,
                   # sortie=data.frame(
                   test_precis="2.2.1",
                   nom_test_precis="colnames_value_empty",
                   # test="format_data.frame",
                   message=paste0("")
                   # )
    )
  }else{
    for(i in noms_colonnes[verif==FALSE]){
      verif <- which(
        (data %>% dplyr::pull(i) %in% c(NA,""))
      )

      ##
      test <- dplyr::bind_rows(test,
                        tibble::tibble(test="2.2",
                               valeur_test="non",
                               nom_test="colnames_value",
                               table_orig=table_orig,
                               # sortie=data.frame(
                               test_precis=paste0("2.2.1_",i),
                               # test="format_data.frame",
                               nom_test_precis="colnames_value_empty",
                               message=paste0("La variable suivante ",
                                              i,
                                              " possède ",
                                              length(verif),
                                              " valeur(s) vide(s).\n Lignes :",
                                              paste0(verif,collapse = ", "))
                        )
      )
    }
  }
  return(test)
}
