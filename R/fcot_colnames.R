#' Verify the names of the data.frame
#'
#' @param data tableau
#' @param noms_colonnes les noms souhaités
#' @param table_orig le nom du tableau
#'
#' @return un tableau de 6 colonnes :
#' - test : code du test
#' - valeur_test : oui ou non oui correspond à la validation du test
#' - table_orig : nom du tableau d'origine
#' - test_precis : code du test
#' - message : message d'erreur
#' @export
#' @import magrittr
#'
#' @examples
#' obj <- data.frame(a=1:5)
#' fcot_colnames(obj,noms_colonnes=c("a"),table_orig="table test")
fcot_colnames <- function(data,noms_colonnes,table_orig){
  ##
  test <- CheckOfTable::fcot_table(data,table_orig)
  ##
  data <- data %>%
    # rename_with(str_to_upper) %>%
    dplyr::rename_with(stringr::str_trim)
  ##
  if(test$valeur_test == "oui"){
    verif <- which(! (noms_colonnes %in% names(data)))
    if(length(verif)==0){
      test <- tibble::tibble(test="1.2",
                     valeur_test="oui",
                     nom_test="data.frame_colnames",
                     table_orig=table_orig,
                     # sortie=data.frame(
                     test_precis="1.2",
                     # test="format_data.frame",
                     message="")
      # )
    }else{
      test <- tibble::tibble(test="1.2",
                     valeur_test="non",
                     nom_test="data.frame_colnames",
                     table_orig=table_orig,
                     # sortie=data.frame(
                     test_precis="1.2",
                     # test="format_data.frame",
                     message=paste0("Les noms des colonnes ne sont pas identifiés : ",
                                    paste0(noms_colonnes[verif],collapse=", ")
                     )
                     # )
      )
    }

  }else{
    test <- tibble::tibble(test="1.2",
                   valeur_test="non",
                   nom_test="colnames_data.frame",
                   table_orig=table_orig,
                   # sortie=data.frame(
                   test_precis="1.2",
                   # test="format_data.frame",
                   message=paste0("Test non effectué car le format du ",table_orig," non valide")
    )
    # )

  }
  ##
  return(test)
}
