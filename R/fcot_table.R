#' Verify to a data.frame
#'
#' @param data jeu de donnée à tester
#' @param table_orig à voir comment le supprimer
#'
#' @return un tableau de 6 colonnes :
#' - test : code du test
#' - valeur_test : oui ou non oui correspond à la validation du test
#' - table_orig : nom du tableau d'origine
#' - test_precis : code du test
#' - message : message d'erreur
#' @export
#'
#' @examples
fcot_format_table <- function(data,table_orig){
  test <- NULL
  if(sum(class(data) %in% "data.frame")==0){
    test <- tibble(test="1.1",
                   valeur_test="non",
                   nom_test="data.frame_format",
                   table_orig=table_orig,
                   # sortie=data.frame(
                   test_precis="1.1",
                   # test="format_data.frame",
                   message=paste0("Le tableau (",
                                  table_orig,
                                  ") n'est pas un data.frame.")
                   # )
    )
  }else{
    test <- tibble(test="1.1",
                   valeur_test="oui",
                   nom_test="data.frame_format",
                   table_orig=table_orig,
                   # sortie=data.frame(
                   test_precis="1.1",
                   # test="format_data.frame",
                   message="")
    # )
  }
  return(test)
}
