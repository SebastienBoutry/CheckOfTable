#' Check numeric type of columns
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
fcot_type_numeric <- function(data,noms_colonnes,table_orig){
  ##
  verif <- which(
    sapply(noms_colonnes,
           function(i)
             ! is.numeric(data %>% dplyr::pull(i)))
  )
  ##
  if(length(verif)==0){
    test <- tibble::tibble(noms_colonnes_non_conforme="",
                   test="2.1",
                   valeur_test="oui",
                   nom_test="colnames_type",
                   table_orig=table_orig,
                   # sortie=data.frame(
                   test_precis="2.1.2",
                   nom_test_precis="colnames_type_numeric",
                   # test="format_data.frame",
                   message=paste0("")
                   # )
    )
  }else{
    test <- tibble::tibble(noms_colonnes_non_conforme=noms_colonnes[verif],
                   test="2.1",
                   valeur_test="non",
                   nom_test="colnames_type",
                   table_orig=table_orig,
                   # sortie=data.frame(
                   # test_precis=paste0("2.1.2_",i),
                   # test="format_data.frame",
                   nom_test_precis="colnames_type_numeric") %>%
      dplyr::mutate( test_precis=paste0("2.1.2_",noms_colonnes_non_conforme),
              message=paste0("La variable suivante ",
                             noms_colonnes_non_conforme,
                             " n'est pas dans le bon type de données (numeric).")
              # )
      )
  }
  return(test %>% dplyr::select(-noms_colonnes_non_conforme))
}
