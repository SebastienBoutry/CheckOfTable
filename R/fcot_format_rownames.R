f_format_colnames <- function(data,noms_colonnes,table_orig){
  ##
  test <- f_format_table(data,table_orig)
  ##
  data <- data %>%
    # rename_with(str_to_upper) %>%
    rename_with(str_trim)
  ##
  if(test$valeur_test == "oui"){
    verif <- which(! (noms_colonnes %in% names(data)))
    if(length(verif)==0){
      test <- tibble(test="1.2",
                     valeur_test="oui",
                     nom_test="data.frame_colnames",
                     table_orig=table_orig,
                     # sortie=data.frame(
                     test_precis="1.2",
                     # test="format_data.frame",
                     message="")
      # )
    }else{
      test <- tibble(test="1.2",
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
    test <- tibble(test="1.2",
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
