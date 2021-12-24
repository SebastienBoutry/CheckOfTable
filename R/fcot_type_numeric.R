fcot_type_numeric <- function(data,noms_colonnes,table_orig){
  ##
  verif <- which(
    sapply(noms_colonnes,
           function(i)
             ! is.numeric(data %>% pull(i)))
  )
  ##
  if(length(verif)==0){
    test <- tibble(noms_colonnes_non_conforme="",
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
    test <- tibble(noms_colonnes_non_conforme=noms_colonnes[verif],
                   test="2.1",
                   valeur_test="non",
                   nom_test="colnames_type",
                   table_orig=table_orig,
                   # sortie=data.frame(
                   # test_precis=paste0("2.1.2_",i),
                   # test="format_data.frame",
                   nom_test_precis="colnames_type_numeric") %>%
      mutate( test_precis=paste0("2.1.2_",noms_colonnes_non_conforme),
              message=paste0("La variable suivante ",
                             noms_colonnes_non_conforme,
                             " n'est pas dans le bon type de données (numeric).")
              # )
      )
  }
  return(test %>% select(-noms_colonnes_non_conforme))
}
