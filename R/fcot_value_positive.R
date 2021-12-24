fcot_value_positive <- function(data,noms_colonnes,table_orig){
  ##
  test <- NULL
  ##
  verif <- sapply(noms_colonnes,
                  function(i)
                    (
                      sum(data %>% pull(i)>0,na.rm=TRUE) == nrow(data)
                    )
  )
  ##
  if(sum(verif) == length(noms_colonnes)){
    test <- tibble(test="2.2",
                   valeur_test="oui",
                   nom_test="colnames_value",
                   table_orig=table_orig,
                   # sortie=data.frame(
                   test_precis=paste0("2.2.1_",i),
                   nom_test_precis="colnames_value_positive",
                   # test="format_data.frame",
                   message=paste0("")
                   # )
    )
  }else{
    for(i in noms_colonnes[verif==FALSE]){
      verif <- which(
        (data %>% pull(i)<0)
      )

      ##
      test <- bind_rows(test,
                        tibble(test="2.2",
                               valeur_test="non",
                               nom_test="colnames_value",
                               table_orig=table_orig,
                               # sortie=data.frame(
                               test_precis=paste0("2.2.1_",i),
                               # test="format_data.frame",
                               nom_test_precis="colnames_value_positive",
                               message=paste0("La variable suivante ",
                                              i,
                                              " possède ",
                                              length(verif),
                                              " valeur(s) négative(s).\n Lignes :",
                                              paste0(verif,collapse = ", "))
                        )
      )
    }
  }
  return(test)
}
