# Generic cleaning - setting NA, removing blank variables, collapsing countable non-responses
cchs_cleanup <-
  function (dataframe, phu, peer) {
    phu <- sym(phu)
    peer <- sym(peer)
    suppressWarnings(
      cchs_clean <-
        dataframe %>%
        na_if(996) %>%
        na_if(997) %>%
        na_if(998) %>%
        na_if(999) %>%
        mutate_if(
          is.factor,fct_other,drop = c('Valid skip','Don\'t know','Refusal','Not stated'),other_level = "Refused unknown or missing"
          ) %>%
        mutate_if(is.factor,fct_recode,zno="No") %>%
        na_if("Refused unknown or missing") %>%
        mutate_if(is.factor,fct_drop) %>%
        select_if(~sum(!is.na(.)) > 0) %>%
        mutate(
          phu=if_else(GEODVHR4==phu,"Yes","No"),
          peer=if_else(GEODVHR4!=phu & GEODVPG==peer,"Yes","No"),
          prov="Yes",
          cycle="2015/2016"))
  }
