setup_design <-
  function (in_data, in_weights= "FWGT", in_repweights= "BSW[0-9]+", in_type="bootstrap", in_combined=TRUE, geovar) {
    svrepdesign(
      data = in_data,
      weights = as.formula(paste0("~",in_weights)),
      repweights = in_repweights,
      type = in_type,
      combined.weights = in_combined)
  }

scale100 <- function(x){x*100}

stable <- function (question,dataframe,geo_var,svy_design) {

  if (is_symbol(geo_var)==FALSE){
    geo_var <- sym(geo_var)}
  question <- sym(question)

  raw_n <- dataframe %>% filter((!! geo_var)=="Yes"& !(is.na(!! question))) %>% count(!! question)
  wgt_est <- svymean(as.formula(paste0("~",quo_name(question))),svy_design,na.rm=TRUE)

  bind_cols(raw_n,
            as.tibble(wgt_est),
            as.tibble(cv(wgt_est)),
            as.tibble(confint(wgt_est))) %>%
    rename(`Sample Size`="n",Estimate="mean",CV="value",`Lower 95% CI`="2.5 %",`Upper 95% CI`="97.5 %",ind_level=quo_name(question)) %>%
    select(-SE) %>%
    mutate_at (vars(Estimate,`Lower 95% CI`,`Upper 95% CI`,CV),scale100) %>%
    mutate(
      `Quality indicator`=
        if_else(CV<=15,"",
                if_else(CV<=25,"C",
                        if_else(CV<=35,"D",
                                "NR"))),
      indicator=quo_name(question),
      geo=quo_name(geo_var)) %>%
    mutate_at(vars(Estimate,`Lower 95% CI`,`Upper 95% CI`,CV),list(~ifelse(`Quality indicator`=="NR" | `Sample Size`<10,NA,.)))%>%
    melt(id.vars=c("indicator","ind_level","geo"))%>%
    filter(ind_level!="zno")
}

each_geo <- function (items, geo, cchs_data) {

  if (is_symbol(geo)==FALSE){
    geo <- sym(geo)}

  cchs_df <- cchs_data %>% filter(!! geo=="Yes")


  design <-
    setup_design(in_data=cchs_df)

    suppressWarnings(
    lapply(setNames(items,items), function(item) {
      stable(geo_var = geo,dataframe = cchs_data, svy_design = design, question = item)
    }) %>% bind_rows())
    }

cchs_tableby <- function (question,dataframe,geo_var,svy_design,strat_var) {

  geo_var <- enquo(geo_var)
  question <- sym(question)
  strat_var <- sym(strat_var)

  raw_n <- dataframe %>% count(!! question,!! strat_var) %>% spread(!! question,n)%>% rename_at (vars(- !! strat_var),paste0,"_n")
  wgt_est <- as.tibble(svyby(as.formula(paste0("~", question)),as.formula(paste0("~", strat_var)), svy_design, svymean, vartype =c("ci","cv"))) %>%
    mutate_at(vars(- !! strat_var,),scale100)

  full_join(raw_n,wgt_est,by=quo_name(strat_var))%>%
    rename(strat_level=quo_name(strat_var)) %>%
    mutate_at(vars(contains("cv")), .funs = list(qual = ~if_else(
      .<=15,"", if_else(
        .<=25,"C", if_else(
          .<=35,"D","NR")))))%>%
    rename_at(vars(starts_with(quo_name(question))),str_replace,quo_name(question),"est_")%>%
    rename_at(vars(contains("_qual")),str_replace,"cv.","")%>%
    select(-contains("zno"))
}

ind_bytable <- function (item, stratifiers, geovar, cchsdata = cchs) {

  if (is_symbol(geovar)==FALSE) {
    geovar <- sym(geovar)}
  item <- sym(item)

  cchs_sub <- cchsdata %>% filter((!! geovar)=="Yes" & !(is.na(!! item))) %>% select (FWGT, starts_with("BSW"), stratifiers, !! item)
  cchs_design_ <-setup_design (in_data=cchs_sub)

  to_run <-
    data.frame(strat=stratifiers,item=quo_name(item)) %>%
    mutate(namerow=paste(item,strat,sep=" "))%>%
    column_to_rownames("namerow")

  suppressWarnings(
  results <-
    apply(to_run, 1, function(row) {
      cchs_tableby(geo_var=!! geovar, dataframe = cchs_sub, svy_design = cchs_design_, question=row[2], strat_var=row[1])
    })%>%
    bind_rows(.id="column_label")%>%
    rename_at(vars(contains(quo_name(item))),str_replace,quo_name(item),"") %>%
    melt(id.vars=c("column_label","strat_level")) %>%
    mutate(
      ind_level=str_replace_all(variable,c("ci_l."="","ci_u."="","cv."="","_n"="","est_"="","_qual"="")),
      meas=str_extract_all(variable,"ci_l|ci_u|cv|_n|est|qual"),
      measure=
        if_else(meas=="ci_l","Lower 95% CI",if_else(
          meas=="ci_u","Upper 95% CI", if_else(
            meas=="qual","Quality Indicator", if_else(
              meas=="cv","CV", if_else(
                meas=="_n","Sample Size","Estimate")
              )
            )
          )
        )) %>%
    separate(column_label,c("indicator","stratifier")," ") %>%
    select(-meas,-variable)%>%
    dcast(indicator+stratifier+strat_level+ind_level~measure)%>%
    mutate_at(vars(Estimate,`Lower 95% CI`,`Upper 95% CI`),list(~ifelse(`Quality Indicator`=="NR"|`Sample Size`<10,NA,.)))%>%
    melt(id.vars=c("indicator","ind_level","stratifier","strat_level"))
  )
}

run_geo <- function (geo_var, ind_vars, data, strat_vars) {

  lapply(setNames(ind_vars,ind_vars), function(ind){
    ind_bytable(item = ind,stratifiers = strat_vars, geovar = geo_var, cchsdata = data)
  }) %>% bind_rows() %>% mutate(geo=geo_var) }



cchs_table <- function (indicator_vars,by="none",geo_vars=c("phu","peer","prov"),dataframe = cchs) {

  table <-
    lapply (setNames(geo_vars,geo_vars), function (gv) {
      each_geo (items=indicator_vars, geo=gv, cchs_data=dataframe) }) %>% bind_rows()

  if (by != "none"){
    crosstable <-
      lapply(setNames(geo_vars,geo_vars), function(geo){
      run_geo(geo_var = geo,ind_vars=indicator_vars, data=dataframe, strat_vars=by)
    }) %>% bind_rows()

    bytable <-
      lapply (setNames(geo_vars,geo_vars), function (gv) {
        each_geo (items=by, geo=gv, cchs_data=dataframe) }) %>% bind_rows() %>% rename(stratifier="indicator", strat_level="ind_level")

    results <-
      bind_rows(table,bytable,crosstable)

  }

  else {return(table)}

}

