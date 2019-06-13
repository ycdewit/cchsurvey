cchs_recode <-
  function (dataframe) {
# recoding variables for analysis (mostly to dichotomous variables)
recode_cchs <-
  dataframe %>%
# SDOH and/or stratifying/subset variables
# Reference/basis for most SDOH: http://www.ottawapublichealth.ca/en/reports-research-and-statistics/resources/Documents/mental_health_report_2018_en.pdf
  mutate(
    agegrpmh=
      as_factor(
        if_else(
          DHH_AGE<=19,
          "12-19",
          if_else(
            DHH_AGE<=44,
            "20-44",
            if_else(
              DHH_AGE<=64,
              "45-64",
              "65+"
            )))),
    agegrp4=
      as_factor(
        if_else(
          DHH_AGE<=29,
          "12-29",
          if_else(
            DHH_AGE<=49,
            "30-49",
            if_else(
              DHH_AGE<=64,
              "50-64",
              "65+"
            )))),
    ownhome=recode(DHH_OWN,`Owned by member of hhld, even if it is still being paid for`="Owned",`Rented, even if no cash rent is paid`="Rented"),
    mothertongue=fct_collapse(SDCDVLHM,English=c("English","English and Other"),French=c("French","French and Other"),`English and French`=c("English and French","English, French and Other")),
    aboriginal=fct_collapse(SDCDVABT,zno=c("Non-Aboriginal identity")),
    livingsit=as.factor(if_else(
      DHHDVLVG %in% c("Single parent living with children","Child living with a single parent","Child living with a single parent and siblings"),
      "Single parent and children",
      if_else(
        DHHDVLVG %in% c("Parent living with spouse / partner and children","Child living with two parents","Child living with two parents and siblings"),
        "Parents and children",
        if_else(
          DHHDVLVG %in% c("Unattached individual living with others","Individual living with spouse / partner"),
          "Living with others no children",
          if_else(
            DHHDVLVG=="Unattached individual living alone",
            "Living alone",
            if_else(
              DHHDVLVG=="Other","Other",NULL)
            )))))
    )%>%
    rename(rural="GEODVUR2",education="EHG2DVR3",sex="DHH_SEX")%>%
# GEN Recodes
    mutate_at(c("GEN_005","GEN_015","GENDVHDI","GENDVMHI"),fct_collapse,`Very good or excellent`=c("Very good","Excellent"), zno=c("Good","Fair","Poor")) %>%
    mutate_at(c("GEN_020","GEN_025"),fct_collapse,`Stressful`=c("Quite a bit stressful","Extremely stressful"), zno=c("A bit stressful","Not very stressful","Not at all stressful"))%>%
    mutate(
      GEN_030=fct_collapse(GEN_030,Strong=c("Very strong","Somewhat strong"),zno=c("Somewhat weak","Very weak")),
      GENDVSWL=fct_collapse(GENDVSWL,Satisfied=c("Satisfied","Very Satisfied"),zno=c("Neither satisfied nor dissatisfied","Dissatisfied","Very Dissatisfied"))
    ) %>%
# Recode for fruit veg
    mutate(FVCDVGDT=fct_collapse(FVCDVGDT,`Eats fruits and vegetables more than 5 times per day`=c("Eats fruits and vegetables more than 10 times per day","Eats fruits and vegetables between 5 and 10 times per day")))%>%
    mutate(
      DEPDVSEV=fct_collapse(DEPDVSEV,`Moderate to severe depression`=c("Moderate depression","Moderately severe depression","Severe depression")),
      depsev=fct_collapse(DEPDVSEV,`Mild to severe depression`=c("Mild depression","Moderate to severe depression")),
    )
  }
