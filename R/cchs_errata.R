### CCHS 2015/2016 Errata

### UP TO DATE BASED ON Canadian Community Health Survey – Annual Component Errata !!! JUNE 2018 !!! ###
### IMPORTANT NOTE: If you are using the PMH module from CCHS 2015, you will need to code Errata Item-006.
###                 It is not included here as it is not in the 2015/2016 dataset and would yield errors.

### SET YOUR INPUT FILE (replace "your_file" with name of your file)
# You can either use this code to set your input cchs file or find and replace "cchs_input" with your file name
cchs_errata <- function (dataframe) {

cchs_input <- dataframe

# Create new columns for Item-009

cchs_input$PAATTRAV=rowSums(cchs_input[,c("PAA_010A","PAA_010B","PAA_010C","PAA_010D","PAA_010E","PAA_010F","PAA_010G")]=="Yes",na.rm = TRUE)
cchs_input$PAATREC=rowSums(cchs_input[,c("PAA_040A","PAA_040B","PAA_040C","PAA_040D","PAA_040E","PAA_040F","PAA_040G")]=="Yes",na.rm = TRUE)
cchs_input$PAATOTH=rowSums(cchs_input[,c("PAA_070A","PAA_070B","PAA_070C","PAA_070D","PAA_070E","PAA_070F","PAA_070G")]=="Yes",na.rm = TRUE)

cchs_errata <-
  cchs_input %>%
  mutate(
    # Item-001
    PAY_100=ifelse(PAY_090 == "No",0,PAY_100),
    PAY_105=ifelse(PAY_090 == "No" | PAY_100==168,0,PAY_105),
    # Item-002
    HMC_10_1=as.factor(ifelse(DHHDVHSZ>1 & HMC_005A=="No","Valid skip", as.character(HMC_10_1))),
    HMC_10_2=as.factor(ifelse(DHHDVHSZ>1 & HMC_005B=="No","Valid skip", as.character(HMC_10_2))),
    HMC_10_3=as.factor(ifelse(DHHDVHSZ>1 & HMC_005C=="No","Valid skip", as.character(HMC_10_3))),
    HMC_10_4=as.factor(ifelse(DHHDVHSZ>1 & HMC_005D=="No","Valid skip", as.character(HMC_10_4))),
    HMC_10_5=as.factor(ifelse(DHHDVHSZ>1 & HMC_005E=="No","Valid skip", as.character(HMC_10_5))),
    # Item-003
    DIA_055=as.factor(ifelse(DHH_AGE>34 & DIA_005 %in% c("Yes","No","Don't know"),as.character(CCC_070),"Valid skip")),
    DIA_060=as.factor(ifelse(DHH_AGE>34 & DIA_005 %in% c("Yes","No","Don't know"),as.character(CCC_080),"Valid skip")),
    # Item-004
    ALWDVLTR=
      as.factor(
        ifelse(
          DOALW=="No" | ALC_005 =="No","Valid Skip",
          ifelse(
            ALC_005 %in% c("Don't know", "Refusal", "Not stated")|
              ALC_010 %in% c("Don't know", "Refusal", "Not stated")|
              ALW_005 %in% c("Don't know", "Refusal", "Not stated")|
              between(ALW_010,997,999) | between(ALW_015,997,999) | between(ALW_020,997,999) | between(ALW_025,997,999) |
              between(ALW_030,997,999) | between(ALW_035,997,999) | between(ALW_040,997,999), "Not stated",
            ifelse(
              DHH_SEX=="Male" & (
                between(ALW_010,4,995) | between(ALW_015,4,995) | between(ALW_020,4,995) | between(ALW_025,4,995) |
                  between(ALW_030,4,995) | between(ALW_035,4,995) | between(ALW_040,4,995) | between(ALWDVWKY,16,995)),
              "Increased long term health risk due to drinking",
              ifelse(
                DHH_SEX=="Female" & (
                  between(ALW_010,3,995) | between(ALW_015,3,995) | between(ALW_020,3,995) | between(ALW_025,3,995) |
                    between(ALW_030,3,995) | between(ALW_035,3,995) | between(ALW_040,3,995) | between(ALWDVWKY,11,995)),
                "Increased long term health risk due to drinking",
                ifelse(
                  ALW_005 == "No" | ALC_010 == "No","No increased long term heath risk due to drinking",
                  ifelse(
                    DHH_SEX=="Male" & (ALW_010<=3 &ALW_015<=3 &ALW_020<=3 &ALW_025<=3 &ALW_030<=3 &ALW_035<=3 &ALW_040<=3 &ALWDVWKY<=15),
                    "No increased long term heath risk due to drinking",
                    ifelse(
                      DHH_SEX=="Female" & (ALW_010<=2&ALW_015<=2 &ALW_020<=2 &ALW_025<=2 &ALW_030<=2 &ALW_035<=2 &ALW_040<=2 & ALWDVWKY<=10),
                      "No increased long term heath risk due to drinking","Error"
                    )
                  )
                )
              )
            )
          )
        )
      ),
    ALWDVSTR=as.factor(
      ifelse(
        DOALW=="No" | ALC_005 =="No","Valid Skip",
        ifelse(
          ALC_005 %in% c("Don't know", "Refusal", "Not stated")|
            ALW_005 %in% c("Don't know", "Refusal", "Not stated")|
            between(ALW_010,997,999) | between(ALW_015,997,999) | between(ALW_020,997,999) | between(ALW_025,997,999) |
            between(ALW_030,997,999) | between(ALW_035,997,999) | between(ALW_040,997,999),
          "Not stated",
          ifelse(
            DHH_SEX=="Male" & (
              between(ALW_010,5,995) | between(ALW_015,5,995) | between(ALW_020,5,995) | between(ALW_025,5,995) | between(ALW_030,5,995) |
                between(ALW_035,5,995) | between(ALW_040,5,995) | between(ALWDVWKY,16,995)),
            "Increased short term health risks due to drinking",
            ifelse(
              DHH_SEX=="Female" & (
                between(ALW_010,4,995) | between(ALW_015,4,995) | between(ALW_020,4,995) | between(ALW_025,4,995) | between(ALW_030,4,995) |
                  between(ALW_035,4,995) | between(ALW_040,4,995) | between(ALWDVWKY,11,995)),
              "Increased short term health risks due to drinking",
              ifelse(
                ALW_005 == "No" | ALC_010 == "No","No increased short term risk from due to drinking",
                ifelse(
                  DHH_SEX=="Male" & (
                    ALW_010<=4&ALW_015<=4&ALW_020<=4&ALW_025<=4&ALW_030<=4&ALW_035<=4&ALW_040<=4&ALWDVWKY<=15),
                  "No increased short term risk from due to drinking",
                  ifelse(
                    DHH_SEX=="Female" & (ALW_010<=3&ALW_015<=3&ALW_020<=3&ALW_025<=3&ALW_030<=3&ALW_035<=3&ALW_040<=3&ALWDVWKY<=10),
                    "No increased short term risk from due to drinking","Error"
                  )
                )
              )
            )
          )
        )
      )
    ),
    # Item-005 - NA - just for SAS file
    # Item-006 - NA - for PMH which is a 2015 only module
    # Item-007 - NA - Optional UPE module - not use in Ontario in 15/16
    # Item-008
    EHG2DVR3= as.factor(
      ifelse(
        (EHG2_01 %in% c("Grade 8 or lower (Québec: Secondary II or lower)","Grade 9-10 (Qc: Sec. III or IV, N.L.: 1st yr sec.)") | EHG2_02 == "No") &
          (EHG2_04 == "Less than high school diploma or its equivalent" |EHG2_03=="No"),
        "Less than secondary school graduation",
        ifelse(
          EHG2_04 == "High school diploma or a high school equivalency certificate" |EHG2_02 == "Yes" & EHG2_03 == "No",
          "Secondary school graduation, no post-secondary education",
          ifelse(
            EHG2_04 %in% c("Trade certificate or diploma","College/CEGEP/other non-university certificate or diploma","University certificate or diploma below the bachelor's level", "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)","University certificate, diploma, degree above the BA level"),
            "Post-secondary certificate diploma or univ degree",
            ifelse(EHG2_01 %in% c("Don't know","Refusal","Not Stated") & EHG2_02 == "No" | EHG2_02 %in% c("Don't know","Refusal","Not Stated") | EHG2_03 %in% c("Don't know","Refusal","Not Stated") | EHG2_04 %in% c("Don't know","Refusal","Not Stated"),
                   "Not stated",
                   as.character(EHG2DVR3)
            )
          )
        )
      )
    ),
    EHG2DVR9= as.factor(
      ifelse(
        EHG2_01 == "Grade 8 or lower (Québec: Secondary II or lower)" & (EHG2_03 == "No" | EHG2_04 == "Less than high school diploma or its equivalent"),
        "Grade 8 or lower (QC: Sec 2 or lower)",
        ifelse(
          EHG2_01 == "Grade 9-10 (Qc: Sec. III or IV, N.L.: 1st yr sec.)" & (EHG2_03 == "No"|EHG2_04 == "Less than high school diploma or its equivalent"),
          "Grade 9-10 (Qué: Sec 3/4; Newfoundland & Labrador: Sec 1)",
          ifelse(
            EHG2_01 == "Grade 11-13 (Qc: Sec. V, N.L.: 2-3 yr of sec.)" & EHG2_02 == "No" & (EHG2_03 == "No" |  EHG2_04 == "Less than high school diploma or its equivalent"),
            "Grade 11-13 (Qué: Sec 5; Newfoundland & Labrador: Sec 2/3)",
            ifelse(
              (EHG2_02 == "Yes" & EHG2_03 == "No") | EHG2_04=="High school diploma or a high school equivalency certificate",
              "Secondary school graduation, no post-secondary",
              ifelse(
                EHG2_04 == "Trade certificate or diploma",
                "Trade certificate or diploma",
                ifelse(
                  EHG2_04 == "College/CEGEP/other non-university certificate or diploma",
                  "Certificate/diploma - college, CEGEP, etc (non-trades)",
                  ifelse(
                    EHG2_04 == "University certificate or diploma below the bachelor's level",
                    "University certificate or diploma below bachelor's level",
                    ifelse(
                      EHG2_04 == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)",
                      "Bachelor's degree",
                      ifelse(
                        EHG2_04 == "University certificate, diploma, degree above the BA level",
                        "Certificate/diploma/univ degree above bachelor's level",
                        ifelse(
                          EHG2_01 %in% c("Don't know","Refusal","Not Stated") & EHG2_02 == "No" | EHG2_02 %in% c("Don't know","Refusal","Not Stated") | EHG2_03 %in% c("Don't know","Refusal","Not Stated") | EHG2_04 %in% c("Don't know","Refusal","Not Stated"),
                          "Not stated", as.character(EHG2DVR9)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    # Item-009
    PAATTRAV=ifelse(PAA_010A %in% c("Don't know", "Refusal", "Not stated"),9,PAATTRAV),
    PAATREC=ifelse(PAA_040A %in% c("Don't know", "Refusal", "Not stated"),9,PAATREC),
    PAATOTH=ifelse(PAA_070A %in% c("Don't know", "Refusal", "Not stated"),9,PAATOTH),
    PAADVATR=ifelse(DOPAA=="No"|DHH_AGE<18,9999.6,ifelse(
      PAADVTRV == 99999 | PAATTRAV==9, 9999.9, ifelse(
        PAADVTRV == 0, 0, ifelse(
          between(PAADVTRV,1,10080) & between(PAATTRAV,1,7), round(PAADVTRV/PAATTRAV,digits=2),8888.8)
      )
    )
    ),
    PAADVARC=ifelse(DOPAA=="No"|DHH_AGE < 18,9999.6,ifelse(
      PAADVREC==99999|PAATREC==9,9999.9,ifelse(
        PAADVREC==0,0,ifelse(PAADVREC >= 1 & PAADVREC <= 10080 & PAATREC >= 1 & PAADVREC <= 7,
                             round(PAADVREC/PAATREC,digits=2),PAADVARC)
      )
    )
    ),
    PAADVATH=ifelse(DOPAA=="No"|DHH_AGE < 18,9999.6,ifelse(
      PAADVOTH ==99999| PAATOTH==9,9999.9, ifelse(
        PAADVOTH ==0,0, ifelse(PAADVOTH >= 1 & PAADVOTH <= 10080 & PAATOTH >= 1 & PAATOTH <= 7, round(PAADVOTH/PAATOTH,digits=1),8888.8)
      )
    )
    ),
    # Item 010
    SXBDVPRT=ifelse(DHH_AGE < 15 | DHH_AGE > 64,9996,SXBDVPRT),
    SXBDVSTI=as.factor(ifelse(DHH_AGE < 15 | DHH_AGE > 64,"Valid skip",as.character(SXBDVSTI))),
    SXBDVTST=as.factor(ifelse(DHH_AGE < 15 | DHH_AGE > 64,"Valid skip",as.character(SXBDVTST))),
    # Item 011 - not possible to fix, affected variables: LOP_015, affects 38 cases
    # Item 012
    MEXDVBM6=as.factor(ifelse(MEX_100=="No","Has not breastfed her last baby at all",as.character(MEXDVBM6))),
    # Item 013 - not possible to fix, affected variables: SDC_020A-K/SDCDVCGT, affects 145 responses
    # Item 014
    DHHDVAOS=ifelse(DHHDVAOS < 16, 999, DHHDVAOS),
    # Item 015
    FLU_025A=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025A))),
    FLU_025B=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025B))),
    FLU_025C=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025C))),
    FLU_025D=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025D))),
    FLU_025E=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025E))),
    FLU_025F=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025F))),
    FLU_025G=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025G))),
    FLU_025H=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025H))),
    FLU_025I=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025I))),
    FLU_025J=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025J))),
    FLU_025K=as.factor(ifelse(FLU_015 %in% c("Don't know","Refusal","Not stated"),"Valid skip",as.character(FLU_025K))),
    # Item 016
    GEN_025=as.factor(ifelse(ADM_PRX == "No" & !(MAC_005 %in% c("Working at a paid job or business", "Vacation (from paid work)")) &!(MAC_010 %in% c("Yes","Don't know","Refusal")), "Valid skip", as.character(GEN_025)))
  )
}

