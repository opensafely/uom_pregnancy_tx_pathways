from cohortextractor import codelist, codelist_from_csv, combine_codelists

delivery_codes = codelist_from_csv(
    "codelists/user-VickiPalin-pregnancy_delivery_snomed.csv",
    system="snomed",
    column="code",
)

postnatal_8wk_codes = codelist_from_csv(
    "codelists/user-VickiPalin-pregnancy_postnatal_8wk_snomed.csv",
    system="snomed",
    column="code",
)

postnatal_other_codes = codelist_from_csv(
    "codelists/user-VickiPalin-pregnancy_postnatal_other_snomed.csv",
    system="snomed",
    column="code",
)

postdel_antenatal_codes = codelist_from_csv(
    "codelists/user-VickiPalin-pregnancy_postdel_antenatal_snomed.csv",
    system="snomed",
    column="code",
)


## reviewed codes

delivery_codes_reviewed = codelist_from_csv(
    "codelists/user-VickiPalin-pregnancy_delivery_snomed_reviewed.csv",
    system="snomed",
    column="code",
)

delivery_codes_reviewed_2 = codelist_from_csv(
    "codelists/user-VickiPalin-pregnancy_delivery_snomed_reviewed_2.csv",
    system="snomed",
    column="code",
)

postnatal_8wk_codes_reviewed = codelist_from_csv(
    "codelists/user-VickiPalin-pregnancy_postnatal_8wk_snomed_reviewed.csv",
    system="snomed",
    column="code",
)

# ## ethnicity
# ethnicity_codes = codelist_from_csv(
#     "codelists/opensafely-ethnicity.csv",
#     system="ctv3",
#     column="Code",
#     category_column="Grouping_6",
# )
#### Ethnicity ####
ethnicity_codes_6 = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    system="snomed",
    column="snomedcode",
    category_column="Grouping_6",
)

## for covid variables

covid_primary_care_code = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv",
    system="ctv3",
    column="CTV3ID",
    )

covid_primary_care_positive_test = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
    system="ctv3",
    column="CTV3ID",
    )

covid_primary_care_sequalae = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-sequelae.csv",
    system="ctv3",
    column="CTV3ID",
    )

any_primary_care_code = combine_codelists(
    covid_primary_care_code,
    covid_primary_care_positive_test,
    covid_primary_care_sequalae,
    )

## for Charlson scores

charlson01_cancer= codelist_from_csv(
  "codelists/user-yayang-charlson01_cancer.csv",
  system = "snomed",
  column = "code"
)
charlson02_cvd= codelist_from_csv(
  "codelists/user-yayang-charlson02_cvd.csv",
  system = "snomed",
  column = "code"
)
charlson03_copd= codelist_from_csv(
  "codelists/user-yayang-charlson03_copd.csv",
  system = "snomed",
  column = "code"
)
charlson04_heart_failure= codelist_from_csv(
  "codelists/user-yayang-charlson04_heart_failure.csv",
  system = "snomed",
  column = "code"
)
charlson05_connective_tissue= codelist_from_csv(
  "codelists/user-yayang-charlson05_connective_tissue.csv",
  system = "snomed",
  column = "code"
)
charlson06_dementia= codelist_from_csv(
  "codelists/user-yayang-charlson06_dementia.csv",
  system = "snomed",
  column = "code"
)
charlson07_diabetes= codelist_from_csv(
  "codelists/user-yayang-charlson07_diabetes.csv",
  system = "snomed",
  column = "code"
)
charlson08_diabetes_with_complications= codelist_from_csv(
  "codelists/user-yayang-charlson08_diabetes_with_complications.csv",
  system = "snomed",
  column = "code"
)
charlson09_hemiplegia= codelist_from_csv(
  "codelists/user-yayang-charlson09_hemiplegia.csv",
  system = "snomed",
  column = "code"
)
charlson10_hiv= codelist_from_csv(
  "codelists/user-yayang-charlson10_hiv.csv",
  system = "snomed",
  column = "code"
)
charlson11_metastatic_cancer= codelist_from_csv(
  "codelists/user-yayang-charlson11_metastatic_cancer.csv",
  system = "snomed",
  column = "code"
)
charlson12_mild_liver= codelist_from_csv(
  "codelists/user-yayang-charlson12_mild_liver.csv",
  system = "snomed",
  column = "code"
)
charlson13_mod_severe_liver= codelist_from_csv(
  "codelists/user-yayang-charlson13_mod_severe_liver.csv",
  system = "snomed",
  column = "code"
)
charlson14_moderate_several_renal_disease= codelist_from_csv(
  "codelists/user-yayang-charlson14_moderate_several_renaldiseae.csv",
  system = "snomed",
  column = "code"
)
charlson15_mi= codelist_from_csv(
  "codelists/user-yayang-charlson15_mi.csv",
  system = "snomed",
  column = "code"
)
charlson16_peptic_ulcer= codelist_from_csv(
  "codelists/user-yayang-charlson16_peptic_ulcer.csv",
  system = "snomed",
  column = "code"
)
charlson17_peripheral_vascular= codelist_from_csv(
  "codelists/user-yayang-charlson17_peripheral_vascular.csv",
  system = "snomed",
  column = "code"
)

#high risk pregnancy code
hypertension_codes= codelist_from_csv(
  "codelists/user-djlhayes-high-risk-pregnancy.csv",
  system = "snomed",
  column = "code"
)

## add billy's hbp codes and combine