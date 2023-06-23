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