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


