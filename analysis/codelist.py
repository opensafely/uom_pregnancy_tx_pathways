from cohortextractor import codelist, codelist_from_csv, combine_codelists

delivery_codes = codelist_from_csv(
    "codelists/user-VickiPalin-pregnancy_delivery_snomed",
    system="snomed",
    column="code",
)

