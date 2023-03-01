from cohortextractor import ( 
    StudyDefinition, 
    patients, 
    codelist, 
    codelist_from_csv  
    #Measure
)
#above as in example but with measure added
#doesnt run w Measure, add in once measures created

# Import codelists from codelist.py (which pulls them from the codelist folder)
from codelists import *

from datetime import datetime

#STUDY POPULATION

start_date="2019-01-01"
end_date=datetime.today().strftime('%Y-%m-%d')
#moved index_date to in study def()
#will have to change dates

study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "2019-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.95,
    },

    index_date=start_date,

        population = patients.satisfying(
        """
        NOT 
        has_died
        AND
        registered
        AND
        (age >= 14 AND age <= 49)
        AND
        has_follow_up_previous_year
        AND
        (sex = "F")
        """,

        # DONT USE # WITHIN PATIENTS.SATISFYING()
        #use index_date or 2019-01-01?

        has_died=patients.died_from_any_cause(
            on_or_before="index_date",
            returning="binary_flag",
        ),

        registered=patients.satisfying(
            "registered_at_start",
            registered_at_start=patients.registered_as_of("index_date"),
        ),

        has_follow_up_previous_year=patients.registered_with_one_practice_between(
            start_date="index_date - 1 year",
            end_date="index_date",
            return_expectations={"incidence": 0.95},
        ),

        age=patients.age_as_of(
            "index_date",
            return_expectations={
                "rate": "universal",
                "int": {"distribution": "population_ages"},
            },
        ),
        
        sex=patients.sex(
            return_expectations={
                "rate": "universal",
                "category": {"ratios":{"M": 0.00, "F": 1.0}},
            },
        ),
    )
        #ALL VARIABLES IN STUDY DEF ARE DEFINED HERE
       
        ### Practice
        practice=patients.registered_practice_as_of(
            "index_date",
            returning="pseudo_id",
            return_expectations={"int": {"distribution": "normal",
                                         "mean": 25, "stddev": 5}, "incidence": 1}
        ),

        delivery_code=patients.with_these_clinical_events(
        delivery_code,
        between=["index_date", "today"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 4, "stddev": 1},
            "incidence": 1,
        },
    ),
)


        #region=patients.registered_practice_as_of(
        #    "2019-01-01",
        #    returning="nuts1_region_name",
        #    return_expectations={
        #        "rate": "universal",
        #        "category": {
        #            "ratios": {
        #                "North East": 0.1,
        #                "North West": 0.1,
        #                "Yorkshire and the Humber": 0.1,
        #                "East Midlands": 0.1,
        #                "West Midlands": 0.1,
        #                "East of England": 0.1,
        #                "London": 0.2,
        #                "South East": 0.2,
        #            },
        #        },
        #    },
        #),

    # THINK ABOUT WHICH CATEGORIES WE WANT/NEED
    # FOR ANALYSES ie RATES BY AGE CAT, ETHNICITY, 
    # DEPRIVATION? plus practice/region above
    # ### Age categories

    #   age_cat=patients.categorised_as(
    #     {
    #         "0":"DEFAULT",
    #         "0-13": """ age >= 0 AND age < 13""",
    #         "14-19": """ age >= 14 AND age < 20""",
    #         "20-24": """ age >= 20 AND age < 25""",
    #         "25-29": """ age >= 25 AND age < 30""",
    #         "30-34": """ age >= 30 AND age < 35""",
    #         "35-39": """ age >= 35 AND age < 40""",
    #         "40-44": """ age >= 40 AND age < 45""",
    #         "45+": """ age >= 45 AND age < 120""",
    #     },
    #     return_expectations={
    #         "rate": "universal",
    #         "category": {
    #             "ratios": {
    #                 "0": 0,
    #                 "0-13": 0.01, 
    #                 "14-20": 0.10,
    #                 "20-24": 0.30,
    #                 "25-29": 0.30,
    #                 "30-34": 0.20,
    #                 "35-39": 0.06,
    #                 "40-44": 0.02,
    #                 "45+": 0.01,
    #             }
    #         },
    #     },
    # ),

    #    ethnicity=patients.with_ethnicity_from_sus(
    #        returning="group_6",
    #        use_most_frequent_code=True,
    #        return_expectations={
    #            "category": {"ratios": {"1": 0.8, "2": 0.1, "3": 0.1}},
    #            "incidence": 0.75,
    #            },
    #    ),


	#    imd=patients.categorised_as(
    #        {
    #            "0": "DEFAULT",
    #            "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
    #            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
    #            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
    #            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
    #            "5": """index_of_multiple_deprivation >= 32844*4/5 AND index_of_multiple_deprivation < 32844""",
    #        },
    #        index_of_multiple_deprivation=patients.address_as_of(
    #         "index_date",
    #            returning="index_of_multiple_deprivation",
    #            round_to_nearest=100,
    #        ),
    #        return_expectations={
    #         "rate": "universal",
    #            "category": {
    #                "ratios": {
    #                    "0": 0.05,
    #                    "1": 0.19,
    #                    "2": 0.19,
    #                    "3": 0.19,
    #                    "4": 0.19,
    #                    "5": 0.19,
    #                }
    #            },
    #        },
#)


