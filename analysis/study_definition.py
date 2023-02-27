from cohortextractor import ( 
    StudyDefinition, 
    patients, 
    codelist, 
    codelist_from_csv  
    #Measure
)
#above as in example but with measure added
#doesnt run w Measure, add in once measures created

#need to import codelists for snomed

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

    #code got stuck here once changed to index_date below
    #then worked once moved index_date to line 19
    #and got stuck on NOT has_died
    #fixed once """ re-added
    #then stuck on index_date not defined in index_date
    #worked once index_date moved inside study def
    #OUTPUT GENERATED NOW
    
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
                "incidence": 0.001
            },
        ),
        
        sex=patients.sex(
            return_expectations={
                "rate": "universal",
                "category": {"ratios": {"M": 0.49, "F": 0.51}},
            },
        ),
   
    #ALL VARIABLES IN STUDY DEF ARE DEFINED HERE

    ),
)
    
        # ### Practice
        # practice=patients.registered_practice_as_of(
        #     "index_date",
        #     returning="pseudo_id",
        #     return_expectations={"int": {"distribution": "normal",
        #                                  "mean": 25, "stddev": 5}, "incidence": 1}
        # ),

    
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
    # DEPRIVATION?
    # ### Age categories

    #   age_cat=patients.categorised_as(
    #     {
    #         "0":"DEFAULT",
    #         "0-13": """ age >= 0 AND age < 5""",
    #         "14-20": """ age >= 5 AND age < 15""",
    #         "20-24": """ age >= 15 AND age < 25""",
    #         "25-29": """ age >= 25 AND age < 35""",
    #         "30-34": """ age >= 35 AND age < 45""",
    #         "35-39": """ age >= 45 AND age < 55""",
    #         "40-44": """ age >= 55 AND age < 65""",
    #         "45+": """ age >= 75 AND age < 120""",
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

#code for IMD and ethnicity in hello-world-2 repo


