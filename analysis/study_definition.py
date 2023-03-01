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
from codelist import *

from datetime import datetime

#STUDY POPULATION

start_date = "2019-01-01"
end_date = datetime.today().strftime('%Y-%m-%d')
index_year = 2019
min_age = 14
max_age = 49

# DEFINE STUDY POPULATION ---
study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

    # Set index date to start date
    index_date=start_date,

    ## define the population 
    population=patients.satisfying(
        """
        NOT has_died
        AND
        registered
        AND
        (age > 14)
        AND
        has_follow_up_previous_year
        AND
        (sex = "F")
        """,

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

    ),
    
    age=patients.age_as_of(
        "2019-01-01",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),

    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.00, "F": 1.0}},
        }
    ),

    practice=patients.registered_practice_as_of(
            "index_date",
            returning="pseudo_id",
            return_expectations={"int": {"distribution": "normal",
                                        "mean": 25, "stddev": 5}, "incidence": 1}
        ),


    delivery_code=patients.with_these_clinical_events(
    delivery_codes,
    between=["index_date", "today"],
    returning="number_of_matches_in_period",
    return_expectations={
       "int": {"distribution": "normal", "mean": 4, "stddev": 1},
       "incidence": 1,
       },
    ),
)