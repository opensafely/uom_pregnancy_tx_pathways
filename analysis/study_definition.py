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
        (age >= 14 AND age < 49)
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

    # Number of delivery codes per person
    delivery_code_number=patients.with_these_clinical_events(
    delivery_codes,
    between=["index_date", "today"],
    returning="number_of_matches_in_period",
    return_expectations={
       "int": {"distribution": "normal", "mean": 4, "stddev": 1},
       "incidence": 1,
       },
    ),

    # Date of last delivery code
    delivery_code_date=patients.with_these_clinical_events(
    delivery_codes,
    between=["index_date", "today"],
    returning="date",
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
    return_expectations={           
        "date": {
            "earliest": "2019-01-01",  
            "latest": "today",
            },
        },    
    ),

    # Is there a delivery code present
    delivery_code_present=patients.with_these_clinical_events(
    delivery_codes,
    between=["index_date", "today"],
    returning="binary_flag",    
    return_expectations={
       "int": {"distribution": "normal", "mean": 4, "stddev": 1},
       "incidence": 1,
       },
    ),

    #then need to return what the last code is if present
    
    # delivery_code=patients.with_these_clinical_events(
    # delivery_codes,
    # between=["index_date", "2019-12-31"],
    # returning="????",
    # ),

    # set for a certain period
    # use this as example for 6WC check
    delivery_code_present_2019=patients.with_these_clinical_events(
    delivery_codes,
    between=["index_date", "2019-12-31"],
    returning="binary_flag",    
    return_expectations={
       "int": {"distribution": "normal", "mean": 4, "stddev": 1},
       "incidence": 1,
       },
    ),

    #using delivery_code_dates mean that this should only
    #return codes for those with delivery dates
    #84 days is 12 weeks
    postnatal_8wk_code_present=patients.with_these_clinical_events(
    postnatal_8wk_codes, 
    between=["delivery_code_date", "delivery_code_date + 84 days"],
    returning="binary_flag",
    # return_expectations={  
    #   "int": {"distribution": "normal", "mean": 4, "stddev": 1},
    #    "incidence": 1,
    #    },
    ),

    #do we want to use all of these codes? 
    postnatal_other_code_present=patients.with_these_clinical_events(
    postnatal_other_codes,
    between=["delivery_code_date", "delivery_code_date + 84 days"],
    returning="binary_flag",
    # return_expectations={  
    #   "int": {"distribution": "normal", "mean": 4, "stddev": 1},
    #    "incidence": 1,
    #    },
    ),

    postnatal_antenatal_code_present=patients.with_these_clinical_events(
    postdel_antenatal_codes,
    between=["delivery_code_date", "delivery_code_date + 84 days"],
    returning="binary_flag",
    # return_expectations={  
    #   "int": {"distribution": "normal", "mean": 4, "stddev": 1},
    #    "incidence": 1,
    #    },
    ),

)



# can use 
# between=["delivery_code_date", "delivery_code_date + 84 days"]
# for postnatal code/6WC check

# add check for whether postnatal code within 6w

##add other variables for measures - age_cat, ethnicity, IMD, etc