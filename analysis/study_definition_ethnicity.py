from cohortextractor import ( 
    StudyDefinition, 
    patients, 
    codelist, 
    codelist_from_csv,  
)

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

    index_date=end_date,

    ## define the population 
    population=patients.all(),
     
    ethnicity = patients.categorised_as(
        {
            "6": "DEFAULT",
            "1": "eth6='1'",
            "2": "eth6='2'",
            "3": "eth6='3'",
            "4": "eth6='4'",
            "5": "eth6='5'",
        },
        eth6 = patients.with_these_clinical_events(
            ethnicity_codes_6,
            returning = "category",
            find_last_match_in_period = True,
            on_or_before="index_date",
            include_date_of_match = False,
            return_expectations = {
                "incidence": 0.75,
                "category": {
                    "ratios": {
                        "1": 0.30,
                        "2": 0.20,
                        "3": 0.20,
                        "4": 0.20,
                        "5": 0.05,
                        "6": 0.05,
                    },
                },
            },
        ),
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "1": 0.30,
                    "2": 0.20,
                    "3": 0.20,
                    "4": 0.20,
                    "5": 0.05,
                    "6": 0.05,
                },
            },
        },
    ),
    
    ## ethnicity using codelist

    eth=patients.with_these_clinical_events(
        ethnicity_codes_6,
        returning="category",
        find_last_match_in_period=True,
        on_or_before="index_date",
        include_date_of_match=False,
        return_expectations={
                                "category": {
                                    "ratios": {
                                        "1": 0.2,
                                        "2": 0.2,
                                        "3": 0.2,
                                        "4": 0.2,
                                        "5": 0.2
                                        }
                                    },
                                "incidence": 0.75,
                                },
    ),
    
    ethnicity_sus=patients.with_ethnicity_from_sus(
        returning="group_6",
        use_most_frequent_code=True,
        return_expectations={
            "category": {
                            "ratios": {
                                "1": 0.2,
                                "2": 0.2,
                                "3": 0.2,
                                "4": 0.2,
                                "5": 0.2
                                }
                            },
            "incidence": 0.4,
            },
    ),

    ethnicity2=patients.categorised_as(
            {
                "0": "DEFAULT",
                "1": "eth='1' OR (NOT eth AND ethnicity_sus='1')",
                "2": "eth='2' OR (NOT eth AND ethnicity_sus='2')",
                "3": "eth='3' OR (NOT eth AND ethnicity_sus='3')",
                "4": "eth='4' OR (NOT eth AND ethnicity_sus='4')",
                "5": "eth='5' OR (NOT eth AND ethnicity_sus='5')",
            },
            return_expectations={
                "category": {
                                "ratios": {
                                    "0": 0.1,  
                                    "1": 0.5,
                                    "2": 0.1,
                                    "3": 0.1,
                                    "4": 0.1,
                                    "5": 0.1
                                    }
                                },
                "rate": "universal",
            },
    ),

    eth_old=patients.with_these_clinical_events(
        ethnicity_codes,
        returning="category",
        find_last_match_in_period=True,
        on_or_before="index_date",
        include_date_of_match=False,
        return_expectations={
                                "category": {
                                    "ratios": {
                                        "1": 0.2,
                                        "2": 0.2,
                                        "3": 0.2,
                                        "4": 0.2,
                                        "5": 0.2
                                        }
                                    },
                                "incidence": 0.75,
                                },
    ),

)

