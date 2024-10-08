from cohortextractor import ( 
    StudyDefinition, 
    patients, 
    codelist, 
    codelist_from_csv,  
    Measure,
    params,
)

# Import codelists from codelist.py (which pulls them from the codelist folder)
from codelist import *

from datetime import datetime

# Import parameters
from cohortextractor import params 
...

num_days = params['num_days']

#STUDY POPULATION

start_date = "2019-01-01"
end_date = datetime.today().strftime('%Y-%m-%d')
#index_year = 2019
min_age = 14
max_age = 49

# DEFINE STUDY POPULATION ---
study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.1,
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
        (age >= 14 AND age < 50)
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

    #need to define categories
    age_cat=patients.categorised_as(
        {
            "0":"DEFAULT",
            #"0-13": """ age >= 0 AND age < 13""",
            "14-19": """ age >= 14 AND age < 20""",
            "20-24": """ age >= 20 AND age < 25""",
            "25-29": """ age >= 25 AND age < 30""",
            "30-34": """ age >= 30 AND age < 35""",
            "35-39": """ age >= 35 AND age < 40""",
            "40-44": """ age >= 40 AND age < 45""",
            "45-49": """ age >= 45 AND age < 50""",
            #"50+": """ age >= 50 """,
        },
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0,
                    #"0-13": 0.12, 
                    "14-19": 0.05,
                    "20-24": 0.15,
                    "25-29": 0.15,
                    "30-34": 0.40,
                    "35-39": 0.10,
                    "40-44": 0.10,
                    "45-49": 0.05,
                    #"50+": 0.11,
                }
            },
        },
    ),
    
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.00, "F": 1.0}},
        }
    ),



        ### Ethnicity (6 categories)
    ethnicity = patients.categorised_as(
        {
            "Unknown": "DEFAULT",
            "White": "eth6='1'",
            "Mixed": "eth6='2'",
            "Asian or Asian British": "eth6='3'",
            "Black or Black British": "eth6='4'",
            "Other": "eth6='5'",
        },
        eth6 = patients.with_these_clinical_events(
            ethnicity_codes_6,
            returning = "category",
            find_last_match_in_period = True,
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
                    "White": 0.30,
                    "Mixed": 0.20,
                    "Asian or Asian British": 0.20,
                    "Black or Black British": 0.20,
                    "Other": 0.05,
                    "Unknown": 0.05,
                },
            },
        },
    ),
    

    practice=patients.registered_practice_as_of(
            "index_date",
            returning="pseudo_id",
            return_expectations={"int": {"distribution": "normal",
                                        "mean": 25, "stddev": 5}, "incidence": 1}
    ),
    
    ### Region - NHS England 9 regions
    region=patients.registered_practice_as_of(
        "index_date",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                  "North East": 0.1,
                  "North West": 0.1,
                  "Yorkshire and The Humber": 0.1,
                  "East Midlands": 0.1,
                  "West Midlands": 0.1,
                  "East": 0.1,
                  "London": 0.2,
                  "South West": 0.1,
                  "South East": 0.1, }, },
        },
    ),

    imd=patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
            "5": """index_of_multiple_deprivation >= 32844*4/5 AND index_of_multiple_deprivation < 32844""",
        },
        index_of_multiple_deprivation=patients.address_as_of(
            "index_date",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0.05,
                    "1": 0.19,
                    "2": 0.19,
                    "3": 0.19,
                    "4": 0.19,
                    "5": 0.19,
                }
            },
        },
    ),   
    
    ## BMI, most recent
    bmi=patients.most_recent_bmi(
        between=["2015-01-01", "index_date"],
        minimum_age_at_measurement=18,
        include_measurement_date=True,
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": "index_date"},
            "float": {"distribution": "normal", "mean": 27, "stddev": 6},
            "incidence": 0.70,
        },
    ),

    # Number of delivery codes per person
    delivery_code_number=patients.with_these_clinical_events(
    delivery_codes_reviewed_2,
    between=["index_date", "last_day_of_month(index_date)"],
    returning="number_of_matches_in_period",
    return_expectations={
       "int": {"distribution": "normal", "mean": 4, "stddev": 1},
       "incidence": 0.4,
       },
    ),

    # Date of last delivery code
    delivery_code_date=patients.with_these_clinical_events(
    delivery_codes_reviewed_2,
    between=["index_date", "last_day_of_month(index_date)"],
    returning="date",
    date_format="YYYY-MM-DD",
    find_last_match_in_period=True,
    return_expectations={           
        "date": {
            "earliest": "2019-01-01",  
            "latest": "2023-05-31",
            },
        "incidence": 0.1,
        },    
    ),

    # Is there a delivery code present - Y/N
    delivery_code_present=patients.with_these_clinical_events(
    delivery_codes_reviewed_2,
    between=["index_date", "last_day_of_month(index_date)"],
    returning="binary_flag",    
    return_expectations={
       #"int": {"distribution": "normal", "mean": 4, "stddev": 1},
       "incidence": 0.2,
       },
    ),

    # Returns delivery code - last code by default
    # can use find_first_match_in_period = True or find_last_match_in_period = True
    # can also add include_date_of_match to get the date
    delivery_code=patients.with_these_clinical_events(
    delivery_codes_reviewed_2,
    between=["index_date", "last_day_of_month(index_date)"],
    returning="code", 
    return_expectations={
        "category": {
            "ratios": {
            "10217006":  0.2,
            "106007006": 0.2,
            "109894007": 0.2,
            "111453004": 0.2,
            "1125006": 0.2,
            }
        },
        "incidence": 0.7,
      },
    ),

    ## next three variables are the same but for different codelists
    ## use postnatal_8wk_code initially, need to review others

    # Number of pn codes codes per person
    pn8wk_code_number=patients.with_these_clinical_events(
    postnatal_8wk_codes_reviewed,
    between=["index_date", "last_day_of_month(index_date)"],
    returning="number_of_matches_in_period",
    return_expectations={
       "int": {"distribution": "normal", "mean": 4, "stddev": 1},
       "incidence": 0.4,
       },
    ),

    postnatal_8wk_code_present=patients.with_these_clinical_events(
    postnatal_8wk_codes_reviewed, 
    between=["delivery_code_date", f"delivery_code_date + {num_days} days"],
    #between=["delivery_code_date", "delivery_code_date + 84 days"],
    returning="binary_flag",
    return_expectations={
            "incidence": 0.3,},
    ),

    postnatal_code=patients.with_these_clinical_events(
    postnatal_8wk_codes_reviewed,
    between=["delivery_code_date", f"delivery_code_date + {num_days} days"],
    returning="code", 
    return_expectations={
        "category": {
            "ratios": {
            "10588007":  0.2,
            "10601012007": 0.2,
            "1089000": 0.2,
            "160763002": 0.2,
            "13094009": 0.2,
            }
        },
        "incidence": 0.4,
      },
    ),
 

    ## use below codes for pn code within 8 weeks and 6 weeks 
    #postnatal_8wk_code_present_8wks=patients.with_these_clinical_events(
    #postnatal_8wk_codes, 
    #between=["delivery_code_date", "delivery_code_date + 56 days"],
    #returning="binary_flag",
    #return_expectations={  
    #   "int": {"distribution": "normal", "mean": 4, "stddev": 1},
    #     "incidence": 0.4,
    #     },
    #),

    #postnatal_8wk_code_present_6wks=patients.with_these_clinical_events(
    #postnatal_8wk_codes, 
    #between=["delivery_code_date", "delivery_code_date + 42 days"],
    #returning="binary_flag",
    #return_expectations={  
    #   "int": {"distribution": "normal", "mean": 4, "stddev": 1},
    #     "incidence": 0.4,
    #     },
    #),

    postnatal_other_code_present=patients.with_these_clinical_events(
    postnatal_other_codes,
    between=["delivery_code_date", "delivery_code_date + 84 days"],
    returning="binary_flag",
    return_expectations={
            "incidence": 0.3,},
    ),

    postnatal_antenatal_code_present=patients.with_these_clinical_events(
    postdel_antenatal_codes,
    between=["delivery_code_date", "delivery_code_date + 84 days"],
    returning="binary_flag",
    return_expectations={
            "incidence": 0.3,},
    ),

    
)



measures = [

    # rate of postnatal codes over time by delivery code
    Measure(id="postnatal_check_rate",
            numerator="postnatal_8wk_code_present",
            denominator="population",
            group_by=["delivery_code_present"]
            ),

    # rate of postnatal codes over time by delivery code, age_cat
    Measure(id="postnatal_check_rate_by_age_cat",
            numerator="postnatal_8wk_code_present",
            denominator="population",
            group_by=["delivery_code_present", "age_cat"]
            ),

    # rate of postnatal codes over time by delivery code, practice
    Measure(id="postnatal_check_rate_by_practice",
            numerator="postnatal_8wk_code_present",
            denominator="population",
            group_by=["delivery_code_present", "practice"]
            ),
    
    # rate of postnatal codes over time by delivery code, imd
    Measure(id="postnatal_check_rate_by_imd",
            numerator="postnatal_8wk_code_present",
            denominator="population",
            group_by=["delivery_code_present", "imd"]
            ),

    # rate of postnatal codes over time by delivery code, ethnicity
    Measure(id="postnatal_check_rate_by_ethnicity",
            numerator="postnatal_8wk_code_present",
            denominator="population",
            group_by=["delivery_code_present", "ethnicity"]
            ),

    # rate of postnatal codes over time by delivery code, region
    Measure(id="postnatal_check_rate_by_region",
            numerator="postnatal_8wk_code_present",
            denominator="population",
            group_by=["delivery_code_present", "region"]
            ),

]