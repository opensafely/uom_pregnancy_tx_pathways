
##### Pregnancy Short Data Review #####
##### Starting cohort for 2023 #####
##### interested in frequency of pregnancy related codes and outcomes ####
##### based on CPRD paper https://doi.org/10.1002/pds.4811 ####
##### Read converted to SNOMED and outcomes reviewed ####

from cohortextractor import ( 
    StudyDefinition, 
    patients, 
    codelist, 
    codelist_from_csv,  
    #Measure,
)

# Import codelists from codelist.py (which pulls them from the codelist folder)
from codelist import *
from datetime import datetime

# STUDY POPULATION

start_date = "2023-01-01"
end_date = "2024-01-01"
min_age = 14
max_age = 50

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
        (age >= min_age AND age < max_age)
        AND
        has_follow_up_previous_year
        AND
        has_delivery_code
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

        has_delivery_code=patients.with_these_clinical_events(
            delivery_codes_reviewed_2,
            between=[start_date, end_date]
        ),

    ),

    
    age=patients.age_as_of(
        "start_date",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),


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
    

    
    # Date of first delivery code in present year ("index delivery")
    delivery_code_date=patients.with_these_clinical_events(
    delivery_codes_reviewed_2,
    between=["start_date", "end_date"], 
    returning="date",
    date_format="YYYY-MM-DD",
    find_last_match_in_period=False,
    return_expectations={           
        "date": {
            "earliest": start_date,  
            "latest": end_date,
            },
        "incidence": 0.9
        },    
    ),

    # Returns delivery code - first code
    delivery_code=patients.with_these_clinical_events(
    delivery_codes_reviewed_2,
    between=["start_date", "end_date"], 
    find_last_match_in_period=False,
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
        "incidence": 0.95
      },
    ),

    # Number of delivery CODES per person - around the time of the index delivery
    delivery_code_number=patients.with_these_clinical_events(
    delivery_codes_reviewed_2,
    on_or_after = "delivery_code_date + 84 days",
    returning="number_of_matches_in_period",
    return_expectations={
       "int": {"distribution": "normal", "mean": 1, "stddev": 1},
       "incidence": 0.6
       },
    ),
    

    # Number of PRIOR delivery EPISODES per person - at least 84 days apart
    delivery_code_number=patients.with_these_clinical_events(
    delivery_codes_reviewed_2,
    on_or_before = "delivery_code_date - 84 days",
    returning="number_of_episodes",
    episode_defined_as = "series of events each <= 84 days apart",
    return_expectations={
       "int": {"distribution": "normal", "mean": 1, "stddev": 1},
       "incidence": 0.6
       },
    ),


    # Is there pn code present - Y/N - after delivery
    PN_code=patients.with_these_clinical_events(
    postnatal_8wk_codes_reviewed,
    between=["delivery_code_date", "delivery_code_date + 84 days"], 
    returning="binary_flag",    
    return_expectations={
       #"int": {"distribution": "normal", "mean": 0.5, "stddev": 1},
       "incidence": 0.4
       },
    ),

    # first pn code
    postnatal_code=patients.with_these_clinical_events(
    postnatal_8wk_codes_reviewed,
    between=["delivery_code_date", "delivery_code_date + 84 days"],
    returning="code", 
    find_last_match_in_period=False,
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
        "incidence": 0.4
      },
    ),

    # Number of ANY antenatal codes per person - within 9 months before delivery
    antenatal_num=patients.with_these_clinical_events(
        antenatal_codes,
        between=["delivery_code_date-270 days", "delivery_code_date"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),  

    # Number of postterm codes per person
    postterm_num=patients.with_these_clinical_events(
        postterm_codes,
        between=["delivery_code_date - 30 days", "delivery_code_date+ + 3 days"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),  



    ####################################### 
    ### Outcomes from CPRD paper
    ####################################### 
    
    # Number of blighted ovum codes per persons
    blightedovum_num=patients.with_these_clinical_events(
        blighted_ovum,
        between=["delivery_code_date - 84 days", "delivery_code_date+ + 3 days"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),  

    ectopic_num=patients.with_these_clinical_events(
        ectopic,
        between=["delivery_code_date - 84 days", "delivery_code_date+ + 3 days"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),  

    miscarrage_num=patients.with_these_clinical_events(
        miscarrage,
        between=["delivery_code_date - 30 days", "delivery_code_date+ + 3 days"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),  

    molar_num=patients.with_these_clinical_events(
        molar,
        between=["delivery_code_date - 84 days", "delivery_code_date+ + 3 days"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ), 

    # Number of stillbirth codes per persons
    stillbirth_num=patients.with_these_clinical_events(
        stillbirth_codes,
        between=["delivery_code_date - 30 days", "delivery_code_date+ + 3 days"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),  

    ## any loss combining above 5 variables    
    loss_any_num=patients.with_these_clinical_events(
        loss_any_codes,
        between=["delivery_code_date - 84 days", "delivery_code_date+ + 3 days"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ), 

    ### multiples
    multips_num=patients.with_these_clinical_events(
        multi_pregnancy,
        between=["delivery_code_date - 84 days", "delivery_code_date+ + 3 days"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ), 

    ## codelist created using opencodelist searches
    ## preeclampsia, eclampsia
    preeclampsia_num=patients.with_these_clinical_events(
        preeclampsia,
        between=["delivery_code_date - 84 days", "delivery_code_date+ + 3 days"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ), 

    ### TOPS

    #tops only
    top_num=patients.with_these_clinical_events(
        tops,
        between=[start_date, end_date],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ), 
    
    top_probable_num=patients.with_these_clinical_events(
        tops_probable,
        between=[start_date, end_date],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ), 

    tops_any_num=patients.with_these_clinical_events(
        tops_any_codes,
        between=[start_date, end_date],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),

    ########################    
    ## dating related codes:
    lmp_num=patients.with_these_clinical_events(
        lmp_codes,
        between=[start_date, end_date],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),  

    edd_num=patients.with_these_clinical_events(
        edd_codes,
        between=[start_date, end_date],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),  

    edc_num=patients.with_these_clinical_events(
        edc_codes,
        between=[start_date, end_date],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "normal", "mean": 1, "stddev": 1},
            "incidence": 0.6
            },
        ),  

    # #### hypertension codes

    ### history of hypertension in pregnancy
    hbp_pregnancy=patients.with_these_clinical_events(
        hypertension_codes_preg,
        between=["delivery_code_date - 270 days", "delivery_code_date"],
        returning="binary_flag",
        find_first_match_in_period=True,
        return_expectations={"incidence": 0.1, "date": {"earliest": start_date}
        },
    ),

    # all hypertension code
    hbp_all=patients.with_these_clinical_events(
        hypertension_codes_all,
        between=["delivery_code_date - 270 days", "delivery_code_date"],
        returning="binary_flag",
        find_first_match_in_period=True,
        return_expectations={"incidence": 0.1, "date": {"earliest": start_date}
        },
    ),

    # any hypertension code - combined vars above
    hbp_any=patients.with_these_clinical_events(
        any_hypertension_code,
        between=["delivery_code_date - 270 days", "delivery_code_date"],
        returning="binary_flag",
        find_first_match_in_period=True,
        return_expectations={"incidence": 0.1, "date": {"earliest": start_date}
        },
    ),

    
)
