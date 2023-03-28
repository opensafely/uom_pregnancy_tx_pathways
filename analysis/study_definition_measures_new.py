# For multiple del codes in the same month 
# add to existing measures rather than make separate?
#
#  Note: if you want it to extract to end of study period, 
#  then replace "last_day_of_month(index_date)" with 
#  end date (e.g., 2022-12-31)

# First occurrence of code in month 
# delivery_code_date_1=patients.with_these_clinical_events(
#     delivery_codes,
#     between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"],
#     returning="date",
#     date_format="YYYY-MM-DD",
#     find_first_match_in_period=True,
#     return_expectations={           
#         "date": {
#             "earliest": "2019-01-01",  
#             "latest": "today",
#             },
#         },    
#     ),

## Second occurrence of code in month
# delivery_code_date_2=patients.with_these_clinical_events(
#     delivery_codes,
#     between=["delivery_code_date_1", "last_day_of_month(index_date)"],
#     returning="date",
#     date_format="YYYY-MM-DD",
#     find_first_match_in_period=True,
#     return_expectations={           
#         "date": {
#             "earliest": "2019-01-01",  
#             "latest": "today",
#             },
#         },    
#     ),
#
# Third code in month
# delivery_code_date_3=patients.with_these_clinical_events(
#     delivery_codes,
#     between=["delivery_code_date_2", "last_day_of_month(index_date)"],
#     returning="date",
#     date_format="YYYY-MM-DD",
#     find_first_match_in_period=True,
#     return_expectations={           
#         "date": {
#             "earliest": "2019-01-01",  
#             "latest": "today",
#             },
#         },    
#     ),
#
# Fourth
# delivery_code_date_4=patients.with_these_clinical_events(
#     delivery_codes,
#     between=["delivery_code_date_3", "last_day_of_month(index_date)"],
#     returning="date",
#     date_format="YYYY-MM-DD",
#     find_first_match_in_period=True,
#     return_expectations={           
#         "date": {
#             "earliest": "2019-01-01",  
#             "latest": "today",
#             },
#         },    
#     ),
#
# Fifth
# delivery_code_date_5=patients.with_these_clinical_events(
#     delivery_codes,
#     between=["delivery_code_date_4", "last_day_of_month(index_date)"],
#     returning="date",
#     date_format="YYYY-MM-DD",
#     find_first_match_in_period=True,
#     return_expectations={           
#         "date": {
#             "earliest": "2019-01-01",  
#             "latest": "today",
#             },
#         },    
#     ),