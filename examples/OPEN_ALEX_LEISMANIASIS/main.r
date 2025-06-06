# --------------------------------------------------- #
# -- SystematicReview.r ----------------------------- #
# --------------------------------------------------- #

# Source the SystematicReview class definition
source('../../src/SystematicReviewClass.r')

# SystematicReview Class
systematicReview <- SystematicReview$new()

# Add Config Settings
systematicReview$setBibPath("data/open_alex.csv")
systematicReview$setTitle("Leishmaniasis visceral elimination from 2020 to 2024")
systematicReview$setDate("Wednesday, January 26, 2025 1:50:56 AM")
systematicReview$setQuery("TITLE-ABS-KEY ( Leishmaniasis AND visceral AND elimination ) from 2020 to 2024")
systematicReview$setKeywords(c("Leishmaniasis", "visceral", "elimination"))

# Load and Init Data
systematicReview$init()

# Check Status and Requirements
systematicReview$do_m0_check_health_status()
systematicReview$do_m0_check_required_columns()
systematicReview$do_m0_cleaning_data()

# Modules
systematicReview$do_m1_main_information()
#systematicReview$do_m2_author_prod_over_time_regression()
#systematicReview$do_m3_authors()
# systematicReview$do_m4_documents()
# systematicReview$do_m5_clusterings()
# systematicReview$do_m6_conceptual_structure()
# systematicReview$do_m7_social_structure()

# Create Report
# systematicReview$do_m8_report()

# systematicReview$do_m9_save()