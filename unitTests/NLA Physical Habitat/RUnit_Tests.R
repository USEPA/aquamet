# # Create a text file for intermediate messages 
# # 10/19/15 cws COMMENTED OUT IN ENTIRETY
# sink("RUnit_Tests.txt")
# 
# # Load required packages
# require(foreach)
# require(Hmisc)
# require(aquamet)
# require(RUnit)
# 
# # Print the date and session information
# cat(date(), "\n\n")
# cat("Session Information:\n\n")
# sessionInfo()
# 
# # Run the unit tests
# testSuiteNLAMetrics()
# 
# # Close the output text file
# sink()
