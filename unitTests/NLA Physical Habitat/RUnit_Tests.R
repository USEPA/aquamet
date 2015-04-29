# Create a text file for intermediate messages 
sink("RUnit_Tests.txt")

# Load required packages
require(foreach)
require(Hmisc)
require(aquamet)
require(RUnit)

# Print the date and session information
cat(date(), "\n\n")
cat("Session Information:\n\n")
sessionInfo()

# Run the unit tests
testSuiteNLAMetrics()

# Close the output text file
sink()
