library(lubridate)

time_str <- "18H 45M 0S"

# Convert the time string to a period object
time_period <- hm(time_str)

hm("18 45")

as.period("18H 45M 0S")

# Convert the period object to a POSIXlt object with a specific date
time_posix <- as.POSIXlt("2023-05-12") + time_period


time_posix <- as.POSIXlt("2023-05-12") + as.period("18H 45M 0S")

# Format the POSIXlt object as desired
formatted_time <- format(time_posix, "%I:%M%p")

# Print the formatted time
print(formatted_time)


format("18H 45M 0S", "%I:%M%p")
