# readings
readings <- c(101.2, 90.0, 99.0, 102.0, 103.0, 100.2, 89.0, 98.1, 101.5, 102.0)

# deviations
deviation_item <- c()
deviation_index <- c()

# results
results <- c()

# R value
R <- 1.878
# from the table, 1 doubtful per 10 observed so use 1.878

# get mean
average <- mean(readings)
# get standard deviation
standard_dev <- sd(readings)
# get allowed deviation
allowed_dev <- (standard_dev * R)

# calculate unacceptable outliers
plot(readings)
index <- 0
for (item in readings) {
        index <- index + 1
	actual_dev <- abs(item - average)
	if (actual_dev > allowed_dev) {
                deviation_item <- c(deviation_item, item)
                deviation_index <- c(deviation_index, index)
                points(item, index, col="red", pch=20)
	} else {
                results <- c(results, item)
    }
}

# plot on graph
plot(readings)
points(y=deviation_item, x=deviation_index, col="red", pch=20)