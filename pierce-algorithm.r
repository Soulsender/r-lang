# sample
detect_outlier <- function(sample, sample_label) {
        # deviations
        deviation_item <- c()
        deviation_index <- c()

        # results
        results <- c()

        # R value
        R <- 1.878
        # from the table, 1 doubtful per 10 observed so use 1.878

        # get mean
        average <- mean(sample)
        # get standard deviation
        standard_dev <- sd(sample)
        # get allowed deviation
        allowed_dev <- (standard_dev * R)

        # calculate unacceptable outliers
        plot(sample)
        index <- 0
        for (item in sample) {
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
        plot(sample, 
                main=sample_label,
                xlab = "Variable of Interest",
                ylab = "Observation Index"
        )

        # plot deviations
        points(y=deviation_item, x=deviation_index, col="red", pch=20, cex=2)
}


sample <- c(17.2295604,  18.86286086, 22.10660916, 20.34317271, 42.31264902,
             25.8205763,  18.414105,   20.34649474, 21.94089558, 21.29394188,
             22.39554295, 22.39805575, 26.69307004, 19.87572175, 16.73891421,
             16.08649209, 22.16448119, 20.8413674,  18.98891486, 18.50224649,
             20.09286905, 21.60340814, 17.61338842, 20.2410012,  20.19028432,
             10.97335752, 18.59875957, 19.2927548,  22.92002637, 20.801476,
             18.81468756, 18.02019679, 24.10008547, 21.61994415, 16.89897105,
             13.81548718, 20.99336878, 21.77275475, 8.604239269, 19.06038127)


detect_outlier(sample, "Outliers")