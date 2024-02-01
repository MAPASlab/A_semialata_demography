# read command line
args <- commandArgs(trailingOnly = TRUE)

# get filenames to read list and depth files
list <- as.character(args[1])  # file with the list of samples to use
print(list)
depth <- as.character(args[2]) # file with mean depth per sample
print(depth)
propinds <- as.numeric(args[3]) # proportion of samples with data for a site to be considered
print(propinds)

samples <- read.table(list, stringsAsFactor = FALSE, header = FALSE)
print(nrow(samples))
dps <- read.table(depth, sep = "\t", stringsAsFactor = FALSE, header = FALSE)
print(nrow(dps))

samples_dps <- c()

for (i in 1:nrow(samples)) {

	for (j in 1:nrow(dps)) {

		if (samples[i,1]==dps[j,1]) {
			samples_dps <- c(samples_dps, dps[j,2])
		}
	}

}

# calculate min number of samples with data for a site to be considered
minind <- round((nrow(samples))*propinds)
# sum mean dp across samples and calculate min and max dps as 1/2 and *2 of that value
depth_value <- sum(samples_dps)
print(depth_value)
mindp <- round(depth_value/2)
print(mindp)
maxdp <- round(depth_value*2)
print(maxdp)
depth_range <- c(mindp, maxdp, minind)

# write output to a file, as a table with three columns 
write(depth_range, file = "/path/to/depth_range.txt", ncolumns = 3, append = FALSE, sep = "\t")

