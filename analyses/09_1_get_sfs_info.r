# read command line
args <- commandArgs(trailingOnly = TRUE)

# get sfs file
file <- as.character(args[1])

# read file and calculate parameters
pop <- gsub(".sfs", "", file, fixed = TRUE)
 
full_sfs <- read.table(file, header = F)
ind <- (length(full_sfs)-1)/2
seq <- length(full_sfs)-1
len <- round(sum(full_sfs[1,]))	
breakpoint_1 <- round((seq-2)/4)
breakpoint_2 <- round((seq-2)/2)
breakpoint_3 <- round((seq-2)*3/4)
breakpoint_4 <- (seq-2)
freq <- full_sfs[1,1:ind+1]

# check calculated parameters
print(pop)
print(paste(ind, seq, len))
print(paste(breakpoint_1, breakpoint_2, breakpoint_3, breakpoint_4))
print(freq)
print(full_sfs)

# store parameters in a single vector
sfs_info <- c(pop, seq, len, breakpoint_1, breakpoint_2, breakpoint_3, breakpoint_4, freq)

# write output to a file, as a table with 8+ columns 
write.table(sfs_info, file = "sfs_info.txt", quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)

