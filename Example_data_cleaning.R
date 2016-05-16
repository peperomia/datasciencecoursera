
#Example data cleaning

data <- read.delim("DOT - Cleaned Data.txt", header=TRUE, stringsAsFactors=FALSE, skip = 1)
nms <- names(data)

source("Cleanup_functions.R")

# access to devices
(levels_access.txt <- grep("What.devices.do.you.have.access.to", nms, value = TRUE))
(levels_access <- grep("What.devices.do.you.have.access.to", nms, value = FALSE))
access.vars <-data[, levels_access]

(levels_devices <- gsub("What.devices.do.you.have.access.to....", "", levels_access.txt, fixed = TRUE))

bb <- regexpr("((.*?)\\.\\.)", levels_devices, perl = TRUE)   #ok
(devices1 <- unique(regmatches(levels_devices, bb)))
(devices2 <- sub("\\.\\.", "", devices1, perl = TRUE))
(devices <- gsub("\\.", " ", devices2, perl = TRUE))

vv <- regexpr("(\\.\\.(.*?)*$)", levels_devices, perl = TRUE)    #ok
(frequency_devices1 <- unique(regmatches(levels_devices, vv)))
frequency_devices <- frequency_devices1
(frequency_devices[c(1, 9)] <- c("..Multiple.times.a.day.", "I.don't.know."))
(frequency_devices <- sub("\\.\\.", "", frequency_devices,perl = TRUE))
(frequency_devices <- gsub("\\.", " ", frequency_devices))

"Multiple times a day " %in% frequency_devices
"Multiple times a day" %in% frequency_devices
#removing trailing white space

(frequency_devices <- str_trim(frequency_devices))
"Multiple times a day" %in% frequency_devices

#srtd <- sortBy.things(things=devices2, data = access.vars, namesData=names(access.vars))

#bbb <- count.doubles(data = cafe, nms, handle.doubles = "last") 
#(mmm <- bbb[[3]][[552]])
(nmx <- grep(devices1[1], names(access.vars), ignore.case = TRUE, value = T))

accessSortByThings <- sortBy.things(things = devices2, data = access.vars, namesData = names(access.vars))
accessSortByFreq <- sortBy.freq(sorted = accessSortByThings, frequency = frequency_devices1, frequency.clean=frequency_devices, handle.doubles = "first")
names(accessSortByThings)

(access.dev.names <- paste("Acces to device", devices, sep = ": "))
access.dev.vars <- dataframing(data = accessSortByFreq$sortedData, names = access.dev.names)
