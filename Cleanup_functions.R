count.doubles <- function(data, nms, handle.doubles =  c("first", "last")) {
    # counts double entries where there should be only one and returns a list of 
    # data <- some data frame
    #nms = a vector of names
    listed <- apply(data, MARGIN = 1, function(x) nms[which(x == 1)])
    count <- which(lapply(listed, length) > 1)
    if (handle.doubles == "first") {
        single.opt <- lapply(listed, function(x) x[1])
    } else if (handle.doubles == "last") {
        single.opt <- lapply(listed, function(x) rev(x)[1])
    } else {
        single.opt <- listed
    }
    
    return(list(count, listed[count], single.opt))
}

see.doubles <- function(data, nms) {
    # data <- some data frame
    #nms = a vector of names
    listed <- apply(data, MARGIN = 1, function(x) nms[which(x == 1)])
    count <- which(lapply(listed, length) > 1)
    return(list(count, listed[count]))
}

#bbb <- count.doubles(data = cafe, nms, handle.doubles = "last") 
#(mmm <- bbb[[3]][[552]])

sortBy.things <- function(things, data, namesData) {
    # things = devices     
    # data = subset of a dataset(like access.vars) namesData=names(access.vars)
    # namesData -> vector of full variable names(like levels_access.txt)
    byThingsData <- list()
    listed.ns <- list()
    for (i in 1:length(things)) {
        names.indx <- grep(things[i], namesData, value = FALSE)
        first <-data[, grep(things[i], namesData, value = FALSE)]
        listed.ns[[i]] <- names.indx
        byThingsData[[i]] <- first
        names(byThingsData)[i] <- things[i]
    }
    return(byThingsData)
}

#srtd <- sortBy.things(things=devices2, data = access.vars, namesData=names(access.vars))


#(nmx <- grep(devices1[1], names(access.vars), ignore.case = TRUE, value = T))

sortBy.freq <- function(sorted, frequency, frequency.clean, handle.doubles=c("first", "last")) {
    ### sorted <- data returned from sortBy.things() function
    # frequency <- frequency_devices
    byFreqData <- list()
    cnt.doubles <- list()
    for (i in 1:length(sorted)) {
        #names(byFreqData)[i] <- names(sorted)[i]
        freq <- apply(sorted[[i]], MARGIN = 1, function(x) frequency.clean[which(x == 1)])
        is.na(freq) <- which(lapply(freq, length) < 1)
        cnt.doubles[[i]] <- see.doubles(data = sorted[[i]], nms = frequency.clean)
        names(cnt.doubles)[[i]] <- names(sorted)[i]
        if (length(cnt.doubles[[i]][[1]]) >= 1) {
            print(paste("There are multiple entries for frequency for ", names(sorted)[i]))
            byFreqData[[i]] <- unlist(count.doubles(data = sorted[[i]], nms = frequency.clean, handle.doubles = handle.doubles)[[3]])
        } else {
            byFreqData[[i]] <- unlist(freq)
            names(byFreqData)[[i]] <- names(sorted)[i]
        }
        
    }
    return(list(sortedData = byFreqData, doubles = cnt.doubles))
}

dataframing <- function(data, names) {
    # puts output from sortBy.freq() function into data.frame format and gives it nice varnames
    # data = some_name$sortedData, it is a **list**
    # names = varnames you want final data.frame to have
    # returns a dataframe of variables that were cleaned and sorted with sortBy.freq() function, with proper variable names
    df <- as.data.frame(matrix(nrow = 567, ncol = length(data)))
    for (i in 1:length(data)) {
        df[i] <- data[[i]]
    }
    names(df) <- names
    return(df)
}


ordering.vars <- function(data, ord) {
    #puts levels of variables in data (=dataframe) in order
    #data = a dataframe of several variables from the same question, that have the same levels.
    #ord = a vector of labels for the levels and their order
    ret.df <- as.data.frame(matrix(nrow = nrow(data), ncol = ncol(data)))
    for (column in 1:ncol(data)) {
        ordered.var <- factor(data[, column], levels =ord, labels = ord, exclude = NULL, ordered = as.ordered(data[, column]))
        ret.df[, column] <- ordered.var
    }
    return(ret.df)
    
}

varname.extra <- seq(1, 10, 1)
