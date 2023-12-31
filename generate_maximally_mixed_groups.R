
generate_maximally_mixed_groups <- function(x, group_size, ncycles = 2) {

    # sanity checking
    if (!is.vector(x)) {
        stop("x must be a vector")
    }

    if (is.list(x)) {
        x <- unlist(x)
    }
    
    if (class(group_size) == "numeric") {
        group_size <- as.integer(group_size)
    }
    
    if (class(ncycles) == "numeric") {
        ncycles <- as.integer(ncycles)
    }
    
    if (length(group_size) != 1 | group_size < 0L) {
        stop("group_size must be a positive integer")
    }
    
    if (length(x) < group_size) {
        stop("length(x) must be greater than group_size")
    }

    if (!is.integer(ncycles) | length(ncycles) != 1 | ncycles < 0L) {
        stop("ncycles must be a positive integer")
    }
    
    # generates a vector with numbers in x, repeated size number of times,
    # and then shuffles that vector. 
    sample2 <- function(x, size) {
        y <- rep(x, times = size)
        y[sample(1:length(y), size = length(x) * size)]
    }
    
    # calculate the number of groups we can form from the desired group size.
    # if the total number of participants does not divide evenly into that
    # group size, round up to the nearest integer
    ngroups <- ceiling(length(x) / group_size)

    # then calculate the number of additional participants we'd need to
    # fill up all groups
    remainder <- ngroups * group_size - length(x)

    # Add placeholder persons so that we can run the algorithm more conveniently.
    # Placeholders will be removed before returning the results.
    if (remainder > 0) {
        x <- c(x, sprintf("_NA%d", 1:remainder))
    }

    # set up a matrix to track the co-occurrence of pairs of participants
    track <- matrix(0L, nrow = length(x), ncol = length(x))
    
    # set up a data.frame to hold the group structures,
    # with participants in rows and cycles in columns
    # We'll assign values to this data.frame based on group membership
    groups <- matrix(NA_integer_, ncol = ncycles, nrow = length(x))
    groups <- as.data.frame(groups)
    
    # for the first group, we can just sample randomly from our
    # pool of participants
    groups[[1]] <- sample2(1:ngroups, size = group_size)

    # but for the remaining groups, we want to avoid putting the 
    # same people in with people they've been with previously
    # to the maximum degree possible
    if (ncycles > 1) {
        for (i in 2:ncycles) {
    
            # update co-occurrences based on groups in the last cycle
            for (i2 in 1:ngroups) {
                j <- which(groups[[i-1]] == i2)
                track[j, j] <- track[j, j] + 1
            }
            
            # Here's the algorithm:
            # For each group, do this:
            #  1) pick one person, consider that person fixed.
            #  2) make a list of all persons with the smallest number of 
            #     co-occurrences in previous cycles, with the person(s) that
            #     has (have) already been picked. Pick one person at random from
            #     that list.
            #  3) repeat step 2 until we reach the desired group size.
    
            for (gn in 1:ngroups) { # gn = group number
                fp <- sample(which(is.na(groups[[i]])), size = 1) # fixed person
                groups[[i]][fp] <- gn # add that person to current group
    
                # while we haven't reached the desired group size...
                while (sum(groups[[i]] == gn, na.rm=T) < group_size) {
    
                    # people in current group.
                    in_group <- which(groups[[i]] == gn)
                    
                    # people that aren't in any group yet.
                    # these are available to be picked
                    available <- which(is.na(groups[[i]]))
                    
                    # get the overlaps for the people already in the group
                    overlap <- track[in_group,]
                    # add overlaps together for all people
                    if (length(in_group) > 1) {
                        overlap <- colSums(overlap)
                    }
                    
                    lowest_overlap <- min(overlap[available])[1]
                    
                    # construct a list of candidates
                    candidates <- 1:length(x)
                    candidates <- candidates[overlap == lowest_overlap & sapply(1:length(x), `%in%`, available)] 
                    
                    # sample one person from that list
                    # We only sample one person at at time, because we want to 
                    # take co-occurrences for that person into account as well 
                    # when considering the next person
                    np <- sample.int(length(candidates), size = 1)
                    np <- candidates[np]
    
                    # add that person to current group
                    groups[[i]][np] <- gn
                }
            }
        }
    }

    # Some restructuring to make results more interpretable to human eyes
    setNames(lapply(1:ncycles, function(cycle) {
        h <- lapply(1:ngroups, function(group) {
            x[groups[[cycle]] == group]
            #data.table(group = group, participant = participants)
        }) |> do.call(what = rbind) |> t()
        h[grepl("^_NA[0-9]+", h)] <- "-"
        colnames(h) <- sprintf("group%d", 1:ngroups)
        h
    }), sprintf("cycle%d", 1:ncycles))
}

x <- c("Arne", "Freddie", "Stine", "Tore", "John-André", "Martin K", "Lotta", 
       "Uffe", "André", "Kjell", "Michael", "Kathrine", "Hans", "Hiroko","Nils")

# let's have groups with up to 4 members over 3 cycles
generate_maximally_mixed_groups(x, group_size = 4, ncycles = 3)

# or groups with 5 members over 3 cycles
generate_maximally_mixed_groups(x, group_size = 5, ncycles = 3)

# we can have lots of people in small groups
generate_maximally_mixed_groups(1:100, group_size = 10, ncycles = 3)

# we can have gigantic groups
generate_maximally_mixed_groups(1:100, group_size = 25, ncycles = 2)










