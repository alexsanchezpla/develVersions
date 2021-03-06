contractedProfile <- function(prof, nams = NULL) {
  UseMethod("contractedProfile")
}

contractedProfile.ExpandedGOProfile <- function(prof, nams = NULL)  {
  if (!is.null(nams)) {
    if (nrow(prof) != length(nams)) stop("'length(nams)' and 'nrow(prof)' must be equal in method 'contractedProfile.ExpandedGOProfile'")
  }
  else
    nams <- rownames(prof)
  freqs <- as.data.frame(lapply(1:length(prof), icolContract, prof, nams))
  result <- cbind(character(nrow(freqs)), as.factor(rownames(freqs)), freqs)
  colnames(result) <- c("Description", "GOID", "Frequency")
  class(result) <- c("BasicGOProfile", class(result))
  attr(result, "numGenes") <- sapply(freqs, attr, "ngenes")
  names(attr(result, "numGenes")) <- NULL
  attr(result, "numNAs") <- sapply(freqs, attr, "numNAs")
  names(attr(result, "numNAs")) <- NULL
  return(result)
}

contractedProfile.default <- function(prof, nams = NULL) {
    pi. <- contractVector(prof, nams)
    result <- cbind(character(length(pi.)), as.factor(names(pi.)), as.data.frame(pi.))
    colnames(result) <- c("Description", "GOID", "Frequency")
    attr(result, "numGenes") <- attr(pi., "ngenes")
    attr(result, "numNAs") <- attr(pi., "numNAs")
    class(result) <- c("BasicGOProfile", class(result))
    return(result)
}

contractVector <- function(prof, nams = NULL) {
    if (is.null(nams))
      nams <- names(prof)
    else
      if (length(nams) != length(prof))
        stop("'length(nams)' and 'length(prof)' must be equal in method 'contractedProfile.default'")
    setNamesList <- strsplit(nams,"\\.")
    pi.labels <- unique(unlist(setNamesList))
    len.pi. <- length(pi.labels)
#    present.in <- matrix(unlist(lapply(as.numeric(pi.labels), belongs.to, setNamesList)), nrow=len.pi., byrow=T)
    present.in <- matrix(unlist(lapply(pi.labels, belongs.to, setNamesList)), nrow=len.pi., byrow=T)
    pi. <- sapply(1:len.pi., sum.if, present.in, prof)
    if (!all.wholenumber(pi.)) pi. <- pi. * attr(prof,"ngenes")
    names(pi.) <- pi.labels
    attr(pi.,"ngenes") <- attr(prof,"ngenes")
    return(pi.)
}

icolContract <- function(icol, prof, nams) {
  profVec <- prof[[icol]]
  names(profVec) <- nams
  contractVector(profVec)
}

