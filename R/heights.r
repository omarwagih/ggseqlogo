

#' Priors for amino acids and dna
# AA_PRIORS  =   list(human = c(A=0.070, R=0.056, N=0.036, D=0.048,C=0.023,
#                               Q=0.047, E=0.071, G=0.066, H=0.026, I=0.044,
#                               L=0.100, K=0.058, M=0.021, F=0.037, P=0.063,
#                               S=0.083, T=0.053, W=0.012, Y=0.027, V=0.060),
#                     
#                     yeast = c(A=0.055, R=0.045, N=0.061, D=0.058, C=0.013,
#                               Q=0.039, E=0.064, G=0.05, H=0.022, I=0.066,
#                               L=0.096, K=0.073, M=0.021, F=0.045, P=0.044,
#                               S=0.091, T=0.059, W=0.01, Y=0.034, V=0.056))
# 
# DNA_PRIORS =   list(human = c(A=0.293, C=0.207, G=0.200, T=0.300),
#                     yeast = c(A=0.313, C=0.187, G=0.171, T=0.329),
#                     ecoli = c(A=0.247, C=0.260, G=0.257, T=0.236))

#' Namespaces
.AA_NAMESPACE = function() c('A', 'R', 'N', 'D', 'C', 'Q', 'E', 'G', 'H', 'I', 'L', 'K', 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V')
.DNA_NAMESPACE = function() c('A', 'T', 'G', 'C')
.RNA_NAMESPACE = function() c('A', 'U', 'G', 'C')


getPriors <- function(priors='eq', seq_type, namespace){
  
  
  # Number of letters in namespace
  N = length(namespace)
  
  # Type of sequence
  seq_type = toupper(seq_type)
  
  if(priors[1] == 'eq'){
    # If priors is equiprobable
    priors = rep(1/N, N)
    names(priors) = namespace
  }else{
    stop('can only have eq priors for now')
  }
  
  # else if(is.character(priors[1])){
  #   # No precomputed priors for other seq_types
  #   if(seq_type == 'OTHER') stop('Cannot provide organism for seq_type other')
  #   
  #   # We have an organism name, get precomputed priors
  #   organism = priors[1]
  #   priors.list = get(sprintf('%s_PRIORS', seq_type))
  #   
  #   # Can't find organism name
  #   if(! organism %in% names(priors.list) ) stop('Could not find organism in priors!')
  #   priors = priors.list[[organism]]
  # }
  
  # Ensure namespace and priors have same length
  if(length(priors) != N) stop('Priors and namespace must have same length')
  
  sum.pr = sum(priors) 
  if(sum.pr < 1 | sum.pr > 1.05) stop('Priors must sum up to 1')
  
  return(priors)
}

#' Generate letter matrix from vector of sequences
#' 
#' @param input vector of sequences
letterMatrix <- function(input){
  # Ensure kmers are the same length characters 
  seq.len = sapply(input, nchar)
  num_pos = seq.len[1]
  if(! all(seq.len == num_pos)) stop('Sequences in alignment must have identical lengths')
  
  # Construct matrix of letters
  split = unlist( sapply(input, function(seq){strsplit(seq, '')}) )
  
  m = t( matrix(split, seq.len, length(split)/num_pos) )
  m
}


#' Guess sequence type based on letter matrix
#' 
#' @param sp letters
guessSeqType <- function(sp){
  # Ensure we have something
  if(length( intersect(sp, c(.AA_NAMESPACE(), .DNA_NAMESPACE(),.RNA_NAMESPACE())) ) == 0)
    stop('Could not get guess seq_type. Please explicitly define sequence type or use "other" with custom namespaces.')
  
  dat = setdiff(intersect(sp, .AA_NAMESPACE()), c(.DNA_NAMESPACE(),.RNA_NAMESPACE()))
  if(length(dat) > 0){
    return('AA')
  }else if('U' %in% sp){
    return('RNA')
  }
  return('DNA')
}


#' Find namespace
#' 
#' @param letter_mat Matrix of latters
#' @param seq_type Sequence type
#' @param namespace Alphabet
findNamespace <- function(letter_mat, seq_type, namespace){
  
  # Get all letters in our alignment
  sp = as.character(letter_mat)
  
  # Other namespace
  if(seq_type == "other"){
    if(is.null(namespace)) 
      stop('seq_type of "other" must have a defined namespace')
    
    namespace = as.character(namespace)
    # Get unique
    namespace = unique( unlist(strsplit(namespace, '')) )
    
    # Validate
    non_alphanumeric = grepl("[^[:alnum:] ]", namespace)
    if( any( non_alphanumeric ) )
      stop('All letters in the namespace must be alphanumeric')
    
    # Ensure there is something in each column
    apply(letter_mat, 2, function(column_letters){
      int = intersect(namespace, column_letters)
      if(length(int) == 0)
        stop('The alignment has no letters in namespace match aligned sequences in at least one column')
    })
    
  }else{
    if(!is.null(namespace)) 
      stop('For custom namespaces please set seq_type to "other"')
    
    # Guess sequence type
    if(seq_type == "auto")
      seq_type = guessSeqType(sp)
    
    # Get predefined namespace
    namespace = get( sprintf('.%s_NAMESPACE', seq_type) )()
  }
  
  return(list(seq_type = seq_type, 
              namespace = namespace))
}

#' Calcualte bits
#' 
#' @param pwm Position weight matrix
#' @param N Number of letters in namespace
#' @param Nseqs Number of sequences in PWM
computeBits <- function(pwm, N=4, Nseqs=NULL){
  H_i = - apply(pwm, 2, function(col) sum(col * log2(col), na.rm=T))
  e_n = 0
  if(!is.null(Nseqs)) e_n = (1/logb(2)) * (N-1)/(2*Nseqs) 
 
  R_i = log2(N) - (H_i  + e_n)
  # Set any negatives to 0
  R_i = pmax(R_i, 0)
  return(R_i)
}

#' Construct relative frequency matrix 
#' 
#' @param seqs aligned sequences as vector
#' @param seq_type sequence type
#' @param priors piror probabilities 
#' @param namespace letters used for matrix construction
makePFM <- function(seqs, seq_type='auto', priors='eq', namespace=NULL){
  
  if(is.matrix(seqs)){
    # Process matrix
    if(is.null(rownames(seqs))) stop('Matrix must have letters for row names')
    
    num_pos = ncol(seqs)
    
    # Get namespace
    ns = findNamespace(rownames(seqs), seq_type, namespace)
    namespace = ns$namespace
    seq_type = ns$seq_type
    
    nseqs = NULL
    
    bg_prob = NA
    pfm_mat = seqs
    pfm_mat = apply(pfm_mat, 2, function(x) x / sum(x, na.rm=T))
    
    missing_rows = setdiff(namespace, rownames(pfm_mat))
    
    if(length(missing_rows) > 0){
      miss = matrix(rep(0, length(missing_rows) * ncol(pfm_mat)), nrow=length(missing_rows), dimnames = list(missing_rows))
      pfm_mat = rbind(pfm_mat, miss)
    }
    
    pfm_mat = pfm_mat[namespace,]
    
  }else{
    # Process sequences
    
    # Number of positions in alignment
    num_pos = nchar(seqs[1])
    # Number of sequences
    nseqs = length(seqs)
    # Letter matrix
    letter_mat = letterMatrix(seqs)
    
    
    # Get namespace
    ns = findNamespace(letter_mat, seq_type, namespace)
    namespace = ns$namespace
    seq_type = ns$seq_type
    
    # Get priors
    my_priors = getPriors(priors, seq_type, namespace)
    
    # Match priors to namespace 
    bg_prob = my_priors[match(namespace, names(my_priors))]
    
    # Construct PWM
    pfm_mat = apply(letter_mat, 2, function(pos.data){
      # Get frequencies 
      t = table(pos.data)
      # Match to aa
      ind = match(namespace, names(t))
      # Create column
      col = t[ind]
      col[is.na(col)] = 0
      names(col) = namespace
      # Do relative frequencies
      col = col / sum(col)
      col
    })
    
    mat = matrix((letter_mat %in% namespace), nrow=nrow(letter_mat))
    attr(pfm_mat, 'nongapped') = apply(mat, 2, sum)/nseqs
  }
  
  # Number of letters in ns
  N = length(namespace)
  
  # Assign seq type and namespace as attributes
  attr(pfm_mat, 'seq_type') = seq_type
  attr(pfm_mat, 'namespace') = namespace

  # Non-gapped columns
  if(seq_type == 'aa') namespace = c(namespace, 'X', 'B', 'Z')

  # Information content
  attr(pfm_mat, 'bits') = computeBits(pfm_mat, N, nseqs)
  attr(pfm_mat, 'priors') = bg_prob
  
  # Assign AA names to rows/pos col
  rownames(pfm_mat) = namespace
  colnames(pfm_mat) = 1:num_pos
  
  return(pfm_mat)
}



#' Gets height data used to plot logo, uses helper function computeHeights
#' 
#' @param pwm position weight matrix
#' @param unit height method
getHeightData <- function(pwm, method, decreasing=T){
  
  heights = pwm
  heights[is.infinite(heights)] = 0 
  
  if(method == 'bits'){
    ic = attr(pwm, 'bits')
    if(all(ic == 0)) ic = ic + 2
    heights = t(t(pwm) * ic)
  }
  
  dat = lapply(1:ncol(heights), function(i){
    vals = heights[,i]
    vals = vals[vals > 0]
    vals = sort(vals, decreasing = decreasing)
    
    if(length(vals) == 0) return(NULL)
    
    cs = cumsum( vals )
    
    df = data.frame(letter=names(vals), position=i,
               y0=c(0, cs[-length(cs)]), y1=cs, stringsAsFactors = F)
    df
  })
  
  dat = do.call(rbind, dat)
  
  
  # Adjust y spacing 
  space_factor = switch(method, bits = 0.004, probability = 0.006)
  y_pad = max(dat$y1) * space_factor
  dat$y0 = dat$y0 + y_pad
  dat = subset(dat, y1 > y0)
  
  # Dummy points to make sure full plot is drawn
  # Make sure position 1 and n have a dummy empty letter missing
  dummy = data.frame(letter=dat$letter[1], position=NA, y0=0, y1=0)
  
  # Missing first position
  if(dat$position[1] != 1){
    dummy$position = 1
    dat = rbind( dummy, dat )
  }
  
  # Missing last position
  if(dat$position[nrow(dat)] != ncol(pwm)){
    dummy$position = ncol(pwm)
    dat = rbind( dat, dummy )
  }
  rownames(dat) = NULL
  
  dat
}








