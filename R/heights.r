# Namespaces
.AA_NAMESPACE = function() c('A', 'R', 'N', 'D', 'C', 'Q', 'E', 'G', 'H', 'I', 'L', 'K', 'M', 'F', 'P', 'S', 'T', 'W', 'Y', 'V')
.DNA_NAMESPACE = function() c('A', 'T', 'G', 'C')
.RNA_NAMESPACE = function() c('A', 'U', 'G', 'C')

# Generate letter matrix from vector of sequences
# 
# @param input vector of sequences
letterMatrix <- function(input){
  # Ensure kmers are the same length characters 
  seq.len = sapply(input, nchar)
  num_pos = seq.len[1]
  if(! all(seq.len == num_pos)) stop('Sequences in alignment must have identical lengths')
  
  # Construct matrix of letters
  split = unlist( sapply(input, function(seq){strsplit(seq, '')}) )
  
  t( matrix(split, seq.len, length(split)/num_pos) )
}

# Guess sequence type based on letter matrix
# 
# @param sp letters
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


# Find namespace
# 
# @param letter_mat Matrix of latters
# @param seq_type Sequence type
# @param namespace Alphabet
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
    non_alphanumeric = grepl('[^a-zA-Z0-9\u03bb\u03b1\u03b2\u0393\u03b3\u0394\u03b4\u03b5\u03b6\u03b7\u03b8\u0398\u03b9\u03ba\u039b\u039b\u03bc\u039e\u03be\u03a0\u03c0\u03c1\u03c3\u03c4\u03c5\u03a6\u03c6\u03c7\u03c8\u03a8\u03a9\u03c9]', namespace)
    if( any( non_alphanumeric ) )
      stop('All letters in the namespace must be alphanumeric')
    
    # Ensure there is something in each column
    # apply(letter_mat, 2, function(column_letters){
    #   int = intersect(namespace, column_letters)
    #   if(length(int) == 0)
    #     stop('The alignment has no letters in namespace match aligned sequences in at least one column')
    # })
    
  }else{
    if(!is.null(namespace)) 
      stop('For custom namespaces please set seq_type to "other"')
    
    # Guess sequence type
    if(seq_type == "auto")
      seq_type = guessSeqType(sp)
    
    # Get predefined namespace
    namespace = get( sprintf('.%s_NAMESPACE', toupper(seq_type)) )()
  }
  
  return(list(seq_type = toupper(seq_type), 
              namespace = namespace))
}

# Calcualte bits
#
# @param pwm Position weight matrix
# @param N Number of letters in namespace
# @param Nseqs Number of sequences in PWM
computeBits <- function(pwm, N=4, Nseqs=NULL){
  Nseqs = attr(pwm, 'nongapped')
  H_i = - apply(pwm, 2, function(col) sum(col * log2(col), na.rm=T))
  e_n = 0
  if(!is.null(Nseqs)) e_n = (1/logb(2)) * (N-1)/(2*Nseqs) 
 
  R_i = log2(N) - (H_i  + e_n)
  # Set any negatives to 0
  R_i = pmax(R_i, 0)
  return(R_i)
}

# Construct relative frequency matrix
# @param seqs aligned sequences as vector
# @param seq_type sequence type
# @param namespace letters used for matrix construction
# @param keep_letter_mat Keep letter matrix for some height methods
makePFM <- function(seqs, seq_type='auto', namespace=NULL, keep_letter_mat=F){
  
  letter_mat = NA
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
    ns = findNamespace(letter_mat, seq_type, namespace=namespace)
    namespace = ns$namespace
    seq_type = ns$seq_type
    
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
    attr(pfm_mat, 'nongapped') = apply(mat, 2, sum)
    attr(pfm_mat, 'nseqs') = nseqs
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
  
  # Assign AA names to rows/pos col
  rownames(pfm_mat) = namespace
  colnames(pfm_mat) = 1:num_pos
  
  if(keep_letter_mat) return(list(letter_mat = letter_mat, pfm=pfm_mat))

  return(pfm_mat)
}



######################
# Matrix to heights
######################

# General function to convert matrix of heights to polygon data frame 
# @param mat matrix of heghts
# @param seq_type sequence type
# @decreasing Sets order of letters, high to low or low to high
matrix_to_heights <- function(mat, seq_type, decreasing=T){
  
  mat[is.infinite(mat)] = 0 
  
  if(any(duplicated(rownames(mat)))) stop('Matrix input must have unique row names')
  
  dat = lapply(1:ncol(mat), function(i){
    vals = mat[,i]
    
    pos = sort( vals[vals >= 0], decreasing = decreasing)
    neg = sort(vals[vals < 0], decreasing = !decreasing)
    #vals = sort(vals, decreasing = T)
    cs_pos = cumsum( pos )
    cs_neg = cumsum( neg )
    
    df_pos = df_neg = NULL
    
    if(length(pos) > 0)
      df_pos = data.frame(letter=names(pos), position=i,  y0=c(0, cs_pos[-length(cs_pos)]), 
                          y1=cs_pos, stringsAsFactors = F)
    
    if(length(neg) > 0)
      df_neg = data.frame(letter=names(neg), position=i, y0=cs_neg, y1=c(0, cs_neg[-length(cs_neg)]), 
                          stringsAsFactors = F)
    
    rbind(df_pos, df_neg)
  })

  dat = do.call(rbind, dat)

  # Adjust y spacing 
  space_factor = 0.004
  y_pad = max(dat$y1) * space_factor
  dat$y0 = dat$y0 + y_pad
  dat = subset(dat, dat$y1 > dat$y0)
  
  # Dummy points to make sure full plot is drawn
  # Make sure position 1 and n have a dummy empty letter missing
  dummy = data.frame(letter=dat$letter[1], position=NA, y0=0, y1=0)
  
  # Missing first position
  if(dat$position[1] != 1){
    dummy$position = 1
    dat = rbind( dummy, dat )
  }
  
  # Missing last position
  if(dat$position[nrow(dat)] != ncol(mat)){
    dummy$position = ncol(mat)
    dat = rbind( dat, dummy )
  }

  rownames(dat) = NULL
  
  attr(dat, 'seq_type') = seq_type
  
  dat
}



# Shannon entropy method
bits_method <- function(seqs, decreasing, ...){
  # Make PFM
  pfm = makePFM(seqs, ...)

  # Get ic
  ic = attr(pfm, 'bits')
  if(all(ic == 0)){
    warning('All positions have zero information content perhaps due to too few input sequences. Setting all information content to 2.')
    ic = (ic * 0)+2
  }
  heights = t(t(pfm) * ic)

  seq_type = attr(pfm, 'seq_type')
  matrix_to_heights(heights, seq_type, decreasing)
} 

# Probability method
probability_method <- function(seqs, decreasing, ...){
  # Make PFM
  pfm = makePFM(seqs, ...)
  seq_type = attr(pfm, 'seq_type')
  matrix_to_heights(pfm, seq_type, decreasing)
}


#######################
# Two sample logo functions - method not used currently
#######################
# t_test = function(a, b){
#   x = tryCatch({
#     return( t.test(a, b, var.equal = T)$p.value )
#   } , error=function(e) return(1) )
#   x
# }
# 
# binom_test = function(a, b){
#   binom.test(sum(a), length(a), sum(b)/length(b))$p.value
# }
# 
# # ttest pvalue calculation reimplemented from TSL code 
# ttest_p_value <- function(k1, n1, k2, n2)  {
#   mean1 = k1 / n1;
#   mean2 = k2 / n2;
#   
#   var1_mult = (k1*(1-mean1)*(1-mean1)) + ((n1-k1)*mean1*mean1);
#   var2_mult = (k2*(1-mean2)*(1-mean2)) + ((n2-k2)*mean2*mean2);
#   
#   df   = n1 + n2 - 2;
#   svar = (var1_mult + var2_mult) / df;
#   t    = (mean1-mean2) / sqrt(svar*(1.0/n1 + 1.0/n2));
#   return( 2*pt(t, df, lower=FALSE) )
# }
# 
# # Convert to matrix of 1s and 0s
# to_matrix = function (x, seq){
#   X <- matrix(0, length(x), length(seq), dimnames = list(names(x), seq))
#   for (i in 1:length(seq)) X[x == seq[i], i] <- 1
#   return(X)
# }
# 
# twosamplelogo_method <- function(fg, bg, fix_pos=NULL, test='t.test', pval_thresh=0.05, ...){
#   if(!is.character(fg) | !is.character(bg)) 
#     stop('Foreground and background sequences must be character vectors') 
# 
#   if(!identical(unique(nchar(fg)), unique(nchar(bg)))) 
#     stop('Foreground sequences must have same width as background')
# 
#   fg_obj = makePFM(fg, keep_letter_mat=T, ...)
# 
#   namespace = attr(fg_obj$pfm, 'namespace')
#   seq_type = attr(fg_obj$pfm, 'seq_type')
# 
#   # Pass sequence type and namespace - avoid double guessing
#   bg_obj = makePFM(bg, keep_letter_mat=T, seq_type = 'other', namespace = namespace)
# 
#   # Difference in relative frequencies
#   pfm_diff = fg_obj$pfm - bg_obj$pfm 
# 
#   # Get letter matrices
#   fg_lm = fg_obj$letter_mat
#   bg_lm = bg_obj$letter_mat
# 
#   pv_mat = sapply(1:ncol(fg_lm), function(i){
#     p = to_matrix(fg_lm[,i], namespace)
#     n = to_matrix(bg_lm[,i], namespace)
#     
#     np = nrow(p)
#     nn = nrow(n)
#     
#     #pv = sapply(1:ncol(p), function(j) binom_test(p[,j], n[,j]) )
#     pv = sapply(1:ncol(p), function(j) ttest_p_value(sum(p[,j]), np, sum( n[,j] ), nn) )
#     names(pv) = names(p)
#     pv
#   })
# 
#   # Set things below threshold to zero
#   pfm_diff[ pv_mat >= pval_thresh ] = 0
#   pfm_diff = pfm_diff * 100
#   
#   #fix_pos = 1
#   if(!is.null(fix_pos)){
#     i = apply(fg_obj$pfm[,fix_pos,drop=F], 2, which.max)
#     ind = matrix(c(i, fix_pos), ncol=2)
#     x = pfm_diff
#     x[x < 0] = 0
#     pfm_diff[ind] = max( apply(x, 2, sum) )
#   }
#   
#   # Make heights
#   hh = matrix_to_heights(pfm_diff, seq_type)
#   hh
# }


# plogo <- function(fg, bg, pval_thresh=0.05){

#   fg_obj = makePFM(fg, keep_letter_mat=T, NO REL FREQ, ...)

#   namespace = attr(fg_obj$pfm, 'namespace')
#   seq_type = attr(fg_obj$pfm, 'seq_type')

#   # Pass sequence type and namespace - avoid double guessing
#   bg_obj = makePFM(bg, keep_letter_mat=T, seq_type = seq_type, namespace = namespace)


#   # -log( binom.test(1, 100, 0.01, alternative = 'g')$estimate / 
#   #   binom.test(1, 100, 0.01, alternative = 'l')$estimate )

# }



