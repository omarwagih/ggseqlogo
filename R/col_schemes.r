
#' List color schemes available in ggseqlogo
#' 
#' @param v If true, font names are printed to stderr. Otherwise, color scheme names are returned as a character vector
#' @export
list_col_schemes <- function(v=T){
  
  col_schemes = c('auto', 'chemistry', 'chemistry2','hydrophobicity', 'nucleotide', 'nucleotide2',
             'base_pairing', 'clustalx', 'taylor')
  if(!v) return(col_schemes)
  message('Available ggseqlogo color schemes:')
  for(f in col_schemes) message('\t', f)
}


# Get color scheme
# @param col_scheme name of color scheme
# @param seq_type sequence type of color scheme
get_col_scheme = function(col_scheme, seq_type='auto'){
  
  # Check if user-defined color scheme
  if(is.data.frame(col_scheme)){
    if(!'ggseqlogo_cs' %in% class(col_scheme)) 
      stop('Colour scheme must be generated using "make_col_scheme" function')
    return(col_scheme)
  }
 
  # Get ambigious colour scheme
  col_scheme = match.arg(col_scheme, list_col_schemes(F))
  
  # Get default color scheme for sequence type
  if(col_scheme == 'auto'){
    if(seq_type == 'auto') stop('"col_scheme" and "seq_type" cannot both be "auto"')
    
    col_scheme = switch(tolower(seq_type), aa = 'chemistry', 
                        dna = 'nucleotide', rna = 'nucleotide', 
                        other='nucleotide')

  }
  
  
  # Pick from default color schemes
  cs = switch(col_scheme, 
         # Color scheme based on chemistry of amino acids
         chemistry2 = data.frame(
           letter = c('G', 'S', 'T', 'Y', 'C', 'N', 'Q', 'K', 'R', 'H', 'D', 'E', 'P', 'A', 'W', 'F', 'L', 'I', 'M', 'V'),
           group = c(rep('Polar', 5), rep('Neutral', 2), rep('Basic', 3), rep('Acidic', 2), rep('Hydrophobic', 8)),
           col = c(rep('#058644', 5), rep('#720091', 2), rep('#0046C5', 3), rep('#C5003E', 2), rep('#2E2E2E', 8)),
           stringsAsFactors = F
         ), 
         
         # Color scheme based on chemistry of amino acids
         chemistry = data.frame(
           letter = c('G', 'S', 'T', 'Y', 'C', 'N', 'Q', 'K', 'R', 'H', 'D', 'E', 'P', 'A', 'W', 'F', 'L', 'I', 'M', 'V'),
           group = c(rep('Polar', 5), rep('Neutral', 2), rep('Basic', 3), rep('Acidic', 2), rep('Hydrophobic', 8)),
           col = c(rep('#109648', 5), rep('#5E239D', 2), rep('#255C99', 3), rep('#D62839', 2), rep('#221E22', 8)),
           stringsAsFactors = F
         ), 
         
         # Hydrophobicity index (PMID: 7108955) from -4.5 to 4.5
         hydrophobicity = data.frame(
           letter = c('I', 'V', 'L', 'F', 'C', 'M', 'A', 'G', 'T', 'W', 
                      'S', 'Y', 'P', 'H', 'D', 'E', 'N', 'Q', 'K', 'R'),
           group = c(4.5, 4.2, 3.8, 2.8, 2.5, 1.9, 1.8, -0.4, -0.7, -0.9, -0.8,
                       -1.3, -1.6, -3.2, -3.5, -3.5, -3.5, -3.5, -3.9, -4.5),
           stringsAsFactors=F
         ), 
         
         # Colour based on nucleotide
         nucleotide2 = data.frame(
           letter = c('A', 'C', 'G', 'T', 'U'),
           col = c('darkgreen', 'blue', 'orange', 'red', 'red'),
           stringsAsFactors = F
         ), 
         
         #alt red BA1200
         nucleotide = data.frame(
           letter = c('A', 'C', 'G', 'T', 'U'),
           col = c('#109648', '#255C99', '#F7B32B', '#D62839', '#D62839'),
           stringsAsFactors = F
         ), 
         
         base_pairing = data.frame(
           letter = c('A', 'T', 'U', 'G', 'C'),
           group = c(rep('Weak bonds', 3), rep('Strong bonds', 2)),
           col = c(rep('darkorange', 3), rep('blue', 2)),
           stringsAsFactors = F
         ),
         
         # ClustalX color scheme: 
         # http://www.jalview.org/help/html/colourSchemes/clustal.html
         clustalx = data.frame(
           letter = c('W', 'L', 'V', 'I', 'M', 'F', 'A', 'R', 'K', 'T', 'S', 'N', 'Q', 'D', 'E', 'H', 'Y', 'C', 'G', 'P'),
           col = c(rep('#197FE5', 7), rep('#E53319', 2), rep('#19CC19', 4), rep('#CC4CCC', 2), 
                   rep('#19B2B2', 2), '#E57F7F', '#E5994C', '#B0B000'),
           stringsAsFactors = F
         ),
         
         # Taylor color scheme (PMID: 9342138)
         taylor = data.frame(
           letter = c('D','S','T','G','P','C','A','V','I','L','M','F','Y','W','H','R','K','N','Q','E'),
           col = c('#FF0000','#FF3300','#FF6600','#FF9900','#FFCC00','#FFFF00','#CCFF00','#99FF00',
                   '#66FF00','#33FF00','#00FF00','#00FF66','#00FFCC','#00CCFF','#0066FF','#0000FF',
                   '#6600FF','#CC00FF','#FF00CC','#FF0066'),
           stringsAsFactors = F
         )
  )
  
  if(!'group' %in% names(cs)) cs$group = cs$letter
  
  # Set attributes
  attr(cs, 'cs_label') = col_scheme
  class(cs) = c('data.frame','ggseqlogo_cs')
  
  return(cs)
}





#' Create new sequence logo color scheme
#' 
#' @param chars Vector of one letter characters 
#' @param groups Vector of groups for letters with same length as chars (optional if cols parameter is provided) 
#' @param cols Vector of colors with same length as chars (optional if values parameter is provided) 
#' @param values Vector of numerical values with same length as chars
#' @param name Name of color scheme
#' 
#' @export
#' 
#' @importFrom grDevices col2rgb
#' @examples 
#' 
#' # Discrete color scheme examples
#' cs1 = make_col_scheme(chars=c('A', 'T', 'G', 'C'), groups=c('g1', 'g1', 'g2', 'g2'), 
#'                       cols=c('red', 'red', 'blue', 'blue'), name='custom1')
#' 
#' cs2 = make_col_scheme(chars=c('A', 'T', 'G', 'C'), cols=c('red', 'red', 'blue', 'blue'), 
#'                       name='custom2')
#' 
#' # Quantitative color scheme
#' cs3 = make_col_scheme(chars=c('A', 'T', 'G', 'C'), values=1:4, name='custom3')
make_col_scheme <- function(chars=NULL, groups=NULL, cols=NULL, values=NULL, name=''){
  
  
  if(is.null(chars) | any(nchar(chars) != 1) | !is.character(chars))
    stop('"chars" must be a character vector of one letter characters')
  
  
  if(is.null(values)){
    # Discrete colour scheme
    
    # Error check lengths
    if(length(chars) != length(cols)) stop('"chars" and "cols" must have same length')
    # Error check types
    if(!is.character(cols)) stop('"cols" must be a character vector')
    
    # Check valid colours
    tmp = col2rgb(cols); rm(tmp)
    
    if(is.null(groups)) groups = chars
    
    cs = data.frame( letter=chars, group=groups, col=cols, stringsAsFactors = F )
    
  }else{
    
    # Quantitative color scheme
    if(length(chars) != length(values)) stop('"chars" and "values" must have same length')
    cs = data.frame( letter=chars, group=values, stringsAsFactors=F )
  }
  
  # Remove duplicate letters
  cs = cs[!duplicated(cs$letter),]
  
  # Set attributes
  attr(cs, 'cs_label') = name
  class(cs) = c('data.frame','ggseqlogo_cs')
  
  return(cs)
}



