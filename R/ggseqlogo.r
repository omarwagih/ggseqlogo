# if(T){
#   require(ggplot2)
#   setwd('~/Development/ggseqlogo/')
#   source('R/heights.r')
#   source('R/col_schemes.r')
#   GGSEQLOGO_FONT_BASE = '~/Development/ggseqlogo/inst/fonts/'
# }


# Change range of values
newRange <- function(old_vals, new_min=0, new_max=1){
  old_min = min(old_vals)
  old_max = max(old_vals)
  
  new_vals = (((old_vals - old_min) * (new_max - new_min)) / (old_max - old_min)) + new_min
  new_vals
}


#' List fonts available in ggseqlogo
#' 
#' @param v If true, font names are printed to stderr. Otherwise, font names are returned as a character vector
#' @export
list_fonts <- function(v=T){
  
  fonts = c('helvetica_regular','helvetica_bold', 'helvetica_light',
            'roboto_medium','roboto_bold', 'roboto_regular',
            'akrobat_bold', 'akrobat_regular', 
            'roboto_slab_bold', 'roboto_slab_regular', 'roboto_slab_light', 
            'xkcd_regular')
  if(!v) return(fonts)
  message('Available ggseqlogo fonts:')
  for(f in fonts) message('\t', f)
}


# Read font from file if not in global envir.
get_font <- function(font){
  
  GGSEQLOGO_FONT_BASE = getOption('GGSEQLOGO_FONT_BASE')
  if(is.null(GGSEQLOGO_FONT_BASE)){
    GGSEQLOGO_FONT_BASE=system.file("extdata", "", package = "ggseqlogo")
    options(GGSEQLOGO_FONT_BASE=GGSEQLOGO_FONT_BASE)
  }
  
  #all_fonts = c('sf_bold', 'sf_regular', 'ms_bold', 'ms_regular', 'xkcd_regular')
  font = match.arg(tolower(font), list_fonts(F))
  font_filename = paste0(font, '.font')
  font_obj_name = sprintf('.ggseqlogo_font_%s', font)
  
  font_obj = getOption(font_obj_name)
  if(is.null(font_obj)){
    # Not loaded into global env yet - load it into options
    font_path = file.path(GGSEQLOGO_FONT_BASE, font_filename)
    font_obj_list = list( tmp=readRDS(font_path) )
    names(font_obj_list) = font_obj_name
    options(font_obj_list)
    font_obj = font_obj_list[[1]]
  }
  
  # Return font data
  font_obj
}


# Generate height data for logo
logo_data <- function( seqs, method='bits', stack_width=0.95, 
                       rev_stack_order=F, font, seq_group=1, 
                       seq_type = 'auto', namespace=NULL ){

  # Get font 
  font_df = get_font(font)
  
  # TODO
  # hh = twosamplelogo_method(seqs, seqs_bg, pval_thresh=0.05, seq_type = seq_type, namespace = namespace)
  
  # Generate heights based on method
  if(method == 'bits'){
    hh = bits_method(seqs, decreasing = rev_stack_order, seq_type = seq_type, namespace = namespace)
  }else if(method == 'probability'){
    hh = probability_method(seqs, decreasing = rev_stack_order, seq_type = seq_type, namespace = namespace)
  }else if(method == 'custom'){
    if(seq_type == 'auto') seq_type = guessSeqType(rownames(seqs))
    hh = matrix_to_heights(seqs, seq_type, decreasing = rev_stack_order)
  }else{
    stop('Invalid method!')
  }
  
  # Merge font df and heights
  ff = merge(font_df, hh, by = 'letter')
  # Scale x and ys to new positions
  x_pad = stack_width/2
  ff$x = newRange(ff$x, ff$position - x_pad, ff$position + x_pad)
  ff$y = newRange(ff$y, ff$y0, ff$y1)
  
  # Rename columns
  ff = as.data.frame(ff)[,c('x', 'y', 'letter', 'position', 'order')]
  ff$seq_group = seq_group
  
  # Set sequence type as attribute, to be used downstream
  attr(ff, 'seq_type') = attr(hh, 'seq_type')
  
  # Return data table
  ff
}

#' ggseqlogo custom theme
#' 
#' @param base_size font base size
#' @param base_family font base family
#' 
#' @import ggplot2
#' @export
theme_logo <- function(base_size=12, base_family=''){
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(panel.grid = element_blank(), legend.position = 'bottom', 
          axis.text.x=element_text(colour="black"),
          axis.text.y=element_text(colour="black"))
}

#' Plots sequence logo as a layer on ggplot 
#' 
#' @param data Character vector of sequences or named list of sequences. All sequences must have same width.
#' @param method Height method, can be one of "bits" or "probability" (default: "bits")
#' @param seq_type Sequence type, can be one of "auto", "aa", "dna", "rna" or "other" 
#' (default: "auto", sequence type is automatically guessed)
#' @param namespace Character vector of single letters to be used for custom namespaces. Can be alphanumeric, including Greek characters.
#' @param font Name of font. See \code{list_fonts} for available fonts.
#' @param stack_width Width of letter stack between 0 and 1 (default: 0.95)
#' @param rev_stack_order If \code{TRUE}, order of letter stack is reversed (default: FALSE)
#' @param col_scheme Color scheme applied to the sequence logo. See \code{list_col_schemes} for available fonts.
#' (default: "auto", color scheme is automatically picked based on \code{seq_type}). 
#' One can also pass custom color scheme objects created with the \code{make_col_scheme} function
#' @param low_col,high_col Colors for low and high ends of the gradient if a quantitative color scheme is used (default: "black" and "yellow").
#' @param na_col Color for letters missing in color scheme (default: "grey20")
#' @param plot If \code{FALSE}, plotting data is returned 
#' @param ... Additional arguments passed to layer params
#' 
#' @export
#' @import ggplot2
#' 
#' @examples
#' # Load sample data
#' data(ggseqlogo_sample)
#' 
#' # Produce single sequence logo using geom_logo
#' p1 = ggseqlogo( seqs_dna[[1]] ) 
#' 
geom_logo <- function(data = NULL, method='bits', seq_type='auto', namespace=NULL,
                      font='roboto_medium', stack_width=0.95, rev_stack_order=F, col_scheme = 'auto',
                      low_col='black', high_col='yellow', na_col='grey20',
                      plot=T, ...) {
  
  if(stack_width > 1 | stack_width <= 0) stop('"stack_width" must be between 0 and 1')
  if(is.null(data)) stop('Missing "data" parameter!')
  if(!is.null(namespace)) seq_type = 'other'
  
  # Validate method
  all_methods = c('bits', 'probability','custom')#, 'tsl')
  pind = pmatch(method, all_methods)
  method = all_methods[pind]
  if(is.na(method)) stop("method must be one of 'bits' or 'probability', or 'custom'")
  
  # Convert character seqs to list
  if(is.character(data) | is.matrix(data)) data = list("1"=data)
  
  if(is.list(data)){
    # Set names for list if they dont exist
    if(is.null(names(data))) names(data) = seq_along(data)
    
    lvls = names(data)
    
    # We have list of sequences - loop and rbind
    data_sp = lapply(names(data), function(n){
      curr_seqs = data[[n]]
      logo_data(seqs = curr_seqs, method = method, stack_width = stack_width, 
                rev_stack_order = rev_stack_order, seq_group = n, seq_type = seq_type, 
                font = font, namespace=namespace)
    })
    data = do.call(rbind, data_sp)
    # Set factor for order of facet
    data$seq_group = factor(data$seq_group, levels = lvls)
  }
  
  if(!plot) return(data)
  
  # Get sequence type
  seq_type = attr(data, 'seq_type')
  cs = get_col_scheme( col_scheme, seq_type )
  
  legend_title = attr(cs, 'cs_label')
  
  data = merge(data, cs, by='letter', all.x=T)
  
  # Make sure you retain order after merge
  data = data[order(data$order),]
  
  # Do we have a gradient colscale
  colscale_gradient = is.numeric( cs$group )
  
  colscale_opts = NULL
  if(colscale_gradient){
    # Set gradient colours 
    colscale_opts = scale_fill_gradient(name=legend_title, low = low_col, 
                                        high = high_col, na.value = na_col)
  }else{
    # Make group -> colour map
    tmp = cs[!duplicated(cs$group) & !is.na(cs$group),]
    col_map = unlist( split(tmp$col, tmp$group) )
    
    # Set colour scale options
    colscale_opts = scale_fill_manual(values=col_map, name=legend_title, na.value=na_col)
  } 
  
  # If letters and group are the same, don't draw legend
  guides_opts = NULL
  if(identical(cs$letter, cs$group)) guides_opts = guides(fill=F)
  
  y_lim = NULL
  extra_opts = NULL
  if(method == 'tsl'){
    y_lab = 'Depleted    Enriched'
    tmp = max(abs(data$y))
    #y_lim = c(-tmp, tmp)
    row_a = row_b = data[1,]
    row_a$y = -tmp
    row_b$y = tmp
    data = rbind(data, row_a, row_b)
    data$facet = factor(data$y > 0, c(T, F), c('Enriched', 'Depleted'))
    extra_opts = NULL#facet_grid(facet~., scales='free')
  }else if(method == 'custom'){
    y_lab = ''
  }else{
    y_lab = method
    substr(y_lab, 1, 1) = toupper(substr(y_lab, 1, 1))
  }
  
  # Group data
  data$group_by = with(data, interaction(seq_group, letter, position))
  
  data$x = data$x 
  # Create layer
  logo_layer = layer(
    stat = 'identity', data = data, 
    mapping = aes_string(x = 'x', y = 'y', fill='group', group='group_by'), 
    geom = 'polygon', 
    position = 'identity', show.legend = NA, inherit.aes = F,
    params = list(na.rm = T, ...)
  ) 
  
  
  breaks_fun = function(lim){
    # account for multiplicatuce expansion factor of 0.05
    1: floor( lim[2] / 1.05 )
  }
  
  # Expand 0.05 addidtive 
  list(logo_layer, scale_x_continuous(breaks = breaks_fun, labels = identity), 
       ylab(y_lab), xlab(''), colscale_opts, guides_opts, coord_cartesian(ylim=y_lim), 
       extra_opts)
}


#' Quick sequence logo plot
#' 
#' @description \code{ggseqlogo} is a shortcut for generating sequence logos. 
#' It adds the ggseqlogo theme \code{\link{theme_logo}} by default, and facets when multiple input data are provided. 
#' It serves as a convenient wrapper, so to customise logos beyond the defaults here, please use \code{\link{geom_logo}}.
#' 
#' @param data Character vector of sequences or named list of sequences. All sequences must have same width
#' @param facet Facet type, can be 'wrap' or 'grid'
#' @param scales Facet scales, see \code{\link{facet_wrap}}
#' @param ncol Number of columns, works only when \code{facet='wrap'}, see \code{\link{facet_wrap}}
#' @param nrow Number of rows, same as \code{ncol}
#' @param ... Additional arguments passed to \code{\link{geom_logo}}
#' 
#' @export
#' @examples
#' # Load sample data
#' data(ggseqlogo_sample)
#' 
#' # Plot a single DNA sequence logo
#' p1 = ggseqlogo( seqs_dna[[1]] )
#' print(p1)
#' 
#' # Plot multiple sequence logos at once
#' p2 = ggseqlogo( seqs_dna )
#' print(p2)
ggseqlogo <- function(data, facet='wrap', scales='free_x', ncol=NULL, nrow=NULL, ...){
  
  # Generate the plot with default theme
  p = ggplot() + geom_logo(data = data, ...) + theme_logo() 
  
  # If it's an inidivdual sequence logo, return plot
  if(!'list' %in% class(data)) return(p)
  
  # If we have more than one plot, facet
  facet_opts = c('grid', 'wrap')
  pind = pmatch(facet, facet_opts)
  facet = facet_opts[pind]
  if(is.na(facet)) stop("facet option must be set to 'wrap' or 'grid'")
  
  if(facet == 'grid'){
    p = p + facet_grid(~seq_group, scales = scales)
  }else if(facet == 'wrap'){
    p = p + facet_wrap(~seq_group, scales = scales, nrow = nrow, ncol = ncol)
  }
  
  # Return plot
  return(p)
}


#' List of aligned transcription factor binding sequences 
#'
#' @name seqs_dna
#' @docType data
#' @keywords data
NULL

#' List of aligned kinase-substrate binding sequences 
#'
#' @name seqs_aa
#' @docType data
#' @keywords data
NULL

#' List of position frequency matrices for transcription factors
#'
#' @name pfms_dna
#' @docType data
#' @keywords data
NULL


# message('-- running example')
# load('data/ggseqlogo_sample.rda')
# p = ggseqlogo(sample_data$seqs_dna, nrow=3)
# d = p$layers[[1]]$data
# print(p)
