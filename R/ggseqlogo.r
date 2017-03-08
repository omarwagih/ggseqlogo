# if(T){
#   require(ggplot2)
#   setwd('~/Development/ggseqlogo/')
#   source('R/heights_new.r')
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

# Read font from file if not in global envir.
get_font <- function(font){
  
  if(exists('GGSEQLOGO_FONT_BASE', envir = .GlobalEnv)){
    GGSEQLOGO_FONT_BASE = get('GGSEQLOGO_FONT_BASE', envir = .GlobalEnv)
  }else{
    GGSEQLOGO_FONT_BASE = system.file("fonts", "", package = "ggseqlogo")
  }
  
  all_fonts = c('sf_bold', 'sf_regular')
  font_filename = all_fonts[font]
  if(is.na(font_filename)) stop('Invalid "font" value!')
  font_filename = paste0(font_filename, '.rds')
  font_obj_name = sprintf('.ggseqlogo_font_%s', font)
  
  if(!exists(font_obj_name, envir = .GlobalEnv)){
    # Not loaded into global env yet - load it into global
    font_path = file.path(GGSEQLOGO_FONT_BASE, font_filename)
    assign(font_obj_name, readRDS(font_path), envir = .GlobalEnv)
  }
  
  # Return font object
  get(font_obj_name, envir = .GlobalEnv)
}


validate_pfm <- function(pfm){
  #apply(pfm, 1, )
}

# Generate height data for logo
logo_data <- function( seqs, seqs_bg=NULL, method='bits', stack_width=0.95, 
                       rev_stack_order=F, font=1, seq_group=1, 
                       seq_type = 'auto', namespace=NULL ){

  # Get font 
  sf_df = get_font(font)
  
  if(method == 'bits'){
    hh = bits_method(seqs, decreasing = rev_stack_order, seq_type = seq_type, namespace = namespace)
  }else if(method == 'probability'){
    hh = probability_method(seqs, decreasing = rev_stack_order, seq_type = seq_type, namespace = namespace)
  }else if(method == 'tsl'){
    hh = twosamplelogo_method(seqs, seqs_bg, pval_thresh=0.05, seq_type = seq_type, namespace = namespace)
  }else if(method == 'custom'){
    if(seq_type == 'auto') seq_type = guessSeqType(rownames(seqs))
    hh = matrix_to_heights(seqs, seq_type, decreasing = rev_stack_order)
  }else{
    stop('Invalid method!')
  }
  
  ff = merge(sf_df, hh, by = 'letter')
  
  # Scale x and ys
  x_pad = stack_width/2
  ff$x = newRange(ff$x, ff$position-x_pad, ff$position + x_pad)
  ff$y = newRange(ff$y, ff$y0, ff$y1)
  
  ff = as.data.frame(ff)[,c('x', 'y', 'letter', 'position', 'order')]
  ff$seq_group = seq_group
  
  attr(ff, 'seq_type') = attr(hh, 'seq_type')
  ff
}

#' ggseqlogo custom theme
#' 
#' @param base_size font base size
#' @param base_family font base family
#' 
#' @export
theme_logo <- function(base_size=14, base_family=''){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme(panel.grid = element_blank(), legend.position = 'bottom')
}



#' Plots sequence logo as a layer on ggplot 
#' 
#' @param data Character vector of sequences or named list of sequences. All sequences must have same width.
#' @param method Height method, can be one of "bits" or "probability" (default: "bits")
#' @param seq_type Sequence type, can be one of "auto", "aa", "dna", "rna" or "other" 
#' (default: "auto", sequence type is automatically guessed)
#' @param namespace Character vector of single letters to be used for custom namespaces
#' @param font Integer specifying font
#' @param stack_width Width of letter stack between 0 and 1 (default: 0.95)
#' @param rev_stack_order If \code{TRUE}, order of letter stack is reversed (default: FALSE)
#' @param col_scheme Color scheme applied to the sequence logo, can be one of the following: 
#' "auto", "chemistry", "hydrophobicity", "nucleotide", "base_pairing", "clustalx", "taylor" 
#' (default: "auto", color scheme is automatically picked based on \code{seq_type}). 
#' One can also pass custom color scheme objects created with the \code{make_col_scheme} function
#' @param low_col,high_col Colors for low and high ends of the gradient if a quantitative color scheme is used (default: "black" and "yellow").
#' @param na_col Color for letters missing in color scheme (default: "grey20")
#' @param plot If \code{FALSE}, plotting data is returned 
#' @param ... Additional arguments passed to layer params
#' 
#' @export
#' @import ggplot2
geom_logo <- function(data = NULL, seqs_bg = NULL, method='bits', seq_type='auto', namespace=NULL,
                      font=1, stack_width=0.95, rev_stack_order=F, col_scheme = 'auto',
                      low_col='black', high_col='yellow', na_col='grey20',
                      plot=T, ...) {
  
  if(stack_width > 1 | stack_width <= 0) stop('"stack_width" must be between 0 and 1')
  if(is.null(data)) stop('Missing "data" parameter!')
  
  # Validate method
  all_methods = c('bits', 'probability', 'custom')#, 'tsl')
  pind = pmatch(method, all_methods)
  if(length(pind) != 1) stop("method must be one of 'bits' or 'probability', or 'custom'")
  method = all_methods[pind]
  
  # Convert character seqs to list
  if(is.character(data) | is.matrix(data)) data = list("1"=data)
  
  if(is.list(data)){
    # Set names for list if they dont exist
    if(is.null(names(data))) names(data) = seq_along(data)
    
    lvls = names(data)
    
    # We have list of sequences - loop and rbind
    data_sp = lapply(names(data), function(n){
      curr_seqs = data[[n]]
      logo_data(seqs = curr_seqs, seqs_bg = seqs_bg, method = method, stack_width = stack_width, 
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
  
  assign('data', data, envir = .GlobalEnv)
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
  
  
  data$group_by = with(data, interaction(seq_group, letter, position))
  # Create layer
  logo_layer = layer(
    stat = 'identity', data = data, 
    mapping = aes_string(x = 'x', y = 'y', fill='group', group='group_by'), 
    geom = 'polygon', 
    position = 'identity', show.legend = NA, inherit.aes = F,
    params = list(na.rm = T, ...)
  ) 
  
 
 
  
  breaks_fun = function(lim){
    x = 1: floor( lim[2]-(stack_width/2) )
  }
  
  
  
  list(logo_layer, scale_x_continuous(breaks = breaks_fun, labels = identity), 
       ylab(y_lab), xlab(''), colscale_opts, guides_opts, coord_cartesian(ylim=y_lim), extra_opts)
}


