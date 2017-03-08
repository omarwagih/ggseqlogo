require(gglogo)
require(grid)
require(jpeg)
require(plyr)
require(ggplot2)
require(data.table)
require(parallel)

newRange <- function(old_vals, new_min=0, new_max=1){
  old_min = min(old_vals)
  old_max = max(old_vals)
  
  new_vals = (((old_vals - old_min) * (new_max - new_min)) / (old_max - old_min)) + new_min
  new_vals
}



#' Convert a text element into a clean R object of a jpeg image
letterObject <- function(ch, fontfamily="Helvetica", 
                         fontsize=1400, dim=c(2000, 2000), 
                         fontface = 2) {
  
  if(file.exists(fontfamily)){
    # Preexisting custom letter
    return(readJPEG(fontfamily))
  }
  
  jpeg_tmpfile = function(f) tempfile(pattern = f, fileext=".jpg")
  #jpeg_tmpfile = function(f) paste0(f, ".jpg")
  fname = jpeg_tmpfile("file1")
  
  # Draw letter as jpeg
  jpeg(filename=fname, width=dim[1], height=dim[2], quality = 100)
  grid.newpage()
  
  # Black rectangle
  grid.rect(x = 0, y=0, width=3, height=3, gp=gpar(fill="black"), draw = TRUE, vp = NULL)
  
  # Text ontop
  grid.text(ch, 0.5,0.5, gp=gpar(fontsize=fontsize, fontfamily=fontfamily, col="white", fontface=fontface))
  
  dev.off()
  
  
  readJPEG(fname)
}


# Convert letter to polygon coords
img2poly <- function(letter, fontfamily, fontface=2, do_thin=T){
  
  message('-- doing letter ', letter)
  # im = letterObject(letter, fontfamily = fontfamily, fontface = fontface, 
  #                   dim=c(2000,2000), fontsize = 1200)
  
  im = letterObject(letter, fontfamily = fontfamily, fontface = fontface, 
                    dim=c(3000,3000), fontsize = 1500)
  
  message('converting to data frame')
  imdf = gglogo::fortify(im)
  
  message('making outline')
  outline = gglogo::getOutline(imdf, threshold=0.8, var='green')
  
  message('post processing')
  outline$order <- gglogo::determineOrder(outline$x, outline$y)
  letterpath <- gglogo::identifyParts(outline, tol=5) 
  
  message('simplifying')
  group = NA
  letterpath2 = ddply(letterpath, .(group),  gglogo::simplifyPolygon, tol=1)
  lpath2 = gglogo::mainPlusIslands(letterpath2)
  lpath2$letter = letter
  
  if(do_thin==T & letter %in% c('i', 'j', 'l')){
    lpath2$x = newRange(lpath2$x, 0.2, 0.8)
  }else{
    lpath2$x = newRange(lpath2$x, 0, 1)
  }
  lpath2$y = newRange(lpath2$y)
  
  lpath2[,c('x', 'y','letter', 'order')]
}


makePolyFont <- function(fontfamily, fontface=2, I_fontfamily=fontfamily, is_bold=F, cores=1){
  
  chars = c(0:9, letters, LETTERS)
  #chars = 'Q'
  dat = mclapply(X=chars, mc.cores=cores, FUN=function(ch){
    ff = ifelse(ch == "I", I_fontfamily, fontfamily)
    ff2 = ifelse(ch == "I" & is_bold, 2, fontface)
    img2poly(ch, ff, ff2)
  })
  
  as.data.frame( rbindlist(dat) )
}



plot_font = function(file){
  
}

setwd('~/Development/ggseqlogo/inst/fonts/')
if(F){
  # SF bold
  sf_bold = makePolyFont(fontfamily = 'SF UI Text', fontface = 2, I_fontfamily = 'Menlo', cores=4)
  saveRDS(sf_bold, 'sf_bold.rds')
  
  # SF regular
  sf_regular = makePolyFont(fontfamily = 'SF UI Text', I_fontfamily = 'Menlo', fontface = 1, cores=4)
  saveRDS(sf_regular, 'sf_regular.rds')
}



