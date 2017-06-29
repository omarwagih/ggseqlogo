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
img2poly <- function(letter, fontfamily, fontface=1, do_thin=T, extra_thins=c(), thin_factor=0.22, X=3000){
  
  message('-- doing letter ', letter, ' for family ', fontfamily)
  # im = letterObject(letter, fontfamily = fontfamily, fontface = fontface, 
  #                   dim=c(2000,2000), fontsize = 1200)
  
  if(file.exists(fontfamily)){
    # Font family is an image - read it
    im = jpeg::readJPEG(fontfamily)
  }else{
    im = letterObject(letter, fontfamily = fontfamily, fontface = fontface, 
                      dim=c(X,X), fontsize = X/2)
  }
  
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
  
  if(do_thin==T & letter %in% c('i', 'j', 'l', '1', extra_thins)){
    lpath2$x = newRange(lpath2$x, thin_factor, 1-thin_factor)
  }else{
    lpath2$x = newRange(lpath2$x, 0, 1)
  }
  lpath2$y = newRange(lpath2$y)
  
  lpath2[,c('x', 'y','letter', 'order')]
}


makePolyFont <- function(fontfamily, fontface=2, I_fontfamily=fontfamily, is_bold=F, 
                         cores=1, X=3000, extra_thins=c(), thin_factor=0.22, do_greek=F){
  
  greek='αβΓγΔδεζηΘθικΛλμΞξΠπρστυΦφχΨψΩω'
  chars = c(0:9, letters, LETTERS)
  if(do_greek) chars = c(chars, strsplit(greek, '')[[1]])
  #chars = 'Q'
  dat = mclapply(X=chars, mc.cores=cores, FUN=function(ch){
    ff = ifelse(ch == "I", I_fontfamily, fontfamily)
    img2poly(ch, ff, X=X, extra_thins = extra_thins, thin_factor = thin_factor)
  })
  
  as.data.frame( rbindlist(dat) )
}





setwd('~/Development/ggseqlogo/inst/fonts/')

# if(F){
#   # SF bold
#   sf_bold = makePolyFont(fontfamily = 'SF UI Text', fontface = 2, I_fontfamily = 'Menlo', cores=4)
#   saveRDS(sf_bold, 'sf_bold.rds')
#   
#   # SF regular
#   sf_regular = makePolyFont(fontfamily = 'SF UI Text', I_fontfamily = 'Menlo', fontface = 1, cores=4)
#   saveRDS(sf_regular, 'sf_regular.rds')
# }




#______ Akrobat: sansserif ______
if(F){
  # Bold
  akrobat_bold = makePolyFont(fontfamily = 'Akrobat-Bold', I_fontfamily = 'Menlo-Bold', cores=3)
  saveRDS(akrobat_bold, 'akrobat_bold.font')
  
  # Bold
  akrobat_regular = makePolyFont(fontfamily = 'Akrobat-Regular', I_fontfamily = 'Menlo-Regular', cores=3)
  saveRDS(akrobat_regular, 'akrobat_regular.font')
}



#______ Roboto slab: serif ______
if(F){
  # Bold
  roboto_slab_bold = makePolyFont(fontfamily = 'RobotoSlab-Bold',  do_greek = T, cores=2)
  saveRDS(roboto_slab_bold, 'roboto_slab_bold.font')
  
  # Regular
  roboto_slab_regular = makePolyFont(fontfamily = 'RobotoSlab-Regular', do_greek = T, cores=2)
  saveRDS(roboto_slab_bold, 'roboto_slab_regular.font')
  
  # Light
  roboto_slab_light = makePolyFont(fontfamily = 'RobotoSlab-Light',  do_greek = T, cores=2)
  saveRDS(roboto_slab_light, 'roboto_slab_light.font')
}


#______ XKCD font ______
if(F){
  xkcd_regular = makePolyFont(fontfamily = 'xkcd-Regular', I_fontfamily = 'RoughNotesSample', 
                              extra_thins = '1', cores=3)
  saveRDS(xkcd_regular, 'xkcd_regular.font')
}




#______ Roboto: sansserif ______
if(T){
  # Bold
  roboto_bold = makePolyFont(fontfamily = 'Roboto-Bold', I_fontfamily = 'Menlo-Bold',  do_greek = T, cores=2)
  saveRDS(roboto_bold, 'roboto_bold.font')
  
  # Regular
  roboto_regular = makePolyFont(fontfamily = 'Roboto-Regular', I_fontfamily = 'Menlo-Regular', do_greek = T, cores=2)
  saveRDS(roboto_regular, 'roboto_regular.font')
  
  # Medium
  roboto_medium = makePolyFont(fontfamily = 'Roboto-Medium', I_fontfamily = 'Menlo-Bold', do_greek = T, cores=2)
  saveRDS(roboto_medium, 'roboto_medium.font')
  
}


#______ Helvetica: sansserif ______
if(T){
  #Bold
  helvetica_bold = makePolyFont(fontfamily = 'Helvetica-Bold', I_fontfamily = 'Menlo-Bold',  do_greek = T, cores=2)
  saveRDS(helvetica_bold, 'helvetica_bold.font')
  
  # Regular
  helvetica_regular = makePolyFont(fontfamily = 'Helvetica', I_fontfamily = 'Menlo-Regular',  do_greek = T, cores=2)
  saveRDS(helvetica_regular, 'helvetica_regular.font')
  
  # Light
  helvetica_light = makePolyFont(fontfamily = 'Helvetica-Light', I_fontfamily = 'Menlo-Regular',  do_greek = T, cores=2)
  saveRDS(helvetica_light, 'helvetica_light.font')
}
