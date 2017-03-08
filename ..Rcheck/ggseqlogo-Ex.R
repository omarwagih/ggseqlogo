pkgname <- "ggseqlogo"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "ggseqlogo-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ggseqlogo')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("make_col_scheme")
### * make_col_scheme

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make_col_scheme
### Title: Create new sequence logo color scheme
### Aliases: make_col_scheme

### ** Examples


# Discrete color scheme examples
cs1 = make_col_scheme(chars=c('A', 'T', 'G', 'C'), groups=c('g1', 'g1', 'g2', 'g2'), 
                      cols=c('red', 'red', 'blue', 'blue'), name='custom1')

cs2 = make_col_scheme(chars=c('A', 'T', 'G', 'C'), cols=c('red', 'red', 'blue', 'blue'), 
                      name='custom2')

# Quantitative color scheme
cs3 = make_col_scheme(chars=c('A', 'T', 'G', 'C'), values=1:4, name='custom3')



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make_col_scheme", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
