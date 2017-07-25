setwd('~/Development/ggseqlogo/')
require(devtools)
remove.packages('ggseqlogo')
R_PATH = '/usr/local/bin/R'
targz = 'ggseqlogo_0.1.tar.gz'
system(sprintf('rm -rf dist/%s', targz))

# Regenerate Rwd files
#document('.')

document('.')
build('.', path = 'dist/')
# 
# 
# # Build 
# system(sprintf('%s CMD BUILD ./', R_PATH))
# 
# # Move into dist directory
# system(sprintf('mv %s ./dist/%s', targz, targz))

# Install 
system(sprintf('%s CMD INSTALL dist/%s', R_PATH, targz))

#detach("package:ggseqlogo", unload=TRUE)
