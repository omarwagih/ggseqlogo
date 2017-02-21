


# to_matrix = function (x, seq){
#   X <- matrix(0, length(x), length(seq), dimnames = list(names(x), 
#                                                          seq))
#   for (i in 1:length(seq)) X[x == seq[i], i] <- 1
#   return(X)
# }
# 
# 
# 
# pos = readLines('inst/extdata/cam_pos.txt')
# neg = readLines('inst/extdata/cam_neg.txt')
# 
# lm_pos = letterMatrix(pos)
# lm_neg = letterMatrix(neg)
# 
# t_test = function(a, b){
#   x = tryCatch({
#     return( t.test(a, b)$p.value )
#   } , error=function(e) return(1) )
#   x
# }
# 
# pv_mat = sapply(1:ncol(lm_pos), function(i){
#   p = to_matrix(lm_pos[,i], .AA_NAMESPACE())
#   n = to_matrix(lm_neg[,i], .AA_NAMESPACE())
#   
#   pv = sapply(1:ncol(p), function(j) t_test(p[,j], n[,j]) )
#   names(pv) = names(p)
#   pv
# })
# 
# pfm_pos = makePFM(pos)
# pfm_neg = makePFM(neg)
# 
# pfm_diff = pfm_pos - pfm_neg
# 
# pv = melt(pv_mat)
# df = melt(pfm_diff)
# df$pval = pv$value
# 
# df2 = subset(df, pval < 0.01)
# 
# 
# getHeightData2 <- function(pfm, method='', decreasing=T){
#   # print(seqs)
#   # pfm = makePFM(seqs)
#   pr = getPriors(seq_type = attr(pfm, 'seq_type'), namespace = rownames(pfm))
#   pwm = log2( sweep(pfm, 2, pr, '/') + 0.01 )
#   
#   heights = pwm
#   
#   dat = lapply(1:ncol(heights), function(i){
#     vals = heights[,i]
#     pos = sort( vals[vals >= 0], decreasing = decreasing)
#     neg = sort(vals[vals < 0], decreasing = decreasing)
#     #vals = sort(vals, decreasing = T)
#     cs_pos = cumsum( pos )
#     cs_neg = cumsum( neg )
#     
#     df_pos = df_neg = NULL
#     
#     if(length(pos) > 0)
#       df_pos = data.frame(letter=names(pos), position=i,  y0=c(0, cs_pos[-length(cs_pos)]), y1=cs_pos, stringsAsFactors = F)
#     
#     if(length(neg) > 0)
#       df_neg = data.frame(letter=names(neg), position=i, y0=c(0, cs_neg[-length(cs_neg)]), y1=cs_neg, stringsAsFactors = F)
#     
#     rbind(df_pos, df_neg)
#   })
#   dat = do.call(rbind, dat)
#   
#   dat
# }
# 
