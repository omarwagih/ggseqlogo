# 
# pd = data.frame(
#   letters = strsplit("AGTGACCGACTATCATAGTGACCCAGAATCATAGTGACCGAGTATGAT", "")[[1]],
#   species = rep(c("Human", "Armadillo", "Porcupine"), each=16),
#   x       = rep(1:16, 3),
#   change  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#               0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,
#               0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0),
#   score1  = c(0,0,0,0,0,0,1,1,2,2,2,3,3,3,4,3,
#               0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,
#               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
#   score2  = c(0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,
#               0,0,0,0,2,2,2,2,0,0,0,0,0,0,0,0,
#               0,0,0,0,3,3,3,3,0,0,0,0,0,0,0,0)
# )
# 
# pd = subset(pd, x <= 15)
# 
# 
# 
# p = ggplot(pd[pd$score1 != 0,], aes(x=x, y=species)) +
#   coord_fixed(ratio = 1.2, ylim=c(0.5, 3.5)) +
#   #geom_tile(aes(fill=score1)) +
#   #scale_fill_gradient2("Score 1", limits=c(0,4),low="#762A83", mid="white", high="#1B7837", guide=guide_colorbar(title.position="top")) +
#   geom_text(data=pd, aes(label=letters, color=factor(change)), size=rel(5), family="mono") + theme_logo()#+
#   #scale_color_manual("Change", values=c("black", "#F2A11F"), labels=c("None", "Some"), guide=guide_legend(direction="vertical", title.position="top", override.aes=list(shape = "A")))
# 
# print(p)
# 
# ll = z + guides(fill='none')
# zz =  p + scale_x_continuous(breaks=1:15) + theme(legend.position="none")
# p3 = plot_grid(ll, zz, align = 'v', nrow = 2)
# 
# print(p3)
