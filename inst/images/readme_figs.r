# library("devtools")
# install_github("omarwagih/ggseqlogo")


f = system.file("extdata", "sample_dna.rds", package = "ggseqlogo")
seqs_list = readRDS(f)
seqs_dna = seqs_list[[1]]

# Plot a sequence logo
p1 = ggplot() + geom_logo(seqs_dna) + theme_logo()


p2 = ggplot() + geom_logo(seqs_dna, col_scheme='base_pairing') + theme_logo()


# Col schemes 
cs1 = make_col_scheme(chars=c('A', 'T', 'C', 'G'), groups=c('gr1', 'gr1', 'gr2', 'gr2'), 
                      cols=c('purple', 'purple', 'blue', 'blue'))

p3 = ggplot() + geom_logo(seqs_dna, col_scheme=cs1) + theme_logo()



cs2 = make_col_scheme(chars=c('A', 'T', 'C', 'G'), values=1:4)

p4 = ggplot() + geom_logo(seqs_dna, col_scheme=cs2) + theme_logo()



p5 = ggplot() + geom_logo(seqs_list) + theme_logo() + 
  facet_wrap(~seq_group, ncol=1, scales='free_x') 


seqs_other = c('10010', '10100', '01010', '01001')
p6 = ggplot() +
  geom_logo(seqs_other, method='p', seq_type='other', namespace=0:1) +
  theme_logo()


p7 = ggplot() + geom_logo(seqs_dna, font=2) + theme_logo()


w = 5
h = 2.5
setwd('~/Development/ggseqlogo/inst/images/')
ggsave('fig1.png', p1, width = w, height = h, dpi = 600)
ggsave('fig2.png', p2, width = w, height = h, dpi = 600)
ggsave('fig3.png', p3, width = w, height = h, dpi = 600)
ggsave('fig4.png', p4, width = w, height = h, dpi = 600)
ggsave('fig5.png', p5, width = w, height = h*3, dpi = 600)
ggsave('fig6.png', p6, width = w, height = h, dpi = 600)
ggsave('fig7.png', p7, width = w, height = h, dpi = 600)