<img src="https://cdn.rawgit.com/omarwagih/ggseqlogo/1ae09503/inst/images/logo.svg" alt="ggseqlogo logo" width="350px"><br>


## Installation
Install ggseqlogo using the `devtools` package:

```r
library("devtools")
install_github("omarwagih/ggseqlogo")
```


## Getting started
```r
require(ggseqlogo)

# Some example DNA sequences
f = system.file("extdata", "sample_dna.rds", package = "ggseqlogo")
seqs_list = readRDS(f)

# Get first set of sequences
seqs_dna = seqs_list[[1]]

# Plot a sequence logo
ggplot() + geom_logo(seqs_dna) + theme_logo()
```

<img src="inst/images/fig1.png" width="350px"><br>

## Color schemes
ggseqlogo has preset color schemes that can be set using the `col_scheme` parameter. By default, the `col_scheme` is set to `auto` such that the color scheme is automatically chosen based on your sequence type. 

You can adjust the parameter.
For amino acids you can pick from the following `chemistry`, `hydrophobicity`, `clustalx`, `taylor`. For DNA and RNA sequences `nucleotide` and `base_pairing`. For example:

```r
ggplot() + geom_logo(seqs_dna, col_scheme='base_pairing') + theme_logo()
```

<img src="inst/images/fig2.png" width="350px"><br>

### Custom color schemes
If the presets are not enough for you, you can define custom discrete or continuous color schemes using the `make_col_scheme` function. 

#### Discrete color schemes:

```
cs1 = make_col_scheme(chars=c('A', 'T', 'C', 'G'), groups=c('gr1', 'gr1', 'gr2', 'gr2'), 
					  cols=c('purple', 'purple', 'blue', 'blue'))

ggplot() + geom_logo(seqs_dna, col_scheme=cs1) + theme_logo()
```

Note that the `groups` parameter here is optional


<img src="inst/images/fig3.png" width="350px"><br>

#### Continuous color schemes:

```
cs2 = make_col_scheme(chars=c('A', 'T', 'C', 'G'), values=1:4)

ggplot() + geom_logo(seqs_dna, col_scheme=cs2) + theme_logo()
```


<img src="inst/images/fig4.png" width="350px"><br>


## Facets
You can plot more than one sequence logo with the help of facets. `geom_logo` will accept a named list of sequences

```r			   
ggplot() + geom_logo(seqs_list, method='p') + 
  theme_logo() + 
  facet_wrap(~seq_group, ncol=1, scales='free_x') 
```


<img src="inst/images/fig5.png" width="350px"><br>


## Sequence types
Amino acids, DNA and RNA sequence types are all supported. If however you want to define a custom alphabet you can do so by setting `seq_type` to "other" and set `namespace`. For example, lets say you want a sequence logo of zeros and ones:

```r
seqs_other = c('10010', '10100', '01010', '01001')
ggplot() + 
	geom_logo(seqs_other, method='p', seq_type='other', namespace=0:1) + 
	theme_logo()
```

<img src="inst/images/fig6.png" width="350px"><br>

## Fonts
Currently two fonts are available, with more added soon.

1. San Francisco bold
2. San Francisco regular

You can adjust the font by setting the `font` parameter:

```r
ggplot() + geom_logo(seqs_dna, font=2) + theme_logo()
``` 


<img src="inst/images/fig7.png" width="350px"><br>

## Documentation
For more details on all features and parameters see `?geom_logo` and `?make_col_scheme`


## Feedback
If you have any feedback or suggestions, please drop me a line at (omarwagih(at)gmail.com) or open an issue on github.
