<img src="https://cdn.rawgit.com/omarwagih/ggseqlogo/4938177a/inst/images/logo.svg" alt="ggseqlogo logo" width="350px"><br>
[![](http://cranlogs.r-pkg.org/badges/ggseqlogo)](http://cran.rstudio.com/web/packages/ggseqlogo/index.html)
[![](https://www.r-pkg.org/badges/version/ggseqlogo)](http://cran.rstudio.com/web/packages/ggseqlogo/index.html)
	

ggseqlogo is an R package for generating publication-ready sequence logos using ggplot2. 

## Getting started
First install `ggseqlogo` from github using the `devtools` package:

```r
devtools::install_github("omarwagih/ggseqlogo")
```

Load up the package and sample data

```r
# Load the required packages
require(ggplot2)
require(ggseqlogo)

# Some sample data
data(ggseqlogo_sample)

```

Then draw a sequence logo

```r
# Plot DNA sequence logo
ggseqlogo( seqs_dna$sample_dna_1 )

# Plot protein sequence logo
ggseqlogo( seqs_aa$sample_aa_1 )
```

For more examples, and a list of features **[see the full tutorial here](http://omarwagih.github.io/ggseqlogo/)**.


## Tutorial
A detailed tutorial on how to use ggseqlogo **[can be found here](http://omarwagih.github.io/ggseqlogo/)**.

## Feedback
If you have any feedback or suggestions, drop me a line at (omarwagih(at)gmail.com) or open an issue on github.
