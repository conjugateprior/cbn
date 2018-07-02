# CBN

How to install in a way that makes sure the vignettes viewable.

In RStudio:

* File Menu > New Project > Version Control > Git 
* Set the repository name to `https://github.com/conjugateprior/cbn.git`
* Give it a local name 
* check bottom left to open in a new R session

In the R Console:

```
install.packages("devtools") # only if you don't already have it

devtools::install(build_vignettes = TRUE)
```

You're done.  

## Using the package

Now to crank up the package
```
library(cbn)
```
and view the vignettes
```
browseVignettes("cbn") # select the HTML links
```

To see the man pages
```
?cbn
```

## Updating

To get the latest version of cbn find the 'git' tab on the top
right window. Press 'Pull' and the run the `devtools::install` command above 
again.  But, you know, maybe *don't* do this until suggested. I work on master 
and feel free to break code.

Will Lowe July 2018
