Kaggle Competition: Titanic 
===========================

https://www.kaggle.com/c/titanic


Usage
-----

R packages are managed using
[packrat](https://rstudio.github.io/packrat/). You can set up the
Titanic environment for R by installing packrat then doing this:

```R
b = "https://s3.amazonaws.com/packrat-bundles/titanic-2016-03-26.tar.gz"
d = "/home/tbonza/projects/kaggle/titanic"
packrat::unbundle(bundle=b, where=d)

```

Check out R_Scripts to see what we've been doing.

