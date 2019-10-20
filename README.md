# guttman
R package for computing reliability of Guttman scales

# Installation

1.  Install devtools package: `install.packages("devtools")`
2.  Load devtools package: `library(devtools)`
3.  Install guttman package from GitHub: `devtools::install_github('Libum/guttman')`
4.  Load guttman package: library(guttman)

# Example usage on provided dataset:

```{r}
data("SFd")
guttman_object = guttman(dataset = SFd)
summary(guttman_object)
```