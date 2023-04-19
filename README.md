# ImageCompressor
A nice application to parallel k-means

## Prerequisites
* [Haskell](https://www.haskell.org/)
* [Stack](https://docs.haskellstack.org/en/stable/)
* [Ghcup](https://www.haskell.org/ghc/)
* [Make](https://www.gnu.org/software/make/)

## Description

Second year Epitech project made in `Haskell`. The goal is to reduce the size of an image by implementing the [K-means](https://en.wikipedia.org/wiki/K-means_clustering) algorithm.
Three steps are needed for the implementation:
* read the image and extract the colors of each pixel,
* cluster these colors, and replace each color of a given cluster by the mean color of this cluster,
* index the means of the cluster, and create the compressed image.
The project focuses on the second part, that is `clustering`.

### [Clustering](https://www.analyticsvidhya.com/blog/2016/11/an-introduction-to-clustering-and-different-methods-of-clustering/)

### Arguments

The following arguments are `required` for the program:
|   Option      |   Description     |
|---------------|-------------------|
| `-n` | number of colors in the final image |
| `-l` | convergence limit |
| `-f` | path to the file containing the colors of the pixels |


### Grammar
* The list of pixel given in the file passed as argument follow the following grammar:
```
IN      ::= POINT ' ' COLOR ( '\n' POINT ' ' COLOR )*
POINT   ::= '(' int ',' int ')'
COLOR   ::= '(' SHORT ',' SHORT ',' SHORT ')'
SHORT   ::= '0'.. '255'
```
* The output, which is the clustered color, follow this grammar:
```
OUT     ::= CLUSTER*
CLUSTER ::= '- -\n' COLOR '\n -\ n' ( POINT ' ' COLOR '\n')*
POINT   ::= '(' int ',' int ')'
COLOR   ::= '(' SHORT ',' SHORT ',' SHORT ')'
SHORT   ::= '0 '.. '255'
```

## Build program
To compile the `imageCompressor`, stack is used wrapped in a Makefile, which implements the following rules:
|   Command             |   Result          |
|-----------------------|-------------------|
|   `make`              | create a `imageCompressor` executable with `stack build`. Then copy the produced executable at the root of the project. |
|   `make clean`        | delete the files produced by the compilation (in that case execute `stack clean`). |
|   `make fclean`       | execute a `stack purge`, and then delete the `imageCompressor` executable at the root of project. |
|   `make re`           | do the `fclean` and `all` rules (in that order) |
|   `make tests_run`    | execute unit_tests with `stack test`, and copy the coverage in `test/coverage/` folder. |

## Authors
* [Luka Camus](https://github.com/Lukacms)
* [Louis Bassagal](https://github.com/LouisBassagal)
