# Funcl

funcl is an environment for Mathematica-style exploratory development. It incorporates a simple computer algebra system, while maintaining support for fast numerics. I use it during my own theoretical physics research.

Current features include:

* Function algebra: addition, multiplication, composition of functions
* Fast linear algebra via [magicl][magicl] and BLAS/LAPACK
* Function plotting using eazy-gnuplot

## Usage

The funcl docs currently live in docs/ under the project root directory.

## Installation

Most of funcl's dependencies can be pulled in via Quicklisp. Those which require manual installation are

* [magicl][magicl]
* [bld-ode][bld-ode]
* [eazy-gnuplot][eazy-gnuplot]

  [magicl]: https://github.com/rigetticomputing/magicl
  [bld-ode]: https://github.com/bld/bld-ode
  [eazy-gnuplot]: https://github.com/guicho271828/eazy-gnuplot/

## Author

* Selwyn Simsek (sgs16@ic.ac.uk)

## Copyright

Copyright (c) 2018 Selwyn Simsek (sgs16@ic.ac.uk)

## License

Licensed under the LLGPL License.
