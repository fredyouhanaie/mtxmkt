# mtxmkt

[![Erlang CI](https://github.com/fredyouhanaie/mtxmkt/actions/workflows/erlang.yml/badge.svg)](https://github.com/fredyouhanaie/mtxmkt/actions/workflows/erlang.yml) [![Hex.pm](https://img.shields.io/hexpm/v/mtxmkt.svg)](https://hex.pm/packages/mtxmkt)

[Matrix Market](https://math.nist.gov/MatrixMarket/) is a service from
NIST that provides a repository of test matrices for various
application areas. It is best described on the NIST web site as:

> A visual repository of test data for use in comparative studies of
> algorithms for numerical linear algebra, featuring nearly 500 sparse
> matrices from a variety of applications, as well as matrix generation
> tools and services.

This is an Erlang library to handle Matrix Market files. The format of
the files is described
[here](https://math.nist.gov/MatrixMarket/formats.html).

The library functions are loosely based on the
[mmio C library](https://math.nist.gov/MatrixMarket/mmio-c.html)
provided by Matrix Market.

There are matrices of varying sizes, which can be
[browsed](https://math.nist.gov/MatrixMarket/data/) or
[searched](https://math.nist.gov/MatrixMarket/searchtool.html). The
largest matrix is a square matrix of order 90449!

It is hoped that this will prove useful for anyone wishing to test
numerical analysis and/or linear algebra programs written in Erlang.

## Implementation status

All 22 forms of the martices have now been implemented. One can now
read, and write, matrix market files, in uncompressed or gzip
compressed forms.

There will be more changes mostly related to performance and internal
functions.


## Build and Tests

`rebar3` is used for all aspects of code maintainance.

* To build the library
```
	$ rebar3 do clean,compile
```

* To run the eunit tests
```
	$ rebar3 eunit
```

* To generate the documentation
```
	$ rebar3 edoc
```

* To analyse with dialyzer
```
	$ rebar3 dialyzer
```

## Feedback and contribution

All feedback and contribution is welcome. Please use the Github issue
tracker and pull requests for this.

The software is releasd under the Apache License, see the LICENSE
file. All code contributions should be provided under this license.


Enjoy!

Fred
