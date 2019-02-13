# mtxmkt


[Matrix Market](https://math.nist.gov/MatrixMarket/) provides a
repository of test matrices for various applications areas. It is best
described on the web site:

> A visual repository of test data for use in comparative studies of
> algorithms for numerical linear algebra, featuring nearly 500 sparse
> matrices from a variety of applications, as well as matrix generation
> tools and services.

This is an Erlang library to handle Matrix Market files. The format of
the files is described
[here](https://math.nist.gov/MatrixMarket/formats.html).

The library functions are based on the
[mmio C library](https://math.nist.gov/MatrixMarket/mmio-c.html)
provided by Matrix market.

## Implementation status

Initially functions for reading simpler types,
e.g. `integer`+`general`, of matrix files will be provided. However,
ultimately, the library will enable reading and writing of all 22
types of matrix files.

In the table below the first four columns correspond to the four
fileds following the `%%MatrixMarket ` string on the banner line. The
`Status` column indicates whether read and/or write is implemented for
that combination:

| Object | Format     | Field   | Symmetry       | Status  |
| :----- | :------    | :-----  | :--------      | :------ |
| matrix | array      | integer | general        | read    |
| matrix | coordinate | integer | general        | read    |
| matrix | array      | real    | general        | read    |
| matrix | coordinate | real    | general        | read    |
| matrix | array      | complex | general        | read    |
| matrix | coordinate | complex | general        | read    |
| matrix | array      | integer | symmetric      | read    |
| matrix | coordinate | integer | symmetric      | read    |
| matrix | array      | real    | symmetric      | read    |
| matrix | coordinate | real    | symmetric      | read    |
| matrix | array      | complex | symmetric      | read    |
| matrix | coordinate | complex | symmetric      | read    |
| matrix | array      | integer | skew-symmetric |         |
| matrix | coordinate | integer | skew-symmetric | read    |
| matrix | array      | real    | skew-symmetric |         |
| matrix | coordinate | real    | skew-symmetric | read    |
| matrix | array      | complex | skew-symmetric |         |
| matrix | coordinate | complex | skew-symmetric | read    |
| matrix | array      | complex | hermitian      |         |
| matrix | coordinate | complex | hermitian      | read    |
| matrix | coordinate | pattern | general        | read    |
| matrix | coordinate | pattern | symmetric      | read    |



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
