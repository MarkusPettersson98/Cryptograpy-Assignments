# Fragment of a Math Library for Cryptography
In this task you will implement some functions from number theory that are
commonly used in cryptography.

The aim is to make you familiar with the algorithms by actuallyimplementing
them. To emphasize that this a learning task, you implement the function for
64-bit integers rather than arbitrary size integers as in real cryptographic
applications.

You must implement the following functions:

- the extended Euclidean algorithm.

- modular inverse. 
  - Your function should return `0` if the number is not invertible.

- modular exponentiation.

- Fermat’s primality test. 
  - Instead of picking random values `a` to test the primality of a number `n`, make a start from `2` and increment it by `1` at each
  new iteration, until you have tested all the values below `n/3`.
