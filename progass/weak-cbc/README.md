# Attacking a weak CBC mode cipher
In this subtask you will attack a home-brewed, very badly designed cipher:

	- The cipher is based on a block cipher with block size 96 bits (12 bytes). A longer message is split into 96 bit blocks and encrypted in CBC mode, using a random IV.

	- The key size for the block cipher is also 12 bytes. The encryption of a block `m` with key `k` is simply `E(k, m) = k ⊕ m`.

We see immediately that `E` is the one time pad, and that it is misused by encryptings everal blocks with the same key `k`. 
In addition, you as attacker have access to both an encrypted message `c = c0 || c1 || . . . || cn` (where `c0` is the IV) and to the first block `m_1` of the corresponding plaintext, so you can do a known plaintext attack and recover the complete plaintext.

You should now visit the [data generation and code skeleton page](http://www.cse.chalmers.se/edu/course/TDA352/progass/) where you give the personnummer of one group member and get data generated for your attack. 
The data consists of two lines, first the `12 bytes personnummer` you gave and then an encrypted message (in  hexadecimal  notation).
The plaintext message that is encrypted has the personnummer as its first block, followed by a secret message which you must retrieve.
Note that the skeleton code parses the input file and takes care of necessary conversions from the representation we just mentioned.
Copy the data to `input.txt` in the subdirectory.
