# Attacking ElGamal with a weak PRG

Thanks to its random component, two ElGamal encryptions of the same message can
look completely different. However, this also makes the strength of the
encryption depend on the random number generation. In this assignment you will
attack textbook ElGamal under a weak number generator.

We intercepted an ElGamal encryption of a message together with a partial time
stamp with one-second precision on the ciphertext. We also know that the PRG
used for encryption was in pseudocode

```
integer createRandomNumber:
    return YEAR*(10^10) + month*(10^8) + days*(10^6) + 
           hours*(104) + minute*(10^2) + sec + millisecs;
```

Thus, even though we cannot exactly say which random number was generated during
encryption, the search space is limited. Your task is to decrypt the message
exploiting this weakness of the pseudorandom generator. Assignment data and code
skeleton at the same site as before (see `input.txt`). The encrypted message is
again a sentence in English.

Note that `p` is a `1024` bit prime, so we are here working with numbers
of realistic size.
