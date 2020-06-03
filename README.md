# BitcoinML

[![Build Status](https://travis-ci.org/dakk/bitcoinml.svg)](https://travis-ci.org/dakk/bitcoinml)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/dakk/bitcoinml/blob/master/LICENSE)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://dakk.github.io/bitcoinml/bitcoinml/Bitcoinml/index.html)

Bitcoin data-structures library for OCaml.

## Features

Bitcoinml provides support for the following data-structures:
- Address (with also bech32 addresses)
- Block
- Block_lazy (lazy block evaluation)
- Hash
- Merkleroot
- Script (script execution, common pattern recognizer)
- Tx (In, Out, Witness)
- Varint (CompactSize)
- Params (network parameters for BTC, XTN and BCH)

### Segwit
The library is now fully compatible with segwit transactions (from version 0.3.0). The parser automatically recognizes
a segwit transaction, and parses/serializes it correctly. For those who want to avoid this behaviour, there are the
`*_legacy` functions which disable segwit.


## Installation

```opam install bitcoinml```


## Documentation

The odoc autogenerated documentation is available here: https://dakk.github.io/bitcoinml/


## Examples

You can find some examples in the test files.

- Block: https://github.com/dakk/bitcoinml/blob/master/test/block_test.ml
- Address: https://github.com/dakk/bitcoinml/blob/master/test/address_test.ml
- Transaction: https://github.com/dakk/bitcoinml/blob/master/test/tx_test.ml
- Proto: https://github.com/dakk/bitcoinml/blob/master/test/proto_test.ml
- Other: https://github.com/dakk/bitcoinml/blob/master/test/test_test.ml


## License

```
Copyright (c) 2016-2020 Davide Gessa

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
```


## Donations

Feel free to donate bitcoin to the developer: 13TRVwiqLMveg9aPAmZgcAix5ogKVgpe4T
