# noxstatus
### Gets a list of unpaid client and supplier invoices from accounting site [fortnox](http://fortnox.se) and outputs to stdout

This was mainly made as an exercise in haskell, you can modify the output for use with [BitBar](https://github.com/matryer/bitbar).

Compiles with stack and nix, needs a client secret from fortnox set it in
`app/ClientSecret.hs`, or via the nix derivation:

``` haskell
clientSecret :: String
clientSecret = "CLIENTSECRET GOES HERE"
```
