# noxstatus
### Gets a list of unpaid client and supplier invoices from accounting site [fortnox](http://fortnox.se) and outputs to stdout

This was mainly made as an exercise in haskell, you can modify the output for use with [BitBar](https://github.com/matryer/bitbar).

Compiles with stack, it needs an extra module with the fortnox API client secret ```app/ClientSecret.hs``` containing:
``` haskell
module ClientSecret where

clientSecret :: String
clientSecret = "CLIENTSECRET GOES HERE"
```
