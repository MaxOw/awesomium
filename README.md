# Awesomium Bindings #

High-level bindings to [Awesomium](http://www.awesomium.com)(1.6.5)
for Haskell.

You will need [awesomium-raw](https://github.com/MaxOw/awesomium-raw)
library to use this.

I call this bindings high-level, but in fact it's not really that much
higher than the raw bindings since I already made sure for the raw to
be as friendly as possible. The only things that really differs are
the callbacks, and the usage of
[Data.Aeson](http://hackage.haskell.org/package/aeson) for more
convenient communication with Javascript. Also, more user-friendly
functions naming/modules hierarchy.

## TODO ##
    * Examples
    * Documentation
