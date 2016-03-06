# esab16

[![Clojars Project](https://img.shields.io/clojars/v/net.iovxw/esab16.svg)](https://clojars.org/net.iovxw/esab16)

Invisible base16.

## Usage

```clojure
(require '[esab16.core :as esab16])
;=> nil
(esab16/str-encode "Hello 世界!")
;=> "‬⁣⁡⁠⁡⁬⁡⁬⁡⁯"
(esab16/str-decode *1)
;=> "Hello 世界!"
```

## License

The MIT License (MIT)

Copyright (c) 2016 iovxw
