---
title: 'Hello, World!'
date: 'August 7, 2016'
include: code/helloworld/src/Main.hs
ghci: code/helloworld
---

Here's some code: 

~~~include
header
~~~

Let's run it!

~~~ghci
main
~~~

It works!

<!--more-->

Now let's define a function in ghci:

~~~ghci
let replicateMain n = replicateM_ n main
~~~

Oops!

~~~{.ghci .haskell}
import Control.Monad
let replicateMain n = replicateM_ n main
:t replicateMain
~~~

~~~ghci
replicateMain 2
replicateMain 5
~~~

It works!

This post was generated with [htut](https://github.com/aaronvargo/htut).
