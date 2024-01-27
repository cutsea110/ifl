# Implementing Functional Languages

![](https://github.com/cutsea110/ifl/actions/workflows/haskell.yml/badge.svg)

Reference: [Simon L. Payton Jones, David R. Lester Implementing Functional Languages: a tutorial, 1991](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial)

# How to use

```sh
$ cabal run -- ifl
    ____________
   /  _/ ____/ /
   / // /_  / /
 _/ // __/ / /___
/___/_/   /_____/ Implimenting Functional Languages

> cabal v2-run ifl -- [OPTION...] <program-file>
OPTION
  -c Compiler  --compiler=Compiler  compiler name (mark1 | mark2 | mark3 | mark4 | mark5 | mark5cnv | mark5alt | mark5altcnv | mark5gc | mark5gccnv | mark5revgc | mark5revgccnv | mark5cp | gmark1 | gmark2 | gmark3 | gmark4 | gmark5 | gmark6 | gmark7 | timark1 | timark1cp | timark2)
  -v           --verbose            chatty output on stderr
  -V, -?       --version            show version
```



# Tips

How to confirm compiled code of any program for debug on Gmachine.Mark7.

```
ghci> let p = "fib n = if (n<2) 1 (fib (n-1) + fib (n-2))"
ghci> compileSc . head . parse $ p
```

