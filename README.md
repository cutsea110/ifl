# Implementing Functional Languages

![](https://github.com/cutsea110/ifl/actions/workflows/haskell.yml/badge.svg)

Reference: [Simon L. Payton Jones, David R. Lester Implementing Functional Languages: a tutorial, 1991](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial)

# How to use

## Run on local

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

## Run on docker

See [Dockerhub cutsea110/ifl](https://hub.docker.com/repository/docker/cutsea110/ifl/general).

```sh
$ cat test.ifl
fib n = if (n<2) 1 (fib (n-1) + fib (n-2));
main = fib 10

$ docker run -v ./:/work -it --rm cutsea110/ifl:0.1.0 ifl -c gmark7 /work/test.ifl
89
```

# Tips

How to confirm compiled code of any program for debug on Gmachine.Mark7.

```
ghci> let p = "fib n = if (n<2) 1 (fib (n-1) + fib (n-2))"
ghci> compileSc . head . parse $ p
```


## For Developer

### How to build Docker image

```sh
$ docker build -t cutsea110/ifl:0.1.0 .
```
### How to run on Docker image

I suppose that you have some test programs for ifl in `${PWD}/examples` directory.

```sh
$ docker run -v ./examples:/work -it --rm cutsea110/ifl:0.1.0 ifl -v -c gmark7 /work/testProg80.ifl
```

### Share Dockerhub

```sh
$ docker login
$ docker push cutsea110/ifl:0.1.0
```
