# Implementing Functional Languages

![](https://github.com/cutsea110/ifl/actions/workflows/haskell.yml/badge.svg)

Reference: [Simon L. Payton Jones, David R. Lester Implementing Functional Languages: a tutorial, 1991](https://www.microsoft.com/en-us/research/publication/implementing-functional-languages-a-tutorial)

# How to use

## Run on local

```bash
$ cabal run ifl -- -V
    ____________
   /  _/ ____/ /
   / // /_  / /
 _/ // __/ / /___
/___/_/   /_____/ Implimenting Functional Languages

> cabal run ifl -- [OPTION...] <program-file>
OPTION
  -c Compiler   --compiler=Compiler      compiler name (mark1 | mark2 | mark3 | mark4 | mark5 | mark5alt | mark5gc | mark5revgc | mark5cp | gmark1 | gmark2 | gmark3 | gmark4 | gmark5 | gmark6 | gmark7 | timark1 | timark1cp | timark2 | timark3)
  -v            --verbose                step output on stderr
  -t Threshold  --threshold=Threshold    threshold for Garbage Collection
  -l            --convert-to-list-based  convert to list based program
  -V, -?        --version                show version
```

## Run on docker

See [Dockerhub cutsea110/ifl](https://hub.docker.com/repository/docker/cutsea110/ifl/general).

```bash
$ cat test.ifl
fib n = if (n<2) 1 (fib (n-1) + fib (n-2));
main = fib 10

$ docker run -v ./:/work -it --rm cutsea110/ifl:0.3.10 ifl -c gmark7 /work/test.ifl
89
```

# Tips

How to confirm compiled code of any program for debug on Gmachine.Mark7.

```
ghci> let p = "s f g x = f x (g x)"
ghci> compileSc [] . head . parse $ p
```
When you take recursive function, you have to give an environment like below:

```
ghci> let p = "fib n = if (n<2) 1 (fib (n-1) + fib (n-2))"
ghci> compileSc [("fib", Label "fib")] . head . parse $ p
```


## For Developer

### How to build Docker image

You should specify the version 0.3.11, because the latest version is 0.3.10.

```bash
$ docker build -t cutsea110/ifl:0.3.11 .
```
### How to run on Docker image

I suppose that you have some test programs for ifl in `${PWD}/examples` directory.

```bash
$ docker run -v ${PWD}/examples:/work -it --rm cutsea110/ifl:0.3.11 ifl -v -c gmark7 /work/testProg80.ifl
```

### Share Dockerhub

```bash
$ docker login
$ docker push cutsea110/ifl:0.3.11
```

### Update This README

You should update docker image version for next.
