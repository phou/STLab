# Scala with Multiparty Session Types README



1. **[Getting started](#1-gettin)**

2. **[Quick Start Guide](#2-quick-start-guide)**
    - [2.1](#21-example-protocols) Example Protocols
    - [2.2](#22-generating-scala-code-via-teatrino) Generating Scala Code via **Teatrino**
    - [2.3](#23-running-generated-scala-code) Running Generated Scala Code
3. **[Benchmarking](#3-benchmarking)**
4. **[Artefact Layout](#4-artefact-layout)**



## 1. Getting Started

```bash
$ git clone https://github.com/phou/STLab.git
$ cd STLab
$ make
$ make install
```

You should now have a `HsScribble` executable in your present working directory. 


## 2. User Guide

We provide commands and scripts that will allow you to both quickly generate
code from the examples, and compile and execute that generated
code.

---

### 2.1 Example Protocols

All example protocols can be found in the
`STLab/scribble` directory.

---

### 1.2 Generating Scala Code

To generate Scala code from a given Scribble file, `Protocol.nuscr`, it suffices
to run the command:

```bash
$ HsScribble -f scribble/Protocol.nuscr -e
```

This will generate a new file: `scala/Protocol.scala`.

For example, running

```bash
$ HsScribble -f scribble/EVoting.nuscr -e
```
will result in the output:

```
Input file name: scribble/EVoting.nuscr
Output Directory: scala/
```
and will produce `scala/EVoting.scala`. 

Other optional flags are available and can be seen by running:

```bash
$ HsScribble -h
```

To generate code for all protocols in `scribble/`, you can run the command:
```
$ ./genAll.sh
```
The resulting generated code can be found in `scala/` directory.

---

### 1.3 Running Generated Scala Code

To compile and run generated Scala code, e.g. `scala/Protocol.scala`, run the
`runScala.sh` script:

```bash
$ ./runScala.sh scala/Protocol.scala
```

This will run the Scala build tool `sbt` using the provided `build.sbt`
configuration file. Generated code includes a number of print statements that
are designed to illustrate the communications behaviour of the given example.
For example, for `a_PingPongAll.nuscr` the generated code may produce the
following output:
```
-- Q recursing; t = RecT0
-- Q entering recursion body; t = RecT0
-- Q expecting Ping on c0 (effpi.channel.QueueChannel@1981978917)
-- Q received Ping on c0 (effpi.channel.QueueChannel@1981978917)
-- Q sending Pong on c1 (effpi.channel.QueueChannel@1942478618)
-- P sent Ping on c0 (effpi.channel.QueueChannel@1981978917)
-- P expecting Pong on c1 (effpi.channel.QueueChannel@1942478618)
-- P received Pong on c1 (effpi.channel.QueueChannel@1942478618)
-- P recursing; t = RecT0
-- P entering recursion body; t = RecT0
```
Please note that some examples, including `a_PingPongAll.nuscr`, infinitely
recurse and can be terminated by `ctrl-c`.

The first time `runScala` is used, `sbt` may take additional time to fetch
dependencies and compile the **Effpi** library.

---
---

