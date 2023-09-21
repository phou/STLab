# Scala with Multiparty Session Types README




1. **[User Guide](#1-user-start-guide)**
    - [1.1](#11-getting-started) Getting Started
    - [1.2](#12-example-protocols) Example Protocols
    - [1.3](#13-generating-scala-code-via-teatrino) Generating Scala Code
    - [1.4](#14-running-generated-scala-code) Running Generated Scala Code


## 1. User Guide

We provide commands and scripts that will allow you to both quickly generate
code from the examples, and compile and execute that generated
code.

---
### 1.1 Getting Started
```bash
$ git clone https://github.com/phou/STLab.git
$ cd STLab
$ make
$ make install
```

You should now have a `HsScribble` executable in your present working directory. 


### 1.2 Example Protocols

All example protocols can be found in the
`STLab/scribble` directory.

---

### 1.3 Generating Scala Code

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

The resulting generated code can be found in `scala/` directory.

---

### 1.4 Running Generated Scala Code

To compile and run generated Scala code, e.g. `scala/Protocol.scala`, run the
`runScala.sh` script:

```bash
$ ./runScala.sh scala/Protocol.scala
```

This will run the Scala build tool `sbt` using the provided `build.sbt`
configuration file. Generated code includes a number of print statements that
are designed to illustrate the communications behaviour of the given example.


The first time `runScala` is used, `sbt` may take additional time to fetch
dependencies and compile the **Effpi** library.



