# LCT 0.2.0-alpha

Implementation of the Link Cut Tree data structure. A description of this
implementation is given in the "Implementing the Link-Cut Tree" paper. The
goal of this implementation is to provide a simple API for this data
structure and two implementations that can be used when testing
experimental algorithms. We also plan to support bindings for several
languages. We currently support C, Java, Python and lisp.

## Table of contents

- [Getting Started]
   - [Prerequisites]
   - [Installing]
   - [Running]
- [Contributing]
- [Versioning]
- [Authors]
- [License]
- [Acknowledgments]

## Getting Started

To get a copy of this software download or clone the GitHub repository.

Download:

```
wget https://github.com/LuisRusso-INESC-ID/LCT/archive/main.zip
```

Clone:

```
git clone https://github.com/LuisRusso-INESC-ID/LCT.git
```

### Prerequisites

This package was tested with [Arch Linux], it will most likely work with
other Linux distributions and UNIX variants. The experimental evaluation
was performed on a [Debian] server. Some version of the following
components must exist in the system.

For Linux:

* C compiler, [gcc]
* [GNU Make]
* glibc
* [splint]
* [Python] at least 3.11
* [Java] java-8-openjdk
* [Pahole] dwarves


### Installing

Execute `make` to obtain the binaries `CLI`, `pCLI`, `project`, `tester`,
`linTester`, `graphTester`, `pointerLCT.so` and `splayLCT.so`.

```
make
```

This compiles the unweighted version. To obtain binaries for the weighted
version uncomment the following line on the `LCT.h` file.

```
/* #define _VERSION_W */
```

Make sure to clean up before re-compiling, so:

```
make clean
make
```

### Running

There is a sample input file, which can be used with `CLI` and `pCLI`
binaries. These correspond to the splay and pointer implementations. A
simple sequence of commands is given in the file `input`. Hence simple
execution is the following:

```
./CLI < input
```

Note that this input is for the non-weigthed version. Likewise the same can
be done with the pointer implementation `pLCI`. The same `input` can also
be used with the Python version.

```
python LCT.py "./splayLCT.so" < input
```

For the pointer version use "pointerLCT.so".

There is a script to test that the implementation is sound, just run:

```
bash runTests.sh
```

This is script depends on the `tester` and `linTester` binaries.

The java version does not automatically build, so use the following commands:

```
make jLCT.h libSplayLCT.so libPointerLCT.so
java -cp . -Djava.library.path=. jLCT < input
```

For the lisp version we use [cffi] through quicklisp. Use the following
commands:

```
make libSplayLCT.so libPointerLCT.so
clisp lct.lisp < input
```


## Contributing

If you found this project useful please share it, also you can create an
[issue] with comments and suggestions.

## Versioning

We use [SemVer] for versioning. For the versions available, see the [tags]
on this repository.

## Authors

* **Luís M. S. Russo** - *Initial work* - [LuisRusso]

See also the list of [contributors] who participated in this project.

## License

This project is licensed under the MIT License - see
the [LICENSE file] for details

## Acknowledgments

* This software was produced for research that was funded in by national funds
through Fundação para a Ciência e Tecnologia ([FCT]) with reference
[DOI] 10.54499/UIDB/50021/2020.

* Thanks to [PurpleBooth] for the [README-Template].
* The [grip] tool by [Joe Esposito] was very handy for producing this file.


[Getting Started]: #getting-started
[Prerequisites]: #prerequisites
[Installing]: #installing
[Running]: #running
[Contributing]: #contributing
[Versioning]: #versioning
[Authors]: #authors
[License]: #license
[Acknowledgments]: #acknowledgments

[Arch Linux]: https://archlinux.org/
[Debian]: https://www.debian.org/
[gcc]: https://gcc.gnu.org/
[clang]: https://clang.llvm.org/
[GNU Make]: https://www.gnu.org/software/make/
[splint]: http://splint.org/
[Python]: https://www.python.org/
[Java]: https://openjdk.org/
[Pahole]: https://github.com/acmel/dwarves
[DOI]: https://doi.org/10.54499/UIDB/50021/2020
[cffi]: https://cffi.common-lisp.dev/

[issue]: ../../issues
[luis.russo@tecnico.ulisboa.pt]: mailto:luis.russo@tecnico.ulisboa.pt
[SemVer]: http://semver.org/
[tags]: ../../tags
[LuisRusso]: https://github.com/LuisRusso-INESC-ID
[contributors]: ../../contributors
[LICENSE file]: ./LICENSE
[FCT]: https://www.fct.pt/
[PurpleBooth]: https://gist.github.com/PurpleBooth
[README-Template]: https://gist.github.com/PurpleBooth/109311bb0361f32d87a2
[grip]: https://github.com/joeyespo/grip
[Joe Esposito]: https://github.com/joeyespo
