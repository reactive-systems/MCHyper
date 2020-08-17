# MCHyper
A hardware model checker for hyperproperties 

See also [https://www.react.uni-saarland.de/tools/mchyper/](https://www.react.uni-saarland.de/tools/mchyper/). 

### Introduction

Hyperproperties specify relations between multiple system exectuions and thereby genearalize trace properties. 
HyperLTL is a temporal logic capable of expressing hyperproperties. 
It is an extension of the linear-time temporal logic (LTL) with quantification over trace variables. 
With HyperLTL, we can formalize many interesting hyperproperties including information-flow policies and symmetry requirements. 

With MCHyper, it is possible to model check hardware circuits against these hyperproperties. 
To do this, MCHyper builds on the standard verification techniques from hardware model checking by using the hardware model checking tool ABC as its backend. 

### Online Interface and Tutorial

Try MCHyper without installation directly in our [online interface](https://www.react.uni-saarland.de/tools/online/MCHyper/) and check out the [tutorial](https://www.react.uni-saarland.de/tools/online/MCHyper/help.html). 

### Installation 

1. Install and build the dependencies: 
	* [GHC](https://www.haskell.org/platform/) (tested with versions 7.8.3, 8.4.4, and 8.10.1)
	* Additional Haskell dependencies: the parsec compiler framework, packages hashable and MissingH. Install using `cabal update; cabal install parsec hashable MissingH`. Make sure that ghc finds the dependencies when compiling. 
	* [Python](https://www.python.org) (tested with version 2.7)
	* [Aiger tools](http://fmv.jku.at/aiger/) (version 1.9.4): Compile using `./configure; make` in the directory aiger/.
	* [ABC model checker](https://github.com/berkeley-abc/abc) (version 1.01): Compile using `make`. If necessary, install the readline package and a g++ compiler using, e.g., `sudo apt-get install libreadline-dev build-essential`. 
1. Clone this repository: `git clone https://github.com/reactive-systems/MCHyper.git`
1. Compile MCHyper: `cd src; ghc Main.hs; cd ..`
1. Update the paths in the Python script mchyper.py pointing to the mchyper binary, the Aiger tools and to ABC. (E.g. `abc_bin = os.path.dirname(sys.argv[0]) + '/../abc/abc'`)
1. Try MCHyper by running `./mchyper.py -f "Forall (AP \"select<0>\" 0)" case-studies/quantifier-alternation_2019/bakery/good_bakery.atom.nondet2.aag -pdr -cex`. 

The last step should output: 

	Hyper Hyper!
	ABC is running (use '-v 1' to see output of ABC)
	
	Counterexample found. Safety violation.
	Writing counterexample to: tmp.cex.

If you obtain `Error: Tool did not provide a model.` make sure you are using unix line endigs. 

Please note that this tool is in a prototype stage. Thus, there may be bugs in the implementation. If you find one, please contact [Norine Coenen](https://www.react.uni-saarland.de/people/coenen.html). 

### Usage

The main inputs of MCHyper are the system to check (as an Aiger circuit) and the formula that should be checked. 
You can run MCHyper on your own inputs or execute our case studies: 
 
* The alternation-free case studies [FRS15] provide formulas.txt files containing the MCHyper calls that should be executed. 
* The quantifier-alternation case studies [CFST19] provide shell scripts that run the respective experiments. 
More information on the software doping case study can be found in the corresponding paper [DBBFH17]. 

To use MCHyper, execute the Python script and give formula, system circuit, strategy circuit (if necessary) and further options as arguments. 
The formula (following option `-f`) needs to be fully parenthesized, the operators are Forall, Exists, X (Next), Until, G, F, Neg, And, Or, Eq, Implies (amongst others, the full list of operators can be found in src/Logic.hs) and an atomic proposition ap is given as AP \"ap\" i, where i is the index of the quantifier it belongs to. 
A path to the system Aiger file is given without any preceding option. 
If a strategy circuit is provided, the path to this file is given after the option `-s`. 
Further options are `-v` followed by a number indicating the level of verbosity and options that should be used to call the backend tool ABC. 
Use `-h` or `--help` to print information on the usage of MCHyper. 

The online interface also allows a more readable input format for the formula and using Verilog as input format for the system. 
Here, [Yosys](http://www.clifford.at/yosys/) is used to obtain the corresponding Aiger circuit from the Verilog description. 

## Contributors

* [Markus N. Rabe](https://www.react.uni-saarland.de/people/rabe.html) - Initial version handeling the alternation-free fragment
* [Norine Coenen](https://www.react.uni-saarland.de/people/coenen.html) - Extension to one quantifier alternation

## Publications

* [FRS15]: [Algorithms for Model Checking HyperLTL and HyperCTL\*](https://www.react.uni-saarland.de/publications/FRS15.html), CAV 2015, Bernd Finkbeiner, Markus N. Rabe, Cesar Sanchez
* [CFST19]: [Verifying Hyperliveness](https://www.react.uni-saarland.de/publications/CFST19.html), CAV 2019, Norine Coenen, Bernd Finkbeiner, Cesar Sanchez, Leander Tentrup

### Other Related Publications

* [DBBFH17]: [Is your Software on Dope?](https://www.react.uni-saarland.de/publications/DBBFH17.html), ESOP 2017, Pedro R. D'Argenio, Gilles Barthe, Sebastian Biewer, Bernd Finkbeiner, Holger Hermanns
* [The Linear-Hyper-Branching Spectrum of Temporal Logics](https://www.react.uni-saarland.de/publications/FR14.html), Information Technology 2014, Bernd Finkbeiner and Markus N. Rabe
* [Temporal Logics for Hyperproperties](https://www.react.uni-saarland.de/publications/CFKMRS14.html), POST 2014, Michael Clarkson, Bernd Finkbeiner, Masoud Koleini, Kristopher K. Micinski, Markus N. Rabe, Cesar Sanchez
* [Model Checking Information Flow in Reactive Systems](https://www.react.uni-saarland.de/publications/DFKRS12.html), VMCAI 2012, Rayna Dimitrova, Bernd Finkbeiner, Mate Kovacs, Markus N. Rabe, Helmut Seidl

## Hyper

[Hyper](https://www.youtube.com/watch?v=RHVSshgPlQs&feature=youtu.be)