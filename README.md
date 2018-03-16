[![DOI](https://zenodo.org/badge/108546376.svg)](https://zenodo.org/badge/latestdoi/108546376)

# ECDFT
Code and data to reproduce the results of the paper
__Probabilistic performance estimators for computational chemistry methods: the empirical cumulative distribution function of absolute errors__ by P. Pernot and A. Savin [_J. Chem. Phys._ __148__:241707 (2018)](http://dx.doi.org/10.1063/1.5016248)     

* [dataG399.csv](dataG399.csv) Dataset of DFT calculations of atomization energies for molecules of the G3/99 set [Curtiss, L. A.; Raghavachari, K.; Redfern, P. C. & Pople, J. A. (2000) _J. Chem. Phys._ __112__:7374-7383](http://dx.doi.org/10.1063/1.481336). Contains:
    + __systems__: name of the molecule
    + __nAtoms__: number of atoms
    + __compos__: elemental composition
    + __Eref__: reference atomization energy
    + __B3LYP__ ... __PW86PBE__: errors (calculated - Eref) on atomization energy for a set of density functional approximations


* [analysis.R](analysis.R) Code which reads and processes dataG399.csv to produce the tables and figures of the article    
