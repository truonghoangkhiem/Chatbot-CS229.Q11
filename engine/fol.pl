:- module(fol, [drs_to_fol/2]).
drs_to_fol(drs(U,C), fol(exists(U), and(C))).
