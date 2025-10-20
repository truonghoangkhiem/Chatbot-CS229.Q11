:- module(lexicon, [to_drs/2]).

to_drs(yn(conj(Cs)),  drs([], Cs)).      % nhiều điều kiện
to_drs(yn(Prop),       drs([], [Prop])).
to_drs(who(X,Prop),    drs([X], [Prop])).
to_drs(what(X,Rel),    drs([X], [Rel])).
