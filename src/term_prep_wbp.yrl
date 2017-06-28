Nonterminals properties property.
Terminals integer var ';' '..'.
Rootsymbol properties.
properties -> property properties : ['$1' | '$2'].
properties -> property : ['$1'].
property -> integer ';' var : ['$1', '$3'].
property -> integer '..' integer ';' var : [{'$1', '$3'}, '$5'].
