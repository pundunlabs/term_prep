Nonterminals words word.
Terminals string.
Rootsymbol words.
words -> word words : ['$1' | '$2'].
words -> word : ['$1'].
word -> string : ['$1'].
