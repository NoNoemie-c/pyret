### rapport du 14/12

les fichiers sources sont:
- error.ml: quelques exceptions destinées à être levées en cas de programme mal écrit et attrapées par pyretc.ml
- ast.ml / tats.ml: l'AST et sa version typée, ainsi que des pretty-printers
- lexer.mll / parser.mly / typer.ml / emiter.ml: les sources de chaque étape de la chaine de compilation (emiter est pour l'instant essentiellement vide)
- pyretc.ml: le fichier principal 

L'AST a la même structure que la grammaire de pyret, à ceci près que les opérations du type x + y + z sont modélisés en un objet EOp (+, [x; y; z]).
La plupart des objets de l'AST sont augmentés de leur position de le fichier à compiler et ceux du TAST sont augmentés de leur type.

## analyses lexicales et syntaxiques

Pour des raisons de conflits sur le token IDENT, je me suis retrouvée à le copier 7 fois, en fonction du token suivant, et de même un token RPLP = ")(" permet de traiter les appels, et en particulier les for sans conflit. A ceci près, le lexer ne comporte rien de spécial.
Le parser utilise l'analyse plus poussée du lexer pour au final comporter assez peu d'états et de duplications de règles.

## typage

Toute la puissance de l'algorithme w n'est ici pas nécessaire. Cependant, pour des raisons de temps j'ai en grande partie repris et adapté le code du TD 6.
Il n'y a pas grand chose à signaler, la principale difficulté a été pour moi de faire attention à ne pas unifier les variables de type T dans le corps d'une fonction f`<T>` (ce qui est fait en gardant dans l'environnement une map des variables polymorphes).

## ce que je n'ai pas eu le temps de faire

- 2 statements peuvent être sur la même ligne, bien que pyret refuse ce cas de figure.
- Mon automate de parsing contient 1 conflit reduce/reduce (bien qu'il me semble que les deux interprétations possibles produisent le même AST). Je pense pouvoir résoudre ce problème en renvoyant des listes de token au lexing, ce que je n'ai pas eu le temps d'implémenter.
- La plupart des messages d'erreurs contiennent des localisations par défaut, tout particulièrement les erreurs de syntaxe.