#SimpleRegex

SimpleRegex is a VERY simple regex library  I created in Haskell.  The purpose of the project was so that I could become more proficient in Haskell and functional programming, as well as learn how to parse and execute regular expressions.  The program defines a library called SimpleRegex which has two operators: =~ ( a greedy match) and =~? (a non greedy match).  This source code is a good example of how to turn a regex string into an NFA using Thompson construction, convert an NFA to DFA using epsilon closures and run a DFA on input strings.

SimpleRegex only supports the | (or), * (closure) and concatenation operators.

##Links
* [SimpleRegex home page](http://matthewmanela.com/projects/simpleregex-in-haskell/)
