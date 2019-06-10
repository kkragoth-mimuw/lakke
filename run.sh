# add bnfc to path
PATH=/home/students/inf/PUBLIC/MRJP/bin/:$PATH

#remove generated
rm -rf generated

#generate grammar
make generate_grammar

#rm test
rm generated/TestLakke.hs

ghc -o lakke generated/*.hs src/Typechecker/*.hs src/Interpreter/*.hs src/Interpreter/Semantics/*.hs  Main.hs
