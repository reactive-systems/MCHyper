#! /bin/sh
set -e

echo "start experiments:"


echo "\n\n1) Bakery Sym5 AE \n--------------------------"
time ./../../../mchyper.py -f "Forall (Exists (Implies (And (G (Implies (AP \"select<1>\" 0) (Neg (AP \"select<0>\" 0)))) (G (Implies (AP \"sym_break<1>\" 0) (Neg (AP \"sym_break<0>\" 0))))) (G (And (And (Eq (AP \"bakery|pc<*0*><0>_out\" 0) (AP \"bakery|pc<*1*><0>_out\" 1)) (Eq (AP \"bakery|pc<*0*><0>_out\" 1) (AP \"bakery|pc<*1*><0>_out\" 0))) (And (And (Eq (AP \"bakery|pc<*0*><1>_out\" 0) (AP \"bakery|pc<*1*><1>_out\" 1)) (Eq (AP \"bakery|pc<*0*><1>_out\" 1) (AP \"bakery|pc<*1*><1>_out\" 0))) (And (And (Eq (AP \"bakery|pc<*0*><2>_out\" 0) (AP \"bakery|pc<*1*><2>_out\" 1)) (Eq (AP \"bakery|pc<*0*><2>_out\" 1) (AP \"bakery|pc<*1*><2>_out\" 0))) (And (Eq (AP \"bakery|pc<*0*><3>_out\" 0) (AP \"bakery|pc<*1*><3>_out\" 1)) (Eq (AP \"bakery|pc<*0*><3>_out\" 1) (AP \"bakery|pc<*1*><3>_out\" 0)))))))))" ./good_bakery.atom.nondet2.aag -pdr -cex -s ./sym4.aag


echo "\n\n2) Bakery Sym6 AE \n--------------------------"
time ./../../../mchyper.py -f "Forall (Exists (WUntil (And (And (Eq (AP \"bakery|pc<*0*><0>_out\" 0) (AP \"bakery|pc<*1*><0>_out\" 1)) (Eq (AP \"bakery|pc<*0*><0>_out\" 1) (AP \"bakery|pc<*1*><0>_out\" 0))) (And (And (Eq (AP \"bakery|pc<*0*><1>_out\" 0) (AP \"bakery|pc<*1*><1>_out\" 1)) (Eq (AP \"bakery|pc<*0*><1>_out\" 1) (AP \"bakery|pc<*1*><1>_out\" 0))) (And (And (Eq (AP \"bakery|pc<*0*><2>_out\" 0) (AP \"bakery|pc<*1*><2>_out\" 1)) (Eq (AP \"bakery|pc<*0*><2>_out\" 1) (AP \"bakery|pc<*1*><2>_out\" 0))) (And (Eq (AP \"bakery|pc<*0*><3>_out\" 0) (AP \"bakery|pc<*1*><3>_out\" 1)) (Eq (AP \"bakery|pc<*0*><3>_out\" 1) (AP \"bakery|pc<*1*><3>_out\" 0)))))) (Neg (And (Implies (AP \"select<1>\" 0) (Neg (AP \"select<0>\" 0))) (Implies (AP \"sym_break<1>\" 0) (Neg (AP \"sym_break<0>\" 0)))))))" ./good_bakery.atom.nondet2.aag -pdr -cex -s ./sym4.aag 


echo "\n\n3) Bakery Sym7 AE \n--------------------------"
time ./../../../mchyper.py -f "Forall (Exists (Implies (G (And (And (Implies (AP \"sym_break<1>\" 0) (Neg (AP \"sym_break<0>\" 0))) (Implies (AP \"sym_break<2>\" 0) (And (Neg (AP \"sym_break<1>\" 0)) (Neg (AP \"sym_break<0>\" 0))))) (And (Implies (AP \"select<1>\" 0) (Neg (AP \"select<0>\" 0))) (Implies (AP \"select<2>\" 0) (And (Neg (AP \"select<1>\" 0)) (Neg (AP \"select<0>\" 0))))))) (G (And (And (Eq (AP \"bakery|pc<*0*><0>_out\" 0) (AP \"bakery|pc<*1*><0>_out\" 1)) (Eq (AP \"bakery|pc<*0*><0>_out\" 1) (AP \"bakery|pc<*1*><0>_out\" 0))) (And (And (Eq (AP \"bakery|pc<*0*><1>_out\" 0) (AP \"bakery|pc<*1*><1>_out\" 1)) (Eq (AP \"bakery|pc<*0*><1>_out\" 1) (AP \"bakery|pc<*1*><1>_out\" 0))) (And (And (Eq (AP \"bakery|pc<*0*><2>_out\" 0) (AP \"bakery|pc<*1*><2>_out\" 1)) (Eq (AP \"bakery|pc<*0*><2>_out\" 1) (AP \"bakery|pc<*1*><2>_out\" 0))) (And (Eq (AP \"bakery|pc<*0*><3>_out\" 0) (AP \"bakery|pc<*1*><3>_out\" 1)) (Eq (AP \"bakery|pc<*0*><3>_out\" 1) (AP \"bakery|pc<*1*><3>_out\" 0)))))))))" ./good_bakery.atom.nondet2.proc5.aag -pdr -cex -s ./sym7.aag 


echo "\n\n4) Bakery Sym8 AE \n--------------------------"
time ./../../../mchyper.py -f "Forall (Exists (WUntil (And (And (Eq (AP \"bakery|pc<*0*><0>_out\" 0) (AP \"bakery|pc<*1*><0>_out\" 1)) (Eq (AP \"bakery|pc<*0*><0>_out\" 1) (AP \"bakery|pc<*1*><0>_out\" 0))) (And (And (Eq (AP \"bakery|pc<*0*><1>_out\" 0) (AP \"bakery|pc<*1*><1>_out\" 1)) (Eq (AP \"bakery|pc<*0*><1>_out\" 1) (AP \"bakery|pc<*1*><1>_out\" 0))) (And (And (Eq (AP \"bakery|pc<*0*><2>_out\" 0) (AP \"bakery|pc<*1*><2>_out\" 1)) (Eq (AP \"bakery|pc<*0*><2>_out\" 1) (AP \"bakery|pc<*1*><2>_out\" 0))) (And (Eq (AP \"bakery|pc<*0*><3>_out\" 0) (AP \"bakery|pc<*1*><3>_out\" 1)) (Eq (AP \"bakery|pc<*0*><3>_out\" 1) (AP \"bakery|pc<*1*><3>_out\" 0)))))) (Neg (And (And (Implies (AP \"sym_break<1>\" 0) (Neg (AP \"sym_break<0>\" 0))) (Implies (AP \"sym_break<2>\" 0) (And (Neg (AP \"sym_break<1>\" 0)) (Neg (AP \"sym_break<0>\" 0))))) (And (Implies (AP \"select<1>\" 0) (Neg (AP \"select<0>\" 0))) (Implies (AP \"select<2>\" 0) (And (Neg (AP \"select<1>\" 0)) (Neg (AP \"select<0>\" 0)))))))))" ./good_bakery.atom.nondet2.proc5.aag -pdr -cex -s ./sym7.aag 

