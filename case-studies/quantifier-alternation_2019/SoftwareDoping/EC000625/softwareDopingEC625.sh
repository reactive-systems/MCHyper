#! /bin/sh
set -e

echo "start experiments:"


echo "\n1) EC625 12 AA (from paper) \n--------------------------"
time ./../../../../mchyper.py -f "(Forall (Forall (Implies (G (And (AP \"valid\" 0) (AP \"valid\" 1))) (Implies (G (Neg (Or (Or (AP \"throttle@4\" 0) (And (And (Neg (AP \"throttle@4\" 0)) (AP \"throttle@3\" 0)) (AP \"throttle@2\" 0))) (And (And (And (And (Neg (AP \"throttle@4\" 0)) (AP \"throttle@3\" 0)) (Neg (AP \"throttle@2\" 0))) (AP \"throttle@1\" 0)) (AP \"throttle@0\" 0))))) (WUntil (Neg (Or (And (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 0) (Neg (AP \"NOx@7\" 1))) (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (And (AP \"NOx@6\" 0) (Neg (AP \"NOx@6\" 1))))) (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (And (AP \"NOx@5\" 0) (Neg (AP \"NOx@5\" 1))))) (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (And (AP \"NOx@4\" 0) (Neg (AP \"NOx@4\" 1))))) (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 1))) (And (AP \"NOx@3\" 0) (Neg (AP \"NOx@3\" 1))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 1))) (And (AP \"NOx@2\" 0) (Neg (AP \"NOx@2\" 1))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 1))) (Eq (AP \"NOx@2\" 0) (AP \"NOx@2\" 1))) (And (AP \"NOx@1\" 0) (Neg (AP \"NOx@1\" 1))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 1))) (Eq (AP \"NOx@2\" 0) (AP \"NOx@2\" 1))) (Eq (AP \"NOx@1\" 0) (AP \"NOx@1\" 1))) (And (AP \"NOx@0\" 0) (Neg (AP \"NOx@0\" 1))))) (And (Neg (AP \"n_k@8\" 1)) (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 0) (Neg (AP \"n_k@7\" 1))) (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 1)) (And (AP \"NOx@6\" 0) (Neg (AP \"n_k@6\" 1))))) (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 1))) (And (AP \"NOx@5\" 0) (Neg (AP \"n_k@5\" 1))))) (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 1))) (And (AP \"NOx@4\" 0) (Neg (AP \"n_k@4\" 1))))) (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"n_k@4\" 1))) (And (AP \"NOx@3\" 0) (Neg (AP \"n_k@3\" 1))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"n_k@4\" 1))) (Eq (AP \"NOx@3\" 0) (AP \"n_k@3\" 1))) (And (AP \"NOx@2\" 0) (Neg (AP \"n_k@2\" 1))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"n_k@4\" 1))) (Eq (AP \"NOx@3\" 0) (AP \"n_k@3\" 1))) (Eq (AP \"NOx@2\" 0) (AP \"n_k@2\" 1))) (And (AP \"NOx@1\" 0) (Neg (AP \"n_k@1\" 1))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"n_k@4\" 1))) (Eq (AP \"NOx@3\" 0) (AP \"n_k@3\" 1))) (Eq (AP \"NOx@2\" 0) (AP \"n_k@2\" 1))) (Eq (AP \"NOx@1\" 0) (AP \"n_k@1\" 1))) (And (AP \"NOx@0\" 0) (Neg (AP \"n_k@0\" 1))))))) (And (Neg (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 0) (Neg (AP \"NOx@7\" 1))) (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (And (AP \"NOx@6\" 0) (Neg (AP \"NOx@6\" 1))))) (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (And (AP \"NOx@5\" 0) (Neg (AP \"NOx@5\" 1))))) (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (And (AP \"NOx@4\" 0) (Neg (AP \"NOx@4\" 1))))) (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 1))) (And (AP \"NOx@3\" 0) (Neg (AP \"NOx@3\" 1))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 1))) (And (AP \"NOx@2\" 0) (Neg (AP \"NOx@2\" 1))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 1))) (Eq (AP \"NOx@2\" 0) (AP \"NOx@2\" 1))) (And (AP \"NOx@1\" 0) (Neg (AP \"NOx@1\" 1))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 1))) (Eq (AP \"NOx@2\" 0) (AP \"NOx@2\" 1))) (Eq (AP \"NOx@1\" 0) (AP \"NOx@1\" 1))) (And (AP \"NOx@0\" 0) (Neg (AP \"NOx@0\" 1)))))) (And (Neg (AP \"n_k@8\" 0)) (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 1) (Neg (AP \"n_k@7\" 0))) (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 0)) (And (AP \"NOx@6\" 1) (Neg (AP \"n_k@6\" 0))))) (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 0))) (And (AP \"NOx@5\" 1) (Neg (AP \"n_k@5\" 0))))) (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 0))) (And (AP \"NOx@4\" 1) (Neg (AP \"n_k@4\" 0))))) (And (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 0))) (Eq (AP \"NOx@4\" 1) (AP \"n_k@4\" 0))) (And (AP \"NOx@3\" 1) (Neg (AP \"n_k@3\" 0))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 0))) (Eq (AP \"NOx@4\" 1) (AP \"n_k@4\" 0))) (Eq (AP \"NOx@3\" 1) (AP \"n_k@3\" 0))) (And (AP \"NOx@2\" 1) (Neg (AP \"n_k@2\" 0))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 0))) (Eq (AP \"NOx@4\" 1) (AP \"n_k@4\" 0))) (Eq (AP \"NOx@3\" 1) (AP \"n_k@3\" 0))) (Eq (AP \"NOx@2\" 1) (AP \"n_k@2\" 0))) (And (AP \"NOx@1\" 1) (Neg (AP \"n_k@1\" 0))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 0))) (Eq (AP \"NOx@4\" 1) (AP \"n_k@4\" 0))) (Eq (AP \"NOx@3\" 1) (AP \"n_k@3\" 0))) (Eq (AP \"NOx@2\" 1) (AP \"n_k@2\" 0))) (Eq (AP \"NOx@1\" 1) (AP \"n_k@1\" 0))) (And (AP \"NOx@0\" 1) (Neg (AP \"n_k@0\" 0))))))))) (Or (And (Or (Or (Or (Or (And (AP \"throttle@4\" 0) (Neg (AP \"throttle@4\" 1))) (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 1)) (And (AP \"throttle@3\" 0) (Neg (AP \"throttle@3\" 1))))) (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 1))) (And (AP \"throttle@2\" 0) (Neg (AP \"throttle@2\" 1))))) (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 1))) (Eq (AP \"throttle@2\" 0) (AP \"throttle@2\" 1))) (And (AP \"throttle@1\" 0) (Neg (AP \"throttle@1\" 1))))) (And (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 1))) (Eq (AP \"throttle@2\" 0) (AP \"throttle@2\" 1))) (Eq (AP \"throttle@1\" 0) (AP \"throttle@1\" 1))) (And (AP \"throttle@0\" 0) (Neg (AP \"throttle@0\" 1))))) (And (Neg (AP \"t_k@5\" 1)) (Or (Or (Or (Or (And (AP \"throttle@4\" 0) (Neg (AP \"t_k@4\" 1))) (And (Eq (AP \"throttle@4\" 0) (AP \"t_k@4\" 1)) (And (AP \"throttle@3\" 0) (Neg (AP \"t_k@3\" 1))))) (And (And (Eq (AP \"throttle@4\" 0) (AP \"t_k@4\" 1)) (Eq (AP \"throttle@3\" 0) (AP \"t_k@3\" 1))) (And (AP \"throttle@2\" 0) (Neg (AP \"t_k@2\" 1))))) (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"t_k@4\" 1)) (Eq (AP \"throttle@3\" 0) (AP \"t_k@3\" 1))) (Eq (AP \"throttle@2\" 0) (AP \"t_k@2\" 1))) (And (AP \"throttle@1\" 0) (Neg (AP \"t_k@1\" 1))))) (And (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"t_k@4\" 1)) (Eq (AP \"throttle@3\" 0) (AP \"t_k@3\" 1))) (Eq (AP \"throttle@2\" 0) (AP \"t_k@2\" 1))) (Eq (AP \"throttle@1\" 0) (AP \"t_k@1\" 1))) (And (AP \"throttle@0\" 0) (Neg (AP \"t_k@0\" 1))))))) (And (Neg (Or (Or (Or (Or (And (AP \"throttle@4\" 0) (Neg (AP \"throttle@4\" 1))) (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 1)) (And (AP \"throttle@3\" 0) (Neg (AP \"throttle@3\" 1))))) (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 1))) (And (AP \"throttle@2\" 0) (Neg (AP \"throttle@2\" 1))))) (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 1))) (Eq (AP \"throttle@2\" 0) (AP \"throttle@2\" 1))) (And (AP \"throttle@1\" 0) (Neg (AP \"throttle@1\" 1))))) (And (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 1))) (Eq (AP \"throttle@2\" 0) (AP \"throttle@2\" 1))) (Eq (AP \"throttle@1\" 0) (AP \"throttle@1\" 1))) (And (AP \"throttle@0\" 0) (Neg (AP \"throttle@0\" 1)))))) (And (Neg (AP \"t_k@5\" 0)) (Or (Or (Or (Or (And (AP \"throttle@4\" 1) (Neg (AP \"t_k@4\" 0))) (And (Eq (AP \"throttle@4\" 1) (AP \"t_k@4\" 0)) (And (AP \"throttle@3\" 1) (Neg (AP \"t_k@3\" 0))))) (And (And (Eq (AP \"throttle@4\" 1) (AP \"t_k@4\" 0)) (Eq (AP \"throttle@3\" 1) (AP \"t_k@3\" 0))) (And (AP \"throttle@2\" 1) (Neg (AP \"t_k@2\" 0))))) (And (And (And (Eq (AP \"throttle@4\" 1) (AP \"t_k@4\" 0)) (Eq (AP \"throttle@3\" 1) (AP \"t_k@3\" 0))) (Eq (AP \"throttle@2\" 1) (AP \"t_k@2\" 0))) (And (AP \"throttle@1\" 1) (Neg (AP \"t_k@1\" 0))))) (And (And (And (And (Eq (AP \"throttle@4\" 1) (AP \"t_k@4\" 0)) (Eq (AP \"throttle@3\" 1) (AP \"t_k@3\" 0))) (Eq (AP \"throttle@2\" 1) (AP \"t_k@2\" 0))) (Eq (AP \"throttle@1\" 1) (AP \"t_k@1\" 0))) (And (AP \"throttle@0\" 1) (Neg (AP \"t_k@0\" 0)))))))))))))" ./nox.aag -pdr -cex

#####################################################################################################################################################

echo "\n\n2) EC625 10.1 AAE (1st part of property that should actually be checked) \n--------------------------"
time ./../../../../mchyper.py -f "(Forall (Forall (Exists (Implies (G (And (AP \"valid\" 0) (AP \"valid\" 1))) (And (AP \"valid\" 2) (Implies (G (Neg (Or (Or (AP \"throttle@4\" 0) (And (And (Neg (AP \"throttle@4\" 0)) (AP \"throttle@3\" 0)) (AP \"throttle@2\" 0))) (And (And (And (And (Neg (AP \"throttle@4\" 0)) (AP \"throttle@3\" 0)) (Neg (AP \"throttle@2\" 0))) (AP \"throttle@1\" 0)) (AP \"throttle@0\" 0))))) (And (G (And (And (And (And (Eq (AP \"throttle@0\" 1) (AP \"throttle@0\" 2)) (Eq (AP \"throttle@1\" 1) (AP \"throttle@1\" 2))) (Eq (AP \"throttle@2\" 1) (AP \"throttle@2\" 2))) (Eq (AP \"throttle@3\" 1) (AP \"throttle@3\" 2))) (Eq (AP \"throttle@4\" 1) (AP \"throttle@4\" 2)))) (WUntil (Neg (Or (And (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 0) (Neg (AP \"NOx@7\" 2))) (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (And (AP \"NOx@6\" 0) (Neg (AP \"NOx@6\" 2))))) (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (And (AP \"NOx@5\" 0) (Neg (AP \"NOx@5\" 2))))) (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (And (AP \"NOx@4\" 0) (Neg (AP \"NOx@4\" 2))))) (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 2))) (And (AP \"NOx@3\" 0) (Neg (AP \"NOx@3\" 2))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 2))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 2))) (And (AP \"NOx@2\" 0) (Neg (AP \"NOx@2\" 2))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 2))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 2))) (Eq (AP \"NOx@2\" 0) (AP \"NOx@2\" 2))) (And (AP \"NOx@1\" 0) (Neg (AP \"NOx@1\" 2))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 2))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 2))) (Eq (AP \"NOx@2\" 0) (AP \"NOx@2\" 2))) (Eq (AP \"NOx@1\" 0) (AP \"NOx@1\" 2))) (And (AP \"NOx@0\" 0) (Neg (AP \"NOx@0\" 2))))) (And (Neg (AP \"n_k@8\" 2)) (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 0) (Neg (AP \"n_k@7\" 2))) (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 2)) (And (AP \"NOx@6\" 0) (Neg (AP \"n_k@6\" 2))))) (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 2))) (And (AP \"NOx@5\" 0) (Neg (AP \"n_k@5\" 2))))) (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 2))) (And (AP \"NOx@4\" 0) (Neg (AP \"n_k@4\" 2))))) (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"n_k@4\" 2))) (And (AP \"NOx@3\" 0) (Neg (AP \"n_k@3\" 2))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"n_k@4\" 2))) (Eq (AP \"NOx@3\" 0) (AP \"n_k@3\" 2))) (And (AP \"NOx@2\" 0) (Neg (AP \"n_k@2\" 2))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"n_k@4\" 2))) (Eq (AP \"NOx@3\" 0) (AP \"n_k@3\" 2))) (Eq (AP \"NOx@2\" 0) (AP \"n_k@2\" 2))) (And (AP \"NOx@1\" 0) (Neg (AP \"n_k@1\" 2))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"n_k@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"n_k@4\" 2))) (Eq (AP \"NOx@3\" 0) (AP \"n_k@3\" 2))) (Eq (AP \"NOx@2\" 0) (AP \"n_k@2\" 2))) (Eq (AP \"NOx@1\" 0) (AP \"n_k@1\" 2))) (And (AP \"NOx@0\" 0) (Neg (AP \"n_k@0\" 2))))))) (And (Neg (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 0) (Neg (AP \"NOx@7\" 2))) (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (And (AP \"NOx@6\" 0) (Neg (AP \"NOx@6\" 2))))) (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (And (AP \"NOx@5\" 0) (Neg (AP \"NOx@5\" 2))))) (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (And (AP \"NOx@4\" 0) (Neg (AP \"NOx@4\" 2))))) (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 2))) (And (AP \"NOx@3\" 0) (Neg (AP \"NOx@3\" 2))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 2))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 2))) (And (AP \"NOx@2\" 0) (Neg (AP \"NOx@2\" 2))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 2))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 2))) (Eq (AP \"NOx@2\" 0) (AP \"NOx@2\" 2))) (And (AP \"NOx@1\" 0) (Neg (AP \"NOx@1\" 2))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 0) (AP \"NOx@7\" 2)) (Eq (AP \"NOx@6\" 0) (AP \"NOx@6\" 2))) (Eq (AP \"NOx@5\" 0) (AP \"NOx@5\" 2))) (Eq (AP \"NOx@4\" 0) (AP \"NOx@4\" 2))) (Eq (AP \"NOx@3\" 0) (AP \"NOx@3\" 2))) (Eq (AP \"NOx@2\" 0) (AP \"NOx@2\" 2))) (Eq (AP \"NOx@1\" 0) (AP \"NOx@1\" 2))) (And (AP \"NOx@0\" 0) (Neg (AP \"NOx@0\" 2)))))) (And (Neg (AP \"n_k@8\" 0)) (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 2) (Neg (AP \"n_k@7\" 0))) (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 0)) (And (AP \"NOx@6\" 2) (Neg (AP \"n_k@6\" 0))))) (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 0))) (And (AP \"NOx@5\" 2) (Neg (AP \"n_k@5\" 0))))) (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 0))) (And (AP \"NOx@4\" 2) (Neg (AP \"n_k@4\" 0))))) (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 0))) (Eq (AP \"NOx@4\" 2) (AP \"n_k@4\" 0))) (And (AP \"NOx@3\" 2) (Neg (AP \"n_k@3\" 0))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 0))) (Eq (AP \"NOx@4\" 2) (AP \"n_k@4\" 0))) (Eq (AP \"NOx@3\" 2) (AP \"n_k@3\" 0))) (And (AP \"NOx@2\" 2) (Neg (AP \"n_k@2\" 0))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 0))) (Eq (AP \"NOx@4\" 2) (AP \"n_k@4\" 0))) (Eq (AP \"NOx@3\" 2) (AP \"n_k@3\" 0))) (Eq (AP \"NOx@2\" 2) (AP \"n_k@2\" 0))) (And (AP \"NOx@1\" 2) (Neg (AP \"n_k@1\" 0))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 0)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 0))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 0))) (Eq (AP \"NOx@4\" 2) (AP \"n_k@4\" 0))) (Eq (AP \"NOx@3\" 2) (AP \"n_k@3\" 0))) (Eq (AP \"NOx@2\" 2) (AP \"n_k@2\" 0))) (Eq (AP \"NOx@1\" 2) (AP \"n_k@1\" 0))) (And (AP \"NOx@0\" 2) (Neg (AP \"n_k@0\" 0))))))))) (Or (And (Or (Or (Or (Or (And (AP \"throttle@4\" 0) (Neg (AP \"throttle@4\" 2))) (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 2)) (And (AP \"throttle@3\" 0) (Neg (AP \"throttle@3\" 2))))) (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 2)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 2))) (And (AP \"throttle@2\" 0) (Neg (AP \"throttle@2\" 2))))) (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 2)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 2))) (Eq (AP \"throttle@2\" 0) (AP \"throttle@2\" 2))) (And (AP \"throttle@1\" 0) (Neg (AP \"throttle@1\" 2))))) (And (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 2)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 2))) (Eq (AP \"throttle@2\" 0) (AP \"throttle@2\" 2))) (Eq (AP \"throttle@1\" 0) (AP \"throttle@1\" 2))) (And (AP \"throttle@0\" 0) (Neg (AP \"throttle@0\" 2))))) (And (Neg (AP \"t_k@5\" 2)) (Or (Or (Or (Or (And (AP \"throttle@4\" 0) (Neg (AP \"t_k@4\" 2))) (And (Eq (AP \"throttle@4\" 0) (AP \"t_k@4\" 2)) (And (AP \"throttle@3\" 0) (Neg (AP \"t_k@3\" 2))))) (And (And (Eq (AP \"throttle@4\" 0) (AP \"t_k@4\" 2)) (Eq (AP \"throttle@3\" 0) (AP \"t_k@3\" 2))) (And (AP \"throttle@2\" 0) (Neg (AP \"t_k@2\" 2))))) (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"t_k@4\" 2)) (Eq (AP \"throttle@3\" 0) (AP \"t_k@3\" 2))) (Eq (AP \"throttle@2\" 0) (AP \"t_k@2\" 2))) (And (AP \"throttle@1\" 0) (Neg (AP \"t_k@1\" 2))))) (And (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"t_k@4\" 2)) (Eq (AP \"throttle@3\" 0) (AP \"t_k@3\" 2))) (Eq (AP \"throttle@2\" 0) (AP \"t_k@2\" 2))) (Eq (AP \"throttle@1\" 0) (AP \"t_k@1\" 2))) (And (AP \"throttle@0\" 0) (Neg (AP \"t_k@0\" 2))))))) (And (Neg (Or (Or (Or (Or (And (AP \"throttle@4\" 0) (Neg (AP \"throttle@4\" 2))) (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 2)) (And (AP \"throttle@3\" 0) (Neg (AP \"throttle@3\" 2))))) (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 2)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 2))) (And (AP \"throttle@2\" 0) (Neg (AP \"throttle@2\" 2))))) (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 2)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 2))) (Eq (AP \"throttle@2\" 0) (AP \"throttle@2\" 2))) (And (AP \"throttle@1\" 0) (Neg (AP \"throttle@1\" 2))))) (And (And (And (And (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 2)) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 2))) (Eq (AP \"throttle@2\" 0) (AP \"throttle@2\" 2))) (Eq (AP \"throttle@1\" 0) (AP \"throttle@1\" 2))) (And (AP \"throttle@0\" 0) (Neg (AP \"throttle@0\" 2)))))) (And (Neg (AP \"t_k@5\" 0)) (Or (Or (Or (Or (And (AP \"throttle@4\" 2) (Neg (AP \"t_k@4\" 0))) (And (Eq (AP \"throttle@4\" 2) (AP \"t_k@4\" 0)) (And (AP \"throttle@3\" 2) (Neg (AP \"t_k@3\" 0))))) (And (And (Eq (AP \"throttle@4\" 2) (AP \"t_k@4\" 0)) (Eq (AP \"throttle@3\" 2) (AP \"t_k@3\" 0))) (And (AP \"throttle@2\" 2) (Neg (AP \"t_k@2\" 0))))) (And (And (And (Eq (AP \"throttle@4\" 2) (AP \"t_k@4\" 0)) (Eq (AP \"throttle@3\" 2) (AP \"t_k@3\" 0))) (Eq (AP \"throttle@2\" 2) (AP \"t_k@2\" 0))) (And (AP \"throttle@1\" 2) (Neg (AP \"t_k@1\" 0))))) (And (And (And (And (Eq (AP \"throttle@4\" 2) (AP \"t_k@4\" 0)) (Eq (AP \"throttle@3\" 2) (AP \"t_k@3\" 0))) (Eq (AP \"throttle@2\" 2) (AP \"t_k@2\" 0))) (Eq (AP \"throttle@1\" 2) (AP \"t_k@1\" 0))) (And (AP \"throttle@0\" 2) (Neg (AP \"t_k@0\" 0))))))))))))))))" ./nox.aag -pdr -cex -s ./strategyEC625_1.aag

echo "\n\n3) EC625 10.2 AAE (2nd part of property that should actually be checked) \n--------------------------"
time ./../../../../mchyper.py -f "(Forall (Forall (Exists (Implies (G (And (AP \"valid\" 0) (AP \"valid\" 1))) (And (AP \"valid\" 2) (Implies (G (Neg (Or (Or (AP \"throttle@4\" 0) (And (And (Neg (AP \"throttle@4\" 0)) (AP \"throttle@3\" 0)) (AP \"throttle@2\" 0))) (And (And (And (And (Neg (AP \"throttle@4\" 0)) (AP \"throttle@3\" 0)) (Neg (AP \"throttle@2\" 0))) (AP \"throttle@1\" 0)) (AP \"throttle@0\" 0))))) (And (G (And (And (And (And (Eq (AP \"throttle@0\" 0) (AP \"throttle@0\" 2)) (Eq (AP \"throttle@1\" 0) (AP \"throttle@1\" 2))) (Eq (AP \"throttle@2\" 0) (AP \"throttle@2\" 2))) (Eq (AP \"throttle@3\" 0) (AP \"throttle@3\" 2))) (Eq (AP \"throttle@4\" 0) (AP \"throttle@4\" 2)))) (WUntil (Neg (Or (And (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 2) (Neg (AP \"NOx@7\" 1))) (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (And (AP \"NOx@6\" 2) (Neg (AP \"NOx@6\" 1))))) (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (And (AP \"NOx@5\" 2) (Neg (AP \"NOx@5\" 1))))) (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (And (AP \"NOx@4\" 2) (Neg (AP \"NOx@4\" 1))))) (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"NOx@4\" 1))) (And (AP \"NOx@3\" 2) (Neg (AP \"NOx@3\" 1))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 2) (AP \"NOx@3\" 1))) (And (AP \"NOx@2\" 2) (Neg (AP \"NOx@2\" 1))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 2) (AP \"NOx@3\" 1))) (Eq (AP \"NOx@2\" 2) (AP \"NOx@2\" 1))) (And (AP \"NOx@1\" 2) (Neg (AP \"NOx@1\" 1))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 2) (AP \"NOx@3\" 1))) (Eq (AP \"NOx@2\" 2) (AP \"NOx@2\" 1))) (Eq (AP \"NOx@1\" 2) (AP \"NOx@1\" 1))) (And (AP \"NOx@0\" 2) (Neg (AP \"NOx@0\" 1))))) (And (Neg (AP \"n_k@8\" 1)) (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 2) (Neg (AP \"n_k@7\" 1))) (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 1)) (And (AP \"NOx@6\" 2) (Neg (AP \"n_k@6\" 1))))) (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 1))) (And (AP \"NOx@5\" 2) (Neg (AP \"n_k@5\" 1))))) (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 1))) (And (AP \"NOx@4\" 2) (Neg (AP \"n_k@4\" 1))))) (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"n_k@4\" 1))) (And (AP \"NOx@3\" 2) (Neg (AP \"n_k@3\" 1))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"n_k@4\" 1))) (Eq (AP \"NOx@3\" 2) (AP \"n_k@3\" 1))) (And (AP \"NOx@2\" 2) (Neg (AP \"n_k@2\" 1))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"n_k@4\" 1))) (Eq (AP \"NOx@3\" 2) (AP \"n_k@3\" 1))) (Eq (AP \"NOx@2\" 2) (AP \"n_k@2\" 1))) (And (AP \"NOx@1\" 2) (Neg (AP \"n_k@1\" 1))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"n_k@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"n_k@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"n_k@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"n_k@4\" 1))) (Eq (AP \"NOx@3\" 2) (AP \"n_k@3\" 1))) (Eq (AP \"NOx@2\" 2) (AP \"n_k@2\" 1))) (Eq (AP \"NOx@1\" 2) (AP \"n_k@1\" 1))) (And (AP \"NOx@0\" 2) (Neg (AP \"n_k@0\" 1))))))) (And (Neg (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 2) (Neg (AP \"NOx@7\" 1))) (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (And (AP \"NOx@6\" 2) (Neg (AP \"NOx@6\" 1))))) (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (And (AP \"NOx@5\" 2) (Neg (AP \"NOx@5\" 1))))) (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (And (AP \"NOx@4\" 2) (Neg (AP \"NOx@4\" 1))))) (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"NOx@4\" 1))) (And (AP \"NOx@3\" 2) (Neg (AP \"NOx@3\" 1))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 2) (AP \"NOx@3\" 1))) (And (AP \"NOx@2\" 2) (Neg (AP \"NOx@2\" 1))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 2) (AP \"NOx@3\" 1))) (Eq (AP \"NOx@2\" 2) (AP \"NOx@2\" 1))) (And (AP \"NOx@1\" 2) (Neg (AP \"NOx@1\" 1))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 2) (AP \"NOx@7\" 1)) (Eq (AP \"NOx@6\" 2) (AP \"NOx@6\" 1))) (Eq (AP \"NOx@5\" 2) (AP \"NOx@5\" 1))) (Eq (AP \"NOx@4\" 2) (AP \"NOx@4\" 1))) (Eq (AP \"NOx@3\" 2) (AP \"NOx@3\" 1))) (Eq (AP \"NOx@2\" 2) (AP \"NOx@2\" 1))) (Eq (AP \"NOx@1\" 2) (AP \"NOx@1\" 1))) (And (AP \"NOx@0\" 2) (Neg (AP \"NOx@0\" 1)))))) (And (Neg (AP \"n_k@8\" 2)) (Or (Or (Or (Or (Or (Or (Or (And (AP \"NOx@7\" 1) (Neg (AP \"n_k@7\" 2))) (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 2)) (And (AP \"NOx@6\" 1) (Neg (AP \"n_k@6\" 2))))) (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 2))) (And (AP \"NOx@5\" 1) (Neg (AP \"n_k@5\" 2))))) (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 2))) (And (AP \"NOx@4\" 1) (Neg (AP \"n_k@4\" 2))))) (And (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 2))) (Eq (AP \"NOx@4\" 1) (AP \"n_k@4\" 2))) (And (AP \"NOx@3\" 1) (Neg (AP \"n_k@3\" 2))))) (And (And (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 2))) (Eq (AP \"NOx@4\" 1) (AP \"n_k@4\" 2))) (Eq (AP \"NOx@3\" 1) (AP \"n_k@3\" 2))) (And (AP \"NOx@2\" 1) (Neg (AP \"n_k@2\" 2))))) (And (And (And (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 2))) (Eq (AP \"NOx@4\" 1) (AP \"n_k@4\" 2))) (Eq (AP \"NOx@3\" 1) (AP \"n_k@3\" 2))) (Eq (AP \"NOx@2\" 1) (AP \"n_k@2\" 2))) (And (AP \"NOx@1\" 1) (Neg (AP \"n_k@1\" 2))))) (And (And (And (And (And (And (And (Eq (AP \"NOx@7\" 1) (AP \"n_k@7\" 2)) (Eq (AP \"NOx@6\" 1) (AP \"n_k@6\" 2))) (Eq (AP \"NOx@5\" 1) (AP \"n_k@5\" 2))) (Eq (AP \"NOx@4\" 1) (AP \"n_k@4\" 2))) (Eq (AP \"NOx@3\" 1) (AP \"n_k@3\" 2))) (Eq (AP \"NOx@2\" 1) (AP \"n_k@2\" 2))) (Eq (AP \"NOx@1\" 1) (AP \"n_k@1\" 2))) (And (AP \"NOx@0\" 1) (Neg (AP \"n_k@0\" 2))))))))) (Or (And (Or (Or (Or (Or (And (AP \"throttle@4\" 2) (Neg (AP \"throttle@4\" 1))) (And (Eq (AP \"throttle@4\" 2) (AP \"throttle@4\" 1)) (And (AP \"throttle@3\" 2) (Neg (AP \"throttle@3\" 1))))) (And (And (Eq (AP \"throttle@4\" 2) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 2) (AP \"throttle@3\" 1))) (And (AP \"throttle@2\" 2) (Neg (AP \"throttle@2\" 1))))) (And (And (And (Eq (AP \"throttle@4\" 2) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 2) (AP \"throttle@3\" 1))) (Eq (AP \"throttle@2\" 2) (AP \"throttle@2\" 1))) (And (AP \"throttle@1\" 2) (Neg (AP \"throttle@1\" 1))))) (And (And (And (And (Eq (AP \"throttle@4\" 2) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 2) (AP \"throttle@3\" 1))) (Eq (AP \"throttle@2\" 2) (AP \"throttle@2\" 1))) (Eq (AP \"throttle@1\" 2) (AP \"throttle@1\" 1))) (And (AP \"throttle@0\" 2) (Neg (AP \"throttle@0\" 1))))) (And (Neg (AP \"t_k@5\" 1)) (Or (Or (Or (Or (And (AP \"throttle@4\" 2) (Neg (AP \"t_k@4\" 1))) (And (Eq (AP \"throttle@4\" 2) (AP \"t_k@4\" 1)) (And (AP \"throttle@3\" 2) (Neg (AP \"t_k@3\" 1))))) (And (And (Eq (AP \"throttle@4\" 2) (AP \"t_k@4\" 1)) (Eq (AP \"throttle@3\" 2) (AP \"t_k@3\" 1))) (And (AP \"throttle@2\" 2) (Neg (AP \"t_k@2\" 1))))) (And (And (And (Eq (AP \"throttle@4\" 2) (AP \"t_k@4\" 1)) (Eq (AP \"throttle@3\" 2) (AP \"t_k@3\" 1))) (Eq (AP \"throttle@2\" 2) (AP \"t_k@2\" 1))) (And (AP \"throttle@1\" 2) (Neg (AP \"t_k@1\" 1))))) (And (And (And (And (Eq (AP \"throttle@4\" 2) (AP \"t_k@4\" 1)) (Eq (AP \"throttle@3\" 2) (AP \"t_k@3\" 1))) (Eq (AP \"throttle@2\" 2) (AP \"t_k@2\" 1))) (Eq (AP \"throttle@1\" 2) (AP \"t_k@1\" 1))) (And (AP \"throttle@0\" 2) (Neg (AP \"t_k@0\" 1))))))) (And (Neg (Or (Or (Or (Or (And (AP \"throttle@4\" 2) (Neg (AP \"throttle@4\" 1))) (And (Eq (AP \"throttle@4\" 2) (AP \"throttle@4\" 1)) (And (AP \"throttle@3\" 2) (Neg (AP \"throttle@3\" 1))))) (And (And (Eq (AP \"throttle@4\" 2) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 2) (AP \"throttle@3\" 1))) (And (AP \"throttle@2\" 2) (Neg (AP \"throttle@2\" 1))))) (And (And (And (Eq (AP \"throttle@4\" 2) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 2) (AP \"throttle@3\" 1))) (Eq (AP \"throttle@2\" 2) (AP \"throttle@2\" 1))) (And (AP \"throttle@1\" 2) (Neg (AP \"throttle@1\" 1))))) (And (And (And (And (Eq (AP \"throttle@4\" 2) (AP \"throttle@4\" 1)) (Eq (AP \"throttle@3\" 2) (AP \"throttle@3\" 1))) (Eq (AP \"throttle@2\" 2) (AP \"throttle@2\" 1))) (Eq (AP \"throttle@1\" 2) (AP \"throttle@1\" 1))) (And (AP \"throttle@0\" 2) (Neg (AP \"throttle@0\" 1)))))) (And (Neg (AP \"t_k@5\" 2)) (Or (Or (Or (Or (And (AP \"throttle@4\" 1) (Neg (AP \"t_k@4\" 2))) (And (Eq (AP \"throttle@4\" 1) (AP \"t_k@4\" 2)) (And (AP \"throttle@3\" 1) (Neg (AP \"t_k@3\" 2))))) (And (And (Eq (AP \"throttle@4\" 1) (AP \"t_k@4\" 2)) (Eq (AP \"throttle@3\" 1) (AP \"t_k@3\" 2))) (And (AP \"throttle@2\" 1) (Neg (AP \"t_k@2\" 2))))) (And (And (And (Eq (AP \"throttle@4\" 1) (AP \"t_k@4\" 2)) (Eq (AP \"throttle@3\" 1) (AP \"t_k@3\" 2))) (Eq (AP \"throttle@2\" 1) (AP \"t_k@2\" 2))) (And (AP \"throttle@1\" 1) (Neg (AP \"t_k@1\" 2))))) (And (And (And (And (Eq (AP \"throttle@4\" 1) (AP \"t_k@4\" 2)) (Eq (AP \"throttle@3\" 1) (AP \"t_k@3\" 2))) (Eq (AP \"throttle@2\" 1) (AP \"t_k@2\" 2))) (Eq (AP \"throttle@1\" 1) (AP \"t_k@1\" 2))) (And (AP \"throttle@0\" 1) (Neg (AP \"t_k@0\" 2))))))))))))))))" ./nox.aag -pdr -cex -s ./strategyEC625_2.aag

