#! /bin/sh
set -e

MCHyperPath=$1

# Colors
RED='\033[0;31m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color


printf "\n${RED}Start Compiler Optimization Experiments:${NC}\n"
printf "${RED}==========================================${NC}\n\n"

printf "${CYAN}Line 1 in Table 1(a): Expression Flattening(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"
time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq
                    (AP \"change\" 0)
                    (Neg (Eq (AP \"x_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"x_src\" 0)))))
                )
                (Eq
                    (AP \"change\" 1)
                    (Neg (Eq (AP \"x_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"x_tar\" 1)))))
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st_src\" 0))) (X (Neg (AP \"st_tar\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st_src\" 0))) (X (AP \"st_tar\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st_tar\" 1))) (X (AP \"st_src\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And (G (F (Neg (AP \"st_src\" 0)))) (G (F (Neg (AP \"st_tar\" 1)))))
            (And
                (Neg (Until
                    (AP \"align\" 0)
                    (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                ))
                (Implies
                    (G (AP \"align\" 0))
                    (G (Eq (AP \"x_src\" 0) (AP \"x_tar\" 1)))
                )
            )
        )
    ))" ./ef/aiger/ef.aag -pdr -cex -v 1
printf "\n"

printf "${CYAN}Line 2 in Table 1(a): Dead-Branch Elimination(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"
time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq
                    (AP \"change\" 0)
                    (Or (Neg (Eq (AP \"public_out_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"public_out_src\" 0))))) (Neg (Eq (AP \"secret_out_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"secret_out_src\" 0))))))
                )
                (Eq
                    (AP \"change\" 1)
                    (Or (Neg (Eq (AP \"public_out_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"public_out_tar\" 1))))) (Neg (Eq (AP \"secret_out_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"secret_out_tar\" 1))))))
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st_src\" 0))) (X (Neg (AP \"st_tar\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st_src\" 0))) (X (AP \"st_tar\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st_tar\" 1))) (X (AP \"st_src\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And (G (F (Neg (AP \"st_src\" 0)))) (G (F (Neg (AP \"st_tar\" 1)))))
            (Implies
                (EqualOn 0 1 [\"secret_in\"])
                (And
                    (Neg (Until
                        (AP \"align\" 0)
                        (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                    ))
                    (Implies
                        (G (AP \"align\" 0))
                        (G (And (Eq (AP \"public_out_src\" 0) (AP \"public_out_tar\" 1)) (Eq (AP \"secret_out_src\" 0) (AP \"secret_out_tar\" 1))))
                    )
                )
            )
        )
    ))" ./dbe/aiger/dbe.aag -pdr -cex -v 1
printf "\n"

printf "${CYAN}Line 3 in Table 1(a): Common Branch Factorization(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"
time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq
                    (AP \"change\" 0)
                    (Or (Neg (Eq (AP \"a_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"a_src\" 0))))) (Neg (Eq (AP \"b_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"b_src\" 0))))))
                )
                (Eq
                    (AP \"change\" 1)
                    (Or (Neg (Eq (AP \"a_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"a_tar\" 1))))) (Neg (Eq (AP \"b_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"b_tar\" 1))))))
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st_src\" 0))) (X (Neg (AP \"st_tar\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st_src\" 0))) (X (AP \"st_tar\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st_tar\" 1))) (X (AP \"st_src\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And (G (F (Neg (AP \"st_src\" 0)))) (G (F (Neg (AP \"st_tar\" 1)))))
            (Implies
                (EqualOn 0 1 [\"arr_size\",\"arr[0]\",\"arr[1]\",\"j\"])
                (And
                    (Neg (Until
                        (AP \"align\" 0)
                        (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                    ))
                    (Implies
                        (G (AP \"align\" 0))
                        (G (And (Eq (AP \"a_src\" 0) (AP \"a_tar\" 1)) (Eq (AP \"b_src\" 0) (AP \"b_tar\" 1))))
                    )
                )
            )
        )
    ))" ./cbf/aiger/cbf.aag -pdr -cex -v 1
printf "\n"

printf "${CYAN}Line 4 in Table 1(a): Loop Peeling(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"
time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq
                    (AP \"change\" 0)
                    (Or (Neg (Eq (AP \"public_out_src[0]\" 0) (X (Until (AP \"st_src\" 0) (AP \"public_out_src[0]\" 0))))) (Neg (Eq (AP \"public_out_src[1]\" 0) (X (Until (AP \"st_src\" 0) (AP \"public_out_src[1]\" 0))))))
                )
                (Eq
                    (AP \"change\" 1)
                    (Or (Neg (Eq (AP \"public_out_tar[0]\" 1) (X (Until (AP \"st_tar\" 1) (AP \"public_out_tar[0]\" 1))))) (Neg (Eq (AP \"public_out_tar[1]\" 1) (X (Until (AP \"st_tar\" 1) (AP \"public_out_tar[1]\" 1))))))
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st_src\" 0))) (X (Neg (AP \"st_tar\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st_src\" 0))) (X (AP \"st_tar\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st_tar\" 1))) (X (AP \"st_src\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And (G (F (Neg (AP \"st_src\" 0)))) (G (F (Neg (AP \"st_tar\" 1)))))
            (Implies
                (EqualOn 0 1 [\"secret_in[0]\",\"secret_in[1]\"])
                (And
                    (Neg (Until
                        (AP \"align\" 0)
                        (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                    ))
                    (Implies
                        (G (AP \"align\" 0))
                        (G (And (Eq (AP \"public_out_src[0]\" 0) (AP \"public_out_tar[0]\" 1)) (Eq (AP \"public_out_src[1]\" 0) (AP \"public_out_tar[1]\" 1))))
                    )
                )
            )
        )
    ))" ./lp/aiger/lp.aag -pdr -cex -v 1
printf "\n"

# =================================================
# =================================================

printf "${CYAN}Line 5 in Table 1(a): Common Branch Factorization + Dead Branch Elimination(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"

time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq
                    (AP \"change\" 0)
                    (Or (Neg (Eq (AP \"a_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"a_src\" 0))))) (Neg (Eq (AP \"b_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"b_src\" 0))))))
                )
                (Eq
                    (AP \"change\" 1)
                    (Or (Neg (Eq (AP \"a_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"a_tar\" 1))))) (Neg (Eq (AP \"b_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"b_tar\" 1))))))
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st_src\" 0))) (X (Neg (AP \"st_tar\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st_src\" 0))) (X (AP \"st_tar\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st_tar\" 1))) (X (AP \"st_src\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And (G (F (Neg (AP \"st_src\" 0)))) (G (F (Neg (AP \"st_tar\" 1)))))
            (Implies
                (EqualOn 0 1 [\"arr_size\",\"arr[0]\",\"arr[1]\",\"j\"])
                (And
                    (Neg (Until
                        (AP \"align\" 0)
                        (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                    ))
                    (Implies
                        (G (AP \"align\" 0))
                        (G (And (Eq (AP \"a_src\" 0) (AP \"a_tar\" 1)) (Eq (AP \"b_src\" 0) (AP \"b_tar\" 1))))
                    )
                )
            )
        )
    ))" ./cbf_dbe/aiger/cbf_dbe.aag -pdr -cex -v 1
printf "\n"

printf "${CYAN}Line 6 in Table 1(a): Common Branch Factorization + Dead Branch Elimination + Expression Flattening(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"
time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq
                    (AP \"change\" 0)
                    (Or (Neg (Eq (AP \"a_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"a_src\" 0))))) (Neg (Eq (AP \"b_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"b_src\" 0))))))
                )
                (Eq
                    (AP \"change\" 1)
                    (Or (Neg (Eq (AP \"a_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"a_tar\" 1))))) (Neg (Eq (AP \"b_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"b_tar\" 1))))))
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st_src\" 0))) (X (Neg (AP \"st_tar\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st_src\" 0))) (X (AP \"st_tar\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st_tar\" 1))) (X (AP \"st_src\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And (G (F (Neg (AP \"st_src\" 0)))) (G (F (Neg (AP \"st_tar\" 1)))))
            (Implies
                (EqualOn 0 1 [\"arr_size\",\"arr[0]\",\"arr[1]\",\"j\"])
                (And
                    (Neg (Until
                        (AP \"align\" 0)
                        (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                    ))
                    (Implies
                        (G (AP \"align\" 0))
                        (G (And (Eq (AP \"a_src\" 0) (AP \"a_tar\" 1)) (Eq (AP \"b_src\" 0) (AP \"b_tar\" 1))))
                    )
                )
            )
        )
    ))" ./cbf_dbe_ef/aiger/cbf_dbe_ef.aag -pdr -cex -v 1
printf "\n"

printf "${CYAN}Line 7 in Table 1(a): Common Branch Factorization + Expression Flattening(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"
time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq
                    (AP \"change\" 0)
                    (Or (Neg (Eq (AP \"a_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"a_src\" 0))))) (Neg (Eq (AP \"b_src\" 0) (X (Until (AP \"st_src\" 0) (AP \"b_src\" 0))))))
                )
                (Eq
                    (AP \"change\" 1)
                    (Or (Neg (Eq (AP \"a_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"a_tar\" 1))))) (Neg (Eq (AP \"b_tar\" 1) (X (Until (AP \"st_tar\" 1) (AP \"b_tar\" 1))))))
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st_src\" 0))) (X (Neg (AP \"st_tar\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st_src\" 0))) (X (AP \"st_tar\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st_tar\" 1))) (X (AP \"st_src\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And (G (F (Neg (AP \"st_src\" 0)))) (G (F (Neg (AP \"st_tar\" 1)))))
            (Implies
                (EqualOn 0 1 [\"arr_size\",\"arr[0]\",\"arr[1]\",\"j\"])
                (And
                    (Neg (Until
                        (AP \"align\" 0)
                        (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                    ))
                    (Implies
                        (G (AP \"align\" 0))
                        (G (And (Eq (AP \"a_src\" 0) (AP \"a_tar\" 1)) (Eq (AP \"b_src\" 0) (AP \"b_tar\" 1))))
                    )
                )
            )
        )
    ))" ./cbf_ef/aiger/cbf_ef.aag -pdr -cex -v 1
printf "\n"

printf "${CYAN}Line 8 in Table 1(a): Expression Flattening + Loop Peeling(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"
time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq
                    (AP \"change\" 0)
                    (Or (Neg (Eq (AP \"public_out_src[0]\" 0) (X (Until (AP \"st_src\" 0) (AP \"public_out_src[0]\" 0))))) (Neg (Eq (AP \"public_out_src[1]\" 0) (X (Until (AP \"st_src\" 0) (AP \"public_out_src[1]\" 0))))))
                )
                (Eq
                    (AP \"change\" 1)
                    (Or (Neg (Eq (AP \"public_out_tar[0]\" 1) (X (Until (AP \"st_tar\" 1) (AP \"public_out_tar[0]\" 1))))) (Neg (Eq (AP \"public_out_tar[1]\" 1) (X (Until (AP \"st_tar\" 1) (AP \"public_out_tar[1]\" 1))))))
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st_src\" 0))) (X (Neg (AP \"st_tar\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st_src\" 0))) (X (AP \"st_tar\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st_tar\" 1))) (X (AP \"st_src\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And (G (F (Neg (AP \"st_src\" 0)))) (G (F (Neg (AP \"st_tar\" 1)))))
            (Implies
                (EqualOn 0 1 [\"secret_in[0]\",\"secret_in[1]\"])
                (And
                    (Neg (Until
                        (AP \"align\" 0)
                        (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                    ))
                    (Implies
                        (G (AP \"align\" 0))
                        (G (And (Eq (AP \"public_out_src[0]\" 0) (AP \"public_out_tar[0]\" 1)) (Eq (AP \"public_out_src[1]\" 0) (AP \"public_out_tar[1]\" 1))))
                    )
                )
            )
        )
    ))" ./ef_lp/aiger/ef_lp.aag -pdr -cex -v 1
