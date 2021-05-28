#! /bin/sh
set -e

MCHyperPath=$1

# Colors
RED='\033[0;31m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color


printf "\n${RED}Start SPI Experiments:${NC}\n"
printf "${RED}==========================================${NC}\n\n"

printf "${CYAN}Line 1 in Table 1(b): Oberservational Determinism(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"
time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq 
                    (AP \"change\" 0)
                    (Neg (Eq (And (AP \"miso\" 0) (And (Neg (AP \"sclk\" 0))  (Neg (AP \"ss\" 0)) )) (X (Until (AP \"st\" 0) (And (AP \"miso\" 0) (And (Neg (AP \"sclk\" 0))  (Neg (AP \"ss\" 0)) ))))))
                )
                (Eq 
                    (AP \"change\" 1)
                    (Neg (Eq (And (AP \"miso\" 1) (And (Neg (AP \"sclk\" 1))  (Neg (AP \"ss\" 1)) )) (X (Until (AP \"st\" 1) (And (AP \"miso\" 1) (And (Neg (AP \"sclk\" 1))  (Neg (AP \"ss\" 1)) ))))))
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st\" 0))) (X (Neg (AP \"st\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st\" 0))) (X (AP \"st\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st\" 1))) (X (AP \"st\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And
                (And (G (F (Neg (AP \"st\" 0)))) (G (F (Neg (AP \"st\" 1)))))
                (G (And
                    (Implies 
                        (AP \"stutter_in\" 0)
                        (And 
                            (Eq (AP \"sclk_in\" 0) (X (AP \"sclk_in\" 0)))
                            (And 
                                (Eq (AP \"ss_in\" 0) (X (AP \"ss_in\" 0)))
                                (Eq (AP \"mosi_in\" 0) (X (AP \"mosi_in\" 0)))
                            )
                        )
                    )
                    (Implies 
                        (AP \"stutter_in\" 1)
                        (And 
                            (Eq (AP \"sclk_in\" 1) (X (AP \"sclk_in\" 1)))
                            (And 
                                (Eq (AP \"ss_in\" 1) (X (AP \"ss_in\" 1)))
                                (Eq (AP \"mosi_in\" 1) (X (AP \"mosi_in\" 1)))
                            )
                        )
                    )
                ))
            )
            (Implies
                (And
                    (And 
                        (And
                            (And (AP \"ss\" 0) (And (Neg (AP \"sclk\" 0)) (Neg (AP \"st\" 0))))
                            (And (AP \"ss\" 1) (And (Neg (AP \"sclk\" 1)) (Neg (AP \"st\" 1))))
                        )
                        (And
                            (And
                                (And
                                    (And (G (F (AP \"ss\" 0))) (G (F (Neg (AP \"ss\" 0)))))
                                    (And (G (F (AP \"sclk\" 0))) (G (F (Neg (AP \"sclk\" 0)))))
                                )
                                (And
                                    (And
                                        (G (Implies (Neg (AP \"ss\" 0)) (Until (Neg (AP \"ss\" 0)) (AP \"byte_transfered\" 0))))
                                        (G (Implies (AP \"start\" 0) (And (And (Neg (AP \"falling_sclk_edge\" 0)) (Neg (AP \"rising_sclk_edge\" 0))) (Neg (AP \"sclk\" 0)))))
                                    )
                                    (G (Implies (AP \"byte_transfered\" 0) (Until (Neg (AP \"sclk\" 0)) (AP \"ss\" 0))))
                                )
                            )
                            (And
                                (And
                                    (And (G (F (AP \"ss\" 1))) (G (F (Neg (AP \"ss\" 1)))))
                                    (And (G (F (AP \"sclk\" 1))) (G (F (Neg (AP \"sclk\" 1)))))
                                )
                                (And
                                    (And
                                        (G (Implies (Neg (AP \"ss\" 1)) (Until (Neg (AP \"ss\" 1)) (AP \"byte_transfered\" 1))))
                                        (G (Implies (AP \"start\" 1) (And (And (Neg (AP \"falling_sclk_edge\" 1)) (Neg (AP \"rising_sclk_edge\" 1))) (Neg (AP \"sclk\" 1)))))
                                    )
                                    (G (Implies (AP \"byte_transfered\" 1) (Until (Neg (AP \"sclk\" 1)) (AP \"ss\" 1))))
                                )
                            )
                        )
                    )
                    (EqualOn 0 1 [\"send_item[0]\",\"send_item[1]\",\"send_item[2]\",\"send_item[3]\",\"send_item[4]\",\"send_item[5]\",\"send_item[6]\",\"send_item[7]\"])
                )
                (And
                    (Neg (Until
                        (AP \"align\" 0)
                        (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                    ))
                    (Implies
                        (G (AP \"align\" 0))
                        (G (Eq (And (AP \"miso\" 0) (And (Neg (AP \"sclk\" 0))  (Neg (AP \"ss\" 0)) )) (And (AP \"miso\" 1) (And (Neg (AP \"sclk\" 1))  (Neg (AP \"ss\" 1)) ))))
                    )
                )  
            )
        )
    ))" ./spi_correct/aiger/spi_slave.aag -pdr -cex -v 1
printf "\n"

printf "${CYAN}Line 2 in Table 1(b): Termination-insensitive Noninterference(TODO:TIME PAPER)${NC}\n"
printf "${CYAN}==========================================${NC}\n"
time $MCHyperPath -f "Forall (Forall (
    Implies
        (G (And
            (And
                (Eq 
                    (AP \"change\" 0)
                    (Or 
                        (Neg (Eq (AP \"byte_transfered\" 0) (X (Until (AP \"st\" 0) (AP \"byte_transfered\" 0)))))
                        (Or 
                            (Or
                                (Or
                                    (Neg (Eq (And (AP \"send_value[0]\" 0) (AP \"byte_transfered\" 0)) (X (Until (AP \"st\" 0) (And (AP \"send_value[0]\" 0) (AP \"byte_transfered\" 0))))))
                                    (Neg (Eq (And (AP \"send_value[1]\" 0) (AP \"byte_transfered\" 0)) (X (Until (AP \"st\" 0) (And (AP \"send_value[1]\" 0) (AP \"byte_transfered\" 0))))))
                                )
                                (Or
                                    (Neg (Eq (And (AP \"send_value[2]\" 0) (AP \"byte_transfered\" 0)) (X (Until (AP \"st\" 0) (And (AP \"send_value[2]\" 0) (AP \"byte_transfered\" 0))))))
                                    (Neg (Eq (And (AP \"send_value[3]\" 0) (AP \"byte_transfered\" 0)) (X (Until (AP \"st\" 0) (And (AP \"send_value[3]\" 0) (AP \"byte_transfered\" 0))))))
                                )
                            )
                            (Or
                                (Or
                                    (Neg (Eq (And (AP \"send_value[4]\" 0) (AP \"byte_transfered\" 0)) (X (Until (AP \"st\" 0) (And (AP \"send_value[4]\" 0) (AP \"byte_transfered\" 0))))))
                                    (Neg (Eq (And (AP \"send_value[5]\" 0) (AP \"byte_transfered\" 0)) (X (Until (AP \"st\" 0) (And (AP \"send_value[5]\" 0) (AP \"byte_transfered\" 0))))))
                                )
                                (Or
                                    (Neg (Eq (And (AP \"send_value[6]\" 0) (AP \"byte_transfered\" 0)) (X (Until (AP \"st\" 0) (And (AP \"send_value[6]\" 0) (AP \"byte_transfered\" 0))))))
                                    (Neg (Eq (And (AP \"send_value[7]\" 0) (AP \"byte_transfered\" 0)) (X (Until (AP \"st\" 0) (And (AP \"send_value[7]\" 0) (AP \"byte_transfered\" 0))))))
                                )
                            )
                        )
                    )
                )
                (Eq 
                    (AP \"change\" 1)
                    (Or 
                        (Neg (Eq (AP \"byte_transfered\" 1) (X (Until (AP \"st\" 1) (AP \"byte_transfered\" 1)))))
                        (Or 
                            (Or
                                (Or
                                    (Neg (Eq (And (AP \"send_value[0]\" 1) (AP \"byte_transfered\" 1)) (X (Until (AP \"st\" 1) (And (AP \"send_value[0]\" 1) (AP \"byte_transfered\" 1))))))
                                    (Neg (Eq (And (AP \"send_value[1]\" 1) (AP \"byte_transfered\" 1)) (X (Until (AP \"st\" 1) (And (AP \"send_value[1]\" 1) (AP \"byte_transfered\" 1))))))
                                )
                                (Or
                                    (Neg (Eq (And (AP \"send_value[2]\" 1) (AP \"byte_transfered\" 1)) (X (Until (AP \"st\" 1) (And (AP \"send_value[2]\" 1) (AP \"byte_transfered\" 1))))))
                                    (Neg (Eq (And (AP \"send_value[3]\" 1) (AP \"byte_transfered\" 1)) (X (Until (AP \"st\" 1) (And (AP \"send_value[3]\" 1) (AP \"byte_transfered\" 1))))))
                                )
                            )
                            (Or
                                (Or
                                    (Neg (Eq (And (AP \"send_value[4]\" 1) (AP \"byte_transfered\" 1)) (X (Until (AP \"st\" 1) (And (AP \"send_value[4]\" 1) (AP \"byte_transfered\" 1))))))
                                    (Neg (Eq (And (AP \"send_value[5]\" 1) (AP \"byte_transfered\" 1)) (X (Until (AP \"st\" 1) (And (AP \"send_value[5]\" 1) (AP \"byte_transfered\" 1))))))
                                )
                                (Or
                                    (Neg (Eq (And (AP \"send_value[6]\" 1) (AP \"byte_transfered\" 1)) (X (Until (AP \"st\" 1) (And (AP \"send_value[6]\" 1) (AP \"byte_transfered\" 1))))))
                                    (Neg (Eq (And (AP \"send_value[7]\" 1) (AP \"byte_transfered\" 1)) (X (Until (AP \"st\" 1) (And (AP \"send_value[7]\" 1) (AP \"byte_transfered\" 1))))))
                                )
                            )
                        )
                    )
                )
            )
            (Eq 
                (AP \"align\" 0)
                (And
                    (Implies (And (X (Neg (AP \"st\" 0))) (X (Neg (AP \"st\" 1)))) (Eq (AP \"change\" 0) (AP \"change\" 1)))
                    (And 
                        (Implies (And (X (Neg (AP \"st\" 0))) (X (AP \"st\" 1))) (Neg (AP \"change\" 0)))
                        (Implies (And (X (Neg (AP \"st\" 1))) (X (AP \"st\" 0))) (Neg (AP \"change\" 1)))
                    )
                )
            )
        ))
        (Implies
            (And
                (And (G (F (Neg (AP \"st\" 0)))) (G (F (Neg (AP \"st\" 1)))))
                (G (And
                    (Implies 
                        (AP \"stutter_in\" 0)
                        (And 
                            (Eq (AP \"sclk_in\" 0) (X (AP \"sclk_in\" 0)))
                            (And 
                                (Eq (AP \"ss_in\" 0) (X (AP \"ss_in\" 0)))
                                (Eq (AP \"mosi_in\" 0) (X (AP \"mosi_in\" 0)))
                            )
                        )
                    )
                    (Implies 
                        (AP \"stutter_in\" 1)
                        (And 
                            (Eq (AP \"sclk_in\" 1) (X (AP \"sclk_in\" 1)))
                            (And 
                                (Eq (AP \"ss_in\" 1) (X (AP \"ss_in\" 1)))
                                (Eq (AP \"mosi_in\" 1) (X (AP \"mosi_in\" 1)))
                            )
                        )
                    )
                ))
            )
            (Implies
                (And
                    (And
                        (And (AP \"ss\" 0) (And (Neg (AP \"sclk\" 0)) (Neg (AP \"st\" 0))))
                        (And (AP \"ss\" 1) (And (Neg (AP \"sclk\" 1)) (Neg (AP \"st\" 1))))
                    )
                    (EqualOn 0 1 [\"send_item[0]\",\"send_item[1]\",\"send_item[2]\",\"send_item[3]\",\"send_item[4]\",\"send_item[5]\",\"send_item[6]\",\"send_item[7]\"])
                )
                (Or
                    (Or (G (Neg (AP \"byte_transfered\" 0))) (G (Neg (AP \"byte_transfered\" 1))))
                    (And
                        (Neg (Until
                            (AP \"align\" 0)
                            (Neg (Eq (G (Neg (AP \"change\" 0))) (G (Neg (AP \"change\" 1)))))
                        ))
                        (Implies
                            (G (AP \"align\" 0))
                            (G (And
                                (And
                                    (And
                                        (Eq (And (AP \"send_value[0]\" 0) (AP \"byte_transfered\" 0)) (And (AP \"send_value[0]\" 1) (AP \"byte_transfered\" 1)))
                                        (Eq (And (AP \"send_value[1]\" 0) (AP \"byte_transfered\" 0)) (And (AP \"send_value[1]\" 1) (AP \"byte_transfered\" 1)))
                                    )
                                    (And
                                        (Eq (And (AP \"send_value[2]\" 0) (AP \"byte_transfered\" 0)) (And (AP \"send_value[2]\" 1) (AP \"byte_transfered\" 1)))
                                        (Eq (And (AP \"send_value[3]\" 0) (AP \"byte_transfered\" 0)) (And (AP \"send_value[3]\" 1) (AP \"byte_transfered\" 1)))
                                    )
                                )
                                (And
                                    (And
                                        (Eq (And (AP \"send_value[4]\" 0) (AP \"byte_transfered\" 0)) (And (AP \"send_value[4]\" 1) (AP \"byte_transfered\" 1)))
                                        (Eq (And (AP \"send_value[5]\" 0) (AP \"byte_transfered\" 0)) (And (AP \"send_value[5]\" 1) (AP \"byte_transfered\" 1)))
                                    )
                                    (And
                                        (Eq (And (AP \"send_value[6]\" 0) (AP \"byte_transfered\" 0)) (And (AP \"send_value[6]\" 1) (AP \"byte_transfered\" 1)))
                                        (Eq (And (AP \"send_value[7]\" 0) (AP \"byte_transfered\" 0)) (And (AP \"send_value[7]\" 1) (AP \"byte_transfered\" 1)))
                                    )
                                )
                            ))
                        )
                    )
                )
            )
        )
    ))" ./spi_term/aiger/spi_slave.aag -pdr -cex -v 1