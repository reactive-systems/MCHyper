module strategy(input sym_break0_0, input sym_break1_0, input sym_break2_0, input select0_0, input select1_0, input select2_0, input pause_0, output sym_break0_1, output sym_break1_1, output sym_break2_1, output select0_1, output select1_1, output select2_1, output pause_1);
    assign sym_break0_1 = ((! sym_break1_0) && (! sym_break2_0)) ? (! sym_break0_0) : 1'b 0 ;
    assign sym_break1_1 = sym_break1_0;
    assign sym_break2_1 = sym_break2_0;
    assign select0_1= ((! select1_0) && (! select2_0)) ? (! select0_0) : 1'b 0 ;
    assign select1_1 = select1_0;
    assign select2_1 = select2_0;
	assign pause_1 = pause_0;
endmodule

// And (And (And (And 
//   (Eq (AP \"sym_break<2>\" 0) (AP \"sym_break<2>\" 1)) 
//   (Eq (AP \"sym_break<1>\" 0) (AP \"sym_break<1>\" 1))) 
// (And (And (And 
//   (Implies (AP \"sym_break<2>\" 0) (And (Neg (AP \"sym_break<1>\" 0)) (Neg (AP \"sym_break<0>\" 0)))) 
//   (Implies (AP \"sym_break<2>\" 1) (And (Neg (AP \"sym_break<1>\" 1)) (Neg (AP \"sym_break<0>\" 1))))) 
// (And 
//   (Implies (AP \"sym_break<1>\" 0) (Neg (AP \"sym_break<0>\" 0))) 
//   (Implies (AP \"sym_break<1>\" 1) (Neg (AP \"sym_break<0>\" 1))))) 
// (Implies (And (Neg (AP \"sym_break<2>\" 0)) (Neg (AP \"sym_break<1>\" 0))) (Neq (AP \"sym_break<0>\" 0) (AP \"sym_break<0>\" 1))))) 
// (And (And 
//   (Eq (AP \"select<2>\" 0) (AP \"select<2>\" 1)) 
//   (Eq (AP \"select<1>\" 0) (AP \"select<1>\" 1))) 
// (And (And (And 
//   (Implies (AP \"select<2>\" 0) (And (Neg (AP \"select<1>\" 0)) (Neg (AP \"select<0>\" 0)))) 
//   (Implies (AP \"select<1>\" 1) (And (Neg (AP \"select<1>\" 1)) (Neg (AP \"select<0>\" 1))))) 
// (And 
//   (Implies (AP \"select<1>\" 0) (Neg (AP \"select<0>\" 0))) 
//   (Implies (AP \"select<2>\" 1) (Neg (AP \"select<0>\" 1))))) 
// (Implies (And (Neg (AP \"select<2>\" 0)) (Neg (AP \"select<1>\" 0))) (Neq (AP \"select<0>\" 0) (AP \"select<0>\" 1)))))) 

// (Eq (AP \"pause\" 0) (AP \"pause\" 1))