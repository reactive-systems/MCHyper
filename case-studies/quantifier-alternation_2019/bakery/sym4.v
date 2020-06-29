module strategy(input sym_break0_0, input sym_break1_0, input select0_0, input select1_0, input pause_0, output sym_break0_1, output sym_break1_1, output select0_1, output select1_1, output pause_1);
    assign sym_break0_1 = (! sym_break1_0) ? (! sym_break0_0) : 1'b 0 ;
    assign sym_break1_1 = sym_break1_0;
    assign select0_1= (! select1_0) ? (! select0_0) : 1'b 0 ;
    assign select1_1 = select1_0;
    assign pause_1 = pause_0;
endmodule