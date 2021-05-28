module SYSTEM(
    input clk,
    input change, align, stutter,

    output x_src,
    output x_tar,
    output st_src, st_tar
);

reg not_first;

initial begin
    not_first <= 0;
end

always @(posedge clk) begin
    if (!not_first) begin
       not_first <= 1; 
    end
end

wire clk_v;
assign clk_v = clk && not_first; 

//Module
SOURCE_CODEBLOCK c_src(clk_v, stutter, x_src, st_src);
TARGET_CODEBLOCK c_tar(clk_v, stutter, x_tar, st_tar);

endmodule

// yosys call: 
// yosys -p "hierarchy -top SYSTEM; proc; flatten; techmap -map +/dff2ff.v; delete SYSTEM/clk; synth; aigmap; write_aiger -ascii -symbols ef.aag" source_codeblock.v target_codeblock.v system.v