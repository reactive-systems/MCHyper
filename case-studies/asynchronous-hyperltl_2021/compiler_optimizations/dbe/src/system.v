module SYSTEM(
    input clk,
    input change, align, stutter,
    input secret_in,

    output public_out_src, secret_out_src,
    output public_out_tar, secret_out_tar,
    output st_src, st_tar
);

reg not_first;
reg secret_in_v;

initial begin
    not_first <= 0;
    secret_in_v <= 0;
end

always @(posedge clk) begin
    if (!not_first) begin
       secret_in_v <= secret_in;
       not_first <= 1; 
    end
end

wire clk_v;
assign clk_v = clk && not_first; 

//Module
SOURCE_CODEBLOCK c_src(clk_v, stutter, secret_in_v, public_out_src, secret_out_src, st_src);
TARGET_CODEBLOCK c_tar(clk_v, stutter, secret_in_v, public_out_tar, secret_out_tar, st_tar);

endmodule

// yosys call: 
// yosys -p "hierarchy -top SYSTEM; proc; flatten; techmap -map +/dff2ff.v; delete SYSTEM/clk; synth; aigmap; write_aiger -ascii -symbols dbe.aag" source_codeblock.v target_codeblock.v system.v