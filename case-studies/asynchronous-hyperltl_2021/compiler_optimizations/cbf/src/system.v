module SYSTEM(
    input clk,
    input change,
    input align,
    input stutter,
    input j, arr_size,
    input [1:0] arr,

    output a_src, b_src,
    output a_tar, b_tar,
    output st_src, st_tar
);

reg not_first;
reg j_v, arr_size_v;
reg [1:0] arr_v;

initial begin
    not_first <= 0;
    j_v <= 0;
    arr_size_v <= 0;
    arr_v <= 0;
end

always @(posedge clk) begin
    if (!not_first) begin
       j_v <= j;
       arr_size_v <= arr_size;
       arr_v <= arr;
       not_first <= 1; 
    end
end

wire clk_v;
assign clk_v = clk && not_first; 

SOURCE_CODEBLOCK c_src(clk_v, stutter, j_v, arr_size_v, arr_v, a_src, b_src, st_src);
TARGET_CODEBLOCK c_tar(clk_v, stutter, j_v, arr_size_v, arr_v, a_tar, b_tar, st_tar);
endmodule

// yosys -p "hierarchy -top SYSTEM; proc; flatten; techmap -map +/dff2ff.v; delete SYSTEM/clk; synth; aigmap; write_aiger -ascii -symbols cbf.aag" source_codeblock.v target_codeblock.v system.v


