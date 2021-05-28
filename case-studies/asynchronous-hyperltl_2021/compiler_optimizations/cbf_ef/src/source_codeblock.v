module SOURCE_CODEBLOCK(
    input clk,
    input stutter_in,
    input j, arr_size,
    input [1:0] arr,

    output reg a,b, stutter
);

    reg [2:0] computation_step;
    initial begin
        computation_step <= 0;
        a <= 0;
        b <= 0;
        stutter <= 0;
    end

    always @(posedge clk) begin
        stutter <= stutter_in;
        if (!stutter_in) begin
            case (computation_step)
                0: begin
                    computation_step <= 1;
                end
                1:
                begin
                    computation_step <= (j <= arr_size) ? 2:4;
                end
                2:
                begin
                    a <= (2 * (arr[0] + 1)) * ((arr[1] - 1) % 2);
                    computation_step <= computation_step + 1;
                end
                3:
                begin
                    b <= arr[j];
                    computation_step <= 6;
                end
                4:
                begin
                    a <= (2 * (arr[0] + 1)) * ((arr[1] - 1) % 2);
                    computation_step <= computation_step + 1;
                end
                5:
                begin
                    b <= arr[arr_size];
                    computation_step <= 6;
                end
                6:
                begin
                    
                end
            endcase
        end
    end

endmodule