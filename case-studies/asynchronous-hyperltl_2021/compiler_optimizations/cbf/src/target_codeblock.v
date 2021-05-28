module TARGET_CODEBLOCK(
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
                    a <= arr[0];
                    computation_step <= computation_step + 1;
                end
                2:
                begin
                    computation_step <= (j <= arr_size) ? 3:4;
                end
                3:
                begin
                    b <= arr[j];
                    computation_step <= 5;
                end
                4:
                begin
                    b <= arr[arr_size];
                    computation_step <= 5;
                end
                5:
                begin
                    
                end
            endcase
        end
    end

endmodule