module TARGET_CODEBLOCK(
    input clk,
    input stutter_in,
    input j, arr_size,
    input [1:0] arr,

    output reg a,b, stutter
);

    reg [3:0] t0;
    reg [3:0] t1;
    reg [1:0] t2; 
    reg [1:0] t3;

    reg [3:0] computation_step;
    initial begin
        computation_step <= 0;
        t0 <= 0;
        t1 <= 0;
        t2 <= 0;
        t3 <= 0;
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
                    t0 <= arr[0] + 1;
                    computation_step <= computation_step + 1;
                end
                2:
                begin
                    t1 <= 2 * t0;
                    computation_step <= computation_step + 1;
                end
                3:
                begin
                    t2 <= arr[1] - 1;
                    computation_step <= computation_step + 1;
                end
                4:
                begin
                    t3 <= t2 % 2;
                    computation_step <= computation_step + 1;
                end
                5:
                begin
                    a <= t1 * t3;
                    computation_step <= computation_step + 1;
                end
                6:
                begin
                    computation_step <= (j <= arr_size) ? 7:8;
                end
                7:
                begin
                    b <= arr[j];
                    computation_step <= 9;
                end
                8:
                begin
                    b <= arr[arr_size];
                    computation_step <= 9;
                end
                9:
                begin
                    
                end
            endcase
        end
    end

endmodule