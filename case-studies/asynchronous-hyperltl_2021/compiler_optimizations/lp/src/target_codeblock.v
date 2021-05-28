module TARGET_CODEBLOCK(
    input clk,
    input stutter_in,
    input [1:0] secret_in,

    output reg [1:0] public_out,
    output reg stutter
);

    reg [3:0] computation_step;
    reg [3:0] x;
    reg [1:0] k;

    initial begin
        computation_step <= 0;
        x <= 0;
        k <= 0;
        public_out <= 0;
        stutter <= 0;
    end

    always @(posedge clk) begin
        stutter <= stutter_in;
        if (!stutter_in) begin
            case (computation_step)
                0:
                begin
                    computation_step <= computation_step + 1;
                end
                1:
                begin
                    x <= 0;
                    computation_step <= computation_step + 1;
                end
                2:
                begin
                    k <= 0;
                    computation_step <= computation_step + 1;
                end
                3:
                begin
                    computation_step <= (k == 0) ? 4 : 5;
                end
                4:
                begin
                    x <= secret_in;
                    computation_step <= 6;
                end
                5:
                begin
                    x <= x + x;
                    computation_step <= 6;
                end
                6:
                begin
                    k <= k + 1;
                    computation_step <= computation_step + 1;
                end
                7:
                begin
                    public_out <= x % k;
                    computation_step <= computation_step + 1;
                end
                8:
                begin
                    computation_step <= (k < 3) ? 9 : 14;
                end
                9:
                begin
                    computation_step <= (k == 0) ? 10 : 11;
                end
                10:
                begin
                    x <= secret_in;
                    computation_step <= 12;
                end
                11:
                begin
                    x <= x + x;
                    computation_step <= 12;
                end
                12:
                begin
                    k <= k + 1;
                    computation_step <= computation_step + 1;
                end
                13:
                begin
                    public_out <= x % k;
                    computation_step <= 8;
                end
                14:
                begin
                    
                end
            endcase
        end
    end

endmodule