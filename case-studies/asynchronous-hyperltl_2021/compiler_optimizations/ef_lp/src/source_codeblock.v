module SOURCE_CODEBLOCK(
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
                    computation_step <= computation_step + 1;;
                end
                2:
                begin
                    k <= 0;
                    computation_step <= computation_step + 1;
                end
                3:
                begin
                    computation_step <= (k < 3) ? 4 : 9;
                end
                4:
                begin
                    computation_step <= (k == 0) ? 5 : 6;
                end
                5:
                begin
                    x <= secret_in;
                    computation_step <= 7;
                end
                6:
                begin
                    x <= x + x;
                    computation_step <= 7;
                end
                7:
                begin
                    k <= k + 1;
                    computation_step <= computation_step + 1;
                end
                8:
                begin
                    public_out <= (4 + (x - 4)) % ((k - k) + 3);
                    computation_step <= 3;
                end
                9:
                begin
                    
                end
            endcase
        end
    end

endmodule