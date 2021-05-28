module SOURCE_CODEBLOCK(
    input clk,
    input stutter_in,
    input secret_in,

    output reg public_out, secret_out, stutter
);

    reg [3:0] computation_step;
    reg x;

    initial begin
        computation_step <= 0;
        x <= 0;
        public_out <= 0;
        secret_out <= 0; 
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
                    x <= secret_in;
                    computation_step <= computation_step + 1;
                end
                2:
                begin
                    computation_step <= (0) ? 3 : 4;
                end
                3:
                begin
                    public_out <= x;
                    computation_step <= 5;
                end
                4:
                begin
                    secret_out <= x;
                    computation_step <= 5;
                end
                5:
                begin

                end
            endcase
        end
    end

endmodule