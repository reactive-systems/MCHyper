module SOURCE_CODEBLOCK(
    input clk,
    input stutter_in,

    output reg x, stutter
);

    reg[1:0] computation_step;

    initial begin
        computation_step <= 0;
        x <= 0;
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
                    x <= ((2 * 1) - 1) * ((1 * 1) / 1);
                    computation_step <= computation_step + 1;
                end
                2:
                begin
                    
                end
            endcase
        end
    end

endmodule