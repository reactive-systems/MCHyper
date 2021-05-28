module TARGET_CODEBLOCK(
    input clk,
    input stutter_in,

    output reg x, stutter
);

    reg [2:0] computation_step;
    reg t0;
    reg t1;
    reg t2; 
    reg t3;

    initial begin
        computation_step <= 0;
        t0 <= 0;
        t1 <= 0;
        t2 <= 0;
        t3 <= 0;
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
                    t0 <= 2 * 1;
                    computation_step <= computation_step + 1;
                end
                2:
                begin
                    t1 <= t0 - 1;
                    computation_step <= computation_step + 1;
                end
                3:
                begin
                    t2 <= 1 * 1;
                    computation_step <= computation_step + 1;
                end
                4:
                begin
                    t3 <= t2 / 1;
                    computation_step <= computation_step + 1;
                end
                5:
                begin
                    x <= t1 * t3;
                    computation_step <= computation_step + 1;
                end
                6:
                begin
                    
                end
            endcase
        end
    end

endmodule