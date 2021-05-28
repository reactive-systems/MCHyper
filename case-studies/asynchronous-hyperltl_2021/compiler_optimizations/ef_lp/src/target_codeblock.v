module TARGET_CODEBLOCK(
    input clk,
    input stutter_in,
    input [1:0] secret_in,

    output reg [1:0] public_out,
    output reg stutter
);

    reg [4:0] computation_step;
    reg [3:0] x;
    reg [1:0] k;
    reg [3:0] t0;
    reg [3:0] t1;
    reg [1:0] t2; 
    reg [1:0] t3;

    initial begin
        computation_step <= 0;
        t0 <= 0;
        t1 <= 0;
        t2 <= 0;
        t3 <= 0;
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
                    t0 <= (x - 4);
                    computation_step <= computation_step + 1;
                end
                8:
                begin
                    t1 <= 4 + t0;
                    computation_step <= computation_step + 1;
                end
                9:
                begin
                    t2 <= k - k;
                    computation_step <= computation_step + 1;
                end
                10:
                begin
                    t3 <= t2 + 3;
                    computation_step <= computation_step + 1;
                end
                11:
                begin
                    public_out <= t1 % t3;
                    computation_step <= computation_step + 1;
                end
                12:
                begin
                    computation_step <= (k < 3) ? 13 : 22;
                end
                13:
                begin
                    computation_step <= (k == 0) ? 14 : 15;
                end
                14:
                begin
                    x <= secret_in;
                    computation_step <= 16;
                end
                15:
                begin
                    x <= x + x;
                    computation_step <= 16;
                end
                16:
                begin
                    k <= k + 1;
                    computation_step <= computation_step + 1;
                end
                17:
                begin
                    t0 <= (x - 4);
                    computation_step <= computation_step + 1;
                end
                18:
                begin
                    t1 <= 4 + t0;
                    computation_step <= computation_step + 1;
                end
                19:
                begin
                    t2 <= k - k;
                    computation_step <= computation_step + 1;
                end
                20:
                begin
                    t3 <= t2 + 3;
                    computation_step <= computation_step + 1;
                end
                21:
                begin
                    public_out <= t1 % t3;
                    computation_step <= 12;
                end
                22:
                begin
                    
                end
            endcase
        end
    end

endmodule