module SPISlave(
    input clk,
    input stutter_in, help_sclk, help_miso, change, align,
    // SPI
    input sclk_in,
    input mosi_in,
    input ss_in,
    output reg miso,
    output reg byte_transfered, 
    output reg sclk,
    output reg mosi,
    output ss,
    output reg st,
    // Additional Input and Outputs
    input[7:0] send_item,
    output reg[7:0] send_value
);

//clock identification
reg rising_edge;
reg falling_edge;
reg prev_sclk;

//delay input because of clock identification
reg[1:0] sclk_d;
wire sclk_v;
assign sclk_v = sclk_d[0];
reg[1:0] mosi_d;
wire mosi_v;
assign mosi_v = mosi_d[0];
reg[1:0] en_d;
wire ss_v;
assign ss_v = ~en_d[0];
reg[1:0] stutter_d;
wire stutter_v;
assign stutter_v = stutter_d[0];

// receiving
reg[7:0] rx_buffer;
reg[4:0] rx_pos;
reg byte_received;

// sending
reg[7:0] tx_buffer;
reg[4:0] tx_pos;

// enable trick
reg en;
assign ss = ~en;

initial begin
    // clock
    rising_edge <= 1'b0;
    falling_edge <= 1'b0;
    prev_sclk <= 1'b0;
    // inputs
    sclk_d <= 2'b00;
    mosi_d <= 2'b00;
    en_d <= 2'b00;
    stutter_d <= 2'b00;
    // receiving
    rx_buffer <= 8'b00000000;
    rx_pos <= 4'b0000;
    byte_received <= 1'b0;
    // sending
    tx_buffer <= 8'b00000000;
    tx_pos <= 4'b0000;
    byte_transfered <= 1'b0;
    // outputs
    miso <= 1'b0;
    mosi <= 1'b0;
    sclk <= 1'b0;
    en <= 1'b0;
    st <= 1'b0;
    send_value <= 8'b00000000;
end

always @(posedge clk) begin
    stutter_d <= {stutter_d[0], stutter_in};
    if(!stutter_v) begin
       // clock
        rising_edge <= ({prev_sclk,sclk_in} == 2'b01);
        falling_edge <= ({prev_sclk,sclk_in} == 2'b10);
        prev_sclk <= sclk_in;
        // input delay
        sclk_d <= {sclk_d[0], sclk_in};
        mosi_d <= {mosi_d[0], mosi_in};
        en_d <= {en_d[0], ~ss_in};
        // active
        if (!ss_v) begin
            // receiving
            if(rising_edge) begin
                if(!byte_received) begin
                    rx_pos <= (rx_pos + 4'b0001) % 8;
                    rx_buffer <= {rx_buffer[6:0], mosi_v};
                    byte_received <= (rx_pos == 4'b0111);
                end 
            end
            //sending
            if(falling_edge) begin
                if(!byte_transfered) begin
                    tx_pos <= (tx_pos + 4'b0001) % 8;
                    miso <= tx_buffer[tx_pos];
                    send_value[tx_pos] <= tx_buffer[tx_pos];
                    byte_transfered <= (tx_pos == 4'b0111);
                end
            end
        end else begin
            byte_received <= 1'b0;
        end
        //outputs
        sclk <= sclk_v;
        mosi <= mosi_v;
        en <= ~ss_v;
        st <= stutter_v;
    end
end

// Write input to tx_buffer in first step
reg not_first_cycle;

initial begin
    not_first_cycle <= 1'b0;
end

always @(posedge clk) begin
    if (!not_first_cycle) begin
        not_first_cycle <= 1'b1;
        tx_buffer <= send_item;
    end
end

endmodule
// yosys -p "hierarchy -top SPISlave; proc; flatten; techmap -map +/dff2ff.v; delete SPISlave/clk; synth; aigmap; write_aiger -ascii -symbols spi_slave.aag" spi_slave.v
