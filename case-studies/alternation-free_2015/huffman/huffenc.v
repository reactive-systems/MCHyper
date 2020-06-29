
module huffmanEnc (clk, addr, cipher, character);
    input        clk;
    input [4:0]  addr;
    output 	 cipher;
    output [7:0] character;

    reg [7:0] 	 character;

    // This function is the map from symbols (ASCII space and uppercase
    // letters) to codes.  Each code consists of from 3 to 9 bits.
    // Since the codes are of variable length, an additional
    // bit is used to mark the end of the symbol.  This bit is the
    // leftmost 1.  The code is sent out LSB first; hence, it is reversed
    // in this map.  For instance, 0000010100 (the entry of the map for S)
    // says that the code for S is 0010.
    function [9:0] code;
	input [7:0] c;
	begin: _code
	    case (c)
	      69: code = 10'b0000001010; // E
	      32: code = 10'b0000001011; // space
	      83: code = 10'b0000010100; // S
	      65: code = 10'b0000011110; // A
	      73: code = 10'b0000010001; // I
	      79: code = 10'b0000011001; // O
	      82: code = 10'b0000010101; // R
	      78: code = 10'b0000011101; // N
              84: code = 10'b0000011111; // T
	      85: code = 10'b0000100000; // U
	      80: code = 10'b0000110000; // P
	      70: code = 10'b0000101000; // F
	      67: code = 10'b0000111000; // C
	      76: code = 10'b0000111100; // L
	      72: code = 10'b0000100110; // H
	      68: code = 10'b0000100111; // D
	      87: code = 10'b0001101100; // W
	      71: code = 10'b0001010110; // G
	      89: code = 10'b0001110110; // Y
	      77: code = 10'b0001110111; // M
	      66: code = 10'b0010010111; // B
	      86: code = 10'b0011010111; // V
	      81: code = 10'b0100001100; // Q
	      75: code = 10'b0101001100; // K
	      88: code = 10'b0111001100; // X
	      90: code = 10'b1010001100; // Z
	      74: code = 10'b1110001100; // J
	      default: code = 10'b0;
	    endcase // case(character)
	end
    endfunction // code

    // This function supplies the ASCII codes of the symbols.
    function [7:0] ROM;
	input [4:0] address;
	begin: _ROM
	    if (address < 26)
	      ROM = 65 + {3'b0, address};
	    else
	      ROM = 32;
	end
    endfunction // ROM

    reg [9:0] shiftreg;

    initial begin
	character = ROM(addr);
	shiftreg = code(character);
    end

    always @ (posedge clk) begin
	if (shiftreg[9:1] == 1) begin
	    character = ROM(addr);
	    shiftreg = code(character); // load a new code
	end else begin
	    shiftreg = {1'b0, shiftreg[9:1]}; // shift right
	end
    end

    assign cipher = shiftreg[0];

endmodule // huffmanEnc
