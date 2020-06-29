// Model of connected Huffman encoder and decoder.
// The alphabet consists of the uppercase letters and the space.
// The Huffman tree used by encoder and decoder is shown below.
// All left branches are labeled 0, and all right branches are labeled 1.
//
//                       +-------------( )---------------+
//                       |                               |
//                       |                               |
//              +-------( )------+               +------( )-----+
//              |                |               |              |
//              |                |               |              |
//        +----( )----+         ( )          +--( )--+         ( )
//        |           |         / \          |       |         / \
//        |           |        |   |         |       |        |   |
//    +--( )--+      ( )      [E] ( )       ( )     ( )      [ ] ( )
//    |       |      / \          / \       / \     / \          / \
//    |       |     |   |        |   |     |   |   |   |        |   |
//   ( )     ( )   [S] ( )      ( ) [A]   [I] [O] [R] [N]      ( ) [T]
//   / \     / \       / \      / \                            / \
//  |   |   |   |     |   |    |   |                          |   |
// [U] [P] [F] [C]   ( ) [L]  [H] ( )                        [D] ( )
//                   / \          / \                            / \
//                  |   |        |   |                          |   |
//            +----( ) [W]      [G] [Y]                        ( ) [M]
//            |      \                                         / \
//            |       |                                       |   |
//           ( )     ( )                                     [B] [V]
//           / \     / \
//          |   |   |   |
//         [Q] ( ) [K] [X]
//             / \
//            |   |
//           [Z] [J]
//
// As an example, the code of W is 001101.
//
// This tree is based on the following assumed frequencies.
//
//  E 130  T 93  N 78  R 77  I 74  O 74  A 73  S 63  D 44
//  H  35  L 35  C 30  F 28  P 27  U 27  M 25  Y 19  G 16
//  W  16  V 13  B  9  X  5  K  3  Q  3  J  2  Z  1
//
// That is, it is assumed that there are 130 Es for every thousand letters.
// It is further assumed that there are 182 spaces for every 1000 letters.
//
// The encoder retrieves the code for each symbol from a map, and shifts it
// out one bit at the time.  The decoder is a finite state machine whose
// state transition graph is obtained from the tree by adding acs from the
// leaves back to the top of the tree.  (To the second level nodes to be
// precise.)  Each node uses ten bits for its encoding.  The code of the root
// is 0.  If a state is not a leaf of the tree, and its encoding is n, then
// the encodings of its two children are 2n+1 and 2n+2.

// Author: Fabio Somenzi <Fabio@Colorado.EDU>

module main(clk, addr);
    input clk;
    input [4:0] addr;

    wire  cipher;
    wire [7:0] character, plain;

    huffmanEnc encoder (clk, addr, cipher, character);

    huffmanDec decoder (clk, cipher, plain);

    // Latch data that we want to refer to in properties.
    reg        ci;
    reg [7:0]  ch;

    initial begin
	ci = 0;
	ch = 0;
    end

    always @ (posedge clk) begin
	ci = cipher;
	ch = character;
    end

endmodule // main


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


// The output plain is 0 except for one clock cycle when a character has
// been decoded.
module huffmanDec (clk,cipher,plain);
    input        clk;
    input 	 cipher;
    output [7:0] plain;

    reg [9:0] 	 state;
	    
    wire 	 leaf;
    wire [7:0] 	 character;

    initial state = 0;

    // This function maps states to characters.  All non-leaf states are
    // mapped to NUL.  The leaf states are mapped to the ASCII code of the
    // corresponding symbol.
    function [7:0] map;
	input [9:0] state;
	begin: _map
	    case (state)
	        9: map = 69; // E
	       13: map = 32; // space
	       17: map = 83; // S
	       22: map = 65; // A
	       23: map = 73; // I
	       24: map = 79; // O
	       25: map = 82; // R
	       26: map = 78; // N
               30: map = 84; // T
	       31: map = 85; // U
	       32: map = 80; // P
	       33: map = 70; // F
	       34: map = 67; // C
	       38: map = 76; // L
	       43: map = 72; // H
	       59: map = 68; // D
	       76: map = 87; // W
	       89: map = 71; // G
	       90: map = 89; // Y
	      122: map = 77; // M
	      243: map = 66; // B
	      244: map = 86; // V
	      303: map = 81; // Q
	      305: map = 75; // K
	      306: map = 88; // X
	      609: map = 90; // Z
	      610: map = 74; // J
	      default: map = 0;
	    endcase // case(state)
	end // block: _map
    endfunction // map

    assign plain = map(state);
    assign leaf = plain != 0;

    always @ (posedge clk) begin
	state = (leaf ? 0 : {state[8:0],1'b0}) + (cipher ? 2 : 1);
    end

endmodule // huffmanDec
