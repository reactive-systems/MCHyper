module strategy(output throttle0_0, output throttle1_0, output throttle2_0, output throttle3_0, output throttle4_0, output throttle0_1, output throttle1_1, output throttle2_1, output throttle3_1, output throttle4_1);
	assign throttle0_0 = 1'b 0;
	assign throttle1_0 = 1'b 1;
	assign throttle2_0 = 1'b 0;
	assign throttle3_0 = 1'b 1;
	assign throttle4_0 = 1'b 0;

	assign throttle0_1 = 1'b 0;
	assign throttle1_1 = 1'b 0;
	assign throttle2_1 = 1'b 1;
	assign throttle3_1 = 1'b 0;
	assign throttle4_1 = 1'b 1;
endmodule


