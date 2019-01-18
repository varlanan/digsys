module Thriller(
	CLOCK_50,
	SW,
	KEY,
	LEDR,
	HEX0, HEX1, HEX2, HEX3, HEX4, HEX5,
	// The ports below are for the VGA output. Do not change.
	VGA_CLK, // VGA Clock
	VGA_HS, // VGA H_SYNC
	VGA_VS, // VGA V_SYNC
	VGA_BLANK_N, // VGA BLANK
	VGA_SYNC_N, // VGA SYNC
	VGA_R, // VGA Red[9:0]
	VGA_G, // VGA Green[9:0]
	VGA_B, // VGA Blue[9:0]
	PS2_CLK,
	PS2_DAT
	);

	input CLOCK_50; // 50 MHz
	inout PS2_CLK, PS2_DAT;
	input [9:0] SW;
	input [8:0] KEY;
	output [9:0] LEDR;
	// Do not change the following outputs
	output VGA_CLK; // VGA Clock
	output VGA_HS; // VGA H_SYNC
	output VGA_VS; // VGA V_SYNC
	output VGA_BLANK_N; // VGA BLANK
	output VGA_SYNC_N; // VGA SYNC
	output [7:0] VGA_R; // VGA Red[7:0] Changed from 10 to 8-bit DAC
	output [7:0] VGA_G; // VGA Green[7:0]
	output [7:0] VGA_B; // VGA Blue[7:0]
	output [6:0] HEX0;
	output [6:0] HEX1;
	output [6:0] HEX2;
	output [6:0] HEX3;
	output [6:0] HEX5, HEX4; //use for score

	wire resetn;
	assign resetn = KEY[0];
	
	//LEDRs used for debugging the control path
	wire flash, user_flash1, user_flash2, user_flash3;
	assign LEDR[0] = flash;
	assign LEDR[1] = user_flash1;
	assign LEDR[2] = user_flash2;
	assign LEDR[3] = user_flash3;
	assign LEDR[9] = gameover;
	
	//input corresponds to the three designated regions of the screen where the boxes fall
	wire user_input1, user_input2, user_input3;
	assign user_input1 = ~KEY[3];
	assign user_input2 = ~KEY[2];
	assign user_input3 = ~KEY[1];

	//start the game
	wire start;
	assign start = SW[0];

	wire access_next, miss, hit, gameover, maingame;
	wire [7:0] score;
	wire [11:0] beatSeq;
	wire [6:0] address_sig;	
	assign LEDR[8] = miss;
	
	//miss and game over output of the HEX display
	reg [6:0]O,U,C,H;
	always@(posedge CLOCK_50)
	begin
		if(miss)
		begin
			H <= 7'b0001001;// H - 4 5 1 2 6
			C <= 7'b1000110; // C - 1 2 6 OFF
			U <= 7'b1000001; // U - 0 6 off
			O <= 7'b1000000; // O - 3 off 
		end
		else begin
			H <= 7'b1111111;
			C <= 7'b1111111;
			U <= 7'b1111111;
			O <= 7'b1111111;
		end
		if (gameover)begin //prints BYE
			H <= 7'b0000110;
			C <= 7'b0010001;
			U <= 7'b0000011;
			O <= 7'b1111111;
		end
		
	end
	
	assign HEX0 = H;
	assign HEX1 = C;
	assign HEX2 = U;
	assign HEX3 = O;
	
	//displays score
	HexDecoder hex4 (.in(score[3:0]), .hex(HEX4[6:0]));
	HexDecoder hex5 (.in(score[7:4]), .hex(HEX5[6:0])); 
	
	reg enable_1, enable_2, enable_3;
	reg [6:0] address_sig_reg;
	//pre-encoded beat sequence with information regarding X-position and speed
	RhythmSeq	RhythmSeq_inst (.address (address_sig),.clock (CLOCK_50),.rden (1),.q (beatSeq));
	//reset address access from the beat Sequence mif
	wire rst;
	assign rst = SW[5];
	
	//address value incremented according to the enable signal from Control Path
	always@(posedge CLOCK_50)
	begin
		if(rst)begin
			address_sig_reg <= 7'b0;
		end
		else if(access_next==1)
			address_sig_reg <= address_sig_reg +1;
	end
	assign address_sig = address_sig_reg;
	
	//according to the beat sequence from the MIF file, appropriate enable signal for a specific box is on
	always@(*)
	begin
		if(beatSeq[2] == 1 && beatSeq[1] == 0 && beatSeq[0] == 0)begin
			enable_1 = 1;
			enable_2 = 0;
			enable_3 = 0;
		end
		else if(beatSeq[2] == 0 && beatSeq[1] == 1 && beatSeq[0] == 0)begin
			enable_2 = 1;
			enable_1 = 0;
			enable_3 = 0;
		end
		else if(beatSeq[2] == 0 && beatSeq[1] == 0 && beatSeq[0] == 1)begin
			enable_3 = 1;
			enable_1 = 0;
			enable_2 = 0;
		end
		else begin
			enable_1 = 0;
			enable_2 = 0;
			enable_3 = 0;
		end	
	end
	
	//predescribed speeds of the falling blocks
	wire [25:0] Speed1 = 26'd1000;
	wire [25:0] Speed2 = 26'd1650;
	wire [25:0] Speed3 = 26'd3300;
	wire [25:0] Speed4 = 26'd6000;
	wire [25:0] speed;
	
	reg [25:0] speed_reg;
	assign speed = speed_reg;

	//depending on the encoded speed patter from the mif, an associated speed will be assigned
	always@(*)
	begin
		case(beatSeq[4:3])
			2'b00: speed_reg = Speed1;
			2'b01: speed_reg = Speed2;
			2'b10: speed_reg = Speed3;
			2'b11: speed_reg = Speed4;
			default: speed_reg = Speed1;
		 endcase
	end
	
	wire enable_sq1, enable_sq2, enable_sq3;
	assign enable_sq1 = enable_1;
	assign enable_sq2 = enable_2;
	assign enable_sq3 = enable_3;
	
	wire [8:0] colourGame;
	wire [8:0] colour_background;
	wire [8:0] colour_start;
	wire [8:0] colour_main;
	
	wire writeEn_background;
	wire writeEn_start;
	wire writeEn_game;
	wire writeEn_main;
	
	wire [7:0] x_game;
	wire [7:0] y_game;
	wire [7:0] x_background;
	wire [7:0] y_background;
	wire [7:0] x_start;
	wire [7:0] y_start;
	wire [7:0] x_main;
	wire [7:0] y_main;
	
	reg [7:0] x;
	reg [6:0] y;
	reg [8:0] colour;
	reg writeEn;
	
	wire startscreen;
	
	//different backgrounds displayed depending on enable signals coming from the Control Path
	always@(*)
	begin
		if(gameover)begin
			x <= x_background;
			y <= y_background;
			writeEn <= ~writeEn_background;
			colour <= colour_background;
		end
		else if(startscreen)begin
			x <= x_start;
			y <= y_start;
			writeEn <= ~writeEn_start;
			colour <= colour_start;
		end
		else if(maingame)begin
			x <= x_main;
			y <= y_main;
			writeEn <= ~writeEn_main;
			colour <= colour_main;
		end
		else begin
			colour <= colourGame;
			x <= x_game;
			y <= y_game;
			writeEn <= writeEn_game;
		end
	end
	//gameover background
	drawBackground inst(.gameover(gameover), .clock(CLOCK_50), .x_final(x_background), .y_final(y_background),
	.count_done(writeEn_background), .color_out(colour_background));
	
	//start screen
	startBackground inst_start(.startscreen(startscreen), .clock(CLOCK_50), .x_final(x_start),.y_final(y_start),
	.count_done(writeEn_start),.color_out(colour_start));
	
	//main game background
	GameBackground inst_main(.maingame(maingame), .clock(CLOCK_50), .x_final(x_main),.y_final(y_main),
	.count_done(writeEn_main),.color_out(colour_main));
	
	//box switches color after goes out of hit region, depending on the user's performance (hit/miss)
	reg [8:0] colourFSM;
	always@(*)
	begin
		if(hit)
			colourFSM <= 9'b000000111;
		else if(miss)
			colourFSM <= 9'b000111000;
		else colourFSM <= 9'b111000000;
	end
	
	// This is the FSM
	FSM fsm_inst (.Clock(CLOCK_50), .C_In(colourFSM), .X_Out(x_game), .Y_Out(y_game),
		.C_Out(colourGame), .PlotToVGA(writeEn_game), .flash(flash), .user_flash1(user_flash1),.user_flash2(user_flash2), .user_flash3(user_flash3),
		.user_input1(user_input1), .user_input2(user_input2), .user_input3(user_input3), .enable_sq1(enable_sq1),
		.enable_sq2(enable_sq2), .enable_sq3(enable_sq3), .access_next(access_next), .speed(speed), .score(score), 
		.miss(miss), .gameover(gameover), .start(start), .startscreen(startscreen), .maingame(maingame),
		.writeEn_main(writeEn_main), .maingame_switch(maingame_switch), .hit(hit));
	// Output will be used for the vga module 
	
	vga_adapter VGA2(
		.resetn(resetn),
		.clock(CLOCK_50),
		.colour(colour),
		.x(x),
		.y(y),
		.plot(writeEn),
		/* Signals for the DAC to drive the monitor. */
		.VGA_R(VGA_R),
		.VGA_G(VGA_G),
		.VGA_B(VGA_B),
		.VGA_HS(VGA_HS),
		.VGA_VS(VGA_VS),
		.VGA_BLANK(VGA_BLANK_N),
		.VGA_SYNC(VGA_SYNC_N),
		.VGA_CLK(VGA_CLK));

	defparam VGA2.RESOLUTION = "160x120";
	defparam VGA2.MONOCHROME = "FALSE";
	defparam VGA2.BITS_PER_COLOUR_CHANNEL = 3;
	defparam VGA2.BACKGROUND_IMAGE = "TapTapWithNameBackground.mif";

endmodule

module FSM (Clock, C_In, X_Out, Y_Out, C_Out, PlotToVGA, flash, user_flash1, user_flash2, user_flash3, 
user_input1, user_input2, user_input3, enable_sq1, enable_sq2, enable_sq3, access_next, speed, 
score, miss, gameover, start, startscreen, maingame, writeEn_main, maingame_switch, hit);

	input Clock;
	input [8:0] C_In;
	input user_input1, user_input2, user_input3, enable_sq1, enable_sq2, enable_sq3;
	input [25:0] speed;
	input start;
	input writeEn_main;
	input maingame_switch;
	
	output access_next;
	output flash, user_flash1, user_flash2, user_flash3;
	output PlotToVGA;
	output [7:0] X_Out;
	output [6:0] Y_Out;
	output [8:0] C_Out;
	
	output [7:0]score;
	output miss;
	output hit;
	output gameover;
	output startscreen;
	output maingame;

	wire ld_x, ld_y, ld_c, frc;
	wire [2:0] x_inc, y_inc;
	wire [8:0] frc_c;
	wire [25:0] ClockFrequency, RateDivider, Max;
	wire [3:0] FrameDivider;
	wire Update, Clear, k, FrameEnable;

	// These variables are used to determine the speed of the box movement
	assign ClockFrequency = 26'd50000000;

	assign Max = (ClockFrequency / speed) - 1'b1;

	RateController rc (.Clock(Clock), .Enable(1), .Clear_b(Clear),.MaxValue(Max), .Q(RateDivider));
	
	// The rateController above creates a pulse for the FrameEnable below
	assign FrameEnable = RateDivider == 26'b0 ? 1 : 0;

	RateController fc (.Clock(Clock), .Enable(FrameEnable), .Clear_b(Clear), .MaxValue(4'b1111), .Q(FrameDivider));
	
	//The rateController above creates a pulse for Update below
	assign Update = FrameDivider == 4'b1111 ? 1 : 0;

	wire [6:0] Y_Pos;
	wire Down;
	
	// The control is dependent on the update, used above
	Control ctrl2 (.Clock(Clock), .Update(Update), .Y_Pos(Y_Pos),
		.Down(Down), .clr(Clear), .plot_to_vga(PlotToVGA), .ld_x(ld_x), .ld_y(ld_y), 
		.ld_c(ld_c), .x_inc(x_inc), .y_inc(y_inc), .frc(frc), .frc_c(frc_c), .reset(Reset), .flash(flash), .user_flash1(user_flash1), .user_flash2(user_flash2), 
		.user_flash3(user_flash3), .user_input1(user_input1), .user_input2(user_input2), .user_input3(user_input3), .access_next(access_next), .enable_sq1(enable_sq1), 
		.enable_sq2(enable_sq2), .enable_sq3(enable_sq3), .score(score), .miss(miss) , .hit(hit),
		.gameover(gameover), .start(start), .startscreen(startscreen), .maingame(maingame), .writeEn_main(writeEn_main),
		.maingame_switch(maingame_switch));
	
	// The datapth is dependent on the control, used above
	DataPath dp2 (.Clock(Clock), .Reset(Reset), .C_In(C_In), .Down(Down), .Y_Pos(Y_Pos), .ld_x(ld_x), .ld_y(ld_y), .ld_c(ld_c), 
		.x_inc(x_inc), .y_inc(y_inc), .frc(frc), .frc_c(frc_c),
		.X_Out(X_Out), .Y_Out(Y_Out), .C_Out(C_Out), .enable_sq1(enable_sq1), .enable_sq2(enable_sq2), .enable_sq3(enable_sq3),
		);

endmodule

module RateController (input Clock, Enable, Clear_b, input [25:0] MaxValue, output reg [25:0] Q);
	
	always @(posedge Clock)
		begin
		if (Clear_b == 1'b1)
			Q <= 0;
		else if (Q >= MaxValue)
			Q <= 0;
		else if (Enable == 1'b1)
			Q <= Q + 1;
		end

endmodule

module Control (Clock, Update, Y_Pos, Down, clr, plot_to_vga, ld_x, ld_y, ld_c, 
	x_inc, y_inc, frc, frc_c, reset, flash, user_flash1, user_flash2, user_flash3, user_input1, 
	user_input2, user_input3, access_next, enable_sq1, enable_sq2, enable_sq3, 
	score, miss, gameover, start, startscreen, maingame, writeEn_main, maingame_switch, hit);

	input Clock, Update, user_input1, user_input2, user_input3, enable_sq1, enable_sq2, enable_sq3;
	input start, writeEn_main, maingame_switch;

	input [6:0] Y_Pos;
	
	output reg access_next;
	output Down;
	output reg flash, user_flash1, user_flash2, user_flash3;
	output reg reset;
	output reg ld_x, ld_y, ld_c, frc, clr, plot_to_vga;
	output reg [2:0] x_inc, y_inc;
	output reg [8:0] frc_c;
	output reg [7:0] score;
	reg score_once;
	output reg miss;
	output reg hit;
	output reg gameover;
	reg [25:0] miss_count;

	localparam  S_START_SCREEN = 4'd0,
					S_MAIN_SCREEN = 4'd1,
					S_RESET_ALL = 4'd2,
					S_REST = 4'd3,
					S_CLEAR = 4'd4,
					S_CLEAR_INCR = 4'd5,
					S_CLEAR_END = 4'd6,
					S_UPDATE_DIR = 4'd7,
					S_UPDATE_POS = 4'd8,
					S_PLOT = 4'd9,
					S_PLOT_INCR = 4'd10,
					S_PLOT_END = 4'd11,
					S_RESET_COUNT = 4'd12;

	reg [3:0] current_state, next_state;
	reg [5:0] inc_count; 
	reg right, down;
	output reg startscreen, maingame;


	// State table
	always@(posedge Clock)
		begin
		case (current_state)
			S_START_SCREEN: next_state= start ? S_MAIN_SCREEN : S_START_SCREEN;
			S_MAIN_SCREEN: next_state = writeEn_main ?  S_RESET_ALL : S_MAIN_SCREEN;
			S_RESET_ALL: next_state = S_REST;
			S_REST: next_state = Update ? S_CLEAR : S_REST;
			S_CLEAR: next_state = S_CLEAR_INCR;
			S_CLEAR_INCR: next_state = S_CLEAR_END;
			S_CLEAR_END: next_state = inc_count == 6'b0 ? S_UPDATE_DIR : S_CLEAR_INCR; // Go into loop 16 times
			S_UPDATE_DIR: next_state = S_UPDATE_POS;
			S_UPDATE_POS: next_state = S_PLOT;
			S_PLOT: next_state = S_PLOT_INCR;
			S_PLOT_INCR: next_state = S_PLOT_END;
			S_PLOT_END: next_state = inc_count == 6'b0 ? S_RESET_COUNT : S_PLOT_INCR; // Go into loop 16 times
			S_RESET_COUNT: next_state = S_REST;
			default: next_state = S_RESET_ALL;
		endcase
	// End of State Table

	ld_x = 0;
	ld_y = 0;
	ld_c = 0;
	plot_to_vga = 0;
	x_inc = 3'b0;
	y_inc = 3'b0;
	frc = 0;
	clr = 0;
	reset = 0;

		case (current_state)
			// This state loads the start screen
			S_START_SCREEN: begin
				startscreen = 1;	
			end
			// This state loads the main game screen that stays on until gameover is reached
			S_MAIN_SCREEN: begin
				startscreen=0;
				maingame=1;
			end
			// This state resets everything to the intial values
			S_RESET_ALL: begin
				reset = 1;
				down <= 1;
				clr = 1;
				maingame = 0;
			end

			S_CLEAR: begin inc_count = 6'd0;

			// This state clears the shape drawn by using the last known
			// x and y points and setting them equal to black
			end
			S_CLEAR_INCR: begin
				plot_to_vga = 1;
				frc = 1; // the 'ld_' that actiavtes the erasing
				frc_c = 9'b0; // the black color for erasing
				x_inc = inc_count[5:3];
				y_inc = inc_count[2:0];
				inc_count = inc_count + 1;
				//enable signal for access of the next note
				if(Y_Pos == 7'd119 &&inc_count==6'b000001)begin
					access_next = 1;
				end
				else access_next = 0;
			end
			// This state checks for user input when the box reaches the key areas of hit 
			// and sends appropriate signals according to the user's performance
			S_UPDATE_DIR: begin
				if (Y_Pos == 7'd0) 
					down <= 1; 
				if (Y_Pos>=7'd90 && Y_Pos <= 7'd100)
				begin
					flash = 1;
					if(user_input1 == 1'b1 && enable_sq1 == 1'b1 )begin
						user_flash1 = 1;
						score = score +1;
					end
					else if(user_input2 == 1'b1 && enable_sq2 == 1'b1) begin
						user_flash2 = 1;
						score = score +1;
					end
					else if(user_input3 == 1'b1 && enable_sq3 == 1'b1 )begin
						user_flash3 = 1;
						score = score +1;
					end
				end
				// if box (note) falls below the limit of 100, and if player has not pressed the key at the right time
				// for the corresponding note, send miss signal to display OUCH
				// and incremenet the amount of hits (miss_count)
				if(Y_Pos > 7'd100 && Y_Pos < 7'd120)
				begin
					if(user_flash1==0 && user_flash2==0 && user_flash3==0)
					begin
							miss = 1;
							hit = 0;
							if(Y_Pos == 7'd101)begin
									miss_count = miss_count +1;
								//user missed more than 6, game is over
								if (miss_count >=26'd6)
									gameover=1;
								else gameover= 0;
							end
						end
					else begin
					miss = 0;
					hit = 1;
					end
					
					flash = 0;
				end
					if(Y_Pos > 7'd120)begin
						miss = 0;
						hit = 0;
						user_flash1 = 0;
						user_flash2 = 0;
						user_flash3 = 0;
					end
			end
			// This states allows the color, x, and y to be loaded
			// into the datapath
			S_UPDATE_POS: begin
				ld_x = 1;
				ld_y = 1;
				ld_c = 1;
			end
			S_PLOT: inc_count = 6'd0;
			// This state draws the new shape onto the screen
			// with the new x and y values
			S_PLOT_INCR: begin
				plot_to_vga = 1;
				x_inc = inc_count[5:3];
				y_inc = inc_count[2:0];
				inc_count = inc_count + 1;
			end
			// This state resets everything
			S_RESET_COUNT: clr = 1;
		endcase
		current_state = next_state;
	end 

	assign Down = down;   // should move down on the screen (Sent as output)

endmodule




module DataPath (Clock, Reset, C_In, Down, ,
	ld_x, ld_y, ld_c, x_inc, y_inc, 
	frc, frc_c, X_Out, Y_Out, C_Out, Y_Pos, enable_sq1, enable_sq2, enable_sq3);

	input Clock, Reset, Down;
	input ld_x, ld_y, ld_c, frc;
	input [2:0] x_inc, y_inc;
	input [8:0] C_In, frc_c;

	input enable_sq1, enable_sq2, enable_sq3;

	output [8:0] C_Out;
	output [6:0] Y_Out, Y_Pos;
	output [7:0] X_Out;

	reg [7:0] x;
	reg [6:0] y;
	reg [8:0] c;

	always @(posedge Clock)
		begin
		// If reset has been activated, make everyhting equal to 0
		if (Reset) begin
			if(enable_sq1)begin
				x <= 8'd30;
			end
			else if(enable_sq2)begin
				x <= 8'd60;
			end
			else if(enable_sq3)begin
				x <= 8'd90;
			end
			y <= 7'b0;
			c <= 9'b111000000;
		end
		else begin
			if(ld_x)begin
				if(enable_sq1 && !enable_sq2 && !enable_sq3)begin
					x <= 8'd30;
				end
				else if(enable_sq2 && !enable_sq1 && !enable_sq3)begin
					x <= 8'd60;
				end
				else if(enable_sq3 && !enable_sq1 && !enable_sq2)begin
					x <= 8'd90;
				end
			end
			if (ld_y) begin
				if (Down) 
					y <= y + 1'b1; // move the shape down
				else 
					y <= y; 
			end
			if (ld_c) 
				c <= C_In; 
		end
	end

	// Send the orginal x, y position back to the vga
	assign Y_Pos = y;
	// Also send back the most outward x, y values of the 4 x 4 bits
	assign X_Out = x + x_inc;
	assign Y_Out = y + y_inc;
	// If the shape is getting erased (meaning that frc has been activated), 
	// frc_c (which is black) will be used instead of the color input
	assign C_Out = frc ? frc_c : c;

endmodule


module HexDecoder(in, hex);
	input[3:0] in; 
	output[6:0] hex;
	
	display4bit u0(
			.x0(in[3]),
			.x1(in[2]),
			.x2(in[1]),
			.x3(in[0]),
			.s0(hex[0]),
			.s1(hex[1]),
			.s2(hex[2]),
			.s3(hex[3]),
			.s4(hex[4]),
			.s5(hex[5]),
			.s6(hex[6])
			);

endmodule

module display4bit(input x0, x1, x2, x3, output s0, s1, s2, s3, s4, s5, s6);

	assign s0 = ~((x0|x1|x2|~x3)&(x0|~x1|x2|x3)&(~x0|x1|~x2|~x3)&(~x0|~x1|x2|~x3));
	assign s1 = ~((x0|~x1|x2|~x3)&(x0|~x1|~x2|x3)&(~x0|x1|~x2|~x3)&(~x0|~x1|x2|x3)&(~x0|~x1|~x2|x3)&(~x0|~x1|~x2|~x3));
	assign s2 = ~((x0|x1|~x2|x3)&(~x0|~x1|x2|x3)&(~x0|~x1|~x2|x3)&(~x0|~x1|~x2|~x3));
	assign s3 = ~((x0|x1|x2|~x3)&(x0|~x1|x2|x3)&(x0|~x1|~x2|~x3)&(~x0|x1|~x2|x3)&(~x0|~x1|~x2|~x3));
	assign s4 = ~((x0|x1|x2|~x3)&(x0|x1|~x2|~x3)&(x0|~x1|x2|x3)&(x0|~x1|x2|~x3)&(x0|~x1|~x2|~x3)&(~x0|x1|x2|~x3));
	assign s5 = ~((x0|x1|x2|~x3)&(x0|x1|~x2|x3)&(x0|x1|~x2|~x3)&(x0|~x1|~x2|~x3)&(~x0|~x1|x2|~x3));
	assign s6 = ~((x0|x1|x2|x3)&(x0|x1|x2|~x3)&(x0|~x1|~x2|~x3)&(~x0|~x1|x2|x3));

endmodule 

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DRAW BACKGROUNDS: END, START, MAIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
module drawBackground(input gameover, clock, output [7:0] x_final, output [7:0] y_final,output reg count_done,
output reg [8:0] color_out);


	wire [8:0] color;
	reg [7:0] count_x;
	reg [7:0] count_y;
	reg [14:0] memory;

	background	ending1 (	.address ( memory ), .clock ( clock ), .rden ( 1'b1 ), .q (color)); //ending screen

	always @(posedge clock)
	begin
		if(!gameover)
		begin
			count_x <= 8'b0;
			count_y <= 8'b0;
			count_done = 1'b0;
			color_out <= 8'b0;
			memory <= 15'b0;
		end
		
		else if(gameover && !count_done)
		begin
			color_out <= color;
			memory <= memory +1;
			if(count_x < 8'd159)
				count_x <= count_x +1;
				
			if(count_x == 8'd159 && count_y< 8'd119)
			begin
				count_x <= 8'b0;
				count_y <= count_y +1;
			end

			if(count_x == 8'd159 && count_y == 8'd119)
				count_done <= 1'b1;
		end
	end

	assign x_final = count_x;
	assign y_final = count_y;

endmodule

module startBackground(input startscreen, clock, output [7:0] x_final,output [7:0] y_final,
output reg count_done,output reg [8:0] color_out);


	wire [8:0] color;
	reg [7:0] count_x;
	reg [7:0] count_y;
	reg [14:0] memory;

	startscreen	start1 (	.address (memory), .clock (clock), .q (color)); //start screen

	always @(posedge clock)
	begin
		if(!startscreen)
		begin
			count_x <= 8'b0;
			count_y <= 8'b0;
			count_done = 1'b0;
			color_out <= 8'b0;
			memory <= 15'b0;
		end
		
		else if(startscreen && !count_done)
		begin
			color_out <= color;
			memory <= memory +1;
			if(count_x < 8'd159)
				count_x <= count_x +1;
				
			if(count_x == 8'd159 && count_y< 8'd119)
			begin
				count_x <= 8'b0;
				count_y <= count_y +1;
			end

			if(count_x == 8'd159 && count_y == 8'd119)
				count_done <= 1'b1;
		end
	end

	assign x_final = count_x;
	assign y_final = count_y;

endmodule

module GameBackground(input maingame, clock,output [7:0] x_final,output [7:0] y_final, 
output reg count_done, output reg [8:0] color_out);


	wire [8:0] color;
	reg [7:0] count_x;
	reg [7:0] count_y;
	reg [14:0] memory;

	maingame	maingame_inst (.address(memory),.clock(clock),.rden(1'b1),.q(color)); //main background

	always @(posedge clock)
	begin
		if(!maingame)
		begin
			count_x <= 8'b0;
			count_y <= 8'b0;
			count_done = 1'b0;
			color_out <= 8'b0;
			memory <= 15'b0;
		end
		
		else if(maingame && !count_done)
		begin
			color_out <= color;
			memory <= memory +1;
			if(count_x < 8'd159)
				count_x <= count_x +1;
				
			if(count_x == 8'd159 && count_y< 8'd119)
			begin
				count_x <= 8'b0;
				count_y <= count_y +1;
			end

			if(count_x == 8'd159 && count_y == 8'd119)
				count_done <= 1'b1;
		end
	end

	assign x_final = count_x;
	assign y_final = count_y;

endmodule


