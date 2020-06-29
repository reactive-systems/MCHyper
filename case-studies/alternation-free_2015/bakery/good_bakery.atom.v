// The bakery algorithm for mutual exclusion simulates a bakery in
// which customers (processes) take a numbered ticket when they enter the
// store, and then wait for their number to be called.
//
// This (almost) parameterized implementation emulates interleaving
// of the system process by a nondeterministic global selector.
//
// This implementation is finite state.  Rather than holding a numerical
// ticket, processes update a matrix that keeps track of the relative age
// of their tickets.  When a process wants to enter the critical region,
// it records the indices of all processes currently holding a ticket.
// These are the processes to which it will defer.  Hence, the information
// stored in a matrix called "defer."
//
// Ties among processes with ticket of the same age are broken according
// to a fixed priority scheme:  a process with lower index has precedence
// over one with higher index.
//
// On exit from the critical region, a process has to clear all the deference
// bits in which it is one of the two parties to prevent deadlock.
//
// Due to restrictions imposed by vl2mv, the parameterization of this
// description is incomplete.  Besides changing the values of the
// parameters, one also has to modify the two functions extract and
// clearBit.
//
// This description still retains the basic sequence of actions (distributed
// among 11 program steps) of the original bakery in the VIS distribution.
// In particular, the setting of the defer bits (corresponding to the choice
// of a ticket) is an atomic operation.  So is the clearing of the defer
// bits on exit from the critical regions.  This is not a very good idea,
// though it reduces the sequential depth of the model.
//
// Author: Fabio Somenzi <Fabio@Colorado.EDU>

// Type of program counter locations.
//typedef enum {L1, L2, L3, L4, L5, L6, L7, L8, L9, L10, L11} loc;

module bakery(clock,select,pause);
    // MSB of the process index variables.  Enough bits should be given
    // to represent HIPROC+1
    parameter		SELMSB = 1;
    // Highest process index.  Indices start at 0.
    // It must be HIPROC < 2**(SELMSB+1).
    parameter 		HIPROC = 2;
    
    
    parameter PCMSB = 3;

    parameter L1 = 0;
    parameter L2 = 1;
    parameter L3 = 2;
    parameter Lx = 3;
    //parameter L4 = 3;
    //parameter L5 = 4;
    //parameter L6 = 5;
    //parameter L7 = 6;
    //parameter L8 = 7;
    parameter L9 = 4;
    parameter L10 = 5;
    parameter L11 = 6;

    
    
    input 		clock;
    // Nondeterministic selection of enabled process.
    input [SELMSB:0] 	select;
    // Nondeterministic choice between pause and progress.
    input 		pause;

    // The ticket-holding flags of the processes.
    reg			ticket[0:HIPROC];
    // More than one process may be choosing a ticket.  Hence, more than
    // one process may end up with the same ticket number.  These global
    // variables allow one process to wait for completion of a choice
    // that is in progress before comparing its ticket to that of another
    // process.  If the ticket number is the same, the process index is
    // used to decide which process accesses the critical section.
    reg 		choosing[0:HIPROC];
    // The program counters of the processes.
    reg [PCMSB:0]            pc[0:HIPROC];
    // The loop indices of the processors.
    reg [SELMSB:0] 	j[0:HIPROC];
    // The latched value of the process selection variable.
    // Th1s variable appears in the fairness constraints.
    reg [SELMSB:0] 	selReg;
    // Register used to hold j[sel].  It could be replaced by a wire,
    // but the BDDs would suffer.
    reg [SELMSB:0] 	k;
    reg [HIPROC:0]	defer[0:HIPROC];
    reg [HIPROC:0] 	defSel, defK;
    reg 		defSelK, defKSel;
    
    integer 		i;

    reg break;

    // Extract one bit from a vector.
    // WARNING: change if HIPROC is modified.
    function extract;
	input [HIPROC:0] in;
	input [SELMSB:0] index;
	begin: _extract
	    if (index == 0)
	      extract = in[0];
	    else if (index == 1)
	      extract = in[1];
	    else if (index == 2)
	      extract = in[2];
	    else
	      extract = 0;	// should not happen
	end
    endfunction // extract


    // Returns the first input with the bit selected by the second input
    // set to 0.
    // WARNING: change if HIPROC is modified.
    function [HIPROC:0] clearBit;
	input [HIPROC:0] in;
	input [SELMSB:0] index;
	begin: _clearBit
	    clearBit = in;
	    if (index == 0)
	      clearBit[0] = 0;
	    else if (index == 1)
	      clearBit[1] = 0;
	    else if (index == 2)
	      clearBit[2] = 0;
	end
    endfunction // clearBit


    task process;
	input [SELMSB:0] sel;
	begin: _process
	    case (pc[sel])
	      L1: begin choosing[sel] = 1; pc[sel] = L2; end
	      L2: begin
		  defSel = 0;
		  for (i = 0; i <= HIPROC; i = i + 1)
		    defSel = {ticket[i], defSel[HIPROC:1]};
		  defer[sel] = defSel;
		  ticket[sel] = 1;
          j[sel] = 0;
		  pc[sel] = L3;
	      end
	      L3: begin choosing[sel] = 0; pc[sel] = Lx; end
	      // Loop over all processes to check ticket.
          Lx: begin
          break = 0;
          //for (j[sel] = 0; j[sel] <= HIPROC; j[sel] = j[sel] + 1)
          for (k = 0; k <= HIPROC; k = k + 1)
          //for (j[sel] = 0; (1 - break) && j[sel] <= HIPROC; j[sel] = j[sel] + 1)
          //for (j[sel] = 0; !break && j[sel] <= HIPROC; j[sel] = j[sel]) 
              begin
              //k = j[sel];
              if (j[sel] <= k && !break)
                  begin
                  if (choosing[k])
                    begin
                    j[sel] = k;
                    break = 1;
                    end
                  else
                    defSel = defer[sel];
                    defK = defer[k];
                    defSelK = defSel[k]; //defSelK = extract(defSel,k);
                    defKSel = extract(defK,sel); //defKSel = defK[sel];
                    if (ticket[k] && (defSelK || (!defKSel && k < sel))) 
                        begin
                        j[sel] = k;
                        break = 1;
                        end
                  end
              end // end for
          if (break) pc[sel] = Lx; else pc[sel] = L9;
          end
	      
          //L4: begin j[sel] = 0; pc[sel] = L5; end
	      //L5: begin
		  //if (j[sel] <= HIPROC) pc[sel] = L6; else pc[sel] = L9;
	      //end
	      //// Wait while (choosing[j[sel]])
	      //L6: begin
		  //k = j[sel];
		  //if (choosing[k]) pc[sel] = L6; else pc[sel] = L7;
	      //end
	      //// Wait while process j[sel] has an older ticket, or it
	      //// has a ticket of the same age and higher priority.
	      //L7: begin
		  //k = j[sel];
		  //defSel = defer[sel];
		  //defK = defer[k];
		  //defSelK = extract(defSel,k);
		  //defKSel = extract(defK,sel);
		  //if (ticket[k] && (defSelK || (!defKSel && k < sel)))
		  //  pc[sel] = L7;
		  //else
		  //  pc[sel] = L8;
	      //end
	      //L8: begin j[sel] = j[sel] + 1; pc[sel] = L5; end
	      
          // Enter critical section.
	      L9: begin if (pause) pc[sel] = L9; else pc[sel] = L10; end
	      // Leave critical section.
	      L10: begin
		  ticket[sel] = 0;
		  for (i = 0; i <= HIPROC; i = i + 1) begin
		      defK = defer[i];
		      defer[i] = clearBit(defK,sel);
		  end
		  pc[sel] = L11;
	      end
	      L11: begin if (pause) pc[sel] = L11; else pc[sel] = L1; end
	    endcase
	end
    endtask // process

    initial begin
	for (i = 0; i <= HIPROC; i = i + 1) begin
	    ticket[i] = 0;
	    defer[i] = 0;
	    choosing[i] = 0;
	    pc[i] = L1;
	    j[i] = 0;
	end
	k = 0;
	selReg = 0;
	defSel = 0;
	defK = 0;
	defSelK = 0;
	defKSel = 0;
    break = 0;
    end

    always @ (posedge clock) begin
	if (select > HIPROC)
	  selReg = 0;
	else
	  selReg = select;
	process(selReg);
    end

endmodule // bakery
