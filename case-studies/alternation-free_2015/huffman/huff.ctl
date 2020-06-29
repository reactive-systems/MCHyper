######################################################################
# Properties of the decoder.
######################################################################

#PASS: The all-zero state is never re-entered.
AX AG !decoder.state[9:0]=0;

#PASS: The output is never 255.
AG !plain[7:0]=255;

#PASS: No two consecutive states have plain != 0.
AG(plain[7:0]=0 + AX plain[7:0]=0);

#PASS: It is always possible to output a Z and a 0.
AG EF plain[7:0]=90;
AG EF plain[7:0]=0;

#PASS: String 010 is decoded as E.
AG((decoder.state[9:0]=0 + decoder.leaf=1) ->
   AX(ci=0 -> AX(ci=1 -> AX(ci=0 -> plain[7:0]=69))));

#FAIL: in this property, "plain" is strobed one clock cycle too late.
AG((decoder.state[9:0]=0 + decoder.leaf=1) ->
   AX(ci=0 -> AX(ci=1 -> AX(ci=0 -> AX plain[7:0]=69))));

#PASS: String 00000 is decoded as U.
AG((decoder.state[9:0]=0 + decoder.leaf=1) ->
   AX(ci=0 -> AX(ci=0 -> AX(ci=0 -> AX(ci=0 -> AX(ci=0 ->
                                                  plain[7:0]=85))))));


######################################################################
# Properties of the encoder.
######################################################################

#PASS: The encoder shift register is never 0.
AG !encoder.shiftreg[9:0]=0;


######################################################################
# Global properties.
######################################################################

#PASS: The encoder sends an E when the decoder is ready to receive it.
EF((decoder.state[9:0]=0 + decoder.leaf=1) * EX(ci=0 * EX(ci=1 * EX(ci=0))));

#PASS: When a new transmission begins, the decoder is ready.
AG(encoder.shiftreg[9:1]=1 -> AX decoder.leaf=1);

#PASS: Characters are successfully transmitted.  (Two equivalent forms.)
AG(decoder.leaf=1 -> plain[7:0]==ch[7:0]);
AG(plain[7:0]=0 + plain[7:0]==ch[7:0]);
