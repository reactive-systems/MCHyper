#!/usr/bin/env python2.7
# coding=utf-8

# reads an aag or an aig file and a formula and a strategy.aag file. 

# requires aig_tools to be available. Adapt variable 'aigertools', if necessary. 

import sys, os, re, itertools
from subprocess import Popen,PIPE,check_output
import subprocess
from collections import deque

aigertools = os.path.dirname(sys.argv[0]) + '/../aiger/'
mchyper_tool = os.path.dirname(sys.argv[0]) + '/src/Main'
abc_bin = os.path.dirname(sys.argv[0]) + '/../abc/abc'

usage_string = 'Usage: {}.py [OPTIONS] <bmc/pdr/dot/aig/show> <-f formula/-t file> [file] [-s strategy]\n\
  \'file\'\t\t can be an .aig file or an .aag file\n\
  \'-f formula\'\t\t the formula to check\n\
  \'-s strategy\'\t\t the strategy circuit as an .aag file to be used for forall-exists formulas\n\
  -h/--help \t\t print usage information\n\
  -o/--out_file file \t specify in which file to write for aag/dot output\n\
  -v <num> \t\t set verbosity level (default 0)'.format(sys.argv[0]) 

def fun():
    file_arg = ''
    formula = ''
    strategy_arg = '' 
    command = ''
    out_file = ''
    only_mchyper = False
    cex_flag = False
    verbosity = 0
    
    if len(sys.argv) == 1:
        print usage_string
        return 1;

    skip = 0
    for i in range(1,len(sys.argv)):
        if skip>0:
            skip -= 1
            continue
        arg = sys.argv[i]
        if arg == '--help' or arg == '-h':
            print usage_string
            return 1;
        elif arg == '-cex':
            cex_flag = True
        elif arg == '-bmc':
            command += 'bmc -F 100;'
        elif arg == '-bmc2':
            command += 'bmc2 -F 100;'
        elif arg == '-bmc3':
            command += 'bmc3 -F 100;'
        elif arg == '-pdr':
            command += 'pdr;'
        elif arg == '-int':
            command += 'int;'
        elif arg == '-dot':
            command += 'write_dot;'
            if out_file == '':
                out_file = 'tmp'
        elif arg == '-aig':
            command += 'write_aiger;'
            if out_file == '':
                out_file = 'tmp'
        elif arg == '-aag':
            command = 'aag'
            if out_file == '':
                out_file = 'tmp'
        elif arg == '-aagdot':
            command = 'aagdot'
            if out_file == '':
                out_file = 'tmp'
        elif arg == '-show':
            command += 'show;'
        elif arg == '--out_file' or arg == '-o':
            assert(len(sys.argv)>i+1)
            out_file = sys.argv[i+1]
            skip = 1
        elif arg == '-f':
            assert(len(sys.argv)>i+1)
            formula = sys.argv[i+1]
            skip = 1
        elif arg == '-s': 
            assert(len(sys.argv)>i+1)
	    strategy_arg = sys.argv[i+1]
	    skip = 1
	    #print strategy_arg
        elif arg == '-v':
            assert(len(sys.argv)>i+1)
            verbosity = int(sys.argv[i+1])
            skip = 1
        else:
            file_arg = arg
    
    # parse formula file, if necessary
    test_cases = []
    if formula == '' and test_file != '':
        tfile = open(test_file, 'r')
        content = tfile.read()
        tfile.close()
        test_cases = content.split('\n')
        
    aigerfile = ''
    if file_arg == '': # no file name given, reading stdin
        aigerfile = sys.stdin.read()
    else: # read file
        af = open(file_arg, 'r')
        aigerfile = af.read()
        af.close()
    
    if file_arg.endswith('.aig') or aigerfile.startswith('aig'):
        print 'Converting to Aiger ASCII format ...\n'
        ps = Popen([aigertools + 'aigtoaig -a'], shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        (output,err) = ps.communicate(aigerfile)
        aigerfile = output

    #print aigerfile

    # run the main transformation
    print 'Hyper Hyper!'
    #print 'Formula: ' + formulas[0].replace('\"','\\\"')

    #print 'given strategy file: ' + strategy_arg
    command_string = mchyper_tool + ' \"' + formula.replace('\"','\\\"') + '\"'
    if strategy_arg != '':
        command_string += ' -s ' + strategy_arg
    #print 'generated command sting: ' + command_string + '\n---- run MCHyper ----'

    ps = Popen(command_string, shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE) 
    (tool_output_raw,err) = ps.communicate(aigerfile)

    #print 'Raw: \n' + tool_output_raw

    #run aigtoaig twice to make sure that the output complies to the conventions regarding the literal order (smallest literals belong to outputs)
    if (not 'no parse' in err) and (tool_output_raw.startswith('aag')):
        # convert output from aag to aig
        ps = Popen([aigertools + 'aigtoaig'], shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        (aig_output,err) = ps.communicate(tool_output_raw)
        
        if err != '': 
            print 'Error (aigtoaig): ' + err

        # convert output from aig to aag
        ps = Popen([aigertools + 'aigtoaig -a'], shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        (tool_output,err) = ps.communicate(aig_output)
        
        if err != '': 
            print 'Error (aigtoaig): ' + err
    else:
        tool_output = tool_output_raw

    #print 'Output: \n' + tool_output 

    if verbosity >= 3: print 'Tool output:\n' + tool_output + '------------\n' 
    if verbosity >= 2 and err != '' and err != None: 
        print 'Tool error: ' + err
        #return 1
    
    if 'no parse' in err: 
        print 'Parsing error in the formula. Unfortunately, no more information is available at the moment.'
        return 1
    
    if not tool_output.startswith('aag'):
        print 'Error: Tool did not provide a model.'
        return 1

    if tool_output.startswith('aag 0 0 0 0 0'):
        print 'Error: Aiger for strategy could not be parsed correctly.'
        return 1
    
    if command == 'aag':
        print 'Tool output (-aag):\n' + tool_output + '-------------------\n' 
        #out_f = open(out_file + '.aag', "w")
        #out_f.write(tool_output)
        #out_f.close
        #print 'Raw aag output written to: ' + out_file + '.aag'
        return 1

    if command == 'aagdot':
        ps = Popen([aigertools + 'aigtodot'], shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        (dot_output,err) = ps.communicate(tool_output)
    
        if err != '': 
            print 'Error: ' + err
            return
        out_f = open(out_file + '.dot', "w")
        
        out_f.write(dot_output)
        out_f.close
        print 'Raw dot output written to: ' + out_file + '.dot'
        return 1
    
    
    # convert output from aag to aig
    ps = Popen([aigertools + 'aigtoaig'], shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    (aig_output,err) = ps.communicate(tool_output)
    
    if err != '': 
        print 'Error (aigtoaig): ' + err
        return
        
    #sys.stdout.write(aig_output)
    aig_tmp_file = open("tmp.aig", "w")
    aig_tmp_file.write(aig_output)
    aig_tmp_file.close()
    
    
    abc_command = 'read tmp.aig;' + command + ' ' + out_file
    if cex_flag:
        #abc_command += 'logic;undc;st;zero;write_cex -f tmp.cex'
        abc_command += 'write_cex -f tmp.cex'
    abc_command = "'" + abc_command + "'"

    abc = Popen(abc_bin + ' -q ' + abc_command, shell=True, stdout=PIPE, stderr=PIPE)

    print "ABC is running (use '-v 1' to see output of ABC)\n"
    
    (abc_output1,abc_err1) = abc.communicate()
    
    if verbosity >= 1: print "---\n" + abc_output1 + "---\n"
    
    cex = False
    liveness_involved = False
    if ('Output 0 of miter \"tmp\" was asserted in frame' in abc_output1):
        cex = True
        print 'Counterexample found. Safety violation.'
        
    if ('Output 1 of miter \"tmp\" was asserted in frame' in abc_output1) :
        cex = True
        liveness_involved = True
        print 'Counterexample found. Liveness involved.'
        
    if cex and cex_flag:
        print 'Writing counterexample to: tmp.cex.'
        cex_file = open("tmp.cex", "r")
        cex_file_content = cex_file.read()
        cex_file.close()
        cex_file_content = cex_file_content.replace('=0 ','=0 \n').replace('=1 ','=1 \n')
        cex_file = open("tmp.cex",'w')
        cex_file.write(cex_file_content)
        cex_file.close()
    
    print ""
    
    return 0
    
    
        
if __name__ == "__main__":
    fun()
    
