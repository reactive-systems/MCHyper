#!/usr/bin/env python3
# coding=utf-8

# reads an aag or an aig file and a formula and a strategy.aag file. 

import sys, os
from subprocess import Popen,PIPE,run
import argparse
from pathlib import Path
from typing import IO

mchyper_tool = os.path.dirname(sys.argv[0]) + '/src/Main'

def is_valid_file_path(parser, arg):
    path = Path(arg)
    if not path.is_file():
        parser.error("The file %s does not exist!" % arg)
    else:
        return path# return an open file handle
    
def is_valid_file(parser, arg):
    path = Path(arg)
    if not path.is_file():
        parser.error("The file %s does not exist!" % arg)
    else:
        return arg# return an open file handle

def mchyper():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(title="subcommands",required=True,dest='command')

    check_command = subparsers.add_parser("check")
    check_command.add_argument('-f','--formula',type=str,required=True)
    check_command.add_argument('-s','--strategy', type=lambda x: is_valid_file(parser, x))
    check_command.add_argument('-m','--model', type=lambda x: is_valid_file_path(parser, x))
    check_command.add_argument('-v','--verbosity',type=int,default=1)
    check_command.add_argument('-r','--reduce-method', choices=['none','dfraig','ifraig','strash','rewrite','refactor','combination1','combination2','combination3','compress','compress2'],default='compress2')
    check_command.add_argument('-c','--check-methods',nargs='*', choices=['bmc','bmc2','bmc3','pdr','int'],default=['bmc','pdr'])
    check_command.add_argument('-b','--bound',type=int,default=100)
    check_command.add_argument("-x", "--counter-example", type=str)
    check_command.add_argument("-X", "--exit-code", action="store_true")

    aag_command = subparsers.add_parser("aag")
    aag_command.add_argument('-f','--formula',type=str,required=True)
    aag_command.add_argument('-s','--strategy', type=lambda x: is_valid_file(parser, x))
    aag_command.add_argument('-m','--model', type=lambda x: is_valid_file_path(parser, x))
    aag_command.add_argument('-v','--verbosity',type=int,default=1)
    aag_command.add_argument('-o','--output-file',type=Path)
    aag_command.add_argument('-r','--reduce-method', choices=['none','dfraig','ifraig','strash','rewrite','refactor','combination1','combination2','combination3','compress','compress2'],default='compress2')
    
    aagdot_command = subparsers.add_parser("aagdot")
    aagdot_command.add_argument('-f','--formula',type=str,required=True)
    aagdot_command.add_argument('-s','--strategy', type=lambda x: is_valid_file(parser, x))
    aagdot_command.add_argument('-m','--model', type=lambda x: is_valid_file_path(parser, x))
    aagdot_command.add_argument('-v','--verbosity',type=int,default=1)
    aagdot_command.add_argument('-o','--output-file',type=Path)
    aagdot_command.add_argument('-r','--reduce-method', choices=['none','dfraig','ifraig','strash','rewrite','refactor','combination1','combination2','combination3','compress','compress2'],default='compress2')

    args = parser.parse_args()

    if args.command == 'check':
        pass
    elif args.command == 'aag':
        pass
    elif args.command == 'aagdot':
        pass
    else:
        sys.exit('Unknown command')

    
    # read in the model in Aiger format 
    aigerfile :bytes = b''
    if args.model: # read file
        af = open(args.model, 'rb')
        aigerfile = af.read()
        af.close()
    else: 
        # no file name given, reading stdin
        aigerfile = sys.stdin.read().encode()
    
    # ensure that the model is in Aiger ASCII format
    if aigerfile.startswith("aig".encode()):
        if args.verbosity > 0:
            print('Converting to Aiger ASCII format ...\n')
        ps = Popen(['aigtoaig','-a'], stdin=PIPE, stdout=PIPE, stderr=PIPE)
        (aigerfile,err) = ps.communicate(aigerfile)
        if len(err) > 0:
            sys.exit('Error (aigtoaig): ' + err.decode())
    elif not aigerfile.startswith("aag".encode()):
        sys.exit("Model is not in Aiger format.")
    

    # run the main transformation
    if args.verbosity > 0:
        print('Hyper Hyper!')

    command = [mchyper_tool, args.formula]
    if args.strategy:
        command.append('-s')
        command.append(args.strategy)

    if args.verbosity > 0:
        print('Running the construction\n')
    ps = Popen(command, stdin=PIPE, stdout=PIPE, stderr=PIPE) 
    (tool_output_raw,err) = ps.communicate(aigerfile)
    tool_output:str =tool_output_raw.decode()

    #run aigtoaig twice to make sure that the output complies to the conventions regarding the literal order (smallest literals belong to outputs)
    aig_output = b''
    if (not 'no parse' in err.decode()) and (tool_output_raw.startswith('aag'.encode())):
        # convert output from aag to aig
        ps = Popen(['aigtoaig'], stdin=PIPE, stdout=PIPE, stderr=PIPE)
        (aig_output,err) = ps.communicate(tool_output_raw)
        if len(err) > 0:
            sys.exit('Error (aigtoaig): ' + err.decode())

        # reduce the AIG
        if args.reduce_method != 'none':
            aig_tmp_file = open("tmp.aig", "wb")
            aig_tmp_file.write(aig_output)
            aig_tmp_file.close()
            mapping={'dfraig':'read tmp.aig; dfraig; write_aiger -s tmp.aig','ifraig':'read tmp.aig; ifraig; write_aiger -s tmp.aig','strash':'read tmp.aig; strash; write_aiger -s tmp.aig','rewrite':'read tmp.aig; rewrite -zl; write_aiger -s tmp.aig','refactor':'read tmp.aig; refactor -zl; write_aiger -s tmp.aig','combination1':'read tmp.aig; strash; refactor -zl; rewrite -zl; write_aiger -s tmp.aig','combination2':'read tmp.aig; strash; refactor -zl; rewrite -zl; strash; refactor -zl; rewrite -zl; write_aiger -s tmp.aig','combination3':'read tmp.aig; strash; refactor -zl; rewrite -zl; strash; refactor -zl; rewrite -zl; dfraig; rewrite -zl; dfraig; write_aiger -s tmp.aig','compress':'read tmp.aig; balance -l; rewrite -l; rewrite -lz; balance -l; rewrite -lz; balance -l; write_aiger -s tmp.aig','compress2':'read tmp.aig; balance -l; rewrite -l; refactor -l; balance -l; rewrite -l; rewrite -lz; balance -l; refactor -lz; rewrite -lz; balance -l; write_aiger -s tmp.aig'}
            abc_reduce = run(['abc','-q',mapping[args.reduce_method]], stdin=PIPE, stdout=PIPE, stderr=PIPE)
            if len(abc_reduce.stderr) > 0:
                sys.exit('Error (abc reduce): ' + abc_reduce.stderr.decode())
            
            aig_tmp_file = open("tmp.aig", "rb")
            aig_output = aig_tmp_file.read()
            aig_tmp_file.close()

        # convert output from aig to aag
        ps = Popen(['aigtoaig', '-a'], stdin=PIPE, stdout=PIPE, stderr=PIPE)
        (tool_output_raw,err) = ps.communicate(aig_output)
        tool_output = tool_output_raw.decode("utf-8", "strict")
        if len(err) > 0:
            sys.exit('Error (aigtoaig): ' + err.decode())


    if args.verbosity >= 3: print('Tool output:\n' + tool_output + '------------\n') 
    if args.verbosity >= 2 and len(err)>0: 
        print('Tool error: ' + err.decode())
    
    if 'no parse' in err.decode(): 
        sys.exit('Parsing error in the formula. Unfortunately, no more information is available at the moment.')
    
    if not tool_output.startswith('aag'):
        sys.exit('Error: Tool did not provide a model.')

    if tool_output.startswith('aag 0 0 0 0 0'):
        sys.exit('Error: Aiger for strategy could not be parsed correctly.')
    
    if args.command == 'check':
        aig_tmp_file = open("tmp.aig", "wb")
        aig_tmp_file.write(aig_output)
        aig_tmp_file.close()
        
        subprocesses:list[tuple[Popen[bytes],str]] = []
        for method in args.check_methods:
            mapping = {'bmc':'bmc -F '+str(args.bound)+';','bmc2':'bmc2 -F '+str(args.bound)+';','bmc3':'bmc3 -F '+str(args.bound)+';','pdr':'pdr;','int':'int;'}
            abc_command = 'read tmp.aig; ' + mapping[method]
            cex = ''
            if args.counter_example:
                cex = args.counter_example + '_'+method+'.aig'
                abc_command += 'write_cex -f ' + cex + ';'
            subprocesses.append((Popen(['abc', '-q', abc_command], stdout=PIPE, stderr=PIPE),cex))
            
        if args.verbosity == 1:
            print("ABC is running (use '-v 2' to see output of ABC)\n")


        while True:
            if len(subprocesses) == 0:
                if args.verbosity > 0:
                    print("Result: UNKNOWN")
                if args.exit_code:
                    sys.exit(30)
                else:
                    sys.exit(1)
            index = 0
            while index < len(subprocesses):
                returncode = subprocesses[index][0].poll()
                if returncode is None:
                    index +=1
                else:
                    stdout : IO[bytes] = subprocesses[index][0].stdout # type: ignore
                    abc_output = stdout.read().decode()
                    if args.verbosity >= 1: 
                        print("---\n" + abc_output + "---\n")

                    success = False
                    if ('was successful' in abc_output):
                        success = True
                        if args.verbosity >= 1: 
                            print('Property proven.')

                    cex = False
                    if ('Output 0 of miter \"tmp\" was asserted in frame' in abc_output):
                        cex = True
                        if args.verbosity >= 1: 
                            print('Counterexample found. Safety violation.')
                        
                    if ('Output 1 of miter \"tmp\" was asserted in frame' in abc_output) :
                        cex = True
                        if args.verbosity >= 1: 
                            print('Counterexample found. Liveness involved.')
                        
                    if cex and args.counter_example:
                        cex_file = subprocesses[index][1]
                        if args.verbosity >= 1: 
                            print('Writing counterexample to: ' + cex_file)
                        cex_f = open(cex_file, "r")
                        cex_file_content = cex_f.read()
                        cex_f.close()
                        cex_file_content = cex_file_content.replace('=0 ','=0 \n').replace('=1 ','=1 \n')
                        cex_f = open(cex_file, "w")
                        cex_f.write(cex_file_content)
                        cex_f.close()

                    del subprocesses[index]

                    if success or cex:
                        for (sub,_) in subprocesses:
                            sub.kill()
                        if args.exit_code:
                            if success:
                                sys.exit(10)
                            else:
                                sys.exit(20)
                        else:
                            return

    elif args.command == 'aag':
        if args.output_file:
            out_f = open(args.output_file, "wb")
            out_f.write(tool_output_raw)
            out_f.close
            print('Aiger written to: ' + str(args.output_file) )
        else:
            # print('Tool output (-aag):\n' + tool_output + '-------------------\n') 
            print(tool_output)
        return 0
    elif args.command == 'aagdot':
        ps = Popen(['aigtodot'], shell=True, stdin=PIPE, stdout=PIPE, stderr=PIPE)
        (dot_output_raw,err_raw) = ps.communicate(tool_output_raw)
        err = err_raw.decode("utf-8", "strict") 
        if err != '': 
            sys.exit('Error: ' + err)
        if args.output_file:
            out_f = open(args.output_file, "wb")
            out_f.write(dot_output_raw)
            out_f.close
            print('Raw dot output written to: ' + str(args.output_file) )
        else:
            print(dot_output_raw.decode())
        return 0
    else:
        sys.exit('Unknown command')
    
    return 0
    
    
        
if __name__ == "__main__":
    mchyper()
    
