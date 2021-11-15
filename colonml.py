import sys
import os

def lint_input(p):
    r = p.replace(":::", "CCC")
    return r

def write_ml_file(p, fn):
    std_begin = "open Colonml_interpreter\n let program () = "
    std_end = "\nlet () = print_endline(string_of_exp (program()));"
    p = std_begin + p + std_end
    f = open(fn+".ml", "w")
    f.write(p)
    f.close()

def compile_ml(fn):
    os.system("ocamlc colonml_interpreter.ml "+fn+".ml -o "+fn+ " && rm *.cmi *.cmo "+fn+".ml")

def main():
    if len(sys.argv) < 3:
        print("not enough cmd args \"python colonml.py srcfile outputfile\"")
        return
    else:
        fn = sys.argv[2]
        rd = sys.argv[1]
    p = open(rd,"r")
    program = p.read()
    program = lint_input(program)
    write_ml_file(program, fn)
    compile_ml(fn)

main()
