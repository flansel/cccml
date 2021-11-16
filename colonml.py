import sys
import os

def lint_input(p):
    r = p.replace(":::", "CCC")
    return r

def write_ml_file(p, fn):
    std_begin = "open Colonml_interpreter\n let ptype () = type_check [] ( "
    std_eval = "let program () = multi_step("
    std_end = "\nlet () = ignore(ptype()); ignore(program());"
    p = std_begin + p + ")\n" + std_eval + p + ")\n" + std_end
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
