# colonml
Lambda Calculus based "programming langauge" made by me in OCaml, yes the colons are required, thats the point.

#compiling :::ml

after writing a :::ml program ensure you have OCaml and Python downloaded.

then use colonml.py with 2 cmd line args
  - arg1 cml src file
  - arg2 name for an exefile


#Programming in :::ml
The Grammar ->

:::Apply (exp 1, exp 2) -> standard application with substitution.

:::Lambda(n,t,exp) -> Typed lambda expression with n being a string name and t being a type from the type grammar.

:::LambdaRec(f,t1,t2,x,exp) -> Must be used for recursive lambda expressions.

:::If(e1,e2,e3) -> if e1 then e2 else e3.

:::Mult(e1,e2)

:::Plus(e1,e2)

:::Div(e1,e2)

:::IsZero(e1)

:::Var x

:::Num n

:::True

:::False

Type Grammer ->

:::Tint

:::TBool

:::TArrow(t1,t2)






