all: func

tests : func
	cd tests && ./run -2 ../func 


func : func.exe
	cp _build/default/func.exe func

func.exe: func.ml
	dune build func.exe 
explain:
	menhir --base /tmp/parser --dump --explain parser.mly
	cat /tmp/parser.conflicts

clean:
	dune clean && rm pjava && rm -rf _build
.PHONY: all clean func.exe explain

