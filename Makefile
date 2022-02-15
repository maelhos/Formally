name = Formally

all:
	@dune build -j 20
	@rm -f releaseBuild/${name}.cma
	@rm -f releaseBuild/${name}.cmxa
	@rm -f releaseBuild/test
	@cp _build/default/lib/${name}.cma releaseBuild/${name}.cma
	@cp _build/default/lib/${name}.cmxa releaseBuild/${name}.cmxa
	@cp _build/default/test.exe releaseBuild/test

clean:
	rm -rvf _build
	rm -f releaseBuild/${name}.cma
	rm -f releaseBuild/${name}.cmxa
	@echo CLEAN
