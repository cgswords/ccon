all: test
	raco pkg remove ccon
	raco pkg install

test:
	raco test ./
