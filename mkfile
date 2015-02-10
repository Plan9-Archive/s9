< /$objtype/mkfile

TARG=p9s
OFILES=\
	main.$O\
	interpret.$O\
	mem.$O\
	io.$O\
	number.$O

HFILES=scheme.h

BIN=$home/bin/$objtype

< /sys/src/cmd/mkone
