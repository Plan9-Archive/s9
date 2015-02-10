#include "scheme.h"

static Sexp *freelist;

static Sexp **blocklist;
static int nblocks, maxblocks;

static Sexp ***roots;
static int nroots, maxroots;

int gcenable;

static void
markcell(Sexp *e)
{
	e->cari |= 1;
}

static void
unmarkcell(Sexp *e)
{
	e->cari &= ~1;
}

static void
freecell(Sexp *e)
{
	e->cari = Free<<2;
	e->cdr = freelist;
	freelist = e;
}

void
sweep(void)
{
	int i, j;
	Sexp *e;

	for(i = 0; i < nblocks; i++)
		for(j = 0; j < 1024; j++){
			e = &blocklist[i][j];
			if(e->cari & 1 ||
			   (e->cari>>2 & 0xF) == Free)
				unmarkcell(e);
			else{
				if(isatom(e) && (gettype(e) == Vector ||
				                 gettype(e) == String))
					free(e->cdr);
				freecell(e);
			}
		}
}

static void
unmark(void)
{
	int i, j;
	Sexp *e;

	for(i = 0; i < nblocks; i++)
		for(j = 0; j < 1024; j++){
			e = &blocklist[i][j];
			if(e->cari & 1 ||
			   (e->cari >> 2 & 0xF) == Free)
				unmarkcell(e);
		}
}

static void
markrecur(Sexp *e)
{
	int i;

	if(e == nil)
		return;
	/* TODO: close files &c. */
	if(e->cari & 1)
		return;
	markcell(e);
	if(ispair(e)){
		markrecur((Sexp*)(e->cari & ~1));
		markrecur(e->cdr);
	}else if(gettype(e) == Vector){
		for(i = 0; i < vectorlen(e); i++)
			markrecur(vectorref(e, i));
	}
}

void
mark(void)
{
	int i;
	for(i = 0; i < nroots; i++)
		markrecur(*roots[i]);
}

static void
addblock(void)
{
	int i;
	Sexp *b;

	if(nblocks == maxblocks){
		maxblocks *= 2;
		blocklist = realloc(blocklist, maxblocks*sizeof(*blocklist));
	}
	blocklist[nblocks++] = b = malloc(1024*sizeof(**blocklist));
	if((uintptr)b & 0x3)
		print("ahh, not aligned %p\n", b);
	for(i = 0; i < 1024; i++)
		freecell(&b[i]);
}

Sexp*
alloccell(void)
{
	Sexp *e;
	if(gcenable){
		mark();
		sweep();
	}
	if(freelist == nil)
		addblock();
	e = freelist;
	freelist = freelist->cdr;
	return e;
}

void
pushroot(Sexp **e)
{
	if(nroots == maxroots){
		maxroots *= 2;
		roots = realloc(roots, maxroots*sizeof(*roots));
	}
	roots[nroots++] = e;
}

void
poproot(void)
{
	if(nroots-- < 0)
		nroots = 0;
}

void
initmem(void)
{
	maxblocks = 1;
	blocklist = malloc(maxblocks*sizeof(*blocklist));
	nblocks = 0;

	freelist = nil;
	addblock();

	maxroots = 10;
	roots = malloc(maxroots*sizeof(*roots));
	nroots = 0;
}

int
issafe(Sexp *e)
{
	int ret;
	mark();
	ret = e->cari & 0x1;
	unmark();
	return ret;
}

void
printroots(void)
{
	int i;
	for(i = 0; i < nroots; i++){
		printsexp(*roots[i], &pout);
		Bprint(&pout, "\n");
	}
	Bflush(&pout);
}
