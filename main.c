#include "scheme.h"

int debug;

Biobuf pin, pout, perr;
Sexp *empty, *true, *false;
Sexp *quote, *quasiquote, *unquote, *unquotesplicing,
     *λ, *lambda, *beta, *delta;

static Sexp *symtab;

static Sexp*
findsymbol(char *s)
{
	Sexp *e, *sym;

	for(e = symtab; e != empty; e = e->cdr){
		sym = e->car;
		if(strcmp((char*)sym->cdr, s) == 0)
			return sym;
	}
	return nil;
}

/* Get a symbol, make one if necessary */
Sexp*
getsymbol(char *s)
{
	Sexp *e;
	e = findsymbol(s);
	if(e == 0){
		e = consatom(Symbol, 0, (uintptr)strdup(s));
		symtab = cons(e, symtab);
	}
	return e;
}

Sexp*
cons(Sexp *a, Sexp *b)
{
	Sexp *e;
	pushroot(&a);
	pushroot(&b);
	e = alloccell();
	poproot();
	poproot();
	e->car = a;
	e->cdr = b;
	return e;
}

Sexp*
copycons(Sexp *a)
{
	Sexp *e;
	pushroot(&a);
	e = alloccell();
	poproot();
	e->car = a->car;
	e->cdr = a->cdr;
	return e;
}

Sexp*
consatom(int type, uintptr a, uintptr b)
{
	Sexp *e;
	a <<= 6;
	a |= 1<<1;
	a |= type<<2;
	e = alloccell();
	e->cari = a;
	e->cdri = b;
	return e;
}

Sexp*
makevector(int len, Sexp *init)
{
	int i;
	Sexp **vec;

	vec = malloc(len*sizeof(*vec));
	for(i = 0; i < len; i++)
		vec[i] = init;
	return consatom(Vector, len, (uintptr)vec);
}

void
vectorset(Sexp *vec, int i, Sexp *e)
{
	((Sexp**)vec->cdr)[i] = e;
}

Sexp*
vectorref(Sexp *vec, int i)
{
	return ((Sexp**)vec->cdr)[i];
}

int
vectorlen(Sexp *vec)
{
	return vec->cari >> 6;
}

int
isatom(Sexp *e)
{
	return e->cari & 0x2;
}

int
ispair(Sexp *e)
{
	return !(e->cari & 0x2);
}

int
gettype(Sexp *e)
{
	return e->cari>>2 & 0xF;
}


/* TODO: iterate */
int
listlen(Sexp *e)
{
	if(e == empty)
		return 0;
	return 1 + listlen(e->cdr);
}

Sexp*
assq(Sexp *obj, Sexp *alist)
{
tail:
	if(alist == empty)
		return false;
	if(alist->car->car == obj)
		return alist->car;

	alist = alist->cdr;
	goto tail;
}

/* TODO: unused, iterate */
Sexp*
pairlis(Sexp *keys, Sexp *values, Sexp *alist)
{
	Sexp *ret;
	if(keys == empty || values == empty)
		return alist;
	pushroot(&keys);
	pushroot(&values);
	ret = cons(cons(keys->car, cons(values->car, empty)),
	           pairlis(keys->cdr, values->cdr, alist));
	poproot();
	poproot();
	return ret;
}

/* TODO: check GC, iterate */
Sexp*
pairargs(Sexp *vars, Sexp *values, Sexp *env)
{
	Sexp *ret, *a, *b;

	if(vars == empty)
		return env;
	if(isatom(vars))
		return cons(cons(vars,
		            cons(values, empty)),
		            env);
	pushroot(&vars);
	pushroot(&values);
	a = cons(vars->car, cons(values->car, empty));
	pushroot(&a);
	b = pairargs(vars->cdr, values->cdr, env);
	pushroot(&b);
	ret = cons(a, b);
	poproot();
	poproot();
	poproot();
	poproot();
	return ret;
}

Sexp*
reverse(Sexp *lst)
{
	Sexp *rev;
	rev = empty;
	// TODO: save lst?
tail:
	if(lst == empty)
		return rev;
	rev = cons(lst->car, rev);

	lst = lst->cdr;
	goto tail;
}

Sexp*
mapcar(Sexp *(*f)(Sexp *), Sexp *lst)
{
	Sexp *tmp;
	if(lst == empty)
		return empty;
	tmp = f(lst->car);
	pushroot(&tmp);
	tmp = cons(tmp, mapcar(f, lst->cdr));
	poproot();
	return tmp;
}

void
init(void)
{
	Binit(&pin, 0, OREAD);
	Binit(&pout, 1, OWRITE);
	Binit(&perr, 2, OWRITE);

	initmem();

	empty = consatom(Special, 0, 0);
	true = consatom(Special, 0, 0);
	false = consatom(Special, 0, 0);
	symtab = empty;
	pushroot(&empty);
	pushroot(&true);
	pushroot(&false);
	pushroot(&symtab);

	λ = getsymbol("λ");
	lambda = getsymbol("lambda");
	beta = getsymbol("β");
	delta = getsymbol("δ");
	quote = getsymbol("quote");
	quasiquote = getsymbol("quasiquote");
	unquote = getsymbol("unquote");
	unquotesplicing = getsymbol("unquotesplicing");

	initinterp();
}

void
dofile(char *name)
{
	Biobuf *f;
	Sexp *e, *ret;

	f = Bopen(name, OREAD);
	if(f == nil){
		Bprint(&perr, "couldn't open file %s\n", name);
		Bflush(&perr);
		return;
	}
	while(e = readsexp(f))
		eval(e, &ret);
	Bterm(f);
}

void
printvalues(int n, Sexp *v, Biobuf *f)
{
	if(n == 0)
		return;
	if(v->car->car == beta)
		Bprint(f, "β-closure");
	else
		printsexp(v->car, f);
	Bprint(f, "\n");
	printvalues(n-1, v->cdr, f);
}

void
main(int argc, char *argv[])
{
	Sexp *e, *ret;
	int n;

	ARGBEGIN{
	case 'd':
		debug++;
		break;
	default:
		break;
	}ARGEND;

	if(sizeof(void*) < sizeof(flonum) ||
	   sizeof(void*) < sizeof(fixnum)){
		fprint(2, "error: fixnum/flonum have wrong size\n");
		exits("size");
	}

	init();

//	dofile("lib.scm");

	gcenable = 1;

	while(e = readsexp(&pin)){
		if(e == nil)
			break;
		n = eval(e, &ret);
		if(n > 0){
			Bprint(&pout, "-> ");
			printvalues(n, ret, &pout);
			Bflush(&pout);
		}
	}
	exits(nil);
}
