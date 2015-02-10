#include "scheme.h"

int
getnumtype(Sexp *e)
{
	return e->cari >> 6 & 0x7;
}

Sexp*
makeinteger(fixnum i)
{
	Sexp *n;
	n = consatom(Number, Integer, 0);
	n->integ = i;
	return n;
}

Sexp*
makereal(flonum f)
{
	Sexp *n;
	n = consatom(Number, Real | Inexact, 0);
	n->real = f;
	return n;
}

static Sexp*
add2nums(Sexp *a, Sexp *b)
{
	int ta, tb;
	ta = getnumtype(a) & 0x3;
	tb = getnumtype(b) & 0x3;
	if(ta == Integer && tb == Integer)
		return makeinteger(a->integ + b->integ);
	if(ta == Integer && tb == Real)
		return makereal(a->integ + b->real);
	if(ta == Real && tb == Integer)
		return makereal(a->real + b->integ);
	if(ta == Real && tb == Real)
		return makereal(a->real + b->real);
	// can't happen
	return makeinteger(0);
}

static Sexp*
mul2nums(Sexp *a, Sexp *b)
{
	int ta, tb;
	ta = getnumtype(a) & 0x3;
	tb = getnumtype(b) & 0x3;
	if(ta == Integer && tb == Integer)
		return makeinteger(a->integ * b->integ);
	if(ta == Integer && tb == Real)
		return makereal(a->integ * b->real);
	if(ta == Real && tb == Integer)
		return makereal(a->real * b->integ);
	if(ta == Real && tb == Real)
		return makereal(a->real * b->real);
	// can't happen
	return makeinteger(0);
}

static Sexp*
sub2nums(Sexp *a, Sexp *b)
{
	int ta, tb;
	ta = getnumtype(a) & 0x3;
	tb = getnumtype(b) & 0x3;
	if(ta == Integer && tb == Integer)
		return makeinteger(a->integ - b->integ);
	if(ta == Integer && tb == Real)
		return makereal(a->integ - b->real);
	if(ta == Real && tb == Integer)
		return makereal(a->real - b->integ);
	if(ta == Real && tb == Real)
		return makereal(a->real - b->real);
	// can't happen
	return makeinteger(0);
}

static Sexp*
div2nums(Sexp *a, Sexp *b)
{
	int ta, tb;
	ta = getnumtype(a) & 0x3;
	tb = getnumtype(b) & 0x3;
	if(ta == Integer && tb == Integer){
		if(a->integ % b->integ == 0)
			return makeinteger(a->integ / b->integ);
		return makereal((flonum)a->integ / b->integ);
	}
	if(ta == Integer && tb == Real)
		return makereal(a->integ / b->real);
	if(ta == Real && tb == Integer)
		return makereal(a->real / b->integ);
	if(ta == Real && tb == Real)
		return makereal(a->real / b->real);
	// can't happen
	return makeinteger(0);
}

#define TESTNUMS(name, op) \
Sexp* \
name(Sexp *a, Sexp *b) \
{ \
	int ta, tb; \
	ta = getnumtype(a) & 0x3; \
	tb = getnumtype(b) & 0x3; \
	if(ta == Integer && tb == Integer) \
		return a->integ op b->integ ? true : false; \
	if(ta == Integer && tb == Real) \
		return a->integ op b->real ? true : false; \
	if(ta == Real && tb == Integer) \
		return a->real op b->integ ? true : false; \
	if(ta == Real && tb == Real) \
		return a->real op b->real ? true : false; \
	/* can't happen */ \
	return makeinteger(0); \
}

TESTNUMS(eq2nums, ==)
TESTNUMS(lt2nums, <)
TESTNUMS(gt2nums, >)
TESTNUMS(le2nums, <=)
TESTNUMS(ge2nums, >=)

#undef TESTNUMS

Sexp*
testnums(Sexp *lst, Sexp *(*f)(Sexp*, Sexp*))
{
tail:
	if(lst == empty)
		return true;
	if(lst->cdr == empty)
		return true;
	if(f(lst->car, lst->cdr->car) == false)
		return false;
	lst = lst->cdr;
	goto tail;
}

Sexp*
addnums(Sexp *lst)
{
	Sexp *ret, *acc;
	//TODO: check all are numbers
	pushroot(&lst);
	if(lst == empty){
		ret = makeinteger(0);
		poproot();
		return ret;
	}

	acc = copycons(lst->car);
	pushroot(&acc);

tail:
	lst = lst->cdr;
	if(lst == empty){
		poproot();
		poproot();
		return acc;
	}
	acc = add2nums(acc, lst->car);
	goto tail;
}

Sexp*
mulnums(Sexp *lst)
{
	Sexp *ret, *acc;
	//TODO: check all are numbers
	pushroot(&lst);
	if(lst == empty){
		ret = makeinteger(1);
		poproot();
		return ret;
	}

	acc = copycons(lst->car);
	pushroot(&acc);

tail:
	lst = lst->cdr;
	if(lst == empty){
		poproot();
		poproot();
		return acc;
	}
	// TODO: check for 0 factor -> return 0
	acc = mul2nums(acc, lst->car);
	goto tail;
}

Sexp*
subnums(Sexp *lst)
{
	Sexp *ret, *a, *b;
	//TODO: check all are numbers, nargs > 0
	pushroot(&lst);
	if(lst->cdr == empty){
		a = makeinteger(0);
		pushroot(&a);
		b = copycons(lst->car);
		pushroot(&b);
	}else{
		a = copycons(lst->car);
		pushroot(&a);
		b = addnums(lst->cdr);
		pushroot(&b);
	}
	ret = sub2nums(a, b);
	poproot();
	poproot();
	poproot();
	return ret;
}

Sexp*
divnums(Sexp *lst)
{
	Sexp *ret, *a, *b;
	//TODO: check all are numbers, nargs > 0
	pushroot(&lst);
	if(lst->cdr == empty){
		a = makeinteger(1);
		pushroot(&a);
		b = copycons(lst->car);
		pushroot(&b);
	}else{
		a = copycons(lst->car);
		pushroot(&a);
		b = mulnums(lst->cdr);
		pushroot(&b);
	}
	ret = div2nums(a, b);
	poproot();
	poproot();
	poproot();
	return ret;
}

