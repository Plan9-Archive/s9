#include "scheme.h"

State s;
Sexp *globalenv, *syntaxenv;
Sexp *labellist;

static Sexp *aeval_l, *evlis_l, *evlis1_l, *evblock_l, *evblock1_l,
            *callwv1_l,
            *if1_l, *and1_l, *or1_l, *set1_l, *define1_l, *nil_l, *error_l;

static Sexp*
makelabel(void (*ret)(void))
{
	return consatom(Number, 0, (uintptr)ret);
}

static void
saveup(Sexp *ret)
{
	s.clink = cons(s.exp,
	          cons(s.unevlis,
	          cons(s.env,
	          cons(s.evlis,
	          cons(ret,
	          cons(s.clink, empty))))));
}

static void
restore(void)
{
	Sexp *temp;

	if(s.clink == empty){
		Bprint(&perr, "process ran out - restore\n");
		Bflush(&perr);
		return;
	}
	temp = s.clink;
	s.exp     = temp->car;	temp = temp->cdr;
	s.unevlis = temp->car;	temp = temp->cdr;
	s.env     = temp->car;	temp = temp->cdr;
	s.evlis   = temp->car;	temp = temp->cdr;
	s.pc      = temp->car;	temp = temp->cdr;
	s.clink   = temp->car;
}

static void
defineglobal(Sexp *key, Sexp *val)
{
	Sexp *a;
	a = assq(key, globalenv);
	if(a != false)
		a->cdr->car = val;
	else{
		a = cons(globalenv->car, globalenv->cdr);
		globalenv->cdr = a;
		globalenv->car = cons(key, cons(val, empty));
	}
}

static void
throwerror(char *str)
{
	s.val0 = consatom(String, 0, (uintptr)str);
	s.vals = empty;
	s.nval = 1;
	s.pc = error_l;
}

static void
aeval(void)
{
	Sexp *tmp;

	if(debug){
		Bprint(&pout, "evaluating: ");
		printsexp(s.exp, &pout);
		Bprint(&pout, "\n");
		Bflush(&pout);
	}

	int atype;
	if(s.exp == true || s.exp == false){
		s.val0 = s.exp;
		s.nval = 1;
		restore();
		return;
	}
	if(isatom(s.exp)){
		atype = gettype(s.exp);
		switch(atype){
		case Vector:
		case Number:
		case Character:
		case String:
		case Primitive:
			s.val0 = s.exp;
			s.nval = 1;
			restore();
			break;

		case Symbol:
			tmp = assq(s.exp, s.env);
			if(tmp != false){
				s.val0 = tmp->cdr->car;
				s.nval = 1;
				restore();
			}else
				throwerror(smprint("Symbol \"%s\" not bound",
				                   (char*)s.exp->cdr));
			break;

		default:
			throwerror(smprint("can't evaluate type %d", atype));
		}
	}else if(isatom(s.exp->car)){
		if(s.exp->car == lambda || s.exp->car == λ){
			s.val0 = cons(beta, cons(s.exp, cons(s.env, empty)));
			s.nval = 1;
			restore();
		}else if(gettype(s.exp->car) == Symbol){
			tmp = assq(s.exp->car, syntaxenv);
			if(tmp != false){
				s.pc = tmp->cdr->car;
			}else{
				s.evlis = empty;
				s.unevlis = s.exp;
				s.pc = evlis_l;
			}
		}else{
			throwerror(smprint("can't apply type %d",
			                   gettype(s.exp->car)));
		}
	}else{
		if(s.exp->car->car == lambda || s.exp->car->car == λ){
			s.evlis = cons(s.exp->car, empty);
			s.unevlis = s.exp->cdr;
			s.pc = evlis_l;
		}else{
			s.evlis = empty;
			s.unevlis = s.exp;
			s.pc = evlis_l;
		}
	}
}

static void
evlis(void)
{
	void (*f)(void);

	if(s.unevlis == empty){
		s.evlis = reverse(s.evlis);
		if(isatom(s.evlis->car)){
			if(gettype(s.evlis->car) != Primitive){
				throwerror(smprint("can't apply non-primitive atom"));
				return;
			}
			f = (void (*)(void)) s.evlis->car->cdr;
			s.exp = s.evlis->cdr;
			f();
			// restore in f() if necessary
		}else{
			if(s.evlis->car->car == lambda || s.evlis->car->car == λ){
				s.env = pairargs(s.evlis->car->cdr->car,
				                 s.evlis->cdr, s.env);
				s.exp = s.evlis->car->cdr->cdr;
				s.pc = evblock_l;
			}else if(s.evlis->car->car == beta){
				s.env = pairargs(s.evlis->car->cdr->car->cdr->car,
				                 s.evlis->cdr,
				                 s.evlis->car->cdr->cdr->car);
				s.exp = s.evlis->car->cdr->car->cdr->cdr;
				s.pc = evblock_l;
			}else if(s.evlis->car->car == delta){
				s.clink = s.evlis->car->cdr->car;
				restore();
			}else{
				throwerror(smprint("can't apply function"));
			}
		}
	}else{
		saveup(evlis1_l);
		s.exp = s.unevlis->car;
		s.pc = aeval_l;
	}
}

static void
evlis1(void)
{
	// TODO: check for nval > 0
	s.evlis = cons(s.val0, s.evlis);
	s.unevlis = s.unevlis->cdr;
	s.pc = evlis_l;
}

static void
evblock(void)
{
	if(s.exp->cdr != empty)
		saveup(evblock1_l);
	s.exp = s.exp->car;
	s.pc = aeval_l;
}

static void
evblock1(void)
{
	s.exp = s.exp->cdr;
	s.pc = evblock_l;
}

static void
list_prim(void)
{
	s.val0 = s.exp;
	s.nval = 1;
	restore();
}

static void
values_prim(void)
{
	s.nval = listlen(s.exp);
	s.val0 = s.exp->car;
	s.vals = s.exp->cdr;
	restore();
}

static void
cons_prim(void)
{
	s.val0 = cons(s.exp->car, s.exp->cdr->car);
	s.nval = 1;
	restore();
}

static void
car_prim(void)
{
	s.val0 = s.exp->car->car;
	s.nval = 1;
	restore();
}

static void
cdr_prim(void)
{
	s.val0 = s.exp->car->cdr;
	s.nval = 1;
	restore();
}

static void
setcar_prim(void)
{
	s.exp->car->car = s.exp->cdr->car;
	s.nval = 0;
	restore();
}

static void
setcdr_prim(void)
{
	s.exp->car->cdr = s.exp->cdr->car;
	s.nval = 0;
	restore();
}

static void
eq_prim(void)
{
	if(s.exp->car == s.exp->cdr->car)
		s.val0 = true;
	else
		s.val0 = false;
	s.nval = 1;
	restore();
}

static void
callcc_prim(void)
{
	Sexp *a;
	a = cons(delta, cons(s.clink, empty));
	pushroot(&a);
	s.evlis = cons(a, cons(s.exp->car, empty));
	poproot();
	s.unevlis = empty;
	s.pc = evlis_l;
}

static void
callwv_prim(void)
{
	saveup(callwv1_l);
	s.evlis = cons(s.exp->car, empty);
	s.unevlis = empty;
	s.pc = evlis_l;
}

static void
callwv1_prim(void)
{
	s.evlis = cons(s.val0, s.vals);
	s.evlis = cons(s.exp->cdr->car, s.evlis);
	s.evlis = reverse(s.evlis);
	s.unevlis = empty;
	s.pc = evlis_l;
}

static void
read_prim(void)
{
	s.val0 = readsexp(&pin);
	s.nval = 1;
	restore();
}

static void
print_prim(void)
{
	printsexp(s.exp->car, &pout);
	Bprint(&pout, "\n");
	Bflush(&pout);
	s.nval = 0;
	restore();
}

static void
pairargs_prim(void)
{
	s.val0 = pairargs(s.exp->car, s.exp->cdr->car, s.exp->cdr->cdr->car);
	s.nval = 1;
	restore();
}

static void
add_prim(void)
{
	s.val0 = addnums(s.exp);
	s.nval = 1;
	restore();
}

static void
mul_prim(void)
{
	s.val0 = mulnums(s.exp);
	s.nval = 1;
	restore();
}

static void
sub_prim(void)
{
	s.val0 = subnums(s.exp);
	s.nval = 1;
	restore();
}

static void
div_prim(void)
{
	s.val0 = divnums(s.exp);
	s.nval = 1;
	restore();
}

static void
eqnum_prim(void)
{
	s.val0 = testnums(s.exp, eq2nums);
	s.nval = 1;
	restore();
}

static void
ltnum_prim(void)
{
	s.val0 = testnums(s.exp, lt2nums);
	s.nval = 1;
	restore();
}

static void
gtnum_prim(void)
{
	s.val0 = testnums(s.exp, gt2nums);
	s.nval = 1;
	restore();
}

static void
lenum_prim(void)
{
	s.val0 = testnums(s.exp, le2nums);
	s.nval = 1;
	restore();
}

static void
genum_prim(void)
{
	s.val0 = testnums(s.exp, ge2nums);
	s.nval = 1;
	restore();
}

static void
quote_syntax(void)
{
	s.val0 = s.exp->cdr->car;
	s.nval = 1;
	restore();
}

static void
quasiquote_syntax(void)
{
	s.val0 = s.exp->cdr->car;
	s.nval = 1;
	restore();
}

static void
if_syntax(void)
{
	saveup(if1_l);
	s.exp = s.exp->cdr->car;
	s.pc = aeval_l;
}

static void
if1_syntax(void)
{
	// TODO: check for nval > 0
	if(s.val0 != false)
		s.exp = s.exp->cdr->cdr->car;
	else
		s.exp = s.exp->cdr->cdr->cdr->car;
	s.pc = aeval_l;
}

static void
and_syntax(void)
{
	s.exp = s.exp->cdr;
	if(s.exp == empty){
		s.val0 = true;
		s.nval = 1;
		restore();
		return;
	}
	saveup(and1_l);
	s.exp = s.exp->car;
	s.pc = aeval_l;
}

static void
and1_syntax(void)
{
	if(s.exp->cdr == empty || s.nval > 0 && s.val0 == false){
		restore();
		return;
	}
	s.exp = s.exp->cdr;
	saveup(and1_l);
	s.exp = s.exp->car;
	s.pc = aeval_l;
}

static void
or_syntax(void)
{
	s.exp = s.exp->cdr;
	if(s.exp == empty){
		s.val0 = false;
		s.nval = 1;
		restore();
		return;
	}
	saveup(or1_l);
	s.exp = s.exp->car;
	s.pc = aeval_l;
}

static void
or1_syntax(void)
{
	if(s.exp->cdr == empty || s.nval > 0 && s.val0 != false){
		restore();
		return;
	}
	s.exp = s.exp->cdr;
	saveup(or1_l);
	s.exp = s.exp->car;
	s.pc = aeval_l;
}

static void
set_syntax(void)
{
	saveup(set1_l);
	s.exp = s.exp->cdr->cdr->car;
	s.pc = aeval_l;
}

static void
set1_syntax(void)
{
	Sexp *a;
	a = assq(s.exp->cdr->car, s.env);
	// TODO: check for nval > 0
	if(a != false)
		a->cdr->car = s.val0;
	else
		defineglobal(s.exp->cdr->car, s.val0);
	s.nval = 0;
	restore();
}

static void
define_syntax(void)
{
//	if(s.env == globalenv)
//		print("define in global env\n");
	saveup(define1_l);
	s.exp = s.exp->cdr->cdr->car;
	s.pc = aeval_l;
}

static void
define1_syntax(void)
{
	// TODO: check for nval > 0
	defineglobal(s.exp->cdr->car, s.val0);
	s.nval = 0;
	restore();
}

static Sexp*
makeclosure(Sexp *e)
{
	return cons(e->car,
	       cons(cons(beta,
	            cons(e->cdr->car,
	            cons(empty, empty))), empty));
}

static void
letrec_syntax(void)
{
	Sexp *labs, *l;
	labs = mapcar(makeclosure, s.exp->cdr->car);
	for(l = labs; l; l = l->cdr){
		l->car->cdr->car->cdr->cdr->car = labs;
		if(l->cdr == empty){
			l->cdr = s.env;
			break;
		}
	}
	s.env = labs;
	s.exp = s.exp->cdr->cdr;
	s.pc = evblock_l;
}

static void
begin_syntax(void)
{
	s.exp = s.exp->cdr;
	evblock();
}

static void
error_f(void)
{
	s.clink = empty;
	s.pc = nil_l;
}

int
eval(Sexp *e, Sexp **ret)
{
	void (*f)(void);

	pushroot(&e);
	saveup(nil_l);
	s.exp = e;
	poproot();
	s.pc = aeval_l;
	while(s.pc != nil_l){
		f = (void (*)(void))s.pc->cdr;
		f();
	}
	if(s.nval == 0)
		*ret = nil;
	else if(s.nval == 1)
		*ret = cons(s.val0, empty);
	else
		*ret = cons(s.val0, s.vals);
	return s.nval;
}

void
initinterp(void)
{
	Sexp *key, *val;
	Sexp *a, *b;
	struct Binding {
		char *name;
		void *func;
	};
	struct Label {
		Sexp **l;
		void *func;
	};
	struct Binding init_prim[] = {
		{"list", list_prim},
		{"values", values_prim},
		{"cons", cons_prim},
		{"car", car_prim},
		{"cdr", cdr_prim},
		{"set-car!", setcar_prim},
		{"set-cdr!", setcdr_prim},
		{"eq?", eq_prim},
		{"call/cc", callcc_prim},
		{"call-with-values", callwv_prim},
		{"read", read_prim},
		{"print", print_prim},
		{"pairargs", pairargs_prim},
		{"+", add_prim},
		{"*", mul_prim},
		{"-", sub_prim},
		{"/", div_prim},
		{"=", eqnum_prim},
		{"<", ltnum_prim},
		{">", gtnum_prim},
		{"<=", lenum_prim},
		{">=", genum_prim},
		{nil, nil}
	};
	struct Binding init_syn[] = {
		{"quote", quote_syntax},
		{"quasiquote", quasiquote_syntax},
		{"if", if_syntax},
		{"and", and_syntax},
		{"or", or_syntax},
		{"set!", set_syntax},
		{"define", define_syntax},
		{"letrec", letrec_syntax},
		{"begin", begin_syntax},
		{nil, nil}
	};
	struct Binding *bp;
	struct Label labs[] = {
		{&aeval_l, aeval},
		{&evlis_l, evlis},
		{&evlis1_l, evlis1},
		{&evblock_l, evblock},
		{&evblock1_l, evblock1},
		{&callwv1_l, callwv1_prim},
		{&if1_l, if1_syntax},
		{&and1_l, and1_syntax},
		{&or1_l, or1_syntax},
		{&set1_l, set1_syntax},
		{&define1_l, define1_syntax},
		{&nil_l, nil},
		{&error_l, error_f},
		{nil, nil}
	};
	struct Label *lp;

	s.exp = s.env = s.clink = s.pc = s.unevlis = s.evlis = empty;
	s.val0 = s.vals = empty;
	s.nval = 0;
	pushroot(&s.exp);
	pushroot(&s.env);
	pushroot(&s.clink);
	pushroot(&s.pc);
	pushroot(&s.unevlis);
	pushroot(&s.evlis);
	pushroot(&s.val0);
	pushroot(&s.vals);

	labellist = globalenv = syntaxenv = empty;
	pushroot(&labellist);
	pushroot(&globalenv);
	pushroot(&syntaxenv);

	for(lp = &labs[0]; lp->l; lp++)
		labellist = cons(*lp->l = makelabel(lp->func), labellist);

	a = b = empty;
	pushroot(&a);
	pushroot(&b);
	for(bp = &init_prim[0]; bp->name; bp++){
		key = getsymbol(bp->name);
		a = cons(key, a);
		val = consatom(Primitive, 0, (uintptr)bp->func);
		b = cons(val, b);
	}
	s.env = globalenv = pairargs(a, b, empty);

	a = b = empty;
	for(bp = &init_syn[0]; bp->name; bp++){
		key = getsymbol(bp->name);
		a = cons(key, a);
		val = makelabel(bp->func);
		b = cons(val, b);
	}
	syntaxenv = pairargs(a, b, empty);

	poproot();
	poproot();

	if(debug){
		printsexp(s.env, &pout);
		Bprint(&pout, "\n");
		Bflush(&pout);
		printsexp(syntaxenv, &pout);
		Bprint(&pout, "\n");
		Bflush(&pout);
	}
}
