#include <u.h>
#include <libc.h>
#include <bio.h>

extern int debug;

/* These must not be larger than a pointer */
typedef int fixnum;
typedef float flonum;

typedef struct Sexp Sexp;
struct Sexp
{
	union {
		Sexp *car;
		uintptr cari;
	};
	union {
		Sexp *cdr;
		uintptr cdri;
		fixnum integ;
		flonum real;
	};
};

/* type */
enum
{
	Symbol = 0,
	Vector,
//	Lambda,
	Primitive,
	Number,
	Character,
	String,
	Port,
	Special,
	Free,
};

enum
{
	Integer = 0,
	Rational,
	Real,
	Complex,

	Inexact = 0x4
};

extern Biobuf pin, pout, perr;
extern Sexp *empty, *true, *false;
extern Sexp *quote, *quasiquote, *unquote, *unquotesplicing,
            *λ, *lambda, *beta, *delta;

extern int gcenable;
Sexp *alloccell(void);
void pushroot(Sexp **e);
void poproot(void);
void initmem(void);
int issafe(Sexp *e);
void printroots(void);

Sexp *getsymbol(char *s);
Sexp *cons(Sexp *a, Sexp *b);
Sexp *copycons(Sexp *a);
Sexp *consatom(int type, uintptr a, uintptr b);
Sexp *makevector(int len, Sexp *init);
void vectorset(Sexp *vec, int i, Sexp *o);
Sexp *vectorref(Sexp *vec, int i);
int vectorlen(Sexp *vec);
int isatom(Sexp *e);
int ispair(Sexp *e);
int gettype(Sexp *e);

int listlen(Sexp *e);
Sexp *assq(Sexp *obj, Sexp *alist);
Sexp *pairlis(Sexp *keys, Sexp *values, Sexp *alist);
Sexp *pairargs(Sexp *vars, Sexp *values, Sexp *env);
Sexp *reverse(Sexp *lst);
Sexp *mapcar(Sexp *(*f)(Sexp *), Sexp *lst);

int getnumtype(Sexp *e);
Sexp *makeinteger(fixnum i);
Sexp *makereal(flonum f);
Sexp *eq2nums(Sexp *a, Sexp *b);
Sexp *lt2nums(Sexp *a, Sexp *b);
Sexp *gt2nums(Sexp *a, Sexp *b);
Sexp *le2nums(Sexp *a, Sexp *b);
Sexp *ge2nums(Sexp *a, Sexp *b);
Sexp *testnums(Sexp *lst, Sexp *(*f)(Sexp*, Sexp*));
Sexp *addnums(Sexp *lst);
Sexp *mulnums(Sexp *lst);
Sexp *subnums(Sexp *lst);
Sexp *divnums(Sexp *lst);

Sexp *readsexp(Biobuf *file);
void printsexp(Sexp *e, Biobuf *file);
void printsexpnl(Sexp *e, Biobuf *file);

int eval(Sexp *e, Sexp **ret);
void initinterp(void);

typedef struct State State;
struct State
{
	Sexp *exp;
	Sexp *env;
	Sexp *clink;
	Sexp *pc;
	Sexp *unevlis, *evlis;

	/* not saved */
	int nval;
	Sexp *val0, *vals;
};

extern State s;
extern Sexp *globalenv, *syntaxenv;
