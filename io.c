#include "scheme.h"

static vlong peekbuf[10];
static int peekind;
static Rune stringbuf[512];
static int bufind;

#define EOF ~0

static int
isdelim(Rune r)
{
	return isspacerune(r) || runestrchr(L"()\"", r);
}

/* For ALL Biobufs */
static void
ungetrune(Rune r)
{
	peekbuf[peekind++] = r;
}

static Rune
nextrunesp(Biobuf *file)
{
	Rune r;
	if(peekind == 0){
		vlong x;
		x = Bgetrune(file);
		r = x;
		if(x < 0)
			r = EOF;
	}else
		r = peekbuf[--peekind];
	if(r == L';')
		while(r = Bgetrune(file), r != L'\n');
	if(isspacerune(r))
		r = L' ';
	return r;
}

static Rune
nextrune(Biobuf *file)
{
	Rune r;
	while(r = nextrunesp(file), r == L' ');
	return r;
}

static Sexp*
readlist(Biobuf *file)
{
	Rune r;
	Sexp *first, *second;

	if(r = nextrune(file), r == L')')
		return empty;
	ungetrune(r);

	first = readsexp(file);
	pushroot(&first);

	if(r = nextrune(file), r == L'.'){
		// TODO: expect delimiter
		second = readsexp(file);
		poproot();
		if(second == nil){
			Bprint(&perr, "error: invalid cdr\n");
			return nil;
		}
		if(r = nextrune(file), r != L')'){
			Bprint(&perr, "error: ')' expected\n");
			return empty;
		}
		return cons(first, second);
	}else{
		ungetrune(r);
		second = readlist(file);
		poproot();
		return cons(first, second);
	}
}

static Sexp*
readproperlist(Biobuf *file)
{
	Rune r;
	Sexp *first, *second;

	if(r = nextrune(file), r == L')')
		return empty;
	ungetrune(r);
	first = readsexp(file);
	pushroot(&first);
	second = readproperlist(file);
	poproot();
	return cons(first, second);
}

static int
getdigit(Rune r, int base)
{
	int d;
	if(r >= L'0' && r <= L'9')
		d = r-L'0';
	else if(r >= L'A' && r <= L'F')
		d = r-L'A' + 10;
	else if(r >= L'a' && r <= L'f')
		d = r-L'a' + 10;
	else
		return -1;
	if(d >= base)
		return -1;
	return d;
}

/* TODO */
static Sexp*
readnumber(Biobuf *file)
{
	Sexp *e;
	Rune r, *sp;
	char *s;
	int d, i;
	double f, div;
	int exp, esign;
	int exactness;
	int base;
	int canprefix, signstate, digitstate;
	int sign;
	int numtype;

	bufind = 0;
	while(r = nextrunesp(file), !isdelim(r))
		stringbuf[bufind++] = r;
	ungetrune(r);
	stringbuf[bufind++] = 0;

	canprefix = 1;
	signstate = 0;	/* 0 can't read sign
	                   1 can read sign
	                   2 can read esign */
	digitstate = 0; /* 0 reading integer part
	                   1 reading fractional part
	                   2 reading exponent */
	base = 10;
	exactness = 0;

	sign = 1;
	i = 0;   /* integer part */
	f = 0.0; /* fractional part */
	div = base;
	exp = 0;
	esign = 1;
	numtype = Integer;
	for(sp = stringbuf; *sp; sp++){
		if(canprefix && *sp == L'#'){
			sp++;
			switch(*sp){
			case L'b':
				div = base = 2;
				break;
			case L'o':
				div = base = 8;
				break;
			case L'd':
				div = base = 10;
				break;
			case L'x':
				div = base = 16;
				break;
			case L'i':
				exactness = Inexact;
				break;
			case L'e':
				exactness = 0;
				break;
			default:
				goto symbol;
			}
			continue;
		}
		canprefix = 0;

		if(signstate && (*sp == '-' || *sp == '+')){
			if(signstate == 1)
				sign = (*sp == '-') ? -1 : 1;
			else
				esign = (*sp == '-') ? -1 : 1;
			signstate = 0;
			continue;
		}
		signstate = 0;

		if((d = getdigit(*sp, base)) >= 0){
			if(digitstate == 0)
				i = i*base + d;
			else if(digitstate == 1){
				f += d/div;
				div *= base;
			}else
				exp = exp*base + d;
			continue;
		}

		if(*sp == L'.'){
			if(digitstate == 1)
				goto symbol;
			digitstate = 1;
			numtype = Real;
			continue;
		}

		if(*sp == L'e'){
			numtype = Real;
			signstate = digitstate = 2;
			continue;
		}

		goto symbol;
	}

	i *= sign;
	if(numtype == Integer){
		return makeinteger(i);
	}else{
		f += i;
		if(esign < 0)
			while(exp--)
				f /= 10.0f;	// TODO: or base?
		else
			while(exp--)
				f *= 10.0f;
		return makereal(f);
	}

symbol:
	s = smprint("%S", stringbuf);
	e = getsymbol(s);
	free(s);
	return e;
}

static Sexp*
readsymbol(Biobuf *file)
{
	Rune r;
	char *s;
	Sexp *e;

	bufind = 0;
	while(r = nextrunesp(file), !isdelim(r))
		stringbuf[bufind++] = r;
	ungetrune(r);
	stringbuf[bufind++] = 0;
	s = smprint("%S", stringbuf);
	e = getsymbol(s);
	free(s);
	return e;
}

Sexp*
readsexp(Biobuf *file)
{
	Rune r, rr;
	Sexp *lst, *vec;
	int len, i;

	if(r = nextrune(file), r == EOF)
		return nil;

	switch(r){
	/* List */
	case L'(':
		return readlist(file);

	case L')':
		Bprint(&perr, "error: s-expression can't start with ')'\n");
		Bflush(&perr);
		return nil;

	/* String */
	case L'"':
		bufind = 0;
		if(peekind != 0){
			Bprint(&perr, "peekind shouldn't be 0 when reading string\n");			Bflush(&perr);
		}
		while(r = Bgetrune(file), r != L'"'){
			if(r == L'\\'){
				r = Bgetrune(file);
				switch(r){
				case L'n':
					r = '\n';
					break;
				case L't':
					r = '\t';
					break;
				default:
					break;
				}
			}
			stringbuf[bufind++] = r;
		}
		stringbuf[bufind++] = 0;
		return consatom(String, 0, (uintptr)smprint("%S", stringbuf));

	case L'#':
		r = nextrunesp(file);
		/* Booleans */
		if(r == L'f')
			return false;
		if(r == L't')
			return true;
		/* Vector */
		if(r == L'('){
			lst = readproperlist(file);
			pushroot(&lst);
			len = listlen(lst);
			vec = makevector(len, empty);
			for(i = 0; i < len; i++, lst = lst->cdr)
				vectorset(vec, i, lst->car);
			poproot();
			return vec;
		}
		/* Character */
		if(r == L'\\'){
			bufind = 0;
			while(r = nextrunesp(file), !isdelim(r))
				stringbuf[bufind++] = r;
			ungetrune(r);
			stringbuf[bufind++] = 0;
			if(bufind == 2)
				return consatom(Character, 0, (uintptr)stringbuf[0]);
			if(runestrcmp(stringbuf, L"newline") == 0)
				return consatom(Character, 0, (uintptr)L'\n');
			if(runestrcmp(stringbuf, L"space") == 0)
				return consatom(Character, 0, (uintptr)L' ');
			Bprint(&perr, "unknown character: %S\n", stringbuf);
			Bflush(&perr);
			return nil;
		}
		if(runestrchr(L"iebodx", r)){
			ungetrune(r);
			ungetrune(L'#');
			return readnumber(file);
		}
		Bprint(&perr, "can't read after hash\n");
		Bflush(&perr);
		return nil;

	case L'.':
		r = nextrunesp(file);
		if(runestrchr(L"0123456789", r)){
			ungetrune(r);
			ungetrune(L'.');
			return readnumber(file);
		}
		if(r == ' '){
			Bprint(&perr, "error: dot not allowed here\n");
			Bflush(&perr);
			return nil;
		}
		ungetrune(r);
		ungetrune(L'.');
		return readsymbol(file);

	case L'+':
	case L'-':
		rr = r;
		r = nextrunesp(file);
		if(runestrchr(L"i0123456789abcdefABCDEF", r)){
			ungetrune(r);
			ungetrune(rr);
			/* can also return symbol */
			return readnumber(file);
		}
		ungetrune(r);
		ungetrune(rr);
		return readsymbol(file);

	case L'\'':
		return cons(quote, cons(readsexp(file), empty));

	case L'`':
		return cons(quasiquote, cons(readsexp(file), empty));

	case L',':
		r = nextrunesp(file);
		if(r == L'@')
			return cons(unquotesplicing, cons(readsexp(file), empty));
		ungetrune(r);
		return cons(unquote, cons(readsexp(file), empty));

	default:
		ungetrune(r);
		if(runestrchr(L"0123456789", r))
			return readnumber(file);
		return readsymbol(file);
	}
}

void
printsexpnl(Sexp *e, Biobuf *file)
{
	printsexp(e, file);
	Bprint(file, "\n");
	Bflush(file);
}

void
printsexp(Sexp *o, Biobuf *file)
{
	int len, i;
	Rune r;

	if(ispair(o)){
		/* HACK: */
		if(o->car == beta){
			Bprint(file, "β-closure");
			return;
		}
		Bprint(file, "(");
		for(; o != empty && ispair(o); o = o->cdr){
			printsexp(o->car, file);
			if(o->cdr != empty){
				Bprint(file, " ");
				if(!ispair(o->cdr)){
					Bprint(file, ". ");
					printsexp(o->cdr, file);
				}
			}
		}
		Bprint(file, ")");
	}else switch(gettype(o)){
	case Symbol:
		Bprint(file, "%s", (char*)o->cdr);
		break;
	case Vector:
		Bprint(file, "#(");
		len = vectorlen(o);
		for(i = 0; i < len; i++){
			printsexp(vectorref(o, i), file);
			if(i+1 != len)
				Bprint(file, " ");
		}
		Bprint(file, ")");
		break;
/*
	case Lambda:
		Bprint(file, "λ");
		break;
*/
	case Primitive:
		Bprint(file, "#<primitive %p>", o->cdr);
		break;
	case Number:
		if((getnumtype(o) & 0x3) == Integer)
			Bprint(file, "%d", o->integ);
		else
			Bprint(file, "%f", o->real);
		break;
	case Character:
		r = (Rune)(uintptr)o->cdr;
		if(r == L'\n')
			Bprint(file, "#\\newline");
		else if(r == L' ')
			Bprint(file, "#\\space");
		else
			Bprint(file, "#\\%C", r);
		break;
	case String:
		Bprint(file, "\"%s\"", (char*)o->cdr);
		break;
	case Port:
		Bprint(file, "port");
		break;
	case Special:
		if(o == empty)
			Bprint(file, "()");
		else if(o == true)
			Bprint(file, "#t");
		else if(o == false)
			Bprint(file, "#f");
		else
			Bprint(file, "special");
		break;
	case Free:
		Bprint(file, "free");
		break;
	}
}
