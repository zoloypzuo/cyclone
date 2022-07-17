%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\swig.swg" %beginfile
/* -----------------------------------------------------------------------------
 * swig.swg
 *
 * Common macro definitions for various SWIG directives.  This file is always 
 * included at the top of each input file.
 * ----------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * User Directives 
 * ----------------------------------------------------------------------------- */

/* Deprecated SWIG-1.1 directives */













/* Code insertion directives such as %wrapper %{ ... %} */







/* Class extension */



/* %ignore directive */




/* Access control directives */









/* Generation of default constructors/destructors (old form, don't use) */





/* Disable the generation of implicit default constructor */




/* Disable the generation of implicit default destructor (dangerous) */




/* Enable the generation of copy constructor */




/* Force the old nodefault behavior, ie disable both constructor and destructor */




/* the %exception directive */








/* the %allowexception directive allows the %exception feature to
   be applied to set/get variable methods */




/* the %exceptionvar directive, as %exception but it is only applied
   to set/get variable methods. You don't need to use the
   %allowexception directive when using %exceptionvar.
*/








/* the %catches directive */



/* the %exceptionclass directive */




/* the %newobject directive */




/* the %delobject directive */




/* the %refobject/%unrefobject directives */








/* Directives for callback functions (experimental) */




/* the %nestedworkaround directive (deprecated) */




/* the %flatnested directive */




/* the %fastdispatch directive */




/* directors directives */




/* naturalvar directives */




/* nspace directives */




/* valuewrapper directives */





/* Contract support - Experimental and undocumented */




/* Macro for setting a dynamic cast function */






/* aggregation support */
/*
  This macro performs constant aggregation.  Basically the idea of
  constant aggregation is that you can group a collection of constants
  together.  For example, suppose you have some code like this:

       #define UP  1
       #define DOWN 2
       #define LEFT 3
       #define RIGHT 4

  Now, suppose you had a function like this:

       int move(int direction)

  In this case, you might want to restrict the direction argument to
  one of the supplied constant names. To do this, you could write some
  typemap code by hand.  Alternatively, you can use the
  %aggregate_check macro defined here to create a simple check
  function for you.  Here is an example:

    %aggregate_check(int, check_direction, UP, DOWN, LEFT, RIGHT);

  Now, using a typemap

    %typemap(check) int direction {
      if (!check_direction($1)) SWIG_exception(SWIG_ValueError,"Bad direction.");
    }

  or a contract (better)

    %contract move(int x) {
    require:
        check_direction(x);
    }

*/
   















/* -----------------------------------------------------------------------------
 * %rename predicates
 * ----------------------------------------------------------------------------- */
/* 
   Predicates to be used with %rename, for example:

   - to rename all the functions:

     %rename("%(utitle)s", %$isfunction) "";

   - to rename only the member methods:

     %rename("m_%(utitle)s", %$isfunction, %$ismember) "";

   - to rename only the global functions:

      %rename("m_%(utitle)s", %$isfunction, %$not %$ismember) "";

     or

      %rename("g_%(utitle)s", %$isfunction, %$isglobal) "";

   - to ignore the enumitems in a given class:

     %rename("$ignore", %$isenumitem, %$classname="MyClass") "";

   we use the prefix '%$' to avoid clashes with other swig
   macros/directives.

*/

 









  /* %constant definition */































/* -----------------------------------------------------------------------------
 * Common includes for warning labels, macros, fragments etc
 * ----------------------------------------------------------------------------- */

%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\swigwarnings.swg" %beginfile
/*
  Include the internal swig macro codes. These macros correspond to
  the one found in Source/Include/swigwarn.h plus the 'SWIG' prefix.
  
  For example, in the include file 'swigwarn.h' you will find

    #define WARN_TYPEMAP_CHARLEAK ...

  and in the 'swigwarn.swg' interface, you will see

    %define SWIGWARN_TYPEMAP_CHARLEAK ...

  This code can be used in warning filters as follows:

    %warnfilter(SWIGWARN_TYPEMAP_CHARLEAK);

  Warnings messages used in typemaps. Message names will be the same
  as those in Lib/swigwarn.swg but with the suffix _MSG.
   
  For example, for the code SWIGWARN_TYPEMAP_CHARLEAK, once you use

    %typemapmsg(CHARLEAK,<msg>);

  you use the message in your typemap as

    %typemap(varin,warning=SWIGWARN_TYPEMAP_CHARLEAK_MSG) char * 

  while you suppress the warning using

    %warnfilter(SWIGWARN_TYPEMAP_CHARLEAK);

  as described above.
*/

/* -----------------------------------------------------------------------------
 * SWIG warning codes
 * ----------------------------------------------------------------------------- */

%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\swigwarn.swg" %beginfile
/* SWIG warning codes - generated from swigwarn.h - do not edit */




/* -- Deprecated features -- */




























/* -- Preprocessor -- */








/* -- C/C++ Parser -- */





























  /* redundant now */
  /* redundant now */


	/* new */
	/* delete */
	/* + */
	/* - */
	/* * */
	/* / */
	/* % */
	/* ^ */
	/* & */
	/* | */
	/* ~ */
	/* ! */
	/* = */
	/* < */
	/* > */
	/* += */
	/* -= */
	/* *= */
	/* /= */
	/* %= */
	/* ^= */
	/* &= */
	/* |= */
	/* << */
	/* >> */
	/* <<= */
	/* >>= */
	/* == */
	/* != */
	/* <= */
	/* >= */
	/* && */
	/* || */
	/* ++ */
	/* -- */
	/* , */
	/* ->* */
	/* -> */
	/* () */
	/* [] */
	/* + */
	/* - */
	/* * */
	/* & */
	/* new [] */
	/* delete [] */
	/* operator *() */

/* 394-399 are reserved */

/* -- Type system and typemaps -- */























	/* mostly used in directorout typemaps */








/* -- Fragments -- */


/* -- General code generation -- */



























/* -- Doxygen comments -- */









/* -- Reserved (600-799) -- */

/* -- Language module specific warnings (700 - 899) -- */

















/* please leave 700-719 free for D */



/* please leave 720-739 free for Scilab */



/* please leave 740-759 free for Python */




/* please leave 800-809 free for Ruby */



















/* please leave 810-829 free for Java */



















/* please leave 830-849 free for C# */













/* please leave 850-869 free for Modula 3 */





/* please leave 870-889 free for PHP */



/* please leave 890-899 free for Go */

/* -- User defined warnings (900 - 999) -- */

%endoffile

/* -----------------------------------------------------------------------------
 * Auxiliary macros
 * ----------------------------------------------------------------------------- */

/* Macro to define warning messages */



/* -----------------------------------------------------------------------------
 *  Typemap related warning messages
 * ----------------------------------------------------------------------------- */







/* -----------------------------------------------------------------------------
 * Operator related warning messages 
 * ----------------------------------------------------------------------------- */



















































/* -----------------------------------------------------------------------------
 * Macros for keyword and built-in names 
 * ----------------------------------------------------------------------------- */





/* -----------------------------------------------------------------------------
 * Warning filter feature
 * ----------------------------------------------------------------------------- */






%endoffile
%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\swigfragments.swg" %beginfile
/* -----------------------------------------------------------------------------
 * swigfragments.swg
 *
 * Common fragments
 * ----------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * Fragments for C header files
 * ----------------------------------------------------------------------------- */

%fragment("<float.h>", "header") %{
#include <float.h>
%}

/* Default compiler options for gcc allow long_long but not LLONG_MAX. 
 * Define SWIG_NO_LLONG_MAX if this added limits support is not wanted. */
%fragment("<limits.h>", "header") %{
#include <limits.h>
#if !defined(SWIG_NO_LLONG_MAX)
# if !defined(LLONG_MAX) && defined(__GNUC__) && defined (__LONG_LONG_MAX__)
#   define LLONG_MAX __LONG_LONG_MAX__
#   define LLONG_MIN (-LLONG_MAX - 1LL)
#   define ULLONG_MAX (LLONG_MAX * 2ULL + 1ULL)
# endif
#endif
%}

%fragment("<math.h>", "header") %{
#include <math.h>
%}

%fragment("<stddef.h>", "header") %{
#include <stddef.h>
%}

%fragment("<stdio.h>", "header") %{
#include <stdio.h>
#if (defined(_MSC_VER) && (_MSC_VER < 1900)) || defined(__BORLANDC__) || defined(_WATCOM)
# ifndef snprintf
#  define snprintf _snprintf
# endif
#endif
%}

%fragment("<stdlib.h>", "header") %{
#include <stdlib.h>
#ifdef _MSC_VER
# ifndef strtoull
#  define strtoull _strtoui64
# endif
# ifndef strtoll
#  define strtoll _strtoi64
# endif
#endif
%}

%fragment("<wchar.h>", "header") %{
#include <wchar.h>
#include <limits.h>
#ifndef WCHAR_MIN
#  define WCHAR_MIN 0
#endif
#ifndef WCHAR_MAX
#  define WCHAR_MAX 65535
#endif
%}

/* -----------------------------------------------------------------------------
 * Fragments for C++ header files
 * ----------------------------------------------------------------------------- */

%fragment("<algorithm>", "header") %{
#include <algorithm>
%}

%fragment("<stdexcept>", "header") %{
#include <stdexcept>
%}

%fragment("<string>", "header") %{
#include <string>
%}

%fragment("<memory>", "header") %{
#include <memory>
%}
%endoffile

/* -----------------------------------------------------------------------------
 * Overloading support
 * ----------------------------------------------------------------------------- */

/*
 * Function/method overloading support.   This is done through typemaps,
 * but also involves a precedence level.
 */

/* Macro for overload resolution */



/* Macros for precedence levels */





































































/* -----------------------------------------------------------------------------
 * Default handling of certain overloaded operators 
 * ----------------------------------------------------------------------------- */


%rename("$ignore:"        "350:operator new ignored"  )     operator new;
%rename("$ignore:"     "351:operator delete ignored"  )  operator delete;
%rename("$ignore:"     "394:operator new[] ignored"  )  operator new[];
%rename("$ignore:"     "395:operator delete[] ignored"  )  operator delete[];

/* add C++ operator aliases */
%rename("operator &&") operator and;    // `and'    `&&'
%rename("operator ||") operator or;     // `or'     `||'
%rename("operator !")  operator not;    // `not'     `!'
%rename("operator &=") operator and_eq; // `and_eq'  `&='
%rename("operator &")  operator bitand; // `bitand'  `&'
%rename("operator |")  operator bitor;  // `bitor'   `|'
%rename("operator ~")  operator compl;  // `compl'   `~'
%rename("operator !=") operator not_eq; // `not_eq'  `!='
%rename("operator |=") operator or_eq;  // `or_eq'   `|='
%rename("operator ^")  operator xor;    // `xor'     `^'
%rename("operator ^=") operator xor_eq; // `xor_eq'  `^='

/* Smart pointer handling */

%rename(__deref__) *::operator->;
%rename(__ref__)   *::operator*();
%rename(__ref__)   *::operator*() const;

/* Define std namespace */
namespace std {
  /* Warn about std::initializer_list usage. The constructor/method where used should probably be ignored. See docs. */
  template<typename T> class initializer_list {};
  %typemap(in, warning="476:Initialization using std::initializer_list." ) initializer_list<T> ""
  %typemap(typecheck, precedence=      0     ) initializer_list<T> ""
}


/* -----------------------------------------------------------------------------
 * Default char * and C array typemaps
 * ----------------------------------------------------------------------------- */

/* Set up the typemap for handling new return strings */


%typemap(newfree) char * "delete [] $1;";




/* Default typemap for handling char * members */


%typemap(memberin) char * {
  delete [] $1;
  if ($input) {
     $1 = ($1_type) (new char[strlen((const char *)$input)+1]);
     strcpy((char *)$1, (const char *)$input);
  } else {
     $1 = 0;
  }
}
%typemap(memberin,warning=        "451:Setting a const char * variable may leak memory." ) const char * {
  if ($input) {
     $1 = ($1_type) (new char[strlen((const char *)$input)+1]);
     strcpy((char *)$1, (const char *)$input);
  } else {
     $1 = 0;
  }
}
%typemap(globalin) char * {
  delete [] $1;
  if ($input) {
     $1 = ($1_type) (new char[strlen((const char *)$input)+1]);
     strcpy((char *)$1, (const char *)$input);
  } else {
     $1 = 0;
  }
}
%typemap(globalin,warning=        "451:Setting a const char * variable may leak memory." ) const char * {
  if ($input) {
     $1 = ($1_type) (new char[strlen((const char *)$input)+1]);
     strcpy((char *)$1, (const char *)$input);
  } else {
     $1 = 0;
  }
}






































/* Character array handling */

%typemap(memberin) char [ANY] {
  if($input) {
    strncpy((char*)$1, (const char *)$input, $1_dim0-1);
    $1[$1_dim0-1] = 0;
  } else {
    $1[0] = 0;
  }
}

%typemap(globalin) char [ANY] {
  if($input) {
    strncpy((char*)$1, (const char *)$input, $1_dim0-1);
    $1[$1_dim0-1] = 0;
  } else {
    $1[0] = 0;
  }
}

%typemap(memberin) char [] {
  if ($input) strcpy((char *)$1, (const char *)$input);
  else $1[0] = 0;
}

%typemap(globalin) char [] {
  if ($input) strcpy((char *)$1, (const char *)$input);
  else $1[0] = 0;
}

/* memberin/globalin typemap for arrays. */

%typemap(memberin) SWIGTYPE [ANY] {
  size_t ii;
  $1_basetype *b = ($1_basetype *) $1;
  for (ii = 0; ii < (size_t)$1_size; ii++) b[ii] = *(($1_basetype *) $input + ii);
}

%typemap(globalin) SWIGTYPE [ANY] {
  size_t ii;
  $1_basetype *b = ($1_basetype *) $1;
  for (ii = 0; ii < (size_t)$1_size; ii++) b[ii] = *(($1_basetype *) $input + ii);
}

/* memberin/globalin typemap for double arrays. */

%typemap(memberin) SWIGTYPE [ANY][ANY] {
  $basetype (*inp)[$1_dim1] = ($basetype (*)[$1_dim1])($input);
  $basetype (*dest)[$1_dim1] = ($basetype (*)[$1_dim1])($1);
  size_t ii = 0;
  for (; ii < $1_dim0; ++ii) {
    $basetype *ip = inp[ii];
    $basetype *dp = dest[ii];
    size_t jj = 0;
    for (; jj < $1_dim1; ++jj) dp[jj] = ip[jj];
  }
}

%typemap(globalin) SWIGTYPE [ANY][ANY] {
  $basetype (*inp)[$1_dim1] = ($basetype (*)[$1_dim1])($input);
  $basetype (*dest)[$1_dim1] = ($basetype (*)[$1_dim1])($1);
  size_t ii = 0;
  for (; ii < $1_dim0; ++ii) {
    $basetype *ip = inp[ii];
    $basetype *dp = dest[ii];
    size_t jj = 0;
    for (; jj < $1_dim1; ++jj) dp[jj] = ip[jj];
  }
}

/* -----------------------------------------------------------------------------
 *  Runtime code
 * ----------------------------------------------------------------------------- */

/*  The SwigValueWrapper class  */

/*  
 * This template wrapper is used to handle C++ objects that are passed or 
 * returned by value.   This is necessary to handle objects that define
 * no default-constructor (making it difficult for SWIG to properly declare
 * local variables).
 *
 * The wrapper is used as follows.  First consider a function like this:
 *
 *      Vector cross_product(Vector a, Vector b)
 *
 * Now, if Vector is defined as a C++ class with no default constructor, 
 * code is generated as follows:
 *
 *     Vector *wrap_cross_product(Vector *inarg1, Vector *inarg2) {
 *          SwigValueWrapper<Vector>  arg1;
 *          SwigValueWrapper<Vector>  arg2;
 *          SwigValueWrapper<Vector> result;
 *
 *          arg1 = *inarg1;
 *          arg2 = *inarg2;
 *          ...            
 *          result = cross_product(arg1,arg2);
 *          ...
 *          return new Vector(result);
 *    }
 *         
 * In the wrappers, the template SwigValueWrapper simply provides a thin
 * layer around a Vector *.  However, it does this in a way that allows
 * the object to be bound after the variable declaration (which is not possible
 * with the bare object when it lacks a default constructor).  
 *
 * An observant reader will notice that the code after the variable declarations
 * is *identical* to the code used for classes that do define default constructors.
 * Thus, this neat trick allows us to fix this special case without having to
 * make massive changes to typemaps and other parts of the SWIG code generator.
 *
 * Note: this code is not included when SWIG runs in C-mode, when classes
 * define default constructors, or when pointers and references are used.
 * SWIG tries to avoid doing this except in very special circumstances.
 *
 * Note: This solution suffers from making a large number of copies
 * of the underlying object.  However, this is needed in the interest of
 * safety and in order to cover all of the possible ways in which a value
 * might be assigned.  For example:
 *
 *       arg1 = *inarg1;       // Assignment from a pointer
 *       arg1 = Vector(1,2,3); // Assignment from a value  
 *
 * The class offers a strong guarantee of exception safety.
 * With regards to the implementation, the private SwigMovePointer nested class is 
 * a simple smart pointer with move semantics, much like std::auto_ptr.
 *
 * This wrapping technique was suggested by William Fulton and is henceforth
 * known as the "Fulton Transform" :-).
 */


%insert("runtime") %{
#ifdef __cplusplus
/* SwigValueWrapper is described in swig.swg */
template<typename T> class SwigValueWrapper {
  struct SwigMovePointer {
    T *ptr;
    SwigMovePointer(T *p) : ptr(p) { }
    ~SwigMovePointer() { delete ptr; }
    SwigMovePointer& operator=(SwigMovePointer& rhs) { T* oldptr = ptr; ptr = 0; delete oldptr; ptr = rhs.ptr; rhs.ptr = 0; return *this; }
  } pointer;
  SwigValueWrapper& operator=(const SwigValueWrapper<T>& rhs);
  SwigValueWrapper(const SwigValueWrapper<T>& rhs);
public:
  SwigValueWrapper() : pointer(0) { }
  SwigValueWrapper& operator=(const T& t) { SwigMovePointer tmp(new T(t)); pointer = tmp; return *this; }
  operator T&() const { return *pointer.ptr; }
  T *operator&() { return pointer.ptr; }
};%}

/*
 * SwigValueInit() is a generic initialisation solution as the following approach:
 * 
 *       T c_result = T();
 * 
 * doesn't compile for all types for example:
 * 
 *       unsigned int c_result = unsigned int();
 */
%insert("runtime") %{
template <typename T> T SwigValueInit() {
  return T();
}
#endif
%}


/*  The swiglabels  */

%insert("runtime") "swiglabels.swg"


%endoffile

%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\lua\\lua.swg" %beginfile
/* -----------------------------------------------------------------------------
 * lua.swg
 *
 * SWIG Configuration File for Lua.
 * This file is parsed by SWIG before reading any other interface file.
 * ----------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 *                          includes
 * ----------------------------------------------------------------------------- */

%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\lua\\luatypemaps.swg" %beginfile
/* -----------------------------------------------------------------------------
 * luatypemaps.swg
 *
 * basic typemaps for Lua.
 * ----------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 *                          standard typemaps
 * ----------------------------------------------------------------------------- */
/* NEW LANGUAGE NOTE:
   the 'checkfn' param is something that I added for typemap(in)
   it is an optional fn call to check the type of the lua object
   the fn call must be of the form
     int checkfn(lua_State *L, int index);
   and return 1/0 depending upon if this is the correct type
   For the typemap(out), an additional SWIG_arg parameter must be incremented
   to reflect the number of values returned (normally SWIG_arg++; will do)
*/
// numbers
%typemap(in,checkfn="lua_isnumber") int, short, long,
             signed char, float, double
%{$1 = ($type)lua_tonumber(L, $input);%}
 
// additional check for unsigned numbers, to not permit negative input
%typemap(in,checkfn="lua_isnumber") unsigned int,
             unsigned short, unsigned long, unsigned char
%{SWIG_contract_assert((lua_tonumber(L,$input)>=0),"number must not be negative")
$1 = ($type)lua_tonumber(L, $input);%}

%typemap(out) int,short,long,
             unsigned int,unsigned short,unsigned long,
             signed char,unsigned char,
             float,double
%{  lua_pushnumber(L, (lua_Number) $1); SWIG_arg++;%}

// we must also provide typemaps for primitives by const reference:
// given a function:
//	int intbyref(const int& i);
// SWIG assumes that this code will need a pointer to int to be passed in
// (this might be ok for objects by const ref, but not for numeric primitives)
// therefore we add a set of typemaps to fix this (for both in & out)
%typemap(in,checkfn="lua_isnumber") const int&($basetype temp)
%{ temp=($basetype)lua_tonumber(L,$input); $1=&temp;%}

%typemap(in,checkfn="lua_isnumber") const unsigned int&($basetype temp)
%{SWIG_contract_assert((lua_tonumber(L,$input)>=0),"number must not be negative")
temp=($basetype)lua_tonumber(L,$input); $1=&temp;%}

%typemap(out) const int&, const unsigned int&
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}

// for the other numbers we can just use an apply statement to cover them
%apply const int & {const short&,const long&,const signed char&,
             const float&,const double&};

%apply const unsigned int & {const unsigned short&,const unsigned long&,
             const unsigned char&};

/* enums have to be handled slightly differently
	VC++ .net will not allow a cast from lua_Number(double) to enum directly.
*/
%typemap(in,checkfn="lua_isnumber") enum SWIGTYPE
%{$1 = ($type)(int)lua_tonumber(L, $input);%}

%typemap(out) enum SWIGTYPE
%{  lua_pushnumber(L, (lua_Number)(int)($1)); SWIG_arg++;%}

// and const refs
%typemap(in,checkfn="lua_isnumber") const enum SWIGTYPE &($basetype temp)
%{ temp=($basetype)(int)lua_tonumber(L,$input); $1=&temp;%}
%typemap(in,checkfn="lua_isnumber") const enum SWIGTYPE &&($basetype temp)
%{ temp=($basetype)(int)lua_tonumber(L,$input); $1=&temp;%}
%typemap(out) const enum SWIGTYPE &
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(out) const enum SWIGTYPE &&
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}


// boolean (which is a special type in lua)
// note: lua_toboolean() returns 1 or 0
// note: 1 & 0 are not booleans in lua, only true & false
%typemap(in,checkfn="lua_isboolean") bool
%{$1 = (lua_toboolean(L, $input)!=0);%}

%typemap(out) bool
%{  lua_pushboolean(L,(int)($1!=0)); SWIG_arg++;%}

// for const bool&, SWIG treats this as a const bool* so we must dereference it
%typemap(in,checkfn="lua_isboolean") const bool& (bool temp)
%{temp=(lua_toboolean(L, $input)!=0);
  $1=&temp;%}

%typemap(out) const bool&
%{  lua_pushboolean(L,(int)((*$1)!=0)); SWIG_arg++;%}

// strings (char * and char[])
%fragment("SWIG_lua_isnilstring", "header") {
SWIGINTERN int SWIG_lua_isnilstring(lua_State *L, int idx) {
  int ret = lua_isstring(L, idx);
  if (!ret)
   ret = lua_isnil(L, idx);
  return ret;
}
}

%typemap(in,checkfn="SWIG_lua_isnilstring",fragment="SWIG_lua_isnilstring") const char *, char *
%{$1 = ($ltype)lua_tostring(L, $input);%}

%typemap(in,checkfn="SWIG_lua_isnilstring",fragment="SWIG_lua_isnilstring") const char[ANY], char[ANY]
%{$1 = ($ltype)lua_tostring(L, $input);%}

%typemap(out) const char *, char *
%{  lua_pushstring(L,(const char *)$1); SWIG_arg++;%}

%typemap(out) const char[ANY], char[ANY]
%{  lua_pushstring(L,(const char *)$1); SWIG_arg++;%}

// char's
// currently treating chars as small strings, not as numbers
// (however signed & unsigned char's are numbers...)
%typemap(in,checkfn="SWIG_lua_isnilstring",fragment="SWIG_lua_isnilstring") char
%{$1 = (lua_tostring(L, $input))[0];%}

%typemap(out) char
%{  lua_pushlstring(L, &$1, 1); SWIG_arg++;%}

// by const ref
%typemap(in,checkfn="SWIG_lua_isnilstring",fragment="SWIG_lua_isnilstring") const char& (char temp)
%{temp = (lua_tostring(L, $input))[0]; $1=&temp;%}

%typemap(out) const char&
%{  lua_pushlstring(L, $1, 1); SWIG_arg++;%}

// pointers and references
// under SWIG rules, it is ok, to have a pass in a lua nil,
// it should be converted to a SWIG NULL.
// This will only be allowed for pointers & arrays, not refs or by value
// the checkfn lua_isuserdata will only work for userdata
// the checkfn SWIG_isptrtype will work for both userdata and nil
%typemap(in,checkfn="SWIG_isptrtype") SWIGTYPE*,SWIGTYPE[]
%{
  if (!SWIG_IsOK(SWIG_ConvertPtr(L,$input,(void**)&$1,$descriptor,$disown))){
    SWIG_fail_ptr("$symname",$argnum,$descriptor);
  }
%}

%typemap(in,checkfn="lua_isuserdata") SWIGTYPE&
%{
  if (!SWIG_IsOK(SWIG_ConvertPtr(L,$input,(void**)&$1,$descriptor,$disown))){
    SWIG_fail_ptr("$symname",$argnum,$descriptor);
  }
%}

%typemap(in,checkfn="lua_isuserdata") SWIGTYPE&&
%{
  if (!SWIG_IsOK(SWIG_ConvertPtr(L,$input,(void**)&$1,$descriptor,$disown))){
    SWIG_fail_ptr("$symname",$argnum,$descriptor);
  }
%}

// out is simple
%typemap(out) SWIGTYPE*,SWIGTYPE&
%{SWIG_NewPointerObj(L,$1,$descriptor,$owner); SWIG_arg++; %}
%typemap(out) SWIGTYPE*,SWIGTYPE&&
%{SWIG_NewPointerObj(L,$1,$descriptor,$owner); SWIG_arg++; %}

// dynamic casts
// this uses the SWIG_TypeDynamicCast() which relies on RTTI to find out what the pointer really is
// the we return it as the correct type
%typemap(out) SWIGTYPE *DYNAMIC,
              SWIGTYPE &DYNAMIC
{
  swig_type_info *ty = SWIG_TypeDynamicCast($1_descriptor, (void **) &$1);
  SWIG_NewPointerObj(L,(void*)$1,ty,$owner); SWIG_arg++; 
}


// passing objects by value
// SWIG_ConvertPtr wants an object pointer (the $&ltype argp)
// then dereferences it to get the object
%typemap(in,checkfn="lua_isuserdata") SWIGTYPE ($&ltype argp)
%{
   if (!SWIG_IsOK(SWIG_ConvertPtr(L,$input,(void**)&argp,$&descriptor,0))){
     SWIG_fail_ptr("$symname",$argnum,$&descriptor);
   }
   $1 = *argp;
%}

// Also needed for object ptrs by const ref
// eg A* const& ref_pointer(A* const& a);
// found in mixed_types.i
%typemap(in,checkfn="SWIG_isptrtype") SWIGTYPE *const&($*ltype temp)
%{temp=($*ltype)SWIG_MustGetPtr(L,$input,$*descriptor,0,$argnum,"$symname");
$1=($1_ltype)&temp;%}

%typemap(out) SWIGTYPE *const&
%{SWIG_NewPointerObj(L,*$1,$*descriptor,$owner); SWIG_arg++; %}


// DISOWN-ing typemaps
// if you have an object pointer which must be disowned, use this typemap
// eg. for void destroy_foo(Foo* toDie);
// use %apply SWIGTYPE* DISOWN {Foo* toDie};
// you could just use %delobject, but this is more flexible
%typemap(in,checkfn="SWIG_isptrtype") SWIGTYPE* DISOWN,SWIGTYPE DISOWN[]
%{  if (!SWIG_IsOK(SWIG_ConvertPtr(L,$input,(void**)&$1,$descriptor,SWIG_POINTER_DISOWN))){
    SWIG_fail_ptr("$symname",$argnum,$descriptor);
  }
%}


// Primitive types--return by value
// must make a new object, copy the data & return the new object
// Note: the brackets are {...} and not %{..%}, because we want them to be included in the wrapper
// this is because typemap(out) does not support local variables, like in typemap(in) does
// and we need the $&1_ltype resultptr; to be declared

%typemap(out) SWIGTYPE
{
  $&1_ltype resultptr = new $1_ltype((const $1_ltype &) $1);
  SWIG_NewPointerObj(L,(void *) resultptr,$&1_descriptor,1); SWIG_arg++;
}










// member function pointer
// a member fn ptr is not 4 bytes like a normal pointer, but 8 bytes (at least on mingw)
// so the standard wrapping cannot be done
// nor can you cast a member function pointer to a void* (obviously)
// therefore a special wrapping functions SWIG_ConvertMember() & SWIG_NewMemberObj() were written
%typemap(in,checkfn="lua_isuserdata") SWIGTYPE (CLASS::*)
%{
  if (!SWIG_IsOK(SWIG_ConvertMember(L,$input,(void*)(&$1),sizeof($type),$descriptor)))
    SWIG_fail_ptr("$symname",$argnum,$descriptor);
%}

%typemap(out) SWIGTYPE (CLASS::*)
%{ 
  SWIG_NewMemberObj(L,(void*)(&$1),sizeof($type),$descriptor); SWIG_arg++; 
%}


// void (must be empty without the SWIG_arg++)
%typemap(out) void "";

/* void* is a special case
A function void fn(void*) should take any kind of pointer as a parameter (just like C/C++ does)
but if its an output, then it should be wrapped like any other SWIG object (using default typemap)
*/
%typemap(in,checkfn="SWIG_isptrtype") void*
%{$1=($1_ltype)SWIG_MustGetPtr(L,$input,0,0,$argnum,"$symname");%}

/* long long is another special case:
as lua only supports one numeric type (lua_Number), we will just
cast it to that & accept the loss of precision.
An alternative solution would be a long long struct or class
with the relevant operators.
*/
%apply long {long long, signed long long, unsigned long long};
%apply const long& {const long long&, const signed long long&, const unsigned long long&};

/* It is possible to also pass a lua_State* into a function, so
void fn(int a, float b, lua_State* s) is wrappable as
> fn(1,4.3) -- note: the state is implicitly passed in
*/
%typemap(in, numinputs=0) lua_State* 
%{$1 = L;%}



/* -----------------------------------------------------------------------------
 *                          typecheck rules
 * ----------------------------------------------------------------------------- */
/* These are needed for the overloaded functions
These define the detection routines which will spot what
parameters match which function
*/

// unfortunately lua only considers one type of number
// so all numbers (int,float,double) match
// you could add an advanced fn to get type & check if its integral
 %typemap(typecheck, precedence=      70    ) 
	 int, short, long,
 	 unsigned int, unsigned short, unsigned long,
	 signed char, unsigned char,
	 long long, unsigned long long, signed long long,
	 const int &, const short &, const long &,
 	 const unsigned int &, const unsigned short &, const unsigned long &,
	 const signed char&, const unsigned char&,
	 const long long &, const unsigned long long &,
	 enum SWIGTYPE,	const enum SWIGTYPE&, const enum SWIGTYPE &&,
	 float, double, const float &, const double&
{
  $1 = lua_isnumber(L,$input);
}

 %typemap(typecheck, precedence=         15    ) 
    bool, const bool &
{
  $1 = lua_isboolean(L,$input);
}

// special check for a char (string of length 1)
 %typemap(typecheck, precedence=        130    ,fragment="SWIG_lua_isnilstring")  char, const char& {
  $1 = SWIG_lua_isnilstring(L,$input) && (lua_rawlen(L,$input)==1);
}

 %typemap(typecheck, precedence=      140    ,fragment="SWIG_lua_isnilstring")  char *, char[] {
  $1 = SWIG_lua_isnilstring(L,$input);
}

 %typemap(typecheck, precedence=      0     )  SWIGTYPE *, SWIGTYPE [] {
  void *ptr;
  if (SWIG_isptrtype(L,$input)==0 || SWIG_ConvertPtr(L,$input, (void **) &ptr, $1_descriptor, 0)) {
    $1 = 0;
  } else {
    $1 = 1;
  }
}

 %typemap(typecheck, precedence=      0     )  SWIGTYPE & {
  void *ptr;
  if (lua_isuserdata(L,$input)==0 || SWIG_ConvertPtr(L,$input, (void **) &ptr, $1_descriptor, SWIG_POINTER_NO_NULL)) {
    $1 = 0;
  } else {
    $1 = 1;
  }
}

 %typemap(typecheck, precedence=      0     )  SWIGTYPE && {
  void *ptr;
  if (lua_isuserdata(L,$input)==0 || SWIG_ConvertPtr(L,$input, (void **) &ptr, $1_descriptor, SWIG_POINTER_NO_NULL)) {
    $1 = 0;
  } else {
    $1 = 1;
  }
}

 %typemap(typecheck, precedence=      0     )  SWIGTYPE {
  void *ptr;
  if (lua_isuserdata(L,$input)==0 || SWIG_ConvertPtr(L,$input, (void **) &ptr, $&1_descriptor, SWIG_POINTER_NO_NULL)) {
    $1 = 0;
  } else {
    $1 = 1;
  }
}

 %typemap(typecheck, precedence=      10    )  void * {
  void *ptr;
  if (SWIG_isptrtype(L,$input)==0 || SWIG_ConvertPtr(L,$input, (void **) &ptr, 0, 0)) {
    $1 = 0;
  } else {
    $1 = 1;
  }
}

// Also needed for object pointers by const ref
// eg const A* ref_pointer(A* const& a);
// found in mixed_types.i
 %typemap(typecheck, precedence=      0     )  SWIGTYPE *const&
{
  void *ptr;
  if (SWIG_isptrtype(L,$input)==0 || SWIG_ConvertPtr(L,$input, (void **) &ptr, $*descriptor, 0)) {
    $1 = 0;
  } else {
    $1 = 1;
  }
}

/* -----------------------------------------------------------------------------
 *                          Others
 * ----------------------------------------------------------------------------- */

// Array reference typemaps
%apply SWIGTYPE & { SWIGTYPE ((&)[ANY]) }
%apply SWIGTYPE && { SWIGTYPE ((&&)[ANY]) }

/* const pointers */
%apply SWIGTYPE * { SWIGTYPE *const }
%apply SWIGTYPE (CLASS::*) { SWIGTYPE (CLASS::*const) }
%apply SWIGTYPE & { SWIGTYPE (CLASS::*const&) }

// size_t (which is just a unsigned long)
%apply unsigned long { size_t };
%apply const unsigned long & { const size_t & };


/* -----------------------------------------------------------------------------
 *                          Specials
 * ----------------------------------------------------------------------------- */
// swig::LANGUAGE_OBJ was added to allow containers of native objects
// however its rather difficult to do this in lua, as you cannot hold pointers
// to native objects (they are held in the interpreter)
// therefore for now: just ignoring this feature

%rename($ignore) swig::LANGUAGE_OBJ;

//%inline %{
%{
namespace swig {
typedef struct{} LANGUAGE_OBJ;
}
%}


%endoffile         /* The typemaps */
%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\lua\\luaruntime.swg" %beginfile
/* -----------------------------------------------------------------------------
 * luaruntime.swg
 *
 * all the runtime code for .
 * ----------------------------------------------------------------------------- */

%insert("runtime") "swigrun.swg"          /* Common C API type-checking code */
%insert("runtime") "swigerrors.swg"       /* SWIG errors */
%insert("runtime") "luarun.swg"           /* Lua runtime stuff */

%insert(initbeforefunc) "swiginit.swg"

%insert(initbeforefunc) %{

/* Forward declaration of where the user's %init{} gets inserted */
void SWIG_init_user(lua_State* L );
    
#ifdef __cplusplus
extern "C" {
#endif
/* this is the initialization function
  added at the very end of the code
  the function is always called SWIG_init, but an earlier #define will rename it
*/
#if ((SWIG_LUA_TARGET == SWIG_LUA_FLAVOR_ELUA) || (SWIG_LUA_TARGET == SWIG_LUA_FLAVOR_ELUAC))
LUALIB_API int SWIG_init(lua_State* L)
#else
SWIGEXPORT int SWIG_init(lua_State* L) /* default Lua action */
#endif
{
#if (SWIG_LUA_TARGET != SWIG_LUA_FLAVOR_ELUAC) /* valid for both Lua and eLua */
  int i;
  int globalRegister = 0;
  /* start with global table */
  lua_pushglobaltable (L);
  /* SWIG's internal initialisation */
  SWIG_InitializeModule((void*)L);
  SWIG_PropagateClientData();
#endif

#if ((SWIG_LUA_TARGET != SWIG_LUA_FLAVOR_ELUA) && (SWIG_LUA_TARGET != SWIG_LUA_FLAVOR_ELUAC)) || defined(SWIG_LUA_ELUA_EMULATE)
  /* add a global fn */
  SWIG_Lua_add_function(L,"swig_type",SWIG_Lua_type);
  SWIG_Lua_add_function(L,"swig_equals",SWIG_Lua_class_equal);
#endif

#if (SWIG_LUA_TARGET != SWIG_LUA_FLAVOR_ELUAC)
  /* set up base class pointers (the hierarchy) */
  for (i = 0; swig_types[i]; i++){
    if (swig_types[i]->clientdata){
      SWIG_Lua_init_base_class(L,(swig_lua_class*)(swig_types[i]->clientdata));
    }
  }
#ifdef SWIG_LUA_MODULE_GLOBAL
  globalRegister = 1;
#endif


#if (SWIG_LUA_TARGET == SWIG_LUA_FLAVOR_LUA)
  SWIG_Lua_namespace_register(L,&swig_SwigModule, globalRegister);
#endif

#if (SWIG_LUA_TARGET == SWIG_LUA_FLAVOR_ELUA) || (SWIG_LUA_TARGET == SWIG_LUA_FLAVOR_ELUAC)
  for (i = 0; swig_types[i]; i++){
    if (swig_types[i]->clientdata){
      SWIG_Lua_elua_class_register_instance(L,(swig_lua_class*)(swig_types[i]->clientdata));
    }
  }
#endif

#if defined(SWIG_LUA_ELUA_EMULATE)
  lua_newtable(L);
  SWIG_Lua_elua_emulate_register(L,swig_SwigModule.ns_methods);
  SWIG_Lua_elua_emulate_register_clear(L);
  if(globalRegister) {
    lua_pushstring(L,swig_SwigModule.name);
    lua_pushvalue(L,-2);
    lua_rawset(L,-4);
  }
#endif

#endif

#if (SWIG_LUA_TARGET != SWIG_LUA_FLAVOR_ELUAC)
  /* invoke user-specific initialization */
  SWIG_init_user(L);
  /* end module */
  /* Note: We do not clean up the stack here (Lua will do this for us). At this
     point, we have the globals table and out module table on the stack. Returning
     one value makes the module table the result of the require command. */
  return 1;
#else
  return 0;
#endif
}

#ifdef __cplusplus
}
#endif

%}

/* Note: the initialization function is closed after all code is generated */

%endoffile          /* The runtime stuff */
%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\lua\\luakw.swg" %beginfile
/*
  Warnings for Lua keywords, built-in names and bad names.
*/




/*
  Warnings for Lua keywords 
  http://www.lua.org/manual/5.2/manual.html#3.1
*/

%namewarn("314"":""'" "and" "' is a Lua keyword, renaming to 'c_" "and" "'",rename="c_%s")  "and";
%namewarn("314"":""'" "break" "' is a Lua keyword, renaming to 'c_" "break" "'",rename="c_%s")  "break";
%namewarn("314"":""'" "do" "' is a Lua keyword, renaming to 'c_" "do" "'",rename="c_%s")  "do";
%namewarn("314"":""'" "else" "' is a Lua keyword, renaming to 'c_" "else" "'",rename="c_%s")  "else";
%namewarn("314"":""'" "elseif" "' is a Lua keyword, renaming to 'c_" "elseif" "'",rename="c_%s")  "elseif";
%namewarn("314"":""'" "end" "' is a Lua keyword, renaming to 'c_" "end" "'",rename="c_%s")  "end";
%namewarn("314"":""'" "false" "' is a Lua keyword, renaming to 'c_" "false" "'",rename="c_%s")  "false";
%namewarn("314"":""'" "for" "' is a Lua keyword, renaming to 'c_" "for" "'",rename="c_%s")  "for";
%namewarn("314"":""'" "function" "' is a Lua keyword, renaming to 'c_" "function" "'",rename="c_%s")  "function";
%namewarn("314"":""'" "goto" "' is a Lua keyword, renaming to 'c_" "goto" "'",rename="c_%s")  "goto";
%namewarn("314"":""'" "if" "' is a Lua keyword, renaming to 'c_" "if" "'",rename="c_%s")  "if";
%namewarn("314"":""'" "in" "' is a Lua keyword, renaming to 'c_" "in" "'",rename="c_%s")  "in";
%namewarn("314"":""'" "local" "' is a Lua keyword, renaming to 'c_" "local" "'",rename="c_%s")  "local";
%namewarn("314"":""'" "nil" "' is a Lua keyword, renaming to 'c_" "nil" "'",rename="c_%s")  "nil";
%namewarn("314"":""'" "not" "' is a Lua keyword, renaming to 'c_" "not" "'",rename="c_%s")  "not";
%namewarn("314"":""'" "or" "' is a Lua keyword, renaming to 'c_" "or" "'",rename="c_%s")  "or";
%namewarn("314"":""'" "repeat" "' is a Lua keyword, renaming to 'c_" "repeat" "'",rename="c_%s")  "repeat";
%namewarn("314"":""'" "return" "' is a Lua keyword, renaming to 'c_" "return" "'",rename="c_%s")  "return";
%namewarn("314"":""'" "then" "' is a Lua keyword, renaming to 'c_" "then" "'",rename="c_%s")  "then";
%namewarn("314"":""'" "true" "' is a Lua keyword, renaming to 'c_" "true" "'",rename="c_%s")  "true";
%namewarn("314"":""'" "until" "' is a Lua keyword, renaming to 'c_" "until" "'",rename="c_%s")  "until";
%namewarn("314"":""'" "while" "' is a Lua keyword, renaming to 'c_" "while" "'",rename="c_%s")  "while";

/*
  Basic functions
  http://www.lua.org/manual/5.2/manual.html#6.1
*/ 

%namewarn("321"":""'" "assert" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "assert";
%namewarn("321"":""'" "collectgarbage" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "collectgarbage";
%namewarn("321"":""'" "dofile" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "dofile";
%namewarn("321"":""'" "error" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "error";
%namewarn("321"":""'" "_G" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "_G"; // Not actually a function
%namewarn("321"":""'" "getmetatable" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "getmetatable";
%namewarn("321"":""'" "ipairs" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "ipairs";
%namewarn("321"":""'" "load" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "load";
%namewarn("321"":""'" "loadfile" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "loadfile";
%namewarn("321"":""'" "next" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "next";
%namewarn("321"":""'" "pairs" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "pairs";
%namewarn("321"":""'" "pcall" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "pcall";
%namewarn("321"":""'" "print" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "print";
%namewarn("321"":""'" "rawequal" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "rawequal";
%namewarn("321"":""'" "rawget" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "rawget";
%namewarn("321"":""'" "rawlen" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "rawlen";
%namewarn("321"":""'" "rawset" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "rawset";
%namewarn("321"":""'" "select" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "select";
%namewarn("321"":""'" "setmetatable" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "setmetatable";
%namewarn("321"":""'" "tonumber" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "tonumber";
%namewarn("321"":""'" "tostring" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "tostring";
%namewarn("321"":""'" "type" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "type";
%namewarn("321"":""'" "_VERSION" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "_VERSION"; // Not actually a function
%namewarn("321"":""'" "xpcall" "' conflicts with a basic function in Lua",            "not"        "match$ismember"="1"  )  "xpcall";



%endoffile               /* Warnings for Lua keywords */

//%include <typemaps/swigmacros.swg>
/* -----------------------------------------------------------------------------
 *                          constants typemaps
 * ----------------------------------------------------------------------------- */
// this basically adds to a table of constants
%typemap(consttab) int, unsigned int, short, unsigned short, long, unsigned long, unsigned char, signed char, bool, enum SWIGTYPE
       {SWIG_LUA_CONSTTAB_INT("$symname", $value)}

%typemap(consttab) float, double
       {SWIG_LUA_CONSTTAB_FLOAT("$symname", $value)}

%typemap(consttab) long long, unsigned long long, signed long long
       {SWIG_LUA_CONSTTAB_FLOAT("$symname", $value)}

%typemap(consttab) const long long&, const unsigned long long&, const signed long long&
       {SWIG_LUA_CONSTTAB_FLOAT("$symname", *$value)}

%typemap(consttab) char *, const char *, char [], const char []
       {SWIG_LUA_CONSTTAB_STRING("$symname", $value)}

// note: char is treated as a separate special type
// signed char & unsigned char are numbers
%typemap(consttab) char
       {SWIG_LUA_CONSTTAB_CHAR("$symname", $value)}

%typemap(consttab) long long, unsigned long long
       {SWIG_LUA_CONSTTAB_STRING("$symname", "$value")}

%typemap(consttab) SWIGTYPE *, SWIGTYPE *const, SWIGTYPE &, SWIGTYPE &&, SWIGTYPE []
       { SWIG_LUA_CONSTTAB_POINTER("$symname",$value, $1_descriptor) }

%typemap(consttab) SWIGTYPE
       { SWIG_LUA_CONSTTAB_POINTER("$symname",&$value, $&1_descriptor) }

// member function pointers
%typemap(consttab) SWIGTYPE (CLASS::*)
       { SWIG_LUA_CONSTTAB_BINARY("$symname", sizeof($type),&$value, $1_descriptor) }


/* -----------------------------------------------------------------------------
 *                          Overloaded operator support
 * ----------------------------------------------------------------------------- */
// lua calls the + operator '__add'
// python likes to call it '__add__'
// Assuming most SWIGers will probably use the __add__ if they extend their classes
// we have two sets of renames
// one to rename the operator+() to __add()
//	(this lets SWIG rename the operator overloads)
// another is to rename __add__() to __add()
//	(this means that people who wrote SWIG code to do that add will also work)


// this is extra renaming for lua
// not all operators are supported, so only those that are, are listed
%rename(__add)			*::operator+;
%rename(__sub)			*::operator-;
%rename(__mul)			*::operator*;
%rename(__div)			*::operator/;
%rename(__unm)      *::operator-();
%rename(__unm)      *::operator-() const;

%rename(__eq)			*::operator==;	
%rename($ignore) *::operator!=;      // note: Lua does not have a notequal operator
						// it just uses 'not (a==b)'
%rename(__lt)			*::operator<;
%rename($ignore) *::operator>;   	// ditto less than vs greater than
%rename(__le)			*::operator<=;	
%rename($ignore) *::operator>=;  // ditto less than vs greater than
%rename($ignore) *::operator!;  // does not support not

%rename(__call)			*::operator();	// the fn call operator

// lua does not support overloading of:
// 	logical/bitwise operators
// 	assign operator
// 	+=,-=,*=, etc
// therefore ignoring them for now
// it also doesn't support non class operators
// eg friends or XX operator+(XX,XX)
// also ignoring
// note: some of these might be better to rename, but not doing that for now
%rename($ignore) *::operator&&;	%rename($ignore) operator&&;
%rename($ignore) *::operator||;	%rename($ignore) operator||;
%rename($ignore) *::operator+=;
%rename($ignore) *::operator-=;
%rename($ignore) *::operator*=;
%rename($ignore) *::operator/=;
%rename($ignore) *::operator%=;
%rename($ignore) *::operator++;	%rename($ignore) *::operator--;

%rename($ignore) *::operator=;	// note: this might be better to rename to assign() or similar

%rename($ignore) operator+;
%rename($ignore) operator-;
%rename($ignore) operator*;
%rename($ignore) operator/;
%rename($ignore) operator%;
%rename($ignore) operator[];
%rename($ignore) operator>;	%rename($ignore) operator>=;	
%rename($ignore) operator<;	%rename($ignore) operator<=;
%rename($ignore) operator==;	%rename($ignore) operator!=;


// renaming the python operators to be compatible with lua
// this means that if a developer has written a fn __add__()
// it will be used for the lua +
%rename(__add)			*::__add__;
%rename(__sub)			*::__sub__;
%rename(__mul)			*::__mul__;
%rename(__div)			*::__div__;
%rename(__unm)			*::__neg__;		// lua calls unary minus,'unm' not 'neg'
%rename(__tostring)		*::__str__;		// both map to __tostring
%rename(__tostring)		*::__repr__;	// both map to __tostring


%rename(__pow)			*::__pow__;		// lua power '^' operator
%rename(__concat)		*::__concat__;  // lua concat '..' operator
%rename(__eq)			*::__eq__;
%rename(__lt)			*::__lt__;
%rename(__le)			*::__le__;
%rename(__call)			*::__call__;	// the fn call operator()

// the [] operator has two parts, the get & the set
%rename(__getitem)			*::__getitem__;	// the v=X[i] (get operator)
%rename(__setitem)			*::__setitem__;	// the X[i]=v (set operator)





/* ------------------------------------------------------------
 *                              Exceptions
 * ------------------------------------------------------------ */
/* Confession: I don't really like C++ exceptions
The python/lua ones are great, but C++ ones I don't like
(mainly because I cannot get the stack trace out of it)
Therefore I have not bothered to try doing much in this

Therefore currently its just enough to get a few test cases running ok

note: if you wish to throw anything related to std::exception
use %include <std_except.i> instead
*/

// number as number+error
%typemap(throws) int,unsigned int,signed int,
				long,unsigned long,signed long,
				short,unsigned short,signed short,
				float,double,
				long long,unsigned long long,
				unsigned char, signed char,
                int&,unsigned int&,signed int&,
				long&,unsigned long&,signed long&,
				short&,unsigned short&,signed short&,
				float&,double&,
				long long&,unsigned long long&,
				unsigned char&, signed char&
%{lua_pushnumber(L,(lua_Number)$1);SWIG_fail; %}

%typemap(throws) bool,bool& 
%{lua_pushboolean(L,(int)($1==true));SWIG_fail; %}

// enum as number+error
%typemap(throws) enum SWIGTYPE
%{lua_pushnumber(L,(lua_Number)(int)$1);SWIG_fail; %}

// strings are just sent as errors
%typemap(throws) char *, const char *
%{lua_pushstring(L,$1);SWIG_fail;%}

// char is changed to a string
%typemap(throws) char
%{lua_pushlstring(L,&$1,1);SWIG_fail;%}

/*
Throwing object is a serious problem:
Assuming some code throws a 'FooBar'
There are a few options:
- return a pointer to it: but its unclear how long this will last for.
- return a copy of it: but not all objects are copyable
	(see exception_partial_info in the test suite for a case where you cannot do this)
- convert to a string & throw that
	it's not so useful, but it works (this is more lua like).
The third option (though not nice) is used
For a more useful solution: see std_except for more details
*/

// basic typemap for structs, classes, pointers & references
// convert to string and error
%typemap(throws) SWIGTYPE
%{(void)$1; /* ignore it */
lua_pushfstring(L,"object exception:%s",SWIG_TypePrettyName($1_descriptor));
SWIG_fail;%}

// code to make a copy of the object and return this
// if you have a function which throws a FooBar & you want SWIG to return a copy of the object as its error
// then use one of the below
//	%apply SWIGTYPE EXCEPTION_BY_VAL {FooBar};
//	%apply SWIGTYPE& EXCEPTION_BY_VAL {FooBar&}; // note: need & twice
%typemap(throws) SWIGTYPE EXCEPTION_BY_VAL
%{SWIG_NewPointerObj(L,(void *)new $1_ltype(($1_ltype &) $1),$&1_descriptor,1);
SWIG_fail;%}

// similar for object reference
// note: swig typemaps seem a little confused around here, therefore we use $basetype
%typemap(throws) SWIGTYPE& EXCEPTION_BY_VAL
%{SWIG_NewPointerObj(L,(void *)new $basetype($1),$1_descriptor,1);
SWIG_fail;%}


// note: no support for object pointers
// its not clear how long the pointer is valid for, therefore not supporting it

/* -----------------------------------------------------------------------------
 *                          extras
 * ----------------------------------------------------------------------------- */
// this %define is to allow insertion of lua source code into the wrapper file



/* ------------------------------ end lua.swg  ------------------------------ */
%endoffile
%includefile(maininput="include\\cyclone\\cyclone.i") "include\\cyclone\\cyclone.i" %beginfile
%module(directors="1") cyclone

%{
#include "cyclone/cyclone.h"
#include "cyclone/precision.h"
#include "cyclone/core.h"
#include "cyclone/random.h"
#include "cyclone/particle.h"
#include "cyclone/body.h"
#include "cyclone/pcontacts.h"
#include "cyclone/plinks.h"
#include "cyclone/pfgen.h"
#include "cyclone/pworld.h"
#include "cyclone/collide_fine.h"
#include "cyclone/contacts.h"
#include "cyclone/fgen.h"
#include "cyclone/joints.h"
#include "cyclone/contacts.h"
using namespace cyclone;
%}

%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\lua\\typemaps.i" %beginfile
/* -----------------------------------------------------------------------------
 * typemaps.swg
 *
 * SWIG Library file containing the main typemap code to support Lua modules.
 * ----------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 *                          Basic inout typemaps
 * ----------------------------------------------------------------------------- */
/*
These provide the basic ability for passing in & out of standard numeric data types
(int,long,float,double, etc)

The basic code looks like this:

%typemap(in,checkfn="lua_isnumber") int *INPUT(int temp), int &INPUT(int temp)
%{ temp = (int)lua_tonumber(L,$input);
   $1 = &temp; %}

%typemap(in, numinputs=0) int *OUTPUT (int temp)
%{ $1 = &temp; %}

%typemap(argout) int *OUTPUT
%{  lua_pushnumber(L, (double) *$1); SWIG_arg++;%}

%typemap(in) int *INOUT = int *INPUT;
%typemap(argout) int *INOUT = int *OUTPUT;

However the code below is a mixture of #defines & such, so nowhere as easy to read

To make you code work correctly its not just a matter of %including this file
You also have to give SWIG the hints on which to use where

eg
extern int add_pointer(int* a1,int* a2); // a1 & a2 are pointer values to be added
extern void swap(int* s1, int* s2);	// does the swap

You will need to either change the argument names
extern int add_pointer(int* INPUT,int* INPUT);

or provide a %apply statement

%apply int* INOUT{ int *s1, int *s2 };
	// if SWIG sees int* s1, int* s2, assume they are inout params
*/






















// now the code
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	unsigned char *INPUT($*ltype temp), unsigned char &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) unsigned char *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) unsigned char *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) unsigned char *INOUT = unsigned char *INPUT;
%typemap(argout) unsigned char *INOUT = unsigned char *OUTPUT;
%typemap(in) unsigned char &OUTPUT = unsigned char *OUTPUT;
%typemap(argout) unsigned char &OUTPUT = unsigned char *OUTPUT;
%typemap(in) unsigned char &INOUT = unsigned char *INPUT;
%typemap(argout) unsigned char &INOUT = unsigned char *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const unsigned char *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/; /*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	signed char *INPUT($*ltype temp), signed char &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) signed char *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) signed char *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) signed char *INOUT = signed char *INPUT;
%typemap(argout) signed char *INOUT = signed char *OUTPUT;
%typemap(in) signed char &OUTPUT = signed char *OUTPUT;
%typemap(argout) signed char &OUTPUT = signed char *OUTPUT;
%typemap(in) signed char &INOUT = signed char *INPUT;
%typemap(argout) signed char &INOUT = signed char *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const signed char *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/;

/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	short *INPUT($*ltype temp), short &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) short *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) short *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) short *INOUT = short *INPUT;
%typemap(argout) short *INOUT = short *OUTPUT;
%typemap(in) short &OUTPUT = short *OUTPUT;
%typemap(argout) short &OUTPUT = short *OUTPUT;
%typemap(in) short &INOUT = short *INPUT;
%typemap(argout) short &INOUT = short *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const short *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/; /*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	unsigned short *INPUT($*ltype temp), unsigned short &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) unsigned short *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) unsigned short *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) unsigned short *INOUT = unsigned short *INPUT;
%typemap(argout) unsigned short *INOUT = unsigned short *OUTPUT;
%typemap(in) unsigned short &OUTPUT = unsigned short *OUTPUT;
%typemap(argout) unsigned short &OUTPUT = unsigned short *OUTPUT;
%typemap(in) unsigned short &INOUT = unsigned short *INPUT;
%typemap(argout) unsigned short &INOUT = unsigned short *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const unsigned short *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/; /*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	signed short *INPUT($*ltype temp), signed short &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) signed short *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) signed short *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) signed short *INOUT = signed short *INPUT;
%typemap(argout) signed short *INOUT = signed short *OUTPUT;
%typemap(in) signed short &OUTPUT = signed short *OUTPUT;
%typemap(argout) signed short &OUTPUT = signed short *OUTPUT;
%typemap(in) signed short &INOUT = signed short *INPUT;
%typemap(argout) signed short &INOUT = signed short *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const signed short *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	int *INPUT($*ltype temp), int &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) int *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) int *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) int *INOUT = int *INPUT;
%typemap(argout) int *INOUT = int *OUTPUT;
%typemap(in) int &OUTPUT = int *OUTPUT;
%typemap(argout) int &OUTPUT = int *OUTPUT;
%typemap(in) int &INOUT = int *INPUT;
%typemap(argout) int &INOUT = int *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const int *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/; /*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	unsigned int *INPUT($*ltype temp), unsigned int &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) unsigned int *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) unsigned int *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) unsigned int *INOUT = unsigned int *INPUT;
%typemap(argout) unsigned int *INOUT = unsigned int *OUTPUT;
%typemap(in) unsigned int &OUTPUT = unsigned int *OUTPUT;
%typemap(argout) unsigned int &OUTPUT = unsigned int *OUTPUT;
%typemap(in) unsigned int &INOUT = unsigned int *INPUT;
%typemap(argout) unsigned int &INOUT = unsigned int *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const unsigned int *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/; /*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	signed int *INPUT($*ltype temp), signed int &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) signed int *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) signed int *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) signed int *INOUT = signed int *INPUT;
%typemap(argout) signed int *INOUT = signed int *OUTPUT;
%typemap(in) signed int &OUTPUT = signed int *OUTPUT;
%typemap(argout) signed int &OUTPUT = signed int *OUTPUT;
%typemap(in) signed int &INOUT = signed int *INPUT;
%typemap(argout) signed int &INOUT = signed int *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const signed int *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	long *INPUT($*ltype temp), long &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) long *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) long *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) long *INOUT = long *INPUT;
%typemap(argout) long *INOUT = long *OUTPUT;
%typemap(in) long &OUTPUT = long *OUTPUT;
%typemap(argout) long &OUTPUT = long *OUTPUT;
%typemap(in) long &INOUT = long *INPUT;
%typemap(argout) long &INOUT = long *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const long *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/; /*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	unsigned long *INPUT($*ltype temp), unsigned long &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) unsigned long *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) unsigned long *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) unsigned long *INOUT = unsigned long *INPUT;
%typemap(argout) unsigned long *INOUT = unsigned long *OUTPUT;
%typemap(in) unsigned long &OUTPUT = unsigned long *OUTPUT;
%typemap(argout) unsigned long &OUTPUT = unsigned long *OUTPUT;
%typemap(in) unsigned long &INOUT = unsigned long *INPUT;
%typemap(argout) unsigned long &INOUT = unsigned long *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const unsigned long *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/; /*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	signed long *INPUT($*ltype temp), signed long &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) signed long *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) signed long *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) signed long *INOUT = signed long *INPUT;
%typemap(argout) signed long *INOUT = signed long *OUTPUT;
%typemap(in) signed long &OUTPUT = signed long *OUTPUT;
%typemap(argout) signed long &OUTPUT = signed long *OUTPUT;
%typemap(in) signed long &INOUT = signed long *INPUT;
%typemap(argout) signed long &INOUT = signed long *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const signed long *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	float *INPUT($*ltype temp), float &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) float *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) float *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) float *INOUT = float *INPUT;
%typemap(argout) float *INOUT = float *OUTPUT;
%typemap(in) float &OUTPUT = float *OUTPUT;
%typemap(argout) float &OUTPUT = float *OUTPUT;
%typemap(in) float &INOUT = float *INPUT;
%typemap(argout) float &INOUT = float *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const float *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	double *INPUT($*ltype temp), double &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) double *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) double *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) double *INOUT = double *INPUT;
%typemap(argout) double *INOUT = double *OUTPUT;
%typemap(in) double &OUTPUT = double *OUTPUT;
%typemap(argout) double &OUTPUT = double *OUTPUT;
%typemap(in) double &INOUT = double *INPUT;
%typemap(argout) double &INOUT = double *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const double *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	enum SWIGTYPE *INPUT($*ltype temp), enum SWIGTYPE &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) enum SWIGTYPE *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) enum SWIGTYPE *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) enum SWIGTYPE *INOUT = enum SWIGTYPE *INPUT;
%typemap(argout) enum SWIGTYPE *INOUT = enum SWIGTYPE *OUTPUT;
%typemap(in) enum SWIGTYPE &OUTPUT = enum SWIGTYPE *OUTPUT;
%typemap(argout) enum SWIGTYPE &OUTPUT = enum SWIGTYPE *OUTPUT;
%typemap(in) enum SWIGTYPE &INOUT = enum SWIGTYPE *INPUT;
%typemap(argout) enum SWIGTYPE &INOUT = enum SWIGTYPE *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const enum SWIGTYPE *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/;
// also for long longs's
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	long long *INPUT($*ltype temp), long long &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) long long *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) long long *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) long long *INOUT = long long *INPUT;
%typemap(argout) long long *INOUT = long long *OUTPUT;
%typemap(in) long long &OUTPUT = long long *OUTPUT;
%typemap(argout) long long &OUTPUT = long long *OUTPUT;
%typemap(in) long long &INOUT = long long *INPUT;
%typemap(argout) long long &INOUT = long long *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const long long *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/; /*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	unsigned long long *INPUT($*ltype temp), unsigned long long &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) unsigned long long *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) unsigned long long *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) unsigned long long *INOUT = unsigned long long *INPUT;
%typemap(argout) unsigned long long *INOUT = unsigned long long *OUTPUT;
%typemap(in) unsigned long long &OUTPUT = unsigned long long *OUTPUT;
%typemap(argout) unsigned long long &OUTPUT = unsigned long long *OUTPUT;
%typemap(in) unsigned long long &INOUT = unsigned long long *INPUT;
%typemap(argout) unsigned long long &INOUT = unsigned long long *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const unsigned long long *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/; /*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,48,SWIG_NUMBER_TYPEMAP@*/
%typemap(in,checkfn="lua_isnumber")	signed long long *INPUT($*ltype temp), signed long long &INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
%typemap(in, numinputs=0) signed long long *OUTPUT ($*ltype temp)
%{ $1 = &temp; %}
%typemap(argout) signed long long *OUTPUT
%{  lua_pushnumber(L, (lua_Number) *$1); SWIG_arg++;%}
%typemap(in) signed long long *INOUT = signed long long *INPUT;
%typemap(argout) signed long long *INOUT = signed long long *OUTPUT;
%typemap(in) signed long long &OUTPUT = signed long long *OUTPUT;
%typemap(argout) signed long long &OUTPUT = signed long long *OUTPUT;
%typemap(in) signed long long &INOUT = signed long long *INPUT;
%typemap(argout) signed long long &INOUT = signed long long *OUTPUT;
// const version (the $*ltype is the basic number without ptr or const's)
%typemap(in,checkfn="lua_isnumber")	const signed long long *INPUT($*ltype temp)
%{ temp = ($*ltype)lua_tonumber(L,$input);
   $1 = &temp; %}
/*@SWIG@*/;

// note we dont do char, as a char* is probably a string not a ptr to a single char

// similar for booleans
%typemap(in,checkfn="lua_isboolean") bool *INPUT(bool temp), bool &INPUT(bool temp)
%{ temp = (lua_toboolean(L,$input)!=0);
   $1 = &temp; %}

%typemap(in, numinputs=0) bool *OUTPUT (bool temp),bool &OUTPUT (bool temp)
%{ $1 = &temp; %}

%typemap(argout) bool *OUTPUT,bool &OUTPUT
%{  lua_pushboolean(L, (int)((*$1)!=0)); SWIG_arg++;%}

%typemap(in) bool *INOUT = bool *INPUT;
%typemap(argout) bool *INOUT = bool *OUTPUT;
%typemap(in) bool &INOUT = bool &INPUT;
%typemap(argout) bool &INOUT = bool &OUTPUT;

/* -----------------------------------------------------------------------------
 *                          Basic Array typemaps
 * ----------------------------------------------------------------------------- */
/*
I have no idea why this kind of code does not exist in SWIG as standard,
but here is it.
This code will convert to/from 1D numeric arrays.
In order to reduce code bloat, there are a few macros
and quite a few functions defined
(unfortunately this makes it a lot less clear)

assuming we have functions
void process_array(int arr[3]);	// nice fixed size array
void process_var_array(float arr[],int len);	// variable sized array
void process_var_array_inout(double* arr,int len);	// variable sized array
			// data passed in & out
void process_enum_inout_array_var(enum Days *arrinout, int len);	// using enums
void return_array_5(int arrout[5]);	// out array only

in order to wrap them correctly requires a typemap

// inform SWIG of the correct typemap
// For fixed length, you must specify it as <type> INPUT[ANY]
%apply (int INPUT[ANY]) {(int arr[3])};
// variable length arrays are just the same
%apply (float INPUT[],int) {(float arr[],int len)};
// it is also ok, to map the TYPE* instead of a TYPE[]
%apply (double *INOUT,int) {(double arr*,int len)};
// for the enum's you must use enum SWIGTYPE
%apply (enum SWIGTYPE *INOUT,int) {(enum Days *arrinout, int len)};
// fixed length out if also fine
%apply (int OUTPUT[ANY]) {(int arrout[5])};

Generally, you could use %typemap(...)=...
but the %apply is neater & easier

a few things of note:
* all Lua tables are indexed from 1, all C/C++ arrays are indexed from 0
	therefore t={6,5,3} -- t[1]==6, t[2]==5, t[3]==3
	when passed to process_array(int arr[3]) becomes
	arr[0]==6, arr[1]==5, arr[2]==3
* for OUTPUT arrays, no array need be passed in, the fn will return a Lua table
	so for the above mentioned return_array_5() would look like
	arr=return_array_5() -- no parameters passed in
* for INOUT arrays, a table must be passed in, and a new table will be returned
	(this is consistent with the way that numbers are processed)
	if you want just use
	arr={...}
	arr=process_var_array_inout(arr)	-- arr is replaced by the new version

The following are not yet supported:
* variable length output only array (inout works ok)
* multidimensional arrays
* arrays of objects/structs
* arrays of pointers

*/

/*
The internals of the array management stuff
helper fns/macros
SWIG_ALLOC_ARRAY(TYPE,LEN)	// returns a typed array TYPE[LEN]
SWIG_FREE_ARRAY(PTR)		// delete the ptr (if not zero)

// counts the specified table & gets the size
// integer version
int SWIG_itable_size(lua_State* L, int index);
// other version
int SWIG_table_size(lua_State* L, int index);

SWIG_DECLARE_TYPEMAP_ARR_FN(NAME,TYPE)
// this fn declares up 4 functions for helping to read/write tables
// these can then be called by the macros ...
// all assume the table is an integer indexes from 1
// but the C array is a indexed from 0
	// created a fixed size array, reads the specified table
	// and then fills the array with numbers
	// returns ptr to the array if ok, or 0 for error
	// (also pushes a error message to the stack)
TYPE* SWIG_get_NAME_num_array_fixed(lua_State* L, int index, int size);
	// as per SWIG_get_NAME_num_array_fixed()
	// but reads the entire table & creates an array of the correct size
	// (if the table is empty, it returns an error rather than a zero length array)
TYPE* SWIG_get_NAME_num_array_var(lua_State* L, int index, int* size);
	// writes a table to Lua with all the specified numbers
void SWIG_write_NAME_num_array(lua_State* L,TYPE *array,int size);
	// read the specified table, and fills the array with numbers
	// returns 1 of ok (only fails if it doesn't find numbers)
	// helper fn (called by SWIG_get_NAME_num_array_*() fns)
int SWIG_read_NAME_num_array(lua_State* L,int index,TYPE *array,int size);

*/

/* Reported that you don't need to check for NULL for delete & free
There probably is some compiler that its not true for, so the code is left here just in case.
#ifdef __cplusplus	
#define SWIG_ALLOC_ARRAY(TYPE,LEN) 	new TYPE[LEN]
#define SWIG_FREE_ARRAY(PTR)		if(PTR){delete[] PTR;}
#else
#define SWIG_ALLOC_ARRAY(TYPE,LEN) 	(TYPE *)malloc(LEN*sizeof(TYPE))
#define SWIG_FREE_ARRAY(PTR)		if(PTR){free(PTR);}
#endif
*/
%{
#ifdef __cplusplus	/* generic alloc/dealloc fns*/
#define SWIG_ALLOC_ARRAY(TYPE,LEN) 	new TYPE[LEN]
#define SWIG_FREE_ARRAY(PTR)		delete[] PTR
#else
#define SWIG_ALLOC_ARRAY(TYPE,LEN) 	(TYPE *)malloc(LEN*sizeof(TYPE))
#define SWIG_FREE_ARRAY(PTR)		free(PTR)
#endif
/* counting the size of arrays:*/
SWIGINTERN int SWIG_itable_size(lua_State* L, int index)
{
	int n=0;
	while(1){
		lua_rawgeti(L,index,n+1);
		if (lua_isnil(L,-1))break;
		++n;
		lua_pop(L,1);
	}
	lua_pop(L,1);
	return n;
}

SWIGINTERN int SWIG_table_size(lua_State* L, int index)
{
	int n=0;
	lua_pushnil(L);  /* first key*/
	while (lua_next(L, index) != 0) {
		++n;
		lua_pop(L, 1);  /* removes `value'; keeps `key' for next iteration*/
	}
	return n;
}

/* super macro to declare array typemap helper fns */
#define SWIG_DECLARE_TYPEMAP_ARR_FN(NAME,TYPE)\
	SWIGINTERN int SWIG_read_##NAME##_num_array(lua_State* L,int index,TYPE *array,int size){\
		int i;\
		for (i = 0; i < size; i++) {\
			lua_rawgeti(L,index,i+1);\
			if (lua_isnumber(L,-1)){\
				array[i] = (TYPE)lua_tonumber(L,-1);\
			} else {\
				lua_pop(L,1);\
				return 0;\
			}\
			lua_pop(L,1);\
		}\
		return 1;\
	}\
	SWIGINTERN TYPE* SWIG_get_##NAME##_num_array_fixed(lua_State* L, int index, int size){\
		TYPE *array;\
		if (!lua_istable(L,index) || SWIG_itable_size(L,index) != size) {\
			SWIG_Lua_pushferrstring(L,"expected a table of size %d",size);\
			return 0;\
		}\
		array=SWIG_ALLOC_ARRAY(TYPE,size);\
		if (!SWIG_read_##NAME##_num_array(L,index,array,size)){\
			SWIG_Lua_pusherrstring(L,"table must contain numbers");\
			SWIG_FREE_ARRAY(array);\
			return 0;\
		}\
		return array;\
	}\
	SWIGINTERN TYPE* SWIG_get_##NAME##_num_array_var(lua_State* L, int index, int* size)\
	{\
		TYPE *array;\
		if (!lua_istable(L,index)) {\
			SWIG_Lua_pusherrstring(L,"expected a table");\
			return 0;\
		}\
		*size=SWIG_itable_size(L,index);\
		if (*size<1){\
			SWIG_Lua_pusherrstring(L,"table appears to be empty");\
			return 0;\
		}\
		array=SWIG_ALLOC_ARRAY(TYPE,*size);\
		if (!SWIG_read_##NAME##_num_array(L,index,array,*size)){\
			SWIG_Lua_pusherrstring(L,"table must contain numbers");\
			SWIG_FREE_ARRAY(array);\
			return 0;\
		}\
		return array;\
	}\
	SWIGINTERN void SWIG_write_##NAME##_num_array(lua_State* L,TYPE *array,int size){\
		int i;\
		lua_newtable(L);\
		for (i = 0; i < size; i++){\
			lua_pushnumber(L,(lua_Number)array[i]);\
			lua_rawseti(L,-2,i+1);/* -1 is the number, -2 is the table*/ \
		}\
	}
%}

/*
This is one giant macro to define the typemaps & the helpers
for array handling
*/















































// the following line of code
// declares the C helper fns for the array typemaps
// as well as defining typemaps for
// fixed len arrays in & out, & variable length arrays in

/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(schar,signed char)%}

// fixed size array's
%typemap(in) signed char INPUT[ANY]
%{	$1 = SWIG_get_schar_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) signed char INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (signed char *INPUT,int)
%{	$1 = SWIG_get_schar_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (signed char *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) signed char OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(signed char,$1_dim0); %}

%typemap(argout) signed char OUTPUT[ANY]
%{	SWIG_write_schar_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) signed char OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) signed char INOUT[ANY]=signed char INPUT[ANY];
%typemap(argout) signed char INOUT[ANY]=signed char OUTPUT[ANY];
%typemap(freearg) signed char INOUT[ANY]=signed char INPUT[ANY];
// inout variable arrays
%typemap(in) (signed char *INOUT,int)=(signed char *INPUT,int);
%typemap(argout) (signed char *INOUT,int)
%{	SWIG_write_schar_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (signed char *INOUT,int)=(signed char *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (signed char INPUT[],int)=(signed char *INPUT,int);
%typemap(freearg) (signed char INPUT[],int)=(signed char *INPUT,int);

/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(uchar,unsigned char)%}

// fixed size array's
%typemap(in) unsigned char INPUT[ANY]
%{	$1 = SWIG_get_uchar_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) unsigned char INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (unsigned char *INPUT,int)
%{	$1 = SWIG_get_uchar_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (unsigned char *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) unsigned char OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(unsigned char,$1_dim0); %}

%typemap(argout) unsigned char OUTPUT[ANY]
%{	SWIG_write_uchar_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) unsigned char OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) unsigned char INOUT[ANY]=unsigned char INPUT[ANY];
%typemap(argout) unsigned char INOUT[ANY]=unsigned char OUTPUT[ANY];
%typemap(freearg) unsigned char INOUT[ANY]=unsigned char INPUT[ANY];
// inout variable arrays
%typemap(in) (unsigned char *INOUT,int)=(unsigned char *INPUT,int);
%typemap(argout) (unsigned char *INOUT,int)
%{	SWIG_write_uchar_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (unsigned char *INOUT,int)=(unsigned char *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (unsigned char INPUT[],int)=(unsigned char *INPUT,int);
%typemap(freearg) (unsigned char INPUT[],int)=(unsigned char *INPUT,int);

/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(int,int)%}

// fixed size array's
%typemap(in) int INPUT[ANY]
%{	$1 = SWIG_get_int_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) int INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (int *INPUT,int)
%{	$1 = SWIG_get_int_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (int *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) int OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(int,$1_dim0); %}

%typemap(argout) int OUTPUT[ANY]
%{	SWIG_write_int_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) int OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) int INOUT[ANY]=int INPUT[ANY];
%typemap(argout) int INOUT[ANY]=int OUTPUT[ANY];
%typemap(freearg) int INOUT[ANY]=int INPUT[ANY];
// inout variable arrays
%typemap(in) (int *INOUT,int)=(int *INPUT,int);
%typemap(argout) (int *INOUT,int)
%{	SWIG_write_int_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (int *INOUT,int)=(int *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (int INPUT[],int)=(int *INPUT,int);
%typemap(freearg) (int INPUT[],int)=(int *INPUT,int);

/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(uint,unsigned int)%}

// fixed size array's
%typemap(in) unsigned int INPUT[ANY]
%{	$1 = SWIG_get_uint_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) unsigned int INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (unsigned int *INPUT,int)
%{	$1 = SWIG_get_uint_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (unsigned int *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) unsigned int OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(unsigned int,$1_dim0); %}

%typemap(argout) unsigned int OUTPUT[ANY]
%{	SWIG_write_uint_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) unsigned int OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) unsigned int INOUT[ANY]=unsigned int INPUT[ANY];
%typemap(argout) unsigned int INOUT[ANY]=unsigned int OUTPUT[ANY];
%typemap(freearg) unsigned int INOUT[ANY]=unsigned int INPUT[ANY];
// inout variable arrays
%typemap(in) (unsigned int *INOUT,int)=(unsigned int *INPUT,int);
%typemap(argout) (unsigned int *INOUT,int)
%{	SWIG_write_uint_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (unsigned int *INOUT,int)=(unsigned int *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (unsigned int INPUT[],int)=(unsigned int *INPUT,int);
%typemap(freearg) (unsigned int INPUT[],int)=(unsigned int *INPUT,int);

/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(short,short)%}

// fixed size array's
%typemap(in) short INPUT[ANY]
%{	$1 = SWIG_get_short_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) short INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (short *INPUT,int)
%{	$1 = SWIG_get_short_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (short *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) short OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(short,$1_dim0); %}

%typemap(argout) short OUTPUT[ANY]
%{	SWIG_write_short_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) short OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) short INOUT[ANY]=short INPUT[ANY];
%typemap(argout) short INOUT[ANY]=short OUTPUT[ANY];
%typemap(freearg) short INOUT[ANY]=short INPUT[ANY];
// inout variable arrays
%typemap(in) (short *INOUT,int)=(short *INPUT,int);
%typemap(argout) (short *INOUT,int)
%{	SWIG_write_short_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (short *INOUT,int)=(short *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (short INPUT[],int)=(short *INPUT,int);
%typemap(freearg) (short INPUT[],int)=(short *INPUT,int);

/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(ushort,unsigned short)%}

// fixed size array's
%typemap(in) unsigned short INPUT[ANY]
%{	$1 = SWIG_get_ushort_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) unsigned short INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (unsigned short *INPUT,int)
%{	$1 = SWIG_get_ushort_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (unsigned short *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) unsigned short OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(unsigned short,$1_dim0); %}

%typemap(argout) unsigned short OUTPUT[ANY]
%{	SWIG_write_ushort_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) unsigned short OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) unsigned short INOUT[ANY]=unsigned short INPUT[ANY];
%typemap(argout) unsigned short INOUT[ANY]=unsigned short OUTPUT[ANY];
%typemap(freearg) unsigned short INOUT[ANY]=unsigned short INPUT[ANY];
// inout variable arrays
%typemap(in) (unsigned short *INOUT,int)=(unsigned short *INPUT,int);
%typemap(argout) (unsigned short *INOUT,int)
%{	SWIG_write_ushort_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (unsigned short *INOUT,int)=(unsigned short *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (unsigned short INPUT[],int)=(unsigned short *INPUT,int);
%typemap(freearg) (unsigned short INPUT[],int)=(unsigned short *INPUT,int);

/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(long,long)%}

// fixed size array's
%typemap(in) long INPUT[ANY]
%{	$1 = SWIG_get_long_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) long INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (long *INPUT,int)
%{	$1 = SWIG_get_long_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (long *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) long OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(long,$1_dim0); %}

%typemap(argout) long OUTPUT[ANY]
%{	SWIG_write_long_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) long OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) long INOUT[ANY]=long INPUT[ANY];
%typemap(argout) long INOUT[ANY]=long OUTPUT[ANY];
%typemap(freearg) long INOUT[ANY]=long INPUT[ANY];
// inout variable arrays
%typemap(in) (long *INOUT,int)=(long *INPUT,int);
%typemap(argout) (long *INOUT,int)
%{	SWIG_write_long_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (long *INOUT,int)=(long *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (long INPUT[],int)=(long *INPUT,int);
%typemap(freearg) (long INPUT[],int)=(long *INPUT,int);

/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(ulong,unsigned long)%}

// fixed size array's
%typemap(in) unsigned long INPUT[ANY]
%{	$1 = SWIG_get_ulong_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) unsigned long INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (unsigned long *INPUT,int)
%{	$1 = SWIG_get_ulong_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (unsigned long *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) unsigned long OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(unsigned long,$1_dim0); %}

%typemap(argout) unsigned long OUTPUT[ANY]
%{	SWIG_write_ulong_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) unsigned long OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) unsigned long INOUT[ANY]=unsigned long INPUT[ANY];
%typemap(argout) unsigned long INOUT[ANY]=unsigned long OUTPUT[ANY];
%typemap(freearg) unsigned long INOUT[ANY]=unsigned long INPUT[ANY];
// inout variable arrays
%typemap(in) (unsigned long *INOUT,int)=(unsigned long *INPUT,int);
%typemap(argout) (unsigned long *INOUT,int)
%{	SWIG_write_ulong_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (unsigned long *INOUT,int)=(unsigned long *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (unsigned long INPUT[],int)=(unsigned long *INPUT,int);
%typemap(freearg) (unsigned long INPUT[],int)=(unsigned long *INPUT,int);

/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(float,float)%}

// fixed size array's
%typemap(in) float INPUT[ANY]
%{	$1 = SWIG_get_float_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) float INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (float *INPUT,int)
%{	$1 = SWIG_get_float_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (float *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) float OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(float,$1_dim0); %}

%typemap(argout) float OUTPUT[ANY]
%{	SWIG_write_float_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) float OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) float INOUT[ANY]=float INPUT[ANY];
%typemap(argout) float INOUT[ANY]=float OUTPUT[ANY];
%typemap(freearg) float INOUT[ANY]=float INPUT[ANY];
// inout variable arrays
%typemap(in) (float *INOUT,int)=(float *INPUT,int);
%typemap(argout) (float *INOUT,int)
%{	SWIG_write_float_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (float *INOUT,int)=(float *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (float INPUT[],int)=(float *INPUT,int);
%typemap(freearg) (float INPUT[],int)=(float *INPUT,int);

/*@SWIG@*/;
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\typemaps.i,298,SWIG_TYPEMAP_NUM_ARR@*/
%{SWIG_DECLARE_TYPEMAP_ARR_FN(double,double)%}

// fixed size array's
%typemap(in) double INPUT[ANY]
%{	$1 = SWIG_get_double_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) double INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (double *INPUT,int)
%{	$1 = SWIG_get_double_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (double *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) double OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(double,$1_dim0); %}

%typemap(argout) double OUTPUT[ANY]
%{	SWIG_write_double_num_array(L,$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) double OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) double INOUT[ANY]=double INPUT[ANY];
%typemap(argout) double INOUT[ANY]=double OUTPUT[ANY];
%typemap(freearg) double INOUT[ANY]=double INPUT[ANY];
// inout variable arrays
%typemap(in) (double *INOUT,int)=(double *INPUT,int);
%typemap(argout) (double *INOUT,int)
%{	SWIG_write_double_num_array(L,$1,$2); SWIG_arg++; %}
%typemap(freearg) (double *INOUT,int)=(double *INPUT,int);

// TODO out variable arrays (is there a standard form for such things?)

// referencing so that (int *INPUT,int) and (int INPUT[],int) are the same
%typemap(in) (double INPUT[],int)=(double *INPUT,int);
%typemap(freearg) (double INPUT[],int)=(double *INPUT,int);

/*@SWIG@*/;

// again enums are a problem so they need their own type
// we use the int conversion routine & recast it
%typemap(in) enum SWIGTYPE INPUT[ANY]
%{	$1 = ($ltype)SWIG_get_int_num_array_fixed(L,$input,$1_dim0);
	if (!$1) SWIG_fail;%}

%typemap(freearg) enum SWIGTYPE INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size arrays
%typemap(in) (enum SWIGTYPE *INPUT,int)
%{	$1 = ($ltype)SWIG_get_int_num_array_var(L,$input,&$2);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (enum SWIGTYPE *INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) enum SWIGTYPE OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY(enum SWIGTYPE,$1_dim0); %}

%typemap(argout) enum SWIGTYPE OUTPUT[ANY]
%{	SWIG_write_int_num_array(L,(int*)$1,$1_dim0); SWIG_arg++; %}

%typemap(freearg) enum SWIGTYPE OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) enum SWIGTYPE INOUT[ANY]=enum SWIGTYPE INPUT[ANY];
%typemap(argout) enum SWIGTYPE INOUT[ANY]=enum SWIGTYPE OUTPUT[ANY];
%typemap(freearg) enum SWIGTYPE INOUT[ANY]=enum SWIGTYPE INPUT[ANY];
// inout variable arrays
%typemap(in) (enum SWIGTYPE *INOUT,int)=(enum SWIGTYPE *INPUT,int);
%typemap(argout) (enum SWIGTYPE *INOUT,int)
%{	SWIG_write_int_num_array(L,(int*)$1,$2); SWIG_arg++; %}
%typemap(freearg) (enum SWIGTYPE *INOUT,int)=(enum SWIGTYPE *INPUT,int);


/* Surprisingly pointer arrays are easier:
this is because all ptr arrays become void**
so only a few fns are needed & a few casts

The function defined are
	// created a fixed size array, reads the specified table
	// and then fills the array with pointers (checking the type)
	// returns ptr to the array if ok, or 0 for error
	// (also pushes a error message to the stack)
void** SWIG_get_ptr_array_fixed(lua_State* L, int index, int size,swig_type_info *type);
	// as per SWIG_get_ptr_array_fixed()
	// but reads the entire table & creates an array of the correct size
	// (if the table is empty, it returns an error rather than a zero length array)
void** SWIG_get_ptr_array_var(lua_State* L, int index, int* size,swig_type_info *type);
	// writes a table to Lua with all the specified pointers
	// all pointers have the ownership value 'own' (normally 0)
void SWIG_write_ptr_array(lua_State* L,void **array,int size,int own);
	// read the specified table, and fills the array with ptrs
	// returns 1 of ok (only fails if it doesn't find correct type of ptrs)
	// helper fn (called by SWIG_get_ptr_array_*() fns)
int SWIG_read_ptr_array(lua_State* L,int index,void **array,int size,swig_type_info *type);

The key thing to remember is that it is assumed that there is no
modification of pointers ownership in the arrays

eg A fn:
void pointers_in(TYPE* arr[],int len);
will make copies of the pointer into a temp array and then pass it into the fn
Lua does not remember that this fn held the pointers, so it is not safe to keep
these pointers until later

eg A fn:
void pointers_out(TYPE* arr[3]);
will return a table containing three pointers
however these pointers are NOT owned by Lua, merely borrowed
so if the C/C++ frees then Lua is not aware

*/

%{
SWIGINTERN int SWIG_read_ptr_array(lua_State* L,int index,void **array,int size,swig_type_info *type){
	int i;
	for (i = 0; i < size; i++) {
		lua_rawgeti(L,index,i+1);
		if (!lua_isuserdata(L,-1) || SWIG_ConvertPtr(L,-1,&array[i],type,0)==-1){
			lua_pop(L,1);
			return 0;
		}
		lua_pop(L,1);
	}
	return 1;
}
SWIGINTERN void** SWIG_get_ptr_array_fixed(lua_State* L, int index, int size,swig_type_info *type){
	void **array;
	if (!lua_istable(L,index) || SWIG_itable_size(L,index) != size) {
		SWIG_Lua_pushferrstring(L,"expected a table of size %d",size);
		return 0;
	}
	array=SWIG_ALLOC_ARRAY(void*,size);
	if (!SWIG_read_ptr_array(L,index,array,size,type)){
		SWIG_Lua_pushferrstring(L,"table must contain pointers of type %s",type->name);
		SWIG_FREE_ARRAY(array);
		return 0;
	}
	return array;
}
SWIGINTERN void** SWIG_get_ptr_array_var(lua_State* L, int index, int* size,swig_type_info *type){
	void **array;
	if (!lua_istable(L,index)) {
		SWIG_Lua_pusherrstring(L,"expected a table");
		return 0;
	}
	*size=SWIG_itable_size(L,index);
	if (*size<1){
		SWIG_Lua_pusherrstring(L,"table appears to be empty");
		return 0;
	}
	array=SWIG_ALLOC_ARRAY(void*,*size);
	if (!SWIG_read_ptr_array(L,index,array,*size,type)){
		SWIG_Lua_pushferrstring(L,"table must contain pointers of type %s",type->name);
		SWIG_FREE_ARRAY(array);
		return 0;
	}
	return array;
}
SWIGINTERN void SWIG_write_ptr_array(lua_State* L,void **array,int size,swig_type_info *type,int own){
	int i;
	lua_newtable(L);
	for (i = 0; i < size; i++){
		SWIG_NewPointerObj(L,array[i],type,own);
		lua_rawseti(L,-2,i+1);/* -1 is the number, -2 is the table*/
	}
}
%}

// fixed size array's
%typemap(in) SWIGTYPE* INPUT[ANY]
%{	$1 = ($ltype)SWIG_get_ptr_array_fixed(L,$input,$1_dim0,$*1_descriptor);
	if (!$1) SWIG_fail;%}

%typemap(freearg) SWIGTYPE* INPUT[ANY]
%{	SWIG_FREE_ARRAY($1);%}

// variable size array's
%typemap(in) (SWIGTYPE **INPUT,int)
%{	$1 = ($ltype)SWIG_get_ptr_array_var(L,$input,&$2,$*1_descriptor);
	if (!$1) SWIG_fail;%}

%typemap(freearg) (SWIGTYPE **INPUT,int)
%{	SWIG_FREE_ARRAY($1);%}

// out fixed arrays
%typemap(in,numinputs=0) SWIGTYPE* OUTPUT[ANY]
%{  $1 = SWIG_ALLOC_ARRAY($*1_type,$1_dim0); %}

%typemap(argout) SWIGTYPE* OUTPUT[ANY]
%{	SWIG_write_ptr_array(L,(void**)$1,$1_dim0,$*1_descriptor,0); SWIG_arg++; %}

%typemap(freearg) SWIGTYPE* OUTPUT[ANY]
%{	SWIG_FREE_ARRAY($1); %}

// inout fixed arrays
%typemap(in) SWIGTYPE* INOUT[ANY]=SWIGTYPE* INPUT[ANY];
%typemap(argout) SWIGTYPE* INOUT[ANY]=SWIGTYPE* OUTPUT[ANY];
%typemap(freearg) SWIGTYPE* INOUT[ANY]=SWIGTYPE* INPUT[ANY];
// inout variable arrays
%typemap(in) (SWIGTYPE** INOUT,int)=(SWIGTYPE** INPUT,int);
%typemap(argout) (SWIGTYPE** INOUT,int)
%{	SWIG_write_ptr_array(L,(void**)$1,$2,$*1_descriptor,0); SWIG_arg++; %}
%typemap(freearg) (SWIGTYPE**INOUT,int)=(SWIGTYPE**INPUT,int);

/* -----------------------------------------------------------------------------
 *                          Pointer-Pointer typemaps
 * ----------------------------------------------------------------------------- */
/*
This code is to deal with the issue for pointer-pointer's
In particular for factory methods.

for example take the following code segment:

struct iMath;    // some structure
int Create_Math(iMath** pptr); // its factory (assume it mallocs)

to use it you might have the following C code:

iMath* ptr;
int ok;
ok=Create_Math(&ptr);
// do things with ptr
//...
free(ptr);

With the following SWIG code
%apply SWIGTYPE** OUTPUT{iMath **pptr };

You can get natural wrapping in Lua as follows:
ok,ptr=Create_Math() -- ptr is a iMath* which is returned with the int
ptr=nil -- the iMath* will be GC'ed as normal
*/

%typemap(in,numinputs=0) SWIGTYPE** OUTPUT ($*ltype temp)
%{ temp = ($*ltype)0;
   $1 = &temp; %}
%typemap(argout) SWIGTYPE** OUTPUT
%{SWIG_NewPointerObj(L,*$1,$*descriptor,1); SWIG_arg++; %}

%endoffile
%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\lua\\std_vector.i" %beginfile
/* -----------------------------------------------------------------------------
 * std_vector.i
 *
 * std::vector typemaps for LUA
 * ----------------------------------------------------------------------------- */

%{
#include <vector>
%}
%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\lua\\std_except.i" %beginfile
/* -----------------------------------------------------------------------------
 * Typemaps used by the STL wrappers that throw exceptions.
 * These typemaps are used when methods are declared with an STL exception
 * specification, such as:
 *   size_t at() const throw (std::out_of_range);
 *
 * std_except.i
 * ----------------------------------------------------------------------------- */

%{
#include <typeinfo>
#include <stdexcept>
%}
%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\exception.i" %beginfile
/* -----------------------------------------------------------------------------
 * exception.i
 *
 * SWIG library file providing language independent exception handling
 * ----------------------------------------------------------------------------- */






%insert("runtime") "swigerrors.swg"











































































































































































































%{
#define SWIG_exception(a,b)\
{ lua_pushfstring(L,"%s:%s",#a,b);SWIG_fail; }
%}






































/*
  You can use the SWIG_CATCH_STDEXCEPT macro with the %exception
  directive as follows:

  %exception {
    try {
      $action
    }
    catch (my_except& e) {
      ...
    }
    SWIG_CATCH_STDEXCEPT // catch std::exception
    catch (...) {
     SWIG_exception(SWIG_UnknownError, "Unknown exception");
    }
  }
*/
%{
#include <typeinfo>
#include <stdexcept>
%}





























/* rethrow the unknown exception */






%typemap(throws,noblock=1) (...) {
  SWIG_exception(SWIG_RuntimeError,"unknown exception");
}




/* exception.i ends here */
%endoffile

namespace std 
{
  %rename($ignore) exception; // not sure if I should ignore this...
  class exception 
  {
  public:
    exception() throw() { }
    virtual ~exception() throw();
    virtual const char* what() const throw();
  }; 
}

// normally objects which are thrown are returned to the interpreter as errors
// (which potentially may have problems if they are not copied)
// therefore all classes based upon std::exception are converted to their strings & returned as errors
%typemap(throws) std::bad_cast          "SWIG_exception(SWIG_TypeError, $1.what());"
%typemap(throws) std::bad_exception     "SWIG_exception(SWIG_RuntimeError, $1.what());"
%typemap(throws) std::domain_error      "SWIG_exception(SWIG_ValueError, $1.what());"
%typemap(throws) std::exception         "SWIG_exception(SWIG_SystemError, $1.what());"
%typemap(throws) std::invalid_argument  "SWIG_exception(SWIG_ValueError, $1.what());"
%typemap(throws) std::length_error      "SWIG_exception(SWIG_IndexError, $1.what());"
%typemap(throws) std::logic_error       "SWIG_exception(SWIG_RuntimeError, $1.what());"
%typemap(throws) std::out_of_range      "SWIG_exception(SWIG_IndexError, $1.what());"
%typemap(throws) std::overflow_error    "SWIG_exception(SWIG_OverflowError, $1.what());"
%typemap(throws) std::range_error       "SWIG_exception(SWIG_IndexError, $1.what());"
%typemap(throws) std::runtime_error     "SWIG_exception(SWIG_RuntimeError, $1.what());"
%typemap(throws) std::underflow_error   "SWIG_exception(SWIG_RuntimeError, $1.what());"
%endoffile // the general exceptions
/*
A really cut down version of the vector class.

Note: this does not match the true std::vector class
but instead is an approximate, so that SWIG knows how to wrapper it.
(Eg, all access is by value, not ref, as SWIG turns refs to pointers)

And no support for iterators & insert/erase

It would be useful to have a vector<->Lua table conversion routine

*/
namespace std {

	template<class T>
    class vector {
      public:
        typedef size_t size_type;
        typedef ptrdiff_t difference_type;
        typedef T value_type;
        typedef value_type* pointer;
        typedef const value_type* const_pointer;
        typedef value_type& reference;
        typedef const value_type& const_reference;

        vector();
        vector(unsigned int);
        vector(const vector& other);
        vector(unsigned int,T);

        unsigned int size() const;
        unsigned int max_size() const;
        bool empty() const;
        void clear();
        void push_back(T val);
        void pop_back();
        T front()const; // only read front & back
        T back()const;  // not write to them
        // operator [] given later:

		%extend // this is a extra bit of SWIG code
		{
			// [] is replaced by __getitem__ & __setitem__
			// simply throws a string, which causes a lua error
			T __getitem__(unsigned int idx) throw (std::out_of_range)
			{
				if (idx>=self->size())
					throw std::out_of_range("in vector::__getitem__()");
				return (*self)[idx];
			}
			void __setitem__(unsigned int idx,T val) throw (std::out_of_range)
			{
				if (idx>=self->size())
					throw std::out_of_range("in vector::__setitem__()");
				(*self)[idx]=val;
			}
		};
    };

}

/*
Vector<->LuaTable fns
These look a bit like the array<->LuaTable fns
but are templated, not %defined
(you must have template support for STL)

*/
/*
%{
// reads a table into a vector of numbers
// lua numbers will be cast into the type required (rounding may occur)
// return 0 if non numbers found in the table
// returns new'ed ptr if ok
template<class T>
std::vector<T>* SWIG_read_number_vector(lua_State* L,int index)
{
	int i=0;
	std::vector<T>* vec=new std::vector<T>();
	while(1)
	{
		lua_rawgeti(L,index,i+1);
		if (!lua_isnil(L,-1))
		{
			lua_pop(L,1);
			break;	// finished
		}
		if (!lua_isnumber(L,-1))
		{
			lua_pop(L,1);
			delete vec;
			return 0;	// error
		}
		vec->push_back((T)lua_tonumber(L,-1));
		lua_pop(L,1);
		++i;
	}
	return vec;	// ok
}
// writes a vector of numbers out as a lua table
template<class T>
int SWIG_write_number_vector(lua_State* L,std::vector<T> *vec)
{
	lua_newtable(L);
	for(int i=0;i<vec->size();++i)
	{
		lua_pushnumber(L,(double)((*vec)[i]));
		lua_rawseti(L,-2,i+1);// -1 is the number, -2 is the table
	}
}
%}

// then the typemaps

%define SWIG_TYPEMAP_NUM_VECTOR(T)

// in
%typemap(in) std::vector<T> *INPUT
%{	$1 = SWIG_read_number_vector<T>(L,$input);
	if (!$1) SWIG_fail;%}

%typemap(freearg) std::vector<T> *INPUT
%{	delete $1;%}

// out
%typemap(argout) std::vector<T> *OUTPUT
%{	SWIG_write_number_vector(L,$1); SWIG_arg++; %}

%enddef
*/
%endoffile
%template(ParticleVector) std::vector<cyclone::Particle*>;

namespace cyclone
{
%rename($ignore) Vector3::operator [];
%feature("director") ParticleContactGenerator;
%feature("director") ParticleForceGenerator;
}


%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\lua\\carrays.i" %beginfile
/* Small change to the standard carrays.i
renaming the field to __getitem & __setitem
for operator[] access
*/
%rename(__getitem) *::getitem; // the v=X[i] (get operator)
%rename(__setitem) *::setitem; // the X[i]=v (set operator)

%includefile "D:\\Resource\\Program\\Portable\\Swig\\Lib\\lua\\..\\carrays.i" %beginfile
/* -----------------------------------------------------------------------------
 * carrays.i
 *
 * SWIG library file containing macros that can be used to manipulate simple
 * pointers as arrays.
 * ----------------------------------------------------------------------------- */

/* -----------------------------------------------------------------------------
 * %array_functions(TYPE,NAME)
 *
 * Generates functions for creating and accessing elements of a C array
 * (as pointers).  Creates the following functions:
 *
 *        TYPE *new_NAME(int nelements)
 *        void delete_NAME(TYPE *);
 *        TYPE NAME_getitem(TYPE *, int index);
 *        void NAME_setitem(TYPE *, int index, TYPE value);
 * 
 * ----------------------------------------------------------------------------- */



































/* -----------------------------------------------------------------------------
 * %array_class(TYPE,NAME)
 *
 * Generates a class wrapper around a C array.  The class has the following
 * interface:
 *
 *          struct NAME {
 *              NAME(int nelements);
 *             ~NAME();
 *              TYPE getitem(int index);
 *              void setitem(int index, TYPE value);
 *              TYPE * cast();
 *              static NAME *frompointer(TYPE *t);
  *         }
 *
 * ----------------------------------------------------------------------------- */














































%endoffile
%endoffile
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\..\carrays.i,21,%array_functions@*/
%{
static int *new_intArray(int nelements) { %}

%{  return new int[nelements](); %}



%{}

static void delete_intArray(int *ary) { %}

%{  delete [] ary; %}



%{}

static int intArray_getitem(int *ary, int index) {
    return ary[index];
}
static void intArray_setitem(int *ary, int index, int value) {
    ary[index] = value;
}
%}

int *new_intArray(int nelements);
void delete_intArray(int *ary);
int intArray_getitem(int *ary, int index);
void intArray_setitem(int *ary, int index, int value);

/*@SWIG@*/
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\..\carrays.i,21,%array_functions@*/
%{
static float *new_floatArray(int nelements) { %}

%{  return new float[nelements](); %}



%{}

static void delete_floatArray(float *ary) { %}

%{  delete [] ary; %}



%{}

static float floatArray_getitem(float *ary, int index) {
    return ary[index];
}
static void floatArray_setitem(float *ary, int index, float value) {
    ary[index] = value;
}
%}

float *new_floatArray(int nelements);
void delete_floatArray(float *ary);
float floatArray_getitem(float *ary, int index);
void floatArray_setitem(float *ary, int index, float value);

/*@SWIG@*/
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\..\carrays.i,21,%array_functions@*/
%{
static double *new_doubleArray(int nelements) { %}

%{  return new double[nelements](); %}



%{}

static void delete_doubleArray(double *ary) { %}

%{  delete [] ary; %}



%{}

static double doubleArray_getitem(double *ary, int index) {
    return ary[index];
}
static void doubleArray_setitem(double *ary, int index, double value) {
    ary[index] = value;
}
%}

double *new_doubleArray(int nelements);
void delete_doubleArray(double *ary);
double doubleArray_getitem(double *ary, int index);
void doubleArray_setitem(double *ary, int index, double value);

/*@SWIG@*/
/*@SWIG:D:\Resource\Program\Portable\Swig\Lib\lua\..\carrays.i,21,%array_functions@*/
%{
static double *new_realArray(int nelements) { %}

%{  return new double[nelements](); %}



%{}

static void delete_realArray(double *ary) { %}

%{  delete [] ary; %}



%{}

static double realArray_getitem(double *ary, int index) {
    return ary[index];
}
static void realArray_setitem(double *ary, int index, double value) {
    ary[index] = value;
}
%}

double *new_realArray(int nelements);
void delete_realArray(double *ary);
double realArray_getitem(double *ary, int index);
void realArray_setitem(double *ary, int index, double value);

/*@SWIG@*/


%includefile "include\\cyclone\\precision.h" %beginfile
/*
 * Interface file for code that changes when the core's precision is
 * altered.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * Because Cyclone is designed to work at either single or double
 * precision, mathematical functions such as sqrt cannot be used
 * in the source code or headers. This file provides defines for
 * the real number type and mathematical formulae that work on it.
 *
 * @note All the contents of this file need to be changed to compile
 * Cyclone at a different precision.
 */





namespace cyclone {









































    
    typedef double real;
    
    
    
    
    
    
    
    
    
    %constant R_PI = 3.14159265358979;

}


%endoffile
%includefile "include\\cyclone\\core.h" %beginfile
/*
 * Interface file for core components and functions.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @mainpage Cyclone Reference
 *
 * Cyclone is a general purpose and robust system for real-time
 * simulation of rigid bodies. The library was designed to be used in
 * computer games, but may be applicable to other areas of simulation
 * or research.
 *
 * @section docs About This Document
 *
 * This documentation contains detailed reference to every aspect of
 * the Cyclone library.
 *
 * @subsection contents Contents
 *
 * Use the navigation system on the left side of the page to view the
 * documentation. The navigation tool uses JavaScript, and requires a
 * version 4 browser or above.
 *
 * All the publically accessible functions and classes of Cyclone
 * are provided in a set of header files. These, and their contents,
 * can be browsed from the File List section.
 *
 * Cyclone is contained in a single namespace, cyclone. Its
 * contents can be viewed in the Compound List section. The Class
 * Hierarchy section provides an alternative way to navigate these
 * classes. The Graphical Class Hierarchy provides an overview of
 * class inheritance.
 *
 * The Compound List section gives an alphabetic list of all symbols
 * in the library, including method names and functions.
 *
 * @subsection graphs Graphs
 *
 * Most of the documentation contains detailed graphical
 * representations of the file and class dependencies. These diagrams
 * are clickable, and provide the fastest mechanism for browsing the
 * documentation. Each diagram is followed by a link to a help file
 * giving a legend.
 *
 * @section use Using Cyclone
 *
 * To set up:
 *
 * @li Create a set of instances of RigidBody.
 *
 * @li Set their mass, inertia tensor, and damping.
 *
 * @li Set their initial location, orientation, velocity and rotation.
 *
 * @li Apply any permanent forces (such as gravity).
 *
 * Then each frame:
 *
 * @li Apply any transient forces (such as springs or thrusts).
 *
 * @li Call eulerIntegrate on each body in turn.
 *
 * @li Fill an array of Contact instances with all contacts on all
 * bodies.
 *
 * @li Call ContactResolver::resolveContacts to resolve the
 * contacts.
 *
 * @li Call calculateInternals to update the bodies' internal
 * properties (such as the transform matrix).
 *
 * @li Render the bodies.
 *
 * @section legal Legal
 *
 * This documentation is distributed under license. Use of this
 * documentation implies agreement with all terms and conditions of
 * the accompanying software and documentation license.
 */



/**
 * @file
 *
 * The core contains utility functions, helpers and a basic set of
 * mathematical types.
 */





/**
 * The cyclone namespace includes all cyclone functions and
 * classes. It is defined as a namespace to allow function and class
 * names to be simple without causing conflicts.
 */
namespace cyclone {

    /**
     * Holds the value for energy under which a body will be put to
     * sleep. This is a global value for the whole solution.  By
     * default it is 0.1, which is fine for simulation when gravity is
     * about 20 units per second squared, masses are about one, and
     * other forces are around that of gravity. It may need tweaking
     * if your simulation is drastically different to this.
     */
    extern real sleepEpsilon;

    /**
     * Sets the current sleep epsilon value: the kinetic energy under
     * which a body may be put to sleep. Bodies are put to sleep if
     * they appear to have a stable kinetic energy less than this
     * value. For simulations that often have low values (such as slow
     * moving, or light objects), this may need reducing.
     *
     * The value is global; all bodies will use it.
     *
     * @see sleepEpsilon
     *
     * @see getSleepEpsilon
     *
     * @param value The sleep epsilon value to use from this point
     * on.
     */
    void setSleepEpsilon(real value);

    /**
     * Gets the current value of the sleep epsilon parameter.
     *
     * @see sleepEpsilon
     *
     * @see setSleepEpsilon
     *
     * @return The current value of the parameter.
     */
    real getSleepEpsilon();

    /**
     * Holds a vector in 3 dimensions. Four data members are allocated
     * to ensure alignment in an array.
     *
     * @note This class contains a lot of inline methods for basic
     * mathematics. The implementations are included in the header
     * file.
     */
    class Vector3
    {
    public:
         /** Holds the value along the x axis. */
        real x;

        /** Holds the value along the y axis. */
        real y;

        /** Holds the value along the z axis. */
        real z;

    private:
        /** Padding to ensure 4 word alignment. */
        real pad{};

    public:
        /** The default constructor creates a zero vector. */
        Vector3() : x(0), y(0), z(0) {}

        /**
         * The explicit constructor creates a vector with the given
         * components.
         */
        Vector3(const real x, const real y, const real z)
            : x(x), y(y), z(z) {}


        // ... Other Vector3 code as before ...


        real operator[](unsigned i) const
        {
            if (i == 0) return x;
            if (i == 1) return y;
            return z;
        }

        real& operator[](unsigned i)
        {
            if (i == 0) return x;
            if (i == 1) return y;
            return z;
        }

        /** Adds the given vector to this. */
        void operator+=(const Vector3& v)
        {
            x += v.x;
            y += v.y;
            z += v.z;
        }

        /**
         * Returns the value of the given vector added to this.
         */
        Vector3 operator+(const Vector3& v) const
        {
            return Vector3(x+v.x, y+v.y, z+v.z);
        }

        /** Subtracts the given vector from this. */
        void operator-=(const Vector3& v)
        {
            x -= v.x;
            y -= v.y;
            z -= v.z;
        }

        /**
         * Returns the value of the given vector subtracted from this.
         */
        Vector3 operator-(const Vector3& v) const
        {
            return Vector3(x-v.x, y-v.y, z-v.z);
        }

        /** Multiplies this vector by the given scalar. */
        void operator*=(const real value)
        {
            x *= value;
            y *= value;
            z *= value;
        }

        /** Returns a copy of this vector scaled the given value. */
        Vector3 operator*(const real value) const
        {
            return Vector3(x*value, y*value, z*value);
        }

        /**
         * Calculates and returns a component-wise product of this
         * vector with the given vector.
         */
        Vector3 componentProduct(const Vector3 &vector) const
        {
            return Vector3(x * vector.x, y * vector.y, z * vector.z);
        }

        /**
         * Performs a component-wise product with the given vector and
         * sets this vector to its result.
         */
        void componentProductUpdate(const Vector3 &vector)
        {
            x *= vector.x;
            y *= vector.y;
            z *= vector.z;
        }

        /**
         * Calculates and returns the vector product of this vector
         * with the given vector.
         */
        Vector3 vectorProduct(const Vector3 &vector) const
        {
            return Vector3(y*vector.z-z*vector.y,
                           z*vector.x-x*vector.z,
                           x*vector.y-y*vector.x);
        }

        /**
         * Updates this vector to be the vector product of its current
         * value and the given vector.
         */
        void operator %=(const Vector3 &vector)
        {
            *this = vectorProduct(vector);
        }

        /**
         * Calculates and returns the vector product of this vector
         * with the given vector.
         */
        Vector3 operator%(const Vector3 &vector) const
        {
            return Vector3(y*vector.z-z*vector.y,
                           z*vector.x-x*vector.z,
                           x*vector.y-y*vector.x);
        }

        /**
         * Calculates and returns the scalar product of this vector
         * with the given vector.
         */
        real scalarProduct(const Vector3 &vector) const
        {
            return x*vector.x + y*vector.y + z*vector.z;
        }

        /**
         * Calculates and returns the scalar product of this vector
         * with the given vector.
         */
        real operator *(const Vector3 &vector) const
        {
            return x*vector.x + y*vector.y + z*vector.z;
        }

        /**
         * Adds the given vector to this, scaled by the given amount.
         */
        void addScaledVector(const Vector3& vector, real scale)
        {
            x += vector.x * scale;
            y += vector.y * scale;
            z += vector.z * scale;
        }

        /** Gets the magnitude of this vector. */
        real magnitude() const
        {
            return sqrt(x*x+y*y+z*z);
        }

        /** Gets the squared magnitude of this vector. */
        real squareMagnitude() const
        {
            return x*x+y*y+z*z;
        }

        /** Limits the size of the vector to the given maximum. */
        void trim(real size)
        {
            if (squareMagnitude() > size*size)
            {
                normalise();
                x *= size;
                y *= size;
                z *= size;
            }
        }

        /** Turns a non-zero vector into a vector of unit length. */
        void normalise()
        {
            real l = magnitude();
            if (l > 0)
            {
                (*this) *= ((real)1)/l;
            }
        }

        /** Returns the normalised version of a vector. */
        Vector3 unit() const
        {
            Vector3 result = *this;
            result.normalise();
            return result;
        }

        /** Checks if the two vectors have identical components. */
        bool operator==(const Vector3& other) const
        {
            return x == other.x &&
                y == other.y &&
                z == other.z;
        }

        /** Checks if the two vectors have non-identical components. */
        bool operator!=(const Vector3& other) const
        {
            return !(*this == other);
        }

        /**
         * Checks if this vector is component-by-component less than
         * the other.
         *
         * @note This does not behave like a single-value comparison:
         * !(a < b) does not imply (b >= a).
         */
        bool operator<(const Vector3& other) const
        {
            return x < other.x && y < other.y && z < other.z;
        }

        /**
         * Checks if this vector is component-by-component less than
         * the other.
         *
         * @note This does not behave like a single-value comparison:
         * !(a < b) does not imply (b >= a).
         */
        bool operator>(const Vector3& other) const
        {
            return x > other.x && y > other.y && z > other.z;
        }

        /**
         * Checks if this vector is component-by-component less than
         * the other.
         *
         * @note This does not behave like a single-value comparison:
         * !(a <= b) does not imply (b > a).
         */
        bool operator<=(const Vector3& other) const
        {
            return x <= other.x && y <= other.y && z <= other.z;
        }

        /**
         * Checks if this vector is component-by-component less than
         * the other.
         *
         * @note This does not behave like a single-value comparison:
         * !(a <= b) does not imply (b > a).
         */
        bool operator>=(const Vector3& other) const
        {
            return x >= other.x && y >= other.y && z >= other.z;
        }

        /** Zero all the components of the vector. */
        void clear()
        {
            x = y = z = 0;
        }

        /** Flips all the components of the vector. */
        void invert()
        {
            x = -x;
            y = -y;
            z = -z;
        }

    };

    /**
     * Holds a three degree of freedom orientation.
     *
     * Quaternions have
     * several mathematical properties that make them useful for
     * representing orientations, but require four items of data to
     * hold the three degrees of freedom. These four items of data can
     * be viewed as the coefficients of a complex number with three
     * imaginary parts. The mathematics of the quaternion is then
     * defined and is roughly correspondent to the math of 3D
     * rotations. A quaternion is only a valid rotation if it is
     * normalised: i.e. it has a length of 1.
     *
     * @note Angular velocity and acceleration can be correctly
     * represented as vectors. Quaternions are only needed for
     * orientation.
     */
    class Quaternion
    {
    public:
        /**
         * Holds the real component of the quaternion.
         */
        real r;

        /**
         * Holds the first complex component of the
         * quaternion.
         */
        real i;

        /**
         * Holds the second complex component of the
         * quaternion.
         */
        real j;

        /**
         * Holds the third complex component of the
         * quaternion.
         */
        real k;

        // ... other Quaternion code as before ...

        /**
         * The default constructor creates a quaternion representing
         * a zero rotation.
         */
        Quaternion() : r(1), i(0), j(0), k(0) {}

        /**
         * The explicit constructor creates a quaternion with the given
         * components.
         *
         * @param r The real component of the rigid body's orientation
         * quaternion.
         *
         * @param i The first complex component of the rigid body's
         * orientation quaternion.
         *
         * @param j The second complex component of the rigid body's
         * orientation quaternion.
         *
         * @param k The third complex component of the rigid body's
         * orientation quaternion.
         *
         * @note The given orientation does not need to be normalised,
         * and can be zero. This function will not alter the given
         * values, or normalise the quaternion. To normalise the
         * quaternion (and make a zero quaternion a legal rotation),
         * use the normalise function.
         *
         * @see normalise
         */
        Quaternion(const real r, const real i, const real j, const real k)
            : r(r), i(i), j(j), k(k)
        {
        }

        /**
         * Normalises the quaternion to unit length, making it a valid
         * orientation quaternion.
         */
        void normalise()
        {
            real d = r*r+i*i+j*j+k*k;

            // Check for zero length quaternion, and use the no-rotation
            // quaternion in that case.
            if (d < DBL_EPSILON) {
                r = 1;
                return;
            }

            d = ((real)1.0)/sqrt(d);
            r *= d;
            i *= d;
            j *= d;
            k *= d;
        }

        /**
         * Multiplies the quaternion by the given quaternion.
         *
         * @param multiplier The quaternion by which to multiply.
         */
        void operator *=(const Quaternion &multiplier)
        {
            Quaternion q = *this;
            r = q.r*multiplier.r - q.i*multiplier.i -
                q.j*multiplier.j - q.k*multiplier.k;
            i = q.r*multiplier.i + q.i*multiplier.r +
                q.j*multiplier.k - q.k*multiplier.j;
            j = q.r*multiplier.j + q.j*multiplier.r +
                q.k*multiplier.i - q.i*multiplier.k;
            k = q.r*multiplier.k + q.k*multiplier.r +
                q.i*multiplier.j - q.j*multiplier.i;
        }

        /**
         * Adds the given vector to this, scaled by the given amount.
         * This is used to update the orientation quaternion by a rotation
         * and time.
         *
         * @param vector The vector to add.
         *
         * @param scale The amount of the vector to add.
         */
        void addScaledVector(const Vector3& vector, real scale)
        {
            Quaternion q(0,
                vector.x * scale,
                vector.y * scale,
                vector.z * scale);
            q *= *this;
            r += q.r * ((real)0.5);
            i += q.i * ((real)0.5);
            j += q.j * ((real)0.5);
            k += q.k * ((real)0.5);
        }

        void rotateByVector(const Vector3& vector)
        {
            Quaternion q(0, vector.x, vector.y, vector.z);
            (*this) *= q;
        }
    };

    /**
     * Holds a transform matrix, consisting of a rotation matrix and
     * a position. The matrix has 12 elements, it is assumed that the
     * remaining four are (0,0,0,1); producing a homogenous matrix.
     */
    class Matrix4
    {
    public:
        /**
         * Holds the transform matrix data in array form.
         */
        real data[12];

        // ... Other Matrix4 code as before ...


        /**
         * Creates an identity matrix.
         */
        Matrix4()
        {
            data[1] = data[2] = data[3] = data[4] = data[6] =
                data[7] = data[8] = data[9] = data[11] = 0;
            data[0] = data[5] = data[10] = 1;
        }

        /**
         * Sets the matrix to be a diagonal matrix with the given coefficients.
         */
        void setDiagonal(real a, real b, real c)
        {
            data[0] = a;
            data[5] = b;
            data[10] = c;
        }

        /**
         * Returns a matrix which is this matrix multiplied by the given
         * other matrix.
         */
        Matrix4 operator*(const Matrix4 &o) const
        {
            Matrix4 result;
            result.data[0] = (o.data[0]*data[0]) + (o.data[4]*data[1]) + (o.data[8]*data[2]);
            result.data[4] = (o.data[0]*data[4]) + (o.data[4]*data[5]) + (o.data[8]*data[6]);
            result.data[8] = (o.data[0]*data[8]) + (o.data[4]*data[9]) + (o.data[8]*data[10]);

            result.data[1] = (o.data[1]*data[0]) + (o.data[5]*data[1]) + (o.data[9]*data[2]);
            result.data[5] = (o.data[1]*data[4]) + (o.data[5]*data[5]) + (o.data[9]*data[6]);
            result.data[9] = (o.data[1]*data[8]) + (o.data[5]*data[9]) + (o.data[9]*data[10]);

            result.data[2] = (o.data[2]*data[0]) + (o.data[6]*data[1]) + (o.data[10]*data[2]);
            result.data[6] = (o.data[2]*data[4]) + (o.data[6]*data[5]) + (o.data[10]*data[6]);
            result.data[10] = (o.data[2]*data[8]) + (o.data[6]*data[9]) + (o.data[10]*data[10]);

            result.data[3] = (o.data[3]*data[0]) + (o.data[7]*data[1]) + (o.data[11]*data[2]) + data[3];
            result.data[7] = (o.data[3]*data[4]) + (o.data[7]*data[5]) + (o.data[11]*data[6]) + data[7];
            result.data[11] = (o.data[3]*data[8]) + (o.data[7]*data[9]) + (o.data[11]*data[10]) + data[11];

            return result;
        }

        /**
         * Transform the given vector by this matrix.
         *
         * @param vector The vector to transform.
         */
        Vector3 operator*(const Vector3 &vector) const
        {
            return Vector3(
                vector.x * data[0] +
                vector.y * data[1] +
                vector.z * data[2] + data[3],

                vector.x * data[4] +
                vector.y * data[5] +
                vector.z * data[6] + data[7],

                vector.x * data[8] +
                vector.y * data[9] +
                vector.z * data[10] + data[11]
            );
        }

        /**
         * Transform the given vector by this matrix.
         *
         * @param vector The vector to transform.
         */
        Vector3 transform(const Vector3 &vector) const
        {
            return (*this) * vector;
        }

        /**
         * Returns the determinant of the matrix.
         */
        real getDeterminant() const;

        /**
         * Sets the matrix to be the inverse of the given matrix.
         *
         * @param m The matrix to invert and use to set this.
         */
        void setInverse(const Matrix4 &m);

        /** Returns a new matrix containing the inverse of this matrix. */
        Matrix4 inverse() const
        {
            Matrix4 result;
            result.setInverse(*this);
            return result;
        }

        /**
         * Inverts the matrix.
         */
        void invert()
        {
            setInverse(*this);
        }

        /**
         * Transform the given direction vector by this matrix.
         *
         * @note When a direction is converted between frames of
         * reference, there is no translation required.
         *
         * @param vector The vector to transform.
         */
        Vector3 transformDirection(const Vector3 &vector) const
        {
            return Vector3(
                vector.x * data[0] +
                vector.y * data[1] +
                vector.z * data[2],

                vector.x * data[4] +
                vector.y * data[5] +
                vector.z * data[6],

                vector.x * data[8] +
                vector.y * data[9] +
                vector.z * data[10]
            );
        }

        /**
         * Transform the given direction vector by the
         * transformational inverse of this matrix.
         *
         * @note This function relies on the fact that the inverse of
         * a pure rotation matrix is its transpose. It separates the
         * translational and rotation components, transposes the
         * rotation, and multiplies out. If the matrix is not a
         * scale and shear free transform matrix, then this function
         * will not give correct results.
         *
         * @note When a direction is converted between frames of
         * reference, there is no translation required.
         *
         * @param vector The vector to transform.
         */
        Vector3 transformInverseDirection(const Vector3 &vector) const
        {
            return Vector3(
                vector.x * data[0] +
                vector.y * data[4] +
                vector.z * data[8],

                vector.x * data[1] +
                vector.y * data[5] +
                vector.z * data[9],

                vector.x * data[2] +
                vector.y * data[6] +
                vector.z * data[10]
            );
        }

        /**
         * Transform the given vector by the transformational inverse
         * of this matrix.
         *
         * @note This function relies on the fact that the inverse of
         * a pure rotation matrix is its transpose. It separates the
         * translational and rotation components, transposes the
         * rotation, and multiplies out. If the matrix is not a
         * scale and shear free transform matrix, then this function
         * will not give correct results.
         *
         * @param vector The vector to transform.
         */
        Vector3 transformInverse(const Vector3 &vector) const
        {
            Vector3 tmp = vector;
            tmp.x -= data[3];
            tmp.y -= data[7];
            tmp.z -= data[11];
            return Vector3(
                tmp.x * data[0] +
                tmp.y * data[4] +
                tmp.z * data[8],

                tmp.x * data[1] +
                tmp.y * data[5] +
                tmp.z * data[9],

                tmp.x * data[2] +
                tmp.y * data[6] +
                tmp.z * data[10]
            );
        }

        /**
         * Gets a vector representing one axis (i.e. one column) in the matrix.
         *
         * @param i The row to return. Row 3 corresponds to the position
         * of the transform matrix.
         *
         * @return The vector.
         */
        Vector3 getAxisVector(int i) const
        {
            return Vector3(data[i], data[i+4], data[i+8]);
        }

        /**
         * Sets this matrix to be the rotation matrix corresponding to
         * the given quaternion.
         */
        void setOrientationAndPos(const Quaternion &q, const Vector3 &pos)
        {
            data[0] = 1 - (2*q.j*q.j + 2*q.k*q.k);
            data[1] = 2*q.i*q.j + 2*q.k*q.r;
            data[2] = 2*q.i*q.k - 2*q.j*q.r;
            data[3] = pos.x;

            data[4] = 2*q.i*q.j - 2*q.k*q.r;
            data[5] = 1 - (2*q.i*q.i  + 2*q.k*q.k);
            data[6] = 2*q.j*q.k + 2*q.i*q.r;
            data[7] = pos.y;

            data[8] = 2*q.i*q.k + 2*q.j*q.r;
            data[9] = 2*q.j*q.k - 2*q.i*q.r;
            data[10] = 1 - (2*q.i*q.i  + 2*q.j*q.j);
            data[11] = pos.z;
        }

        /**
         * Fills the given array with this transform matrix, so it is
         * usable as an open-gl transform matrix. OpenGL uses a column
         * major format, so that the values are transposed as they are
         * written.
         */
        void fillGLArray(float array[16]) const
        {
            array[0] = (float)data[0];
            array[1] = (float)data[4];
            array[2] = (float)data[8];
            array[3] = (float)0;

            array[4] = (float)data[1];
            array[5] = (float)data[5];
            array[6] = (float)data[9];
            array[7] = (float)0;

            array[8] = (float)data[2];
            array[9] = (float)data[6];
            array[10] = (float)data[10];
            array[11] = (float)0;

            array[12] = (float)data[3];
            array[13] = (float)data[7];
            array[14] = (float)data[11];
            array[15] = (float)1;
        }
    };

    /**
     * Holds an inertia tensor, consisting of a 3x3 row-major matrix.
     * This matrix is not padding to produce an aligned structure, since
     * it is most commonly used with a mass (single real) and two
     * damping coefficients to make the 12-element characteristics array
     * of a rigid body.
     */
    class Matrix3
    {
    public:
        /**
         * Holds the tensor matrix data in array form.
         */
        real data[9];

        // ... Other Matrix3 code as before ...

        /**
         * Creates a new matrix.
         */
        Matrix3()
        {
            data[0] = data[1] = data[2] = data[3] = data[4] = data[5] =
                data[6] = data[7] = data[8] = 0;
        }

        /**
         * Creates a new matrix with the given three vectors making
         * up its columns.
         */
        Matrix3(const Vector3 &compOne, const Vector3 &compTwo,
            const Vector3 &compThree)
        {
            setComponents(compOne, compTwo, compThree);
        }

        /**
         * Creates a new matrix with explicit coefficients.
         */
        Matrix3(real c0, real c1, real c2, real c3, real c4, real c5,
            real c6, real c7, real c8)
        {
            data[0] = c0; data[1] = c1; data[2] = c2;
            data[3] = c3; data[4] = c4; data[5] = c5;
            data[6] = c6; data[7] = c7; data[8] = c8;
        }

        /**
         * Sets the matrix to be a diagonal matrix with the given
         * values along the leading diagonal.
         */
        void setDiagonal(real a, real b, real c)
        {
            setInertiaTensorCoeffs(a, b, c);
        }

        /**
         * Sets the value of the matrix from inertia tensor values.
         */
        void setInertiaTensorCoeffs(real ix, real iy, real iz,
            real ixy=0, real ixz=0, real iyz=0)
        {
            data[0] = ix;
            data[1] = data[3] = -ixy;
            data[2] = data[6] = -ixz;
            data[4] = iy;
            data[5] = data[7] = -iyz;
            data[8] = iz;
        }

        /**
         * Sets the value of the matrix as an inertia tensor of
         * a rectangular block aligned with the body's coordinate
         * system with the given axis half-sizes and mass.
         */
        void setBlockInertiaTensor(const Vector3 &halfSizes, real mass)
        {
            Vector3 squares = halfSizes.componentProduct(halfSizes);
            setInertiaTensorCoeffs(0.3f*mass*(squares.y + squares.z),
                0.3f*mass*(squares.x + squares.z),
                0.3f*mass*(squares.x + squares.y));
        }

        /**
         * Sets the matrix to be a skew symmetric matrix based on
         * the given vector. The skew symmetric matrix is the equivalent
         * of the vector product. So if a,b are vectors. a x b = A_s b
         * where A_s is the skew symmetric form of a.
         */
        void setSkewSymmetric(const Vector3 vector)
        {
            data[0] = data[4] = data[8] = 0;
            data[1] = -vector.z;
            data[2] = vector.y;
            data[3] = vector.z;
            data[5] = -vector.x;
            data[6] = -vector.y;
            data[7] = vector.x;
        }

        /**
         * Sets the matrix values from the given three vector components.
         * These are arranged as the three columns of the vector.
         */
        void setComponents(const Vector3 &compOne, const Vector3 &compTwo,
            const Vector3 &compThree)
        {
            data[0] = compOne.x;
            data[1] = compTwo.x;
            data[2] = compThree.x;
            data[3] = compOne.y;
            data[4] = compTwo.y;
            data[5] = compThree.y;
            data[6] = compOne.z;
            data[7] = compTwo.z;
            data[8] = compThree.z;

        }

        /**
         * Transform the given vector by this matrix.
         *
         * @param vector The vector to transform.
         */
        Vector3 operator*(const Vector3 &vector) const
        {
            return Vector3(
                vector.x * data[0] + vector.y * data[1] + vector.z * data[2],
                vector.x * data[3] + vector.y * data[4] + vector.z * data[5],
                vector.x * data[6] + vector.y * data[7] + vector.z * data[8]
            );
        }

        /**
         * Transform the given vector by this matrix.
         *
         * @param vector The vector to transform.
         */
        Vector3 transform(const Vector3 &vector) const
        {
            return (*this) * vector;
        }

        /**
         * Transform the given vector by the transpose of this matrix.
         *
         * @param vector The vector to transform.
         */
        Vector3 transformTranspose(const Vector3 &vector) const
        {
            return Vector3(
                vector.x * data[0] + vector.y * data[3] + vector.z * data[6],
                vector.x * data[1] + vector.y * data[4] + vector.z * data[7],
                vector.x * data[2] + vector.y * data[5] + vector.z * data[8]
            );
        }

        /**
         * Gets a vector representing one row in the matrix.
         *
         * @param i The row to return.
         */
        Vector3 getRowVector(int i) const
        {
            return Vector3(data[i*3], data[i*3+1], data[i*3+2]);
        }

        /**
         * Gets a vector representing one axis (i.e. one column) in the matrix.
         *
         * @param i The row to return.
         *
         * @return The vector.
         */
        Vector3 getAxisVector(int i) const
        {
            return Vector3(data[i], data[i+3], data[i+6]);
        }

        /**
         * Sets the matrix to be the inverse of the given matrix.
         *
         * @param m The matrix to invert and use to set this.
         */
        void setInverse(const Matrix3 &m)
        {
            real t4 = m.data[0]*m.data[4];
            real t6 = m.data[0]*m.data[5];
            real t8 = m.data[1]*m.data[3];
            real t10 = m.data[2]*m.data[3];
            real t12 = m.data[1]*m.data[6];
            real t14 = m.data[2]*m.data[6];

            // Calculate the determinant
            real t16 = (t4*m.data[8] - t6*m.data[7] - t8*m.data[8]+
                        t10*m.data[7] + t12*m.data[5] - t14*m.data[4]);

            // Make sure the determinant is non-zero.
            if (t16 == (real)0.0f) return;
            real t17 = 1/t16;

            data[0] = (m.data[4]*m.data[8]-m.data[5]*m.data[7])*t17;
            data[1] = -(m.data[1]*m.data[8]-m.data[2]*m.data[7])*t17;
            data[2] = (m.data[1]*m.data[5]-m.data[2]*m.data[4])*t17;
            data[3] = -(m.data[3]*m.data[8]-m.data[5]*m.data[6])*t17;
            data[4] = (m.data[0]*m.data[8]-t14)*t17;
            data[5] = -(t6-t10)*t17;
            data[6] = (m.data[3]*m.data[7]-m.data[4]*m.data[6])*t17;
            data[7] = -(m.data[0]*m.data[7]-t12)*t17;
            data[8] = (t4-t8)*t17;
        }

        /** Returns a new matrix containing the inverse of this matrix. */
        Matrix3 inverse() const
        {
            Matrix3 result;
            result.setInverse(*this);
            return result;
        }

        /**
         * Inverts the matrix.
         */
        void invert()
        {
            setInverse(*this);
        }

        /**
         * Sets the matrix to be the transpose of the given matrix.
         *
         * @param m The matrix to transpose and use to set this.
         */
        void setTranspose(const Matrix3 &m)
        {
            data[0] = m.data[0];
            data[1] = m.data[3];
            data[2] = m.data[6];
            data[3] = m.data[1];
            data[4] = m.data[4];
            data[5] = m.data[7];
            data[6] = m.data[2];
            data[7] = m.data[5];
            data[8] = m.data[8];
        }

        /** Returns a new matrix containing the transpose of this matrix. */
        Matrix3 transpose() const
        {
            Matrix3 result;
            result.setTranspose(*this);
            return result;
        }

        /**
         * Returns a matrix which is this matrix multiplied by the given
         * other matrix.
         */
        Matrix3 operator*(const Matrix3 &o) const
        {
            return Matrix3(
                data[0]*o.data[0] + data[1]*o.data[3] + data[2]*o.data[6],
                data[0]*o.data[1] + data[1]*o.data[4] + data[2]*o.data[7],
                data[0]*o.data[2] + data[1]*o.data[5] + data[2]*o.data[8],

                data[3]*o.data[0] + data[4]*o.data[3] + data[5]*o.data[6],
                data[3]*o.data[1] + data[4]*o.data[4] + data[5]*o.data[7],
                data[3]*o.data[2] + data[4]*o.data[5] + data[5]*o.data[8],

                data[6]*o.data[0] + data[7]*o.data[3] + data[8]*o.data[6],
                data[6]*o.data[1] + data[7]*o.data[4] + data[8]*o.data[7],
                data[6]*o.data[2] + data[7]*o.data[5] + data[8]*o.data[8]
                );
        }

        /**
         * Multiplies this matrix in place by the given other matrix.
         */
        void operator*=(const Matrix3 &o)
        {
            real t1;
            real t2;
            real t3;

            t1 = data[0]*o.data[0] + data[1]*o.data[3] + data[2]*o.data[6];
            t2 = data[0]*o.data[1] + data[1]*o.data[4] + data[2]*o.data[7];
            t3 = data[0]*o.data[2] + data[1]*o.data[5] + data[2]*o.data[8];
            data[0] = t1;
            data[1] = t2;
            data[2] = t3;

            t1 = data[3]*o.data[0] + data[4]*o.data[3] + data[5]*o.data[6];
            t2 = data[3]*o.data[1] + data[4]*o.data[4] + data[5]*o.data[7];
            t3 = data[3]*o.data[2] + data[4]*o.data[5] + data[5]*o.data[8];
            data[3] = t1;
            data[4] = t2;
            data[5] = t3;

            t1 = data[6]*o.data[0] + data[7]*o.data[3] + data[8]*o.data[6];
            t2 = data[6]*o.data[1] + data[7]*o.data[4] + data[8]*o.data[7];
            t3 = data[6]*o.data[2] + data[7]*o.data[5] + data[8]*o.data[8];
            data[6] = t1;
            data[7] = t2;
            data[8] = t3;
        }

        /**
         * Multiplies this matrix in place by the given scalar.
         */
        void operator*=(const real scalar)
        {
            data[0] *= scalar; data[1] *= scalar; data[2] *= scalar;
            data[3] *= scalar; data[4] *= scalar; data[5] *= scalar;
            data[6] *= scalar; data[7] *= scalar; data[8] *= scalar;
        }

        /**
         * Does a component-wise addition of this matrix and the given
         * matrix.
         */
        void operator+=(const Matrix3 &o)
        {
            data[0] += o.data[0]; data[1] += o.data[1]; data[2] += o.data[2];
            data[3] += o.data[3]; data[4] += o.data[4]; data[5] += o.data[5];
            data[6] += o.data[6]; data[7] += o.data[7]; data[8] += o.data[8];
        }

        /**
         * Sets this matrix to be the rotation matrix corresponding to
         * the given quaternion.
         */
        void setOrientation(const Quaternion &q)
        {
            data[0] = 1 - (2*q.j*q.j + 2*q.k*q.k);
            data[1] = 2*q.i*q.j + 2*q.k*q.r;
            data[2] = 2*q.i*q.k - 2*q.j*q.r;
            data[3] = 2*q.i*q.j - 2*q.k*q.r;
            data[4] = 1 - (2*q.i*q.i  + 2*q.k*q.k);
            data[5] = 2*q.j*q.k + 2*q.i*q.r;
            data[6] = 2*q.i*q.k + 2*q.j*q.r;
            data[7] = 2*q.j*q.k - 2*q.i*q.r;
            data[8] = 1 - (2*q.i*q.i  + 2*q.j*q.j);
        }

        /**
         * Interpolates a couple of matrices.
         */
        static Matrix3 linearInterpolate(const Matrix3& a, const Matrix3& b, real prop);
    };

}


%endoffile
%includefile "include\\cyclone\\random.h" %beginfile
/*
 * Interface file for the random number generator.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains the definitions for a random number generator.
 */





namespace cyclone {


    /**
     * Keeps track of one random stream: i.e. a seed and its output.
     * This is used to get random numbers. Rather than a funcion, this
     * allows there to be several streams of repeatable random numbers
     * at the same time. Uses the RandRotB algorithm.
     */
    class Random
    {
    public:
    	/**
    	 * left bitwise rotation
    	 */

    	unsigned rotl(unsigned n, unsigned r);
    	/**
    	 * right bitwise rotation
    	 */
    	unsigned rotr(unsigned n, unsigned r);

        /**
         * Creates a new random number stream with a seed based on
         * timing data.
         */
        Random();

        /**
         * Creates a new random stream with the given seed.
         */
        Random(unsigned seed);

        /**
         * Sets the seed value for the random stream.
         */
        void seed(unsigned seed);

        /**
         * Returns the next random bitstring from the stream. This is
         * the fastest method.
         */
        unsigned randomBits();

        /**
         * Returns a random floating point number between 0 and 1.
         */
        real randomReal();

        /**
         * Returns a random floating point number between 0 and scale.
         */
        real randomReal(real scale);

        /**
         * Returns a random floating point number between min and max.
         */
        real randomReal(real min, real max);

        /**
         * Returns a random integer less than the given value.
         */
        unsigned randomInt(unsigned max);

        /**
         * Returns a random binomially distributed number between -scale
         * and +scale.
         */
        real randomBinomial(real scale);

        /**
         * Returns a random vector where each component is binomially
         * distributed in the range (-scale to scale) [mean = 0.0f].
         */
        Vector3 randomVector(real scale);

        /**
         * Returns a random vector where each component is binomially
         * distributed in the range (-scale to scale) [mean = 0.0f],
         * where scale is the corresponding component of the given
         * vector.
         */
        Vector3 randomVector(const Vector3 &scale);

        /**
         * Returns a random vector in the cube defined by the given
         * minimum and maximum vectors. The probability is uniformly
         * distributed in this region.
         */
        Vector3 randomVector(const Vector3 &min, const Vector3 &max);

        /**
         * Returns a random vector where each component is binomially
         * distributed in the range (-scale to scale) [mean = 0.0f],
         * except the y coordinate which is zero.
         */
        Vector3 randomXZVector(real scale);

        /**
         * Returns a random orientation (i.e. normalized) quaternion.
         */
        Quaternion randomQuaternion();

    private:
        // Internal mechanics
        int p1, p2;
        unsigned buffer[17];
    };

} // namespace cyclone


%endoffile
%includefile "include\\cyclone\\particle.h" %beginfile
/*
 * Interface file for the particle class.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains the definitions for the paticle class, which can
 * be used in place of rigid bodies for simpler simulations or
 * assemblies.
 */





namespace cyclone {

    /**
     * A particle is the simplest object that can be simulated in the
     * physics system.
     *
     * It has position data (no orientation data), along with
     * velocity. It can be integrated forward through time, and have
     * linear forces, and impulses applied to it. The particle manages
     * its state and allows access through a set of methods.
     */
    class Particle
    {
    public:

        // ... Other Particle code as before ...


    protected:
        /**
         * @name Characteristic Data and State
         *
         * This data holds the state of the particle. There are two
         * sets of data: characteristics and state.
         *
         * Characteristics are properties of the particle
         * independent of its current kinematic situation. This
         * includes mass, moment of inertia and damping
         * properties. Two identical particles will have the same
         * values for their characteristics.
         *
         * State includes all the characteristics and also includes
         * the kinematic situation of the particle in the current
         * simulation. By setting the whole state data, a particle's
         * exact game state can be replicated. Note that state does
         * not include any forces applied to the body. Two identical
         * rigid bodies in the same simulation will not share the same
         * state values.
         *
         * The state values make up the smallest set of independent
         * data for the particle. Other state data is calculated
         * from their current values. When state data is changed the
         * dependent values need to be updated: this can be achieved
         * either by integrating the simulation, or by calling the
         * calculateInternals function. This two stage process is used
         * because recalculating internals can be a costly process:
         * all state changes should be carried out at the same time,
         * allowing for a single call.
         *
         * @see calculateInternals
         */
        /*@{*/

        /**
         * Holds the inverse of the mass of the particle. It
         * is more useful to hold the inverse mass because
         * integration is simpler, and because in real time
         * simulation it is more useful to have objects with
         * infinite mass (immovable) than zero mass
         * (completely unstable in numerical simulation).
         */
        real inverseMass;

        /**
         * Holds the amount of damping applied to linear
         * motion. Damping is required to remove energy added
         * through numerical instability in the integrator.
         */
        real damping;

        /**
         * Holds the linear position of the particle in
         * world space.
         */
        Vector3 position;

        /**
         * Holds the linear velocity of the particle in
         * world space.
         */
        Vector3 velocity;

        /*@}*/

        /**
         * @name Force Accumulators
         *
         * These data members store the current force and
         * global linear acceleration of the particle.
         */

        /*@{*/

        /**
         * Holds the accumulated force to be applied at the next
         * simulation iteration only. This value is zeroed at each
         * integration step.
         */
        Vector3 forceAccum;

        /**
         * Holds the acceleration of the particle.  This value
         * can be used to set acceleration due to gravity (its primary
         * use), or any other constant acceleration.
         */
        Vector3 acceleration;

        /*@}*/

    public:
        /**
         * @name Constructor and Destructor
         *
         * There are no data members in the particle class that are
         * created on the heap. So all data storage is handled
         * automatically.
         */
        /*@{*/
        /*@}*/

        /**
         * @name Integration and Simulation Functions
         *
         * These functions are used to simulate the particle's
         * motion over time. A normal application sets up one or more
         * rigid bodies, applies permanent forces (i.e. gravity), then
         * adds transient forces each frame, and integrates, prior to
         * rendering.
         *
         * Currently the only integration function provided is the
         * first order Newton Euler method.
         */
        /*@{*/

        /**
         * Integrates the particle forward in time by the given amount.
         * This function uses a Newton-Euler integration method, which is a
         * linear approximation to the correct integral. For this reason it
         * may be inaccurate in some cases.
         */
        void integrate(real duration);

        /*@}*/


        /**
         * @name Accessor Functions for the Particle's State
         *
         * These functions provide access to the particle's
         * characteristics or state.
         */
        /*@{*/

        /**
         * Sets the mass of the particle.
         *
         * @param mass The new mass of the body. This may not be zero.
         * Small masses can produce unstable rigid bodies under
         * simulation.
         *
         * @warning This invalidates internal data for the particle.
         * Either an integration function, or the calculateInternals
         * function should be called before trying to get any settings
         * from the particle.
         */
        void setMass(const real mass);

        /**
         * Gets the mass of the particle.
         *
         * @return The current mass of the particle.
         */
        real getMass() const;

        /**
         * Sets the inverse mass of the particle.
         *
         * @param inverseMass The new inverse mass of the body. This
         * may be zero, for a body with infinite mass
         * (i.e. unmovable).
         *
         * @warning This invalidates internal data for the particle.
         * Either an integration function, or the calculateInternals
         * function should be called before trying to get any settings
         * from the particle.
         */
        void setInverseMass(const real inverseMass);

        /**
         * Gets the inverse mass of the particle.
         *
         * @return The current inverse mass of the particle.
         */
        real getInverseMass() const;

        /**
         * Returns true if the mass of the particle is not-infinite.
         */
        bool hasFiniteMass() const;

        /**
         * Sets both the damping of the particle.
         */
        void setDamping(const real damping);

        /**
         * Gets the current damping value.
         */
        real getDamping() const;

        /**
         * Sets the position of the particle.
         *
         * @param position The new position of the particle.
         */
        void setPosition(const Vector3 &position);

        /**
         * Sets the position of the particle by component.
         *
         * @param x The x coordinate of the new position of the rigid
         * body.
         *
         * @param y The y coordinate of the new position of the rigid
         * body.
         *
         * @param z The z coordinate of the new position of the rigid
         * body.
         */
        void setPosition(const real x, const real y, const real z);

        /**
         * Fills the given vector with the position of the particle.
         *
         * @param position A pointer to a vector into which to write
         * the position.
         */
        void getPosition(Vector3 *position) const;

        /**
         * Gets the position of the particle.
         *
         * @return The position of the particle.
         */
        Vector3 getPosition() const;

        /**
         * Sets the velocity of the particle.
         *
         * @param velocity The new velocity of the particle.
         */
        void setVelocity(const Vector3 &velocity);

        /**
         * Sets the velocity of the particle by component.
         *
         * @param x The x coordinate of the new velocity of the rigid
         * body.
         *
         * @param y The y coordinate of the new velocity of the rigid
         * body.
         *
         * @param z The z coordinate of the new velocity of the rigid
         * body.
         */
        void setVelocity(const real x, const real y, const real z);

        /**
         * Fills the given vector with the velocity of the particle.
         *
         * @param velocity A pointer to a vector into which to write
         * the velocity. The velocity is given in world local space.
         */
        void getVelocity(Vector3 *velocity) const;

        /**
         * Gets the velocity of the particle.
         *
         * @return The velocity of the particle. The velocity is
         * given in world local space.
         */
        Vector3 getVelocity() const;

        /**
         * Sets the constant acceleration of the particle.
         *
         * @param acceleration The new acceleration of the particle.
         */
        void setAcceleration(const Vector3 &acceleration);

        /**
         * Sets the constant acceleration of the particle by component.
         *
         * @param x The x coordinate of the new acceleration of the rigid
         * body.
         *
         * @param y The y coordinate of the new acceleration of the rigid
         * body.
         *
         * @param z The z coordinate of the new acceleration of the rigid
         * body.
         */
        void setAcceleration(const real x, const real y, const real z);

        /**
         * Fills the given vector with the acceleration of the particle.
         *
         * @param acceleration A pointer to a vector into which to write
         * the acceleration. The acceleration is given in world local space.
         */
        void getAcceleration(Vector3 *acceleration) const;

        /**
         * Gets the acceleration of the particle.
         *
         * @return The acceleration of the particle. The acceleration is
         * given in world local space.
         */
        Vector3 getAcceleration() const;

        /*@}*/

        /**
         * @name Force Set-up Functions
         *
         * These functions set up forces to apply to the
         * particle.
         */
        /*@{*/

        /**
         * Clears the forces applied to the particle. This will be
         * called automatically after each integration step.
         */
        void clearAccumulator();

        /**
         * Adds the given force to the particle, to be applied at the
         * next iteration only.
         *
         * @param force The force to apply.
         */
        void addForce(const Vector3 &force);


    };
}


%endoffile
%includefile "include\\cyclone\\body.h" %beginfile
/*
 * Interface file for the rigid body class.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains the definitions for the rigid body class, the
 * basic building block of all the physics system.
 */





namespace cyclone {

    /**
     * A rigid body is the basic simulation object in the physics
     * core.
     *
     * It has position and orientation data, along with first
     * derivatives. It can be integrated forward through time, and
     * have forces, torques and impulses (linear or angular) applied
     * to it. The rigid body manages its state and allows access
     * through a set of methods.
     *
     * A ridid body contains 64 words (the size of which is given
     * by the precision: sizeof(real)). It contains no virtual
     * functions, so should take up exactly 64 words in memory. Of
     * this total 15 words are padding, distributed among the
     * Vector3 data members.
     */
    class RigidBody
    {
    public:

        // ... Other RigidBody code as before ...


    protected:
        /**
         * @name Characteristic Data and State
         *
         * This data holds the state of the rigid body. There are two
         * sets of data: characteristics and state.
         *
         * Characteristics are properties of the rigid body
         * independent of its current kinematic situation. This
         * includes mass, moment of inertia and damping
         * properties. Two identical rigid bodys will have the same
         * values for their characteristics.
         *
         * State includes all the characteristics and also includes
         * the kinematic situation of the rigid body in the current
         * simulation. By setting the whole state data, a rigid body's
         * exact game state can be replicated. Note that state does
         * not include any forces applied to the body. Two identical
         * rigid bodies in the same simulation will not share the same
         * state values.
         *
         * The state values make up the smallest set of independent
         * data for the rigid body. Other state data is calculated
         * from their current values. When state data is changed the
         * dependent values need to be updated: this can be achieved
         * either by integrating the simulation, or by calling the
         * calculateInternals function. This two stage process is used
         * because recalculating internals can be a costly process:
         * all state changes should be carried out at the same time,
         * allowing for a single call.
         *
         * @see calculateInternals
         */
        /*@{*/
        /**
         * Holds the inverse of the mass of the rigid body. It
         * is more useful to hold the inverse mass because
         * integration is simpler, and because in real time
         * simulation it is more useful to have bodies with
         * infinite mass (immovable) than zero mass
         * (completely unstable in numerical simulation).
         */
        real inverseMass;

        /**
         * Holds the inverse of the body's inertia tensor. The
         * inertia tensor provided must not be degenerate
         * (that would mean the body had zero inertia for
         * spinning along one axis). As long as the tensor is
         * finite, it will be invertible. The inverse tensor
         * is used for similar reasons to the use of inverse
         * mass.
         *
         * The inertia tensor, unlike the other variables that
         * define a rigid body, is given in body space.
         *
         * @see inverseMass
         */
        Matrix3 inverseInertiaTensor;

        /**
         * Holds the amount of damping applied to linear
         * motion.  Damping is required to remove energy added
         * through numerical instability in the integrator.
         */
        real linearDamping;

        /**
         * Holds the amount of damping applied to angular
         * motion.  Damping is required to remove energy added
         * through numerical instability in the integrator.
         */
        real angularDamping;

        /**
         * Holds the linear position of the rigid body in
         * world space.
         */
        Vector3 position;

        /**
         * Holds the angular orientation of the rigid body in
         * world space.
         */
        Quaternion orientation;

        /**
         * Holds the linear velocity of the rigid body in
         * world space.
         */
        Vector3 velocity;

        /**
         * Holds the angular velocity, or rotation, or the
         * rigid body in world space.
         */
        Vector3 rotation;

        /*@}*/


        /**
         * @name Derived Data
         *
         * These data members hold information that is derived from
         * the other data in the class.
         */
        /*@{*/

        /**
         * Holds the inverse inertia tensor of the body in world
         * space. The inverse inertia tensor member is specified in
         * the body's local space.
         *
         * @see inverseInertiaTensor
         */
        Matrix3 inverseInertiaTensorWorld;

        /**
         * Holds the amount of motion of the body. This is a recency
         * weighted mean that can be used to put a body to sleap.
         */
        real motion;

        /**
         * A body can be put to sleep to avoid it being updated
         * by the integration functions or affected by collisions
         * with the world.
         */
        bool isAwake;

        /**
         * Some bodies may never be allowed to fall asleep.
         * User controlled bodies, for example, should be
         * always awake.
         */
        bool canSleep;

        /**
         * Holds a transform matrix for converting body space into
         * world space and vice versa. This can be achieved by calling
         * the getPointIn*Space functions.
         *
         * @see getPointInLocalSpace
         * @see getPointInWorldSpace
         * @see getTransform
         */
        Matrix4 transformMatrix;

        /*@}*/


        /**
         * @name Force and Torque Accumulators
         *
         * These data members store the current force, torque and
         * acceleration of the rigid body. Forces can be added to the
         * rigid body in any order, and the class decomposes them into
         * their constituents, accumulating them for the next
         * simulation step. At the simulation step, the accelerations
         * are calculated and stored to be applied to the rigid body.
         */
        /*@{*/

        /**
         * Holds the accumulated force to be applied at the next
         * integration step.
         */
        Vector3 forceAccum;

        /**
         * Holds the accumulated torque to be applied at the next
         * integration step.
         */
        Vector3 torqueAccum;

       /**
         * Holds the acceleration of the rigid body.  This value
         * can be used to set acceleration due to gravity (its primary
         * use), or any other constant acceleration.
         */
        Vector3 acceleration;

        /**
         * Holds the linear acceleration of the rigid body, for the
         * previous frame.
         */
        Vector3 lastFrameAcceleration;

        /*@}*/

    public:
        /**
         * @name Constructor and Destructor
         *
         * There are no data members in the rigid body class that are
         * created on the heap. So all data storage is handled
         * automatically.
         */
        /*@{*/

        /*@}*/


        /**
         * @name Integration and Simulation Functions
         *
         * These functions are used to simulate the rigid body's
         * motion over time. A normal application sets up one or more
         * rigid bodies, applies permanent forces (i.e. gravity), then
         * adds transient forces each frame, and integrates, prior to
         * rendering.
         *
         * Currently the only integration function provided is the
         * first order Newton Euler method.
         */
        /*@{*/

        /**
         * Calculates internal data from state data. This should be called
         * after the body's state is altered directly (it is called
         * automatically during integration). If you change the body's state
         * and then intend to integrate before querying any data (such as
         * the transform matrix), then you can ommit this step.
         */
        void calculateDerivedData();

        /**
         * Integrates the rigid body forward in time by the given amount.
         * This function uses a Newton-Euler integration method, which is a
         * linear approximation to the correct integral. For this reason it
         * may be inaccurate in some cases.
         */
        void integrate(real duration);

        /*@}*/


        /**
         * @name Accessor Functions for the Rigid Body's State
         *
         * These functions provide access to the rigid body's
         * characteristics or state. These data can be accessed
         * individually, or en masse as an array of values
         * (e.g. getCharacteristics, getState). When setting new data,
         * make sure the calculateInternals function, or an
         * integration routine, is called before trying to get data
         * from the body, since the class contains a number of
         * dependent values that will need recalculating.
         */
        /*@{*/

        /**
         * Sets the mass of the rigid body.
         *
         * @param mass The new mass of the body. This may not be zero.
         * Small masses can produce unstable rigid bodies under
         * simulation.
         *
         * @warning This invalidates internal data for the rigid body.
         * Either an integration function, or the calculateInternals
         * function should be called before trying to get any settings
         * from the rigid body.
         */
        void setMass(const real mass);

        /**
         * Gets the mass of the rigid body.
         *
         * @return The current mass of the rigid body.
         */
        real getMass() const;

        /**
         * Sets the inverse mass of the rigid body.
         *
         * @param inverseMass The new inverse mass of the body. This
         * may be zero, for a body with infinite mass
         * (i.e. unmovable).
         *
         * @warning This invalidates internal data for the rigid body.
         * Either an integration function, or the calculateInternals
         * function should be called before trying to get any settings
         * from the rigid body.
         */
        void setInverseMass(const real inverseMass);

        /**
         * Gets the inverse mass of the rigid body.
         *
         * @return The current inverse mass of the rigid body.
         */
        real getInverseMass() const;

        /**
         * Returns true if the mass of the body is not-infinite.
         */
        bool hasFiniteMass() const;

        /**
         * Sets the intertia tensor for the rigid body.
         *
         * @param inertiaTensor The inertia tensor for the rigid
         * body. This must be a full rank matrix and must be
         * invertible.
         *
         * @warning This invalidates internal data for the rigid body.
         * Either an integration function, or the calculateInternals
         * function should be called before trying to get any settings
         * from the rigid body.
         */
        void setInertiaTensor(const Matrix3 &inertiaTensor);

        /**
         * Copies the current inertia tensor of the rigid body into
         * the given matrix.
         *
         * @param inertiaTensor A pointer to a matrix to hold the
         * current inertia tensor of the rigid body. The inertia
         * tensor is expressed in the rigid body's local space.
         */
        void getInertiaTensor(Matrix3 *inertiaTensor) const;

        /**
         * Gets a copy of the current inertia tensor of the rigid body.
         *
         * @return A new matrix containing the current intertia
         * tensor. The inertia tensor is expressed in the rigid body's
         * local space.
         */
        Matrix3 getInertiaTensor() const;

        /**
         * Copies the current inertia tensor of the rigid body into
         * the given matrix.
         *
         * @param inertiaTensor A pointer to a matrix to hold the
         * current inertia tensor of the rigid body. The inertia
         * tensor is expressed in world space.
         */
        void getInertiaTensorWorld(Matrix3 *inertiaTensor) const;

        /**
         * Gets a copy of the current inertia tensor of the rigid body.
         *
         * @return A new matrix containing the current intertia
         * tensor. The inertia tensor is expressed in world space.
         */
        Matrix3 getInertiaTensorWorld() const;

        /**
         * Sets the inverse intertia tensor for the rigid body.
         *
         * @param inverseInertiaTensor The inverse inertia tensor for
         * the rigid body. This must be a full rank matrix and must be
         * invertible.
         *
         * @warning This invalidates internal data for the rigid body.
         * Either an integration function, or the calculateInternals
         * function should be called before trying to get any settings
         * from the rigid body.
         */
        void setInverseInertiaTensor(const Matrix3 &inverseInertiaTensor);

        /**
         * Copies the current inverse inertia tensor of the rigid body
         * into the given matrix.
         *
         * @param inverseInertiaTensor A pointer to a matrix to hold
         * the current inverse inertia tensor of the rigid body. The
         * inertia tensor is expressed in the rigid body's local
         * space.
         */
        void getInverseInertiaTensor(Matrix3 *inverseInertiaTensor) const;

        /**
         * Gets a copy of the current inverse inertia tensor of the
         * rigid body.
         *
         * @return A new matrix containing the current inverse
         * intertia tensor. The inertia tensor is expressed in the
         * rigid body's local space.
         */
        Matrix3 getInverseInertiaTensor() const;

        /**
         * Copies the current inverse inertia tensor of the rigid body
         * into the given matrix.
         *
         * @param inverseInertiaTensor A pointer to a matrix to hold
         * the current inverse inertia tensor of the rigid body. The
         * inertia tensor is expressed in world space.
         */
        void getInverseInertiaTensorWorld(Matrix3 *inverseInertiaTensor) const;

        /**
         * Gets a copy of the current inverse inertia tensor of the
         * rigid body.
         *
         * @return A new matrix containing the current inverse
         * intertia tensor. The inertia tensor is expressed in world
         * space.
         */
        Matrix3 getInverseInertiaTensorWorld() const;

        /**
         * Sets both linear and angular damping in one function call.
         *
         * @param linearDamping The speed that velocity is shed from
         * the rigid body.
         *
         * @param angularDamping The speed that rotation is shed from
         * the rigid body.
         *
         * @see setLinearDamping
         * @see setAngularDamping
         */
        void setDamping(const real linearDamping, const real angularDamping);

        /**
         * Sets the linear damping for the rigid body.
         *
         * @param linearDamping The speed that velocity is shed from
         * the rigid body.
         *
         * @see setAngularDamping
         */
        void setLinearDamping(const real linearDamping);

        /**
         * Gets the current linear damping value.
         *
         * @return The current linear damping value.
         */
        real getLinearDamping() const;

        /**
         * Sets the angular damping for the rigid body.
         *
         * @param angularDamping The speed that rotation is shed from
         * the rigid body.
         *
         * @see setLinearDamping
         */
        void setAngularDamping(const real angularDamping);

        /**
         * Gets the current angular damping value.
         *
         * @return The current angular damping value.
         */
        real getAngularDamping() const;

        /**
         * Sets the position of the rigid body.
         *
         * @param position The new position of the rigid body.
         */
        void setPosition(const Vector3 &position);

        /**
         * Sets the position of the rigid body by component.
         *
         * @param x The x coordinate of the new position of the rigid
         * body.
         *
         * @param y The y coordinate of the new position of the rigid
         * body.
         *
         * @param z The z coordinate of the new position of the rigid
         * body.
         */
        void setPosition(const real x, const real y, const real z);

        /**
         * Fills the given vector with the position of the rigid body.
         *
         * @param position A pointer to a vector into which to write
         * the position.
         */
        void getPosition(Vector3 *position) const;

        /**
         * Gets the position of the rigid body.
         *
         * @return The position of the rigid body.
         */
        Vector3 getPosition() const;

        /**
         * Sets the orientation of the rigid body.
         *
         * @param orientation The new orientation of the rigid body.
         *
         * @note The given orientation does not need to be normalised,
         * and can be zero. This function automatically constructs a
         * valid rotation quaternion with (0,0,0,0) mapping to
         * (1,0,0,0).
         */
        void setOrientation(const Quaternion &orientation);

        /**
         * Sets the orientation of the rigid body by component.
         *
         * @param r The real component of the rigid body's orientation
         * quaternion.
         *
         * @param i The first complex component of the rigid body's
         * orientation quaternion.
         *
         * @param j The second complex component of the rigid body's
         * orientation quaternion.
         *
         * @param k The third complex component of the rigid body's
         * orientation quaternion.
         *
         * @note The given orientation does not need to be normalised,
         * and can be zero. This function automatically constructs a
         * valid rotation quaternion with (0,0,0,0) mapping to
         * (1,0,0,0).
         */
        void setOrientation(const real r, const real i,
            const real j, const real k);

        /**
         * Fills the given quaternion with the current value of the
         * rigid body's orientation.
         *
         * @param orientation A pointer to a quaternion to receive the
         * orientation data.
         */
        void getOrientation(Quaternion *orientation) const;

        /**
         * Gets the orientation of the rigid body.
         *
         * @return The orientation of the rigid body.
         */
        Quaternion getOrientation() const;

        /**
         * Fills the given matrix with a transformation representing
         * the rigid body's orientation.
         *
         * @note Transforming a direction vector by this matrix turns
         * it from the body's local space to world space.
         *
         * @param matrix A pointer to the matrix to fill.
         */
        void getOrientation(Matrix3 *matrix) const;

        /**
         * Fills the given matrix data structure with a transformation
         * representing the rigid body's orientation.
         *
         * @note Transforming a direction vector by this matrix turns
         * it from the body's local space to world space.
         *
         * @param matrix A pointer to the matrix to fill.
         */
        void getOrientation(real matrix[9]) const;

        /**
         * Fills the given matrix with a transformation representing
         * the rigid body's position and orientation.
         *
         * @note Transforming a vector by this matrix turns it from
         * the body's local space to world space.
         *
         * @param transform A pointer to the matrix to fill.
         */
        void getTransform(Matrix4 *transform) const;

        /**
         * Fills the given matrix data structure with a
         * transformation representing the rigid body's position and
         * orientation.
         *
         * @note Transforming a vector by this matrix turns it from
         * the body's local space to world space.
         *
         * @param matrix A pointer to the matrix to fill.
         */
        void getTransform(real matrix[16]) const;

        /**
         * Fills the given matrix data structure with a
         * transformation representing the rigid body's position and
         * orientation. The matrix is transposed from that returned
         * by getTransform. This call returns a matrix suitable
         * for applying as an OpenGL transform.
         *
         * @note Transforming a vector by this matrix turns it from
         * the body's local space to world space.
         *
         * @param matrix A pointer to the matrix to fill.
         */
        void getGLTransform(float matrix[16]) const;

        /**
         * Gets a transformation representing the rigid body's
         * position and orientation.
         *
         * @note Transforming a vector by this matrix turns it from
         * the body's local space to world space.
         *
         * @return The transform matrix for the rigid body.
         */
        Matrix4 getTransform() const;

        /**
         * Converts the given point from world space into the body's
         * local space.
         *
         * @param point The point to covert, given in world space.
         *
         * @return The converted point, in local space.
         */
        Vector3 getPointInLocalSpace(const Vector3 &point) const;

        /**
         * Converts the given point from world space into the body's
         * local space.
         *
         * @param point The point to covert, given in local space.
         *
         * @return The converted point, in world space.
         */
        Vector3 getPointInWorldSpace(const Vector3 &point) const;

        /**
         * Converts the given direction from world space into the
         * body's local space.
         *
         * @note When a direction is converted between frames of
         * reference, there is no translation required.
         *
         * @param direction The direction to covert, given in world
         * space.
         *
         * @return The converted direction, in local space.
         */
        Vector3 getDirectionInLocalSpace(const Vector3 &direction) const;

        /**
         * Converts the given direction from world space into the
         * body's local space.
         *
         * @note When a direction is converted between frames of
         * reference, there is no translation required.
         *
         * @param direction The direction to covert, given in local
         * space.
         *
         * @return The converted direction, in world space.
         */
        Vector3 getDirectionInWorldSpace(const Vector3 &direction) const;

        /**
         * Sets the velocity of the rigid body.
         *
         * @param velocity The new velocity of the rigid body. The
         * velocity is given in world space.
         */
        void setVelocity(const Vector3 &velocity);

        /**
         * Sets the velocity of the rigid body by component. The
         * velocity is given in world space.
         *
         * @param x The x coordinate of the new velocity of the rigid
         * body.
         *
         * @param y The y coordinate of the new velocity of the rigid
         * body.
         *
         * @param z The z coordinate of the new velocity of the rigid
         * body.
         */
        void setVelocity(const real x, const real y, const real z);

        /**
         * Fills the given vector with the velocity of the rigid body.
         *
         * @param velocity A pointer to a vector into which to write
         * the velocity. The velocity is given in world local space.
         */
        void getVelocity(Vector3 *velocity) const;

        /**
         * Gets the velocity of the rigid body.
         *
         * @return The velocity of the rigid body. The velocity is
         * given in world local space.
         */
        Vector3 getVelocity() const;

        /**
         * Applies the given change in velocity.
         */
        void addVelocity(const Vector3 &deltaVelocity);

        /**
         * Sets the rotation of the rigid body.
         *
         * @param rotation The new rotation of the rigid body. The
         * rotation is given in world space.
         */
        void setRotation(const Vector3 &rotation);

        /**
         * Sets the rotation of the rigid body by component. The
         * rotation is given in world space.
         *
         * @param x The x coordinate of the new rotation of the rigid
         * body.
         *
         * @param y The y coordinate of the new rotation of the rigid
         * body.
         *
         * @param z The z coordinate of the new rotation of the rigid
         * body.
         */
        void setRotation(const real x, const real y, const real z);

        /**
         * Fills the given vector with the rotation of the rigid body.
         *
         * @param rotation A pointer to a vector into which to write
         * the rotation. The rotation is given in world local space.
         */
        void getRotation(Vector3 *rotation) const;

        /**
         * Gets the rotation of the rigid body.
         *
         * @return The rotation of the rigid body. The rotation is
         * given in world local space.
         */
        Vector3 getRotation() const;

        /**
         * Applies the given change in rotation.
         */
        void addRotation(const Vector3 &deltaRotation);

        /**
         * Returns true if the body is awake and responding to
         * integration.
         *
         * @return The awake state of the body.
         */
        bool getAwake() const
        {
            return isAwake;
        }

        /**
         * Sets the awake state of the body. If the body is set to be
         * not awake, then its velocities are also cancelled, since
         * a moving body that is not awake can cause problems in the
         * simulation.
         *
         * @param awake The new awake state of the body.
         */
        void setAwake(const bool awake=true);

        /**
         * Returns true if the body is allowed to go to sleep at
         * any time.
         */
        bool getCanSleep() const
        {
            return canSleep;
        }

        /**
         * Sets whether the body is ever allowed to go to sleep. Bodies
         * under the player's control, or for which the set of
         * transient forces applied each frame are not predictable,
         * should be kept awake.
         *
         * @param canSleep Whether the body can now be put to sleep.
         */
        void setCanSleep(const bool canSleep=true);

        /*@}*/


        /**
         * @name Retrieval Functions for Dynamic Quantities
         *
         * These functions provide access to the acceleration
         * properties of the body. The acceleration is generated by
         * the simulation from the forces and torques applied to the
         * rigid body. Acceleration cannot be directly influenced, it
         * is set during integration, and represent the acceleration
         * experienced by the body of the previous simulation step.
         */
        /*@{*/

        /**
         * Fills the given vector with the current accumulated value
         * for linear acceleration. The acceleration accumulators
         * are set during the integration step. They can be read to
         * determine the rigid body's acceleration over the last
         * integration step. The linear acceleration is given in world
         * space.
         *
         * @param linearAcceleration A pointer to a vector to receive
         * the linear acceleration data.
         */
        void getLastFrameAcceleration(Vector3 *linearAcceleration) const;

        /**
         * Gets the current accumulated value for linear
         * acceleration. The acceleration accumulators are set during
         * the integration step. They can be read to determine the
         * rigid body's acceleration over the last integration
         * step. The linear acceleration is given in world space.
         *
         * @return The rigid body's linear acceleration.
         */
        Vector3 getLastFrameAcceleration() const;

        /*@}*/


        /**
         * @name Force, Torque and Acceleration Set-up Functions
         *
         * These functions set up forces and torques to apply to the
         * rigid body.
         */
        /*@{*/

        /**
         * Clears the forces and torques in the accumulators. This will
         * be called automatically after each intergration step.
         */
        void clearAccumulators();

        /**
         * Adds the given force to centre of mass of the rigid body.
         * The force is expressed in world-coordinates.
         *
         * @param force The force to apply.
         */
        void addForce(const Vector3 &force);

        /**
         * Adds the given force to the given point on the rigid body.
         * Both the force and the
         * application point are given in world space. Because the
         * force is not applied at the centre of mass, it may be split
         * into both a force and torque.
         *
         * @param force The force to apply.
         *
         * @param point The location at which to apply the force, in
         * world-coordinates.
         */
        void addForceAtPoint(const Vector3 &force, const Vector3 &point);

        /**
         * Adds the given force to the given point on the rigid body.
         * The direction of the force is given in world coordinates,
         * but the application point is given in body space. This is
         * useful for spring forces, or other forces fixed to the
         * body.
         *
         * @param force The force to apply.
         *
         * @param point The location at which to apply the force, in
         * body-coordinates.
         */
        void addForceAtBodyPoint(const Vector3 &force, const Vector3 &point);

        /**
         * Adds the given torque to the rigid body.
         * The force is expressed in world-coordinates.
         *
         * @param torque The torque to apply.
         */
        void addTorque(const Vector3 &torque);

        /**
         * Sets the constant acceleration of the rigid body.
         *
         * @param acceleration The new acceleration of the rigid body.
         */
        void setAcceleration(const Vector3 &acceleration);

        /**
         * Sets the constant acceleration of the rigid body by component.
         *
         * @param x The x coordinate of the new acceleration of the rigid
         * body.
         *
         * @param y The y coordinate of the new acceleration of the rigid
         * body.
         *
         * @param z The z coordinate of the new acceleration of the rigid
         * body.
         */
        void setAcceleration(const real x, const real y, const real z);

        /**
         * Fills the given vector with the acceleration of the rigid body.
         *
         * @param acceleration A pointer to a vector into which to write
         * the acceleration. The acceleration is given in world local space.
         */
        void getAcceleration(Vector3 *acceleration) const;

        /**
         * Gets the acceleration of the rigid body.
         *
         * @return The acceleration of the rigid body. The acceleration is
         * given in world local space.
         */
        Vector3 getAcceleration() const;

        /*@}*/

    };

} // namespace cyclone


%endoffile
%includefile "include\\cyclone\\pcontacts.h" %beginfile
/*
 * Interface file for the contact resolution system for particles.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains the contact resolution system for particles
 * cyclone.
 *
 * The resolver uses an iterative satisfaction algorithm; it loops
 * through each contact and tries to resolve it. This is a very fast
 * algorithm but can be unstable when the contacts are highly
 * inter-related.
 */





namespace cyclone {

    /*
     * Forward declaration, see full declaration below for complete
     * documentation.
     */
    class ParticleContactResolver;

    /**
     * A Contact represents two objects in contact (in this case
     * ParticleContact representing two Particles). Resolving a
     * contact removes their interpenetration, and applies sufficient
     * impulse to keep them apart. Colliding bodies may also rebound.
     *
     * The contact has no callable functions, it just holds the
     * contact details. To resolve a set of contacts, use the particle
     * contact resolver class.
     */
    class ParticleContact
    {
        // ... Other ParticleContact code as before ...


        /**
         * The contact resolver object needs access into the contacts to
         * set and effect the contact.
         */
        friend class ParticleContactResolver;


    public:
        /**
         * Holds the particles that are involved in the contact. The
         * second of these can be NULL, for contacts with the scenery.
         */
        Particle* particle[2];

        /**
         * Holds the normal restitution coefficient at the contact.
         */
        real restitution;

        /**
         * Holds the direction of the contact in world coordinates.
         */
        Vector3 contactNormal;

        /**
         * Holds the depth of penetration at the contact.
         */
        real penetration;

        /**
         * Holds the amount each particle is moved by during interpenetration
         * resolution.
         */
        Vector3 particleMovement[2];

    protected:
        /**
         * Resolves this contact, for both velocity and interpenetration.
         */
        void resolve(real duration);

        /**
         * Calculates the separating velocity at this contact.
         */
        real calculateSeparatingVelocity() const;

    private:
        /**
         * Handles the impulse calculations for this collision.
         */
        void resolveVelocity(real duration);

        /**
         * Handles the interpenetration resolution for this contact.
         */
        void resolveInterpenetration(real duration);

    };

    /**
     * The contact resolution routine for particle contacts. One
     * resolver instance can be shared for the whole simulation.
     */
    class ParticleContactResolver
    {
    protected:
        /**
         * Holds the number of iterations allowed.
         */
        unsigned iterations;

        /**
         * This is a performance tracking value - we keep a record
         * of the actual number of iterations used.
         */
        unsigned iterationsUsed;

    public:
        /**
         * Creates a new contact resolver.
         */
        ParticleContactResolver(unsigned iterations);

        /**
         * Sets the number of iterations that can be used.
         */
        void setIterations(unsigned iterations);

        /**
         * Resolves a set of particle contacts for both penetration
         * and velocity.
         *
         * Contacts that cannot interact with each other should be
         * passed to separate calls to resolveContacts, as the
         * resolution algorithm takes much longer for lots of contacts
         * than it does for the same number of contacts in small sets.
         *
         * @param contactArray Pointer to an array of particle contact
         * objects.
         *
         * @param numContacts The number of contacts in the array to
         * resolve.
         *
         * @param numIterations The number of iterations through the
         * resolution algorithm. This should be at least the number of
         * contacts (otherwise some constraints will not be resolved -
         * although sometimes this is not noticable). If the
         * iterations are not needed they will not be used, so adding
         * more iterations may not make any difference. But in some
         * cases you would need millions of iterations. Think about
         * the number of iterations as a bound: if you specify a large
         * number, sometimes the algorithm WILL use it, and you may
         * drop frames.
         *
         * @param duration The duration of the previous integration step.
         * This is used to compensate for forces applied.
        */
        void resolveContacts(ParticleContact *contactArray,
            unsigned numContacts,
            real duration);
    };

    /**
     * This is the basic polymorphic interface for contact generators
     * applying to particles.
     */
    class ParticleContactGenerator
    {
    public:
        virtual ~ParticleContactGenerator() = default;

        /**
         * Fills the given contact structure with the generated
         * contact. The contact pointer should point to the first
         * available contact in a contact array, where limit is the
         * maximum number of contacts in the array that can be written
         * to. The method returns the number of contacts that have
         * been written.
         */
        virtual unsigned addContact(ParticleContact *contact,
                                    unsigned limit) const {
            return 0;
        };

    };



} // namespace cyclone


%endoffile
%includefile "include\\cyclone\\plinks.h" %beginfile
/*
 * Interface file for the particle links.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains classes representing the connections between
 * particles.
 */





namespace cyclone {

    /**
     * Links connect two particles together, generating a contact if
     * they violate the constraints of their link. It is used as a
     * base class for cables and rods, and could be used as a base
     * class for springs with a limit to their extension..
     */
    class ParticleLink : public ParticleContactGenerator
    {
    public:
        /**
         * Holds the pair of particles that are connected by this link.
         */
        Particle* particle[2];

    protected:
        /**
         * Returns the current length of the link.
         */
        real currentLength() const;

    public:
        /**
         * Geneates the contacts to keep this link from being
         * violated. This class can only ever generate a single
         * contact, so the pointer can be a pointer to a single
         * element, the limit parameter is assumed to be at least one
         * (zero isn't valid) and the return value is either 0, if the
         * cable wasn't over-extended, or one if a contact was needed.
         *
         * NB: This method is declared in the same way (as pure
         * virtual) in the parent class, but is replicated here for
         * documentation purposes.
         */
        virtual unsigned addContact(ParticleContact *contact,
                                    unsigned limit) const { return 0; }

        void setParticle0(Particle *particle_) {
            this->particle[0] = particle_;
        }

        void setParticle1(Particle *particle_) {
            this->particle[1] = particle_;
        }

        Particle* getParticle0(){ return particle[0];}
        Particle* getParticle1(){ return particle[1];}
    };

    /**
     * Cables link a pair of particles, generating a contact if they
     * stray too far apart.
     */
    class ParticleCable : public ParticleLink
    {
    public:
        /**
         * Holds the maximum length of the cable.
         */
        real maxLength;

        /**
         * Holds the restitution (bounciness) of the cable.
         */
        real restitution;

    public:
        /**
         * Fills the given contact structure with the contact needed
         * to keep the cable from over-extending.
         */
        virtual unsigned addContact(ParticleContact *contact,
                                    unsigned limit) const;
    };

    /**
     * Rods link a pair of particles, generating a contact if they
     * stray too far apart or too close.
     */
    class ParticleRod : public ParticleLink
    {
    public:
        /**
         * Holds the length of the rod.
         */
        real length;

    public:
        /**
         * Fills the given contact structure with the contact needed
         * to keep the rod from extending or compressing.
         */
        virtual unsigned addContact(ParticleContact *contact,
                                     unsigned limit) const;
    };

    /**
    * Constraints are just like links, except they connect a particle to
    * an immovable anchor point.
    */
    class ParticleConstraint : public ParticleContactGenerator
    {
    public:
        /**
        * Holds the particles connected by this constraint.
        */
        Particle* particle;

        /**
         * The point to which the particle is anchored.
         */
        Vector3 anchor;

    protected:
        /**
        * Returns the current length of the link.
        */
        real currentLength() const;

    public:
        /**
        * Geneates the contacts to keep this link from being
        * violated. This class can only ever generate a single
        * contact, so the pointer can be a pointer to a single
        * element, the limit parameter is assumed to be at least one
        * (zero isn't valid) and the return value is either 0, if the
        * cable wasn't over-extended, or one if a contact was needed.
        *
        * NB: This method is declared in the same way (as pure
        * virtual) in the parent class, but is replicated here for
        * documentation purposes.
        */
        virtual unsigned addContact(ParticleContact *contact,
            unsigned limit) const = 0;
    };

    /**
    * Cables link a particle to an anchor point, generating a contact if they
    * stray too far apart.
    */
    class ParticleCableConstraint : public ParticleConstraint
    {
    public:
        /**
        * Holds the maximum length of the cable.
        */
        real maxLength;

        /**
        * Holds the restitution (bounciness) of the cable.
        */
        real restitution;

    public:
        /**
        * Fills the given contact structure with the contact needed
        * to keep the cable from over-extending.
        */
        virtual unsigned addContact(ParticleContact *contact,
            unsigned limit) const;
    };

    /**
    * Rods link a particle to an anchor point, generating a contact if they
    * stray too far apart or too close.
    */
    class ParticleRodConstraint : public ParticleConstraint
    {
    public:
        /**
        * Holds the length of the rod.
        */
        real length;

    public:
        /**
        * Fills the given contact structure with the contact needed
        * to keep the rod from extending or compressing.
        */
        virtual unsigned addContact(ParticleContact *contact,
            unsigned limit) const;
    };
} // namespace cyclone


%endoffile
%includefile "include\\cyclone\\pfgen.h" %beginfile
/*
 * Interface file for the force generators.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains the interface and sample force generators.
 */







namespace cyclone {

    /**
     * A force generator can be asked to add a force to one or more
     * particles.
     */
    class ParticleForceGenerator
    {
    public:
        virtual ~ParticleForceGenerator() = default;

        /**
         * Overload this in implementations of the interface to calculate
         * and update the force applied to the given particle.
         */
        virtual void updateForce(Particle *particle, real duration) {};
    };

    /**
     * A force generator that applies a gravitational force. One instance
     * can be used for multiple particles.
     */
    class ParticleGravity : public ParticleForceGenerator
    {
        /** Holds the acceleration due to gravity. */
        Vector3 gravity;

    public:

        /** Creates the generator with the given acceleration. */
        ParticleGravity(const Vector3 &gravity);

        /** Applies the gravitational force to the given particle. */
        virtual void updateForce(Particle *particle, real duration);
    };

    /**
     * A force generator that applies a drag force. One instance
     * can be used for multiple particles.
     */
    class ParticleDrag : public ParticleForceGenerator
    {
        /** Holds the velocity drag coeffificent. */
        real k1;

        /** Holds the velocity squared drag coeffificent. */
        real k2;

    public:

        /** Creates the generator with the given coefficients. */
        ParticleDrag(real k1, real k2);

        /** Applies the drag force to the given particle. */
        virtual void updateForce(Particle *particle, real duration);
    };

    /**
     * A force generator that applies a Spring force, where
     * one end is attached to a fixed point in space.
     */
    class ParticleAnchoredSpring : public ParticleForceGenerator
    {
    protected:
        /** The location of the anchored end of the spring. */
        Vector3 *anchor;

        /** Holds the sprint constant. */
        real springConstant;

        /** Holds the rest length of the spring. */
        real restLength;

    public:
        ParticleAnchoredSpring();

        /** Creates a new spring with the given parameters. */
        ParticleAnchoredSpring(Vector3 *anchor,
                               real springConstant,
                               real restLength);

        /** Retrieve the anchor point. */
        const Vector3* getAnchor() const { return anchor; }

        /** Set the spring's properties. */
        void init(Vector3 *anchor,
                  real springConstant,
                  real restLength);

        /** Applies the spring force to the given particle. */
        virtual void updateForce(Particle *particle, real duration);
    };

    /**
    * A force generator that applies a bungee force, where
    * one end is attached to a fixed point in space.
    */
    class ParticleAnchoredBungee : public ParticleAnchoredSpring
    {
    public:
        /** Applies the spring force to the given particle. */
        virtual void updateForce(Particle *particle, real duration);
    };

    /**
     * A force generator that fakes a stiff spring force, and where
     * one end is attached to a fixed point in space.
     */
    class ParticleFakeSpring : public ParticleForceGenerator
    {
        /** The location of the anchored end of the spring. */
        Vector3 *anchor;

        /** Holds the sprint constant. */
        real springConstant;

        /** Holds the damping on the oscillation of the spring. */
        real damping;

    public:

        /** Creates a new spring with the given parameters. */
        ParticleFakeSpring(Vector3 *anchor, real springConstant,
            real damping);

        /** Applies the spring force to the given particle. */
        virtual void updateForce(Particle *particle, real duration);
    };

    /**
     * A force generator that applies a Spring force.
     */
    class ParticleSpring : public ParticleForceGenerator
    {
        /** The particle at the other end of the spring. */
        Particle *other;

        /** Holds the sprint constant. */
        real springConstant;

        /** Holds the rest length of the spring. */
        real restLength;

    public:

        /** Creates a new spring with the given parameters. */
        ParticleSpring(Particle *other,
            real springConstant, real restLength);

        /** Applies the spring force to the given particle. */
        virtual void updateForce(Particle *particle, real duration);
    };

    /**
     * A force generator that applies a spring force only
     * when extended.
     */
    class ParticleBungee : public ParticleForceGenerator
    {
        /** The particle at the other end of the spring. */
        Particle *other;

        /** Holds the sprint constant. */
        real springConstant;

        /**
         * Holds the length of the bungee at the point it begins to
         * generator a force.
         */
        real restLength;

    public:

        /** Creates a new bungee with the given parameters. */
        ParticleBungee(Particle *other,
            real springConstant, real restLength);

        /** Applies the spring force to the given particle. */
        virtual void updateForce(Particle *particle, real duration);
    };

    /**
     * A force generator that applies a buoyancy force for a plane of
     * liquid parrallel to XZ plane.
     */
    class ParticleBuoyancy : public ParticleForceGenerator
    {
        /**
         * The maximum submersion depth of the object before
         * it generates its maximum boyancy force.
         */
        real maxDepth;

        /**
         * The volume of the object.
         */
        real volume;

        /**
         * The height of the water plane above y=0. The plane will be
         * parrallel to the XZ plane.
         */
        real waterHeight;

        /**
         * The density of the liquid. Pure water has a density of
         * 1000kg per cubic meter.
         */
        real liquidDensity;

    public:

        /** Creates a new buoyancy force with the given parameters. */
        ParticleBuoyancy(real maxDepth, real volume, real waterHeight,
            real liquidDensity = 1000.0f);

        /** Applies the buoyancy force to the given particle. */
        virtual void updateForce(Particle *particle, real duration);
    };

    /**
     * Holds all the force generators and the particles they apply to.
     */
    class ParticleForceRegistry
    {
    private:

        /**
         * Keeps track of one force generator and the particle it
         * applies to.
         */
        struct ParticleForceRegistration
        {
            Particle *particle;
            ParticleForceGenerator *fg;
        };

        /**
         * Holds the list of registrations.
         */
        typedef std::vector<ParticleForceRegistration> Registry;
        Registry registrations;

    public:
        /**
         * Registers the given force generator to apply to the
         * given particle.
         */
        void add(Particle* particle, ParticleForceGenerator *fg);

        /**
         * Removes the given registered pair from the registry.
         * If the pair is not registered, this method will have
         * no effect.
         */
        void remove(Particle* particle, ParticleForceGenerator *fg);

        /**
         * Clears all registrations from the registry. This will
         * not delete the particles or the force generators
         * themselves, just the records of their connection.
         */
        void clear();

        /**
         * Calls all the force generators to update the forces of
         * their corresponding particles.
         */
        void updateForces(real duration);
    };
}


%endoffile
%includefile "include\\cyclone\\pworld.h" %beginfile
/*
 * Interface file for the particle / mass aggregate world structure.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains the definitions for a structure to hold any number o
 * particle masses, and their connections.
 */






namespace cyclone {

    /**
     * Keeps track of a set of particles, and provides the means to
     * update them all.
     */
    class ParticleWorld
    {
    public:
        typedef std::vector<Particle*> Particles;
        typedef std::vector<ParticleContactGenerator*> ContactGenerators;

    protected:
        /**
         * Holds the particles
         */
        Particles particles;

        /**
         * True if the world should calculate the number of iterations
         * to give the contact resolver at each frame.
         */
        bool calculateIterations;

        /**
         * Holds the force generators for the particles in this world.
         */
        ParticleForceRegistry registry;

        /**
         * Holds the resolver for contacts.
         */
        ParticleContactResolver resolver;

        /**
         * Contact generators.
         */
        ContactGenerators contactGenerators;

        /**
         * Holds the list of contacts.
         */
        ParticleContact *contacts;

        /**
         * Holds the maximum number of contacts allowed (i.e. the
         * size of the contacts array).
         */
        unsigned maxContacts;

    public:

        /**
         * Creates a new particle simulator that can handle up to the
         * given number of contacts per frame. You can also optionally
         * give a number of contact-resolution iterations to use. If you
         * don't give a number of iterations, then twice the number of
         * contacts will be used.
         */
        ParticleWorld(unsigned maxContacts, unsigned iterations=0);

        /**
         * Deletes the simulator.
         */
        ~ParticleWorld();

        /**
         * Calls each of the registered contact generators to report
         * their contacts. Returns the number of generated contacts.
         */
        unsigned generateContacts();

        /**
         * Integrates all the particles in this world forward in time
         * by the given duration.
         */
        void integrate(real duration);

        /**
         * Processes all the physics for the particle world.
         */
        void runPhysics(real duration);

        /**
         * Initializes the world for a simulation frame. This clears
         * the force accumulators for particles in the world. After
         * calling this, the particles can have their forces for this
         * frame added.
         */
        void startFrame();

        /**
         *  Returns the list of particles.
         */
        Particles& getParticles();

        /**
         * Returns the list of contact generators.
         */
        ContactGenerators& getContactGenerators();

        /**
         * Returns the force registry.
         */
        ParticleForceRegistry& getForceRegistry();

        void appendContactGenerator(ParticleContactGenerator& contactGenerator){
            contactGenerators.push_back(&contactGenerator);
        }

        void appendParticles(Particle& particle){
            particles.push_back(&particle);
        }

    };

    /**
      * A contact generator that takes an STL vector of particle pointers and
     * collides them against the ground.
     */
    class GroundContacts : public cyclone::ParticleContactGenerator
    {
        cyclone::ParticleWorld::Particles *particles;

    public:
        void init(cyclone::ParticleWorld::Particles *particles);

        virtual unsigned addContact(cyclone::ParticleContact *contact,
            unsigned limit) const;
    };

} // namespace cyclone


%endoffile
%includefile "include\\cyclone\\collide_fine.h" %beginfile
/*
 * Interface file for the fine grained collision detection system.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains the fine grained collision detection system.
 * It is used to return contacts between pairs of primitives.
 *
 * There are two groups of tests in this file. Intersection tests
 * use the fastest separating axis method to check if two objects
 * intersect, and the collision tests generate the contacts. The
 * collision tests typically use the intersection tests as an early
 * out.
 */





namespace cyclone {

    // Forward declarations of primitive friends
    class IntersectionTests;
    class CollisionDetector;

    /**
     * Represents a primitive to detect collisions against.
     */
    class CollisionPrimitive
    {
    public:
        /**
         * This class exists to help the collision detector
         * and intersection routines, so they should have
         * access to its data.
         */
        friend class IntersectionTests;
        friend class CollisionDetector;

        /**
         * The rigid body that is represented by this primitive.
         */
        RigidBody * body;

        /**
         * The offset of this primitive from the given rigid body.
         */
        Matrix4 offset;

        /**
         * Calculates the internals for the primitive.
         */
        void calculateInternals();

        /**
         * This is a convenience function to allow access to the
         * axis vectors in the transform for this primitive.
         */
        Vector3 getAxis(unsigned index) const
        {
            return transform.getAxisVector(index);
        }

        /**
         * Returns the resultant transform of the primitive, calculated from
         * the combined offset of the primitive and the transform
         * (orientation + position) of the rigid body to which it is
         * attached.
         */
        const Matrix4& getTransform() const
        {
            return transform;
        }


    protected:
        /**
         * The resultant transform of the primitive. This is
         * calculated by combining the offset of the primitive
         * with the transform of the rigid body.
         */
        Matrix4 transform;
    };

    /**
     * Represents a rigid body that can be treated as a sphere
     * for collision detection.
     */
    class CollisionSphere : public CollisionPrimitive
    {
    public:
        /**
         * The radius of the sphere.
         */
        real radius;
    };

    /**
     * The plane is not a primitive: it doesn't represent another
     * rigid body. It is used for contacts with the immovable
     * world geometry.
     */
    class CollisionPlane
    {
    public:
        /**
         * The plane normal
         */
        Vector3 direction;

        /**
         * The distance of the plane from the origin.
         */
        real offset;
    };

    /**
     * Represents a rigid body that can be treated as an aligned bounding
     * box for collision detection.
     */
    class CollisionBox : public CollisionPrimitive
    {
    public:
        /**
         * Holds the half-sizes of the box along each of its local axes.
         */
        Vector3 halfSize;
    };

    /**
     * A wrapper class that holds fast intersection tests. These
     * can be used to drive the coarse collision detection system or
     * as an early out in the full collision tests below.
     */
    class IntersectionTests
    {
    public:

        static bool sphereAndHalfSpace(
            const CollisionSphere &sphere,
            const CollisionPlane &plane);

        static bool sphereAndSphere(
            const CollisionSphere &one,
            const CollisionSphere &two);

        static bool boxAndBox(
            const CollisionBox &one,
            const CollisionBox &two);

        /**
         * Does an intersection test on an arbitrarily aligned box and a
         * half-space.
         *
         * The box is given as a transform matrix, including
         * position, and a vector of half-sizes for the extend of the
         * box along each local axis.
         *
         * The half-space is given as a direction (i.e. unit) vector and the
         * offset of the limiting plane from the origin, along the given
         * direction.
         */
        static bool boxAndHalfSpace(
            const CollisionBox &box,
            const CollisionPlane &plane);
    };


    /**
     * A helper structure that contains information for the detector to use
     * in building its contact data.
     */
    struct CollisionData
    {
        /**
         * Holds the base of the collision data: the first contact
         * in the array. This is used so that the contact pointer (below)
         * can be incremented each time a contact is detected, while
         * this pointer points to the first contact found.
         */
        Contact *contactArray;

        /** Holds the contact array to write into. */
        Contact *contacts;

        /** Holds the maximum number of contacts the array can take. */
        int contactsLeft;

        /** Holds the number of contacts found so far. */
        unsigned contactCount;

        /** Holds the friction value to write into any collisions. */
        real friction;

        /** Holds the restitution value to write into any collisions. */
        real restitution;

        /**
         * Holds the collision tolerance, even uncolliding objects this
         * close should have collisions generated.
         */
        real tolerance;

        /**
         * Checks if there are more contacts available in the contact
         * data.
         */
        bool hasMoreContacts()
        {
            return contactsLeft > 0;
        }

        /**
         * Resets the data so that it has no used contacts recorded.
         */
        void reset(unsigned maxContacts)
        {
            contactsLeft = maxContacts;
            contactCount = 0;
            contacts = contactArray;
        }

        /**
         * Notifies the data that the given number of contacts have
         * been added.
         */
        void addContacts(unsigned count)
        {
            // Reduce the number of contacts remaining, add number used
            contactsLeft -= count;
            contactCount += count;

            // Move the array forward
            contacts += count;
        }
    };

    struct CollisionDataEx : public CollisionData {
    private:
        Contact contactBuffer[256];
        ContactResolver resolver;

    public:
        explicit CollisionDataEx(int iteration) : CollisionData(), resolver(iteration) {
            contactArray = contactBuffer;
        }

        void resolve(real duration) {
            resolver.resolveContacts(contactArray, contactCount, duration);
        }
    };

    /**
     * A wrapper class that holds the fine grained collision detection
     * routines.
     *
     * Each of the functions has the same format: it takes the details
     * of two objects, and a pointer to a contact array to fill. It
     * returns the number of contacts it wrote into the array.
     */
    class CollisionDetector
    {
    public:

        static unsigned sphereAndHalfSpace(
            const CollisionSphere &sphere,
            const CollisionPlane &plane,
            CollisionData *data
            );

        static unsigned sphereAndTruePlane(
            const CollisionSphere &sphere,
            const CollisionPlane &plane,
            CollisionData *data
            );

        static unsigned sphereAndSphere(
            const CollisionSphere &one,
            const CollisionSphere &two,
            CollisionData *data
            );

        /**
         * Does a collision test on a collision box and a plane representing
         * a half-space (i.e. the normal of the plane
         * points out of the half-space).
         */
        static unsigned boxAndHalfSpace(
            const CollisionBox &box,
            const CollisionPlane &plane,
            CollisionData *data
            );

        static unsigned boxAndBox(
            const CollisionBox &one,
            const CollisionBox &two,
            CollisionData *data
            );

        static unsigned boxAndPoint(
            const CollisionBox &box,
            const Vector3 &point,
            CollisionData *data
            );

        static unsigned boxAndSphere(
            const CollisionBox &box,
            const CollisionSphere &sphere,
            CollisionData *data
            );
    };



} // namespace cyclone


%endoffile
%includefile "include\\cyclone\\contacts.h" %beginfile
/*
 * Interface file for the contact resolution system.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains the contact resolution system for cyclone,
 * although it is called the contact resolution system, it handles
 * collisions, contacts (sliding and resting), and constraints (such
 * as joints).
 *
 * The resolver uses an iterative satisfaction algorithm; it loops
 * through each contact and tries to resolve it. This is a very fast
 * algorithm but can be unstable when the contacts are highly
 * inter-related.
 */





namespace cyclone {

    /*
     * Forward declaration, see full declaration below for complete
     * documentation.
     */
    class ContactResolver;

    /**
     * A contact represents two bodies in contact. Resolving a
     * contact removes their interpenetration, and applies sufficient
     * impulse to keep them apart. Colliding bodies may also rebound.
     * Contacts can be used to represent positional joints, by making
     * the contact constraint keep the bodies in their correct
     * orientation.
     *
     * It can be a good idea to create a contact object even when the
     * contact isn't violated. Because resolving one contact can violate
     * another, contacts that are close to being violated should be
     * sent to the resolver; that way if one resolution moves the body,
     * the contact may be violated, and can be resolved. If the contact
     * is not violated, it will not be resolved, so you only loose a
     * small amount of execution time.
     *
     * The contact has no callable functions, it just holds the contact
     * details. To resolve a set of contacts, use the contact resolver
     * class.
     */
    class Contact
    {
        // ... Other data as before ...

        /**
         * The contact resolver object needs access into the contacts to
         * set and effect the contact.
         */
        friend class ContactResolver;

    public:
        /**
         * Holds the bodies that are involved in the contact. The
         * second of these can be NULL, for contacts with the scenery.
         */
        RigidBody* body[2];

        /**
         * Holds the lateral friction coefficient at the contact.
         */
        real friction;

        /**
         * Holds the normal restitution coefficient at the contact.
         */
        real restitution;

        /**
         * Holds the position of the contact in world coordinates.
         */
        Vector3 contactPoint;

        /**
         * Holds the direction of the contact in world coordinates.
         */
        Vector3 contactNormal;

        /**
         * Holds the depth of penetration at the contact point. If both
         * bodies are specified then the contact point should be midway
         * between the inter-penetrating points.
         */
        real penetration;

        /**
         * Sets the data that doesn't normally depend on the position
         * of the contact (i.e. the bodies, and their material properties).
         */
        void setBodyData(RigidBody* one, RigidBody *two,
                         real friction, real restitution);

    protected:

        /**
         * A transform matrix that converts co-ordinates in the contact's
         * frame of reference to world co-ordinates. The columns of this
         * matrix form an orthonormal set of vectors.
         */
        Matrix3 contactToWorld;

        /**
         * Holds the closing velocity at the point of contact. This is set
         * when the calculateInternals function is run.
         */
        Vector3 contactVelocity;

        /**
         * Holds the required change in velocity for this contact to be
         * resolved.
         */
        real desiredDeltaVelocity;

        /**
         * Holds the world space position of the contact point relative to
         * centre of each body. This is set when the calculateInternals
         * function is run.
         */
        Vector3 relativeContactPosition[2];

    protected:
        /**
         * Calculates internal data from state data. This is called before
         * the resolution algorithm tries to do any resolution. It should
         * never need to be called manually.
         */
        void calculateInternals(real duration);

        /**
         * Reverses the contact. This involves swapping the two rigid bodies
         * and reversing the contact normal. The internal values should then
         * be recalculated using calculateInternals (this is not done
         * automatically).
         */
        void swapBodies();

        /**
         * Updates the awake state of rigid bodies that are taking
         * place in the given contact. A body will be made awake if it
         * is in contact with a body that is awake.
         */
        void matchAwakeState();

        /**
         * Calculates and sets the internal value for the desired delta
         * velocity.
         */
        void calculateDesiredDeltaVelocity(real duration);

        /**
         * Calculates and returns the velocity of the contact
         * point on the given body.
         */
        Vector3 calculateLocalVelocity(unsigned bodyIndex, real duration);

        /**
         * Calculates an orthonormal basis for the contact point, based on
         * the primary friction direction (for anisotropic friction) or
         * a random orientation (for isotropic friction).
         */
        void calculateContactBasis();

        /**
         * Applies an impulse to the given body, returning the
         * change in velocities.
         */
        void applyImpulse(const Vector3 &impulse, RigidBody *body,
                          Vector3 *velocityChange, Vector3 *rotationChange);

        /**
         * Performs an inertia-weighted impulse based resolution of this
         * contact alone.
         */
        void applyVelocityChange(Vector3 velocityChange[2],
                                 Vector3 rotationChange[2]);

        /**
         * Performs an inertia weighted penetration resolution of this
         * contact alone.
         */
        void applyPositionChange(Vector3 linearChange[2],
                                 Vector3 angularChange[2],
                                 real penetration);

        /**
         * Calculates the impulse needed to resolve this contact,
         * given that the contact has no friction. A pair of inertia
         * tensors - one for each contact object - is specified to
         * save calculation time: the calling function has access to
         * these anyway.
         */
        Vector3 calculateFrictionlessImpulse(Matrix3 *inverseInertiaTensor);

        /**
         * Calculates the impulse needed to resolve this contact,
         * given that the contact has a non-zero coefficient of
         * friction. A pair of inertia tensors - one for each contact
         * object - is specified to save calculation time: the calling
         * function has access to these anyway.
         */
        Vector3 calculateFrictionImpulse(Matrix3 *inverseInertiaTensor);
    };

    /**
     * The contact resolution routine. One resolver instance
     * can be shared for the whole simulation, as long as you need
     * roughly the same parameters each time (which is normal).
     *
     * @section algorithm Resolution Algorithm
     *
     * The resolver uses an iterative satisfaction algorithm; it loops
     * through each contact and tries to resolve it. Each contact is
     * resolved locally, which may in turn put other contacts in a worse
     * position. The algorithm then revisits other contacts and repeats
     * the process up to a specified iteration limit. It can be proved
     * that given enough iterations, the simulation will get to the
     * correct result. As with all approaches, numerical stability can
     * cause problems that make a correct resolution impossible.
     *
     * @subsection strengths Strengths
     *
     * This algorithm is very fast, much faster than other physics
     * approaches. Even using many more iterations than there are
     * contacts, it will be faster than global approaches.
     *
     * Many global algorithms are unstable under high friction, this
     * approach is very robust indeed for high friction and low
     * restitution values.
     *
     * The algorithm produces visually believable behaviour. Tradeoffs
     * have been made to err on the side of visual realism rather than
     * computational expense or numerical accuracy.
     *
     * @subsection weaknesses Weaknesses
     *
     * The algorithm does not cope well with situations with many
     * inter-related contacts: stacked boxes, for example. In this
     * case the simulation may appear to jiggle slightly, which often
     * dislodges a box from the stack, allowing it to collapse.
     *
     * Another issue with the resolution mechanism is that resolving
     * one contact may make another contact move sideways against
     * friction, because each contact is handled independently, this
     * friction is not taken into account. If one object is pushing
     * against another, the pushed object may move across its support
     * without friction, even though friction is set between those bodies.
     *
     * In general this resolver is not suitable for stacks of bodies,
     * but is perfect for handling impact, explosive, and flat resting
     * situations.
     */
    class ContactResolver
    {
    protected:
        /**
         * Holds the number of iterations to perform when resolving
         * velocity.
         */
        unsigned velocityIterations;

        /**
         * Holds the number of iterations to perform when resolving
         * position.
         */
        unsigned positionIterations;

        /**
         * To avoid instability velocities smaller
         * than this value are considered to be zero. Too small and the
         * simulation may be unstable, too large and the bodies may
         * interpenetrate visually. A good starting point is the default
         * of 0.01.
         */
        real velocityEpsilon;

        /**
         * To avoid instability penetrations
         * smaller than this value are considered to be not interpenetrating.
         * Too small and the simulation may be unstable, too large and the
         * bodies may interpenetrate visually. A good starting point is
         * the default of0.01.
         */
        real positionEpsilon;

    public:
        /**
         * Stores the number of velocity iterations used in the
         * last call to resolve contacts.
         */
        unsigned velocityIterationsUsed;

        /**
         * Stores the number of position iterations used in the
         * last call to resolve contacts.
         */
        unsigned positionIterationsUsed;

    private:
        /**
         * Keeps track of whether the internal settings are valid.
         */
        bool validSettings;

    public:
        /**
         * Creates a new contact resolver with the given number of iterations
         * per resolution call, and optional epsilon values.
         */
        ContactResolver(unsigned iterations,
            real velocityEpsilon=(real)0.01,
            real positionEpsilon=(real)0.01);

        /**
         * Creates a new contact resolver with the given number of iterations
         * for each kind of resolution, and optional epsilon values.
         */
        ContactResolver(unsigned velocityIterations,
            unsigned positionIterations,
            real velocityEpsilon=(real)0.01,
            real positionEpsilon=(real)0.01);

        /**
         * Returns true if the resolver has valid settings and is ready to go.
         */
        bool isValid()
        {
            return (velocityIterations > 0) &&
                   (positionIterations > 0) &&
                   (positionEpsilon >= 0.0f) &&
                   (positionEpsilon >= 0.0f);
        }

        /**
         * Sets the number of iterations for each resolution stage.
         */
        void setIterations(unsigned velocityIterations,
                           unsigned positionIterations);

        /**
         * Sets the number of iterations for both resolution stages.
         */
        void setIterations(unsigned iterations);

        /**
         * Sets the tolerance value for both velocity and position.
         */
        void setEpsilon(real velocityEpsilon,
                        real positionEpsilon);

        /**
         * Resolves a set of contacts for both penetration and velocity.
         *
         * Contacts that cannot interact with
         * each other should be passed to separate calls to resolveContacts,
         * as the resolution algorithm takes much longer for lots of
         * contacts than it does for the same number of contacts in small
         * sets.
         *
         * @param contactArray Pointer to an array of contact objects.
         *
         * @param numContacts The number of contacts in the array to resolve.
         *
         * @param numIterations The number of iterations through the
         * resolution algorithm. This should be at least the number of
         * contacts (otherwise some constraints will not be resolved -
         * although sometimes this is not noticable). If the iterations are
         * not needed they will not be used, so adding more iterations may
         * not make any difference. In some cases you would need millions
         * of iterations. Think about the number of iterations as a bound:
         * if you specify a large number, sometimes the algorithm WILL use
         * it, and you may drop lots of frames.
         *
         * @param duration The duration of the previous integration step.
         * This is used to compensate for forces applied.
         */
        void resolveContacts(Contact *contactArray,
            unsigned numContacts,
            real duration);

    protected:
        /**
         * Sets up contacts ready for processing. This makes sure their
         * internal data is configured correctly and the correct set of bodies
         * is made alive.
         */
        void prepareContacts(Contact *contactArray, unsigned numContacts,
            real duration);

        /**
         * Resolves the velocity issues with the given array of constraints,
         * using the given number of iterations.
         */
        void adjustVelocities(Contact *contactArray,
            unsigned numContacts,
            real duration);

        /**
         * Resolves the positional issues with the given array of constraints,
         * using the given number of iterations.
         */
        void adjustPositions(Contact *contacts,
            unsigned numContacts,
            real duration);
    };

    /**
     * This is the basic polymorphic interface for contact generators
     * applying to rigid bodies.
     */
    class ContactGenerator
    {
    public:
        /**
         * Fills the given contact structure with the generated
         * contact. The contact pointer should point to the first
         * available contact in a contact array, where limit is the
         * maximum number of contacts in the array that can be written
         * to. The method returns the number of contacts that have
         * been written.
         */
        virtual unsigned addContact(Contact *contact, unsigned limit) const = 0;
    };

} // namespace cyclone


%endoffile
%includefile "include\\cyclone\\fgen.h" %beginfile
/*
 * Interface file for the force generators.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under license. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software license.
 */

/**
 * @file
 *
 * This file contains the interface and sample force generators.
 */






namespace cyclone {

    /**
     * A force generator can be asked to add a force to one or more
     * bodies.
     */
    class ForceGenerator
    {
    public:
        virtual ~ForceGenerator()= default;
        /**
         * Overload this in implementations of the interface to calculate
         * and update the force applied to the given rigid body.
         */
        virtual void updateForce(RigidBody *body, real duration) {};
    };

    /**
     * A force generator that applies a gravitational force. One instance
     * can be used for multiple rigid bodies.
     */
    class Gravity : public ForceGenerator
    {
        /** Holds the acceleration due to gravity. */
        Vector3 gravity;

    public:

        /** Creates the generator with the given acceleration. */
        Gravity(const Vector3 &gravity);

        /** Applies the gravitational force to the given rigid body. */
        virtual void updateForce(RigidBody *body, real duration);
    };

    /**
     * A force generator that applies a Spring force.
     */
    class Spring : public ForceGenerator
    {
        /**
         * The point of connection of the spring, in local
         * coordinates.
         */
        Vector3 connectionPoint;

        /**
         * The point of connection of the spring to the other object,
         * in that object's local coordinates.
         */
        Vector3 otherConnectionPoint;

        /** The particle at the other end of the spring. */
        RigidBody *other;

        /** Holds the sprint constant. */
        real springConstant;

        /** Holds the rest length of the spring. */
        real restLength;

    public:

        /** Creates a new spring with the given parameters. */
        Spring(const Vector3 &localConnectionPt,
               RigidBody *other,
               const Vector3 &otherConnectionPt,
               real springConstant,
               real restLength);

        /** Applies the spring force to the given rigid body. */
        virtual void updateForce(RigidBody *body, real duration);
    };

    /**
     * A force generator that applies an aerodynamic force.
     */
    class Aero : public ForceGenerator
    {
    protected:
        /**
         * Holds the aerodynamic tensor for the surface in body
         * space.
         */
        Matrix3 tensor;

        /**
         * Holds the relative position of the aerodynamic surface in
         * body coordinates.
         */
        Vector3 position;

        /**
         * Holds a pointer to a vector containing the windspeed of the
         * environment. This is easier than managing a separate
         * windspeed vector per generator and having to update it
         * manually as the wind changes.
         */
        const Vector3* windspeed;

    public:
        /**
         * Creates a new aerodynamic force generator with the
         * given properties.
         */
        Aero(const Matrix3 &tensor, const Vector3 &position,
             const Vector3 *windspeed);

        /**
         * Applies the force to the given rigid body.
         */
        virtual void updateForce(RigidBody *body, real duration);

    protected:
        /**
         * Uses an explicit tensor matrix to update the force on
         * the given rigid body. This is exactly the same as for updateForce
         * only it takes an explicit tensor.
         */
        void updateForceFromTensor(RigidBody *body, real duration,
                                   const Matrix3 &tensor);
    };

    class AeroEx: public Aero
    {
    public:
        AeroEx(const Matrix3 &tensor, const Vector3 &position,
             const Vector3 *windspeed):Aero(tensor, position, windspeed){}

        void updateWindspeed(const Vector3& windspeed){
            this->windspeed = &windspeed;
        }
    };

    /**
    * A force generator with a control aerodynamic surface. This
    * requires three inertia tensors, for the two extremes and
    * 'resting' position of the control surface.  The latter tensor is
    * the one inherited from the base class, the two extremes are
    * defined in this class.
    */
    class AeroControl : public AeroEx
    {
    protected:
        /**
         * The aerodynamic tensor for the surface, when the control is at
         * its maximum value.
         */
        Matrix3 maxTensor;

        /**
         * The aerodynamic tensor for the surface, when the control is at
         * its minimum value.
         */
        Matrix3 minTensor;

        /**
        * The current position of the control for this surface. This
        * should range between -1 (in which case the minTensor value
        * is used), through 0 (where the base-class tensor value is
        * used) to +1 (where the maxTensor value is used).
        */
        real controlSetting;

    private:
        /**
         * Calculates the final aerodynamic tensor for the current
         * control setting.
         */
        Matrix3 getTensor();

    public:
        /**
         * Creates a new aerodynamic control surface with the given
         * properties.
         */
        AeroControl(const Matrix3 &base,
                    const Matrix3 &min, const Matrix3 &max,
                    const Vector3 &position, const Vector3 *windspeed);

        /**
         * Sets the control position of this control. This * should
        range between -1 (in which case the minTensor value is *
        used), through 0 (where the base-class tensor value is used) *
        to +1 (where the maxTensor value is used). Values outside that
        * range give undefined results.
        */
        void setControl(real value);

        /**
         * Applies the force to the given rigid body.
         */
        virtual void updateForce(RigidBody *body, real duration);
    };

    ///**
    // * A force generator with an aerodynamic surface that can be
    // * re-oriented relative to its rigid body. This derives the
    // */
    //class AngledAero : public Aero
    //{
    //    /**
    //     * Holds the orientation of the aerodynamic surface relative
    //     * to the rigid body to which it is attached.
    //     */
    //    Quaternion orientation;

    //public:
    //    /**
    //     * Creates a new aerodynamic surface with the given properties.
    //     */
    //    AngledAero(const Matrix3 &tensor, const Vector3 &position,
    //         const Vector3 *windspeed);

    //    /**
    //     * Sets the relative orientation of the aerodynamic surface,
    //     * relative to the rigid body it is attached to. Note that
    //     * this doesn't affect the point of connection of the surface
    //     * to the body.
    //     */
    //    //void setOrientation(const Quaternion &quat);

    //    /**
    //     * Applies the force to the given rigid body.
    //     */
    //    virtual void updateForce(RigidBody *body, real duration);
    //};

    /**
     * A force generator to apply a buoyant force to a rigid body.
     */
    class Buoyancy : public ForceGenerator
    {
        /**
         * The maximum submersion depth of the object before
         * it generates its maximum buoyancy force.
         */
        real maxDepth;

        /**
         * The volume of the object.
         */
        real volume;

        /**
         * The height of the water plane above y=0. The plane will be
         * parallel to the XZ plane.
         */
        real waterHeight;

        /**
         * The density of the liquid. Pure water has a density of
         * 1000kg per cubic meter.
         */
        real liquidDensity;

        /**
         * The centre of buoyancy of the rigid body, in body coordinates.
         */
        Vector3 centreOfBuoyancy;

    public:

        /** Creates a new buoyancy force with the given parameters. */
        Buoyancy(const Vector3 &cOfB,
            real maxDepth, real volume, real waterHeight,
            real liquidDensity = 1000.0f);

        /**
         * Applies the force to the given rigid body.
         */
        virtual void updateForce(RigidBody *body, real duration);
    };

    /**
    * Holds all the force generators and the bodies they apply to.
    */
    class ForceRegistry
    {
    private:

        /**
        * Keeps track of one force generator and the body it
        * applies to.
        */
        struct ForceRegistration
        {
            RigidBody *body;
            ForceGenerator *fg;
        };

        /**
        * Holds the list of registrations.
        */
        typedef std::vector<ForceRegistration> Registry;
        Registry registrations;

    public:
        /**
        * Registers the given force generator to apply to the
        * given body.
        */
        void add(RigidBody* body, ForceGenerator *fg);

        /**
        * Removes the given registered pair from the registry.
        * If the pair is not registered, this method will have
        * no effect.
        */
        void remove(RigidBody* body, ForceGenerator *fg);

        /**
        * Clears all registrations from the registry. This will
        * not delete the bodies or the force generators
        * themselves, just the records of their connection.
        */
        void clear();

        /**
        * Calls all the force generators to update the forces of
        * their corresponding bodies.
        */
        void updateForces(real duration);
    };
}



%endoffile
%includefile "include\\cyclone\\joints.h" %beginfile
/*
 * Interface file for joints between rigid bodies.
 *
 * Part of the Cyclone physics system.
 *
 * Copyright (c) Icosagon 2003. All Rights Reserved.
 *
 * This software is distributed under licence. Use of this software
 * implies agreement with all terms and conditions of the accompanying
 * software licence.
 */

/**
 * @file
 *
 * This file contains the definitions for joints that link together
 * different rigid bodies.
 */





namespace cyclone {

    /**
     * Joints link together two rigid bodies and make sure they do not
     * separate.  In a general phyiscs engine there may be many
     * different types of joint, that reduce the number of relative
     * degrees of freedom between two objects. This joint is a common
     * position joint: each object has a location (given in
     * body-coordinates) that will be kept at the same point in the
     * simulation.
     */
    class Joint : public ContactGenerator
    {
    public:
        /**
         * Holds the two rigid bodies that are connected by this joint.
         */
        RigidBody* body[2];

        /**
         * Holds the relative location of the connection for each
         * body, given in local coordinates.
         */
        Vector3 position[2];

        /**
         * Holds the maximum displacement at the joint before the
         * joint is considered to be violated. This is normally a
         * small, epsilon value.  It can be larger, however, in which
         * case the joint will behave as if an inelastic cable joined
         * the bodies at their joint locations.
         */
        real error;

        /**
         * Configures the joint in one go.
         */
        void init(
            RigidBody *a, const Vector3& a_pos,
            RigidBody *b, const Vector3& b_pos,
            real error
            );

        /**
         * Generates the contacts required to restore the joint if it
         * has been violated.
         */
        unsigned addContact(Contact *contact, unsigned limit) const;
    };

} // namespace cyclone


%endoffile

%endoffile
