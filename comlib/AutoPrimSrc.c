/*-----------------------------------------------------------
 (c) 1998,  Daan Leijen, leijen@@fwi.uva.nl
     1999-, Sigbjorn Finne sof@galconn.com

  Changes:
    * made cygwin32 (&gcc) friendly.
        -- sof 10/98
-----------------------------------------------------------*/


#if !defined(__CYGWIN32__) && !defined(__MINGW32__)
#define CINTERFACE
#define COBJMACROS
#endif
#include <windows.h>
#include <stdio.h>

#if defined(__CYGWIN32__) || defined(__MINGW32__)
#include <w32api.h>
#endif

#ifdef __GNUC__
#include "comPrim.h"
#include "autoPrim.h"
#include "PointerSrc.h"
#include <stdlib.h>
#include <string.h>

/* No support for nameless unions..yet. */
#if __W32API_MAJOR_VERSION == 1
#define cyVal n1.n2.n3.cyVal
#define bVal n1.n2.n3.bVal
#define punkVal n1.n2.n3.punkVal
#define boolVal n1.n2.n3.boolVal
#define pdispVal n1.n2.n3.pdispVal
#define bstrVal  n1.n2.n3.bstrVal
#define date     n1.n2.n3.date
#define dblVal   n1.n2.n3.dblVal
#define fltVal   n1.n2.n3.fltVal
#define lVal     n1.n2.n3.lVal
#define ulVal    n1.n2.n3.ulVal
#define iVal     n1.n2.n3.iVal
#define parray   n1.n2.n3.parray
#define pparray  n1.n2.n3.pparray
#define decVal   n1.decVal
#define vt       n1.n2.vt
#endif
#endif

/*-----------------------------------------------------------
-- Invoke
-----------------------------------------------------------*/

HRESULT dispatchInvoke( IDispatch* obj, DISPID dispid, LCID lcid,
                        BOOL isfunction, unsigned flags,
                        int cargs, int cargsout,
                        VARIANT* args, VARIANT* argsout,
                        EXCEPINFO** info )
{
   HRESULT      hr;
   DISPPARAMS   dp;
   EXCEPINFO    excepInfo;
   UINT         argErr;
   VARIANT      varRes;
   VARIANT*     res;
   DISPID       dispput;
   int          i;


   if (info) *info = NULL;

   /* create VT_BYREF variants for out parameters */
   for (i = 0; i < cargsout; i++)
   {
        VariantInit( args+i );
        args[i].vt    = argsout[i].vt | VT_BYREF;
#if !defined(__GNUC__) || __W32API_MAJOR_VERSION > 1
        args[i].byref = &(argsout[i].byref);
#else
        args[i].n1.n2.n3.byref = &(argsout[i].n1.n2.n3.byref);
#endif
   }

   if (!obj) return E_POINTER;

   dp.cArgs  = cargs;
   dp.rgvarg = args;

   /* set special flags for property puts */
   if (flags & DISPATCH_PROPERTYPUT)
   {
     dispput = DISPID_PROPERTYPUT;
     dp.rgdispidNamedArgs = &dispput;
     dp.cNamedArgs = 1;
   }
   else
   {
     dp.rgdispidNamedArgs = NULL;
     dp.cNamedArgs = 0;
   }

   /* if this is surely a function, return the
      result in the first element of argsout, and
      decrement the number of arguments */
   if (isfunction)
   {
      dp.cArgs--;
      dp.rgvarg++;
      res = argsout;
   }
   else
   {
      res = &varRes;
      VariantInit(res);
   }

   hr = IDispatch_Invoke( obj, dispid, &IID_NULL, lcid, flags,
                          &dp, res, &excepInfo, &argErr );

   /* if guessed that this was not a function, but it returned
      a result (in varRes) or it complained about the number
      of arguments, than we again try to call it but now as a function. */

   /* Turned this smart off, leaving it to HDirect (and the user) to
      make the distinction between memberX and functionX.  -- sof
      
      No, let's not -- sometimes the type information isn't precise
      enough about this (e.g., the type info for msi.dll's
      Installer.OpenDatabase() suggests its a _method_, but it turns
      out to be a _function_.
   */
   if (!isfunction && flags == DISPATCH_METHOD &&
       (hr == DISP_E_BADPARAMCOUNT || 
        hr == DISP_E_TYPEMISMATCH  ||
        res->vt != VT_EMPTY) ) {
       isfunction = TRUE;
       dp.cArgs--;
       dp.rgvarg++;
       VariantClear(res);
       res = argsout;

       hr = IDispatch_Invoke( obj, dispid, &IID_NULL, lcid, flags,
                          &dp, res, &excepInfo, &argErr );
   }
   if (!isfunction)
        VariantClear( res );

   /* free the input arguments */
   for (i = cargsout; i < cargs; i++)
        VariantClear( args+i );

   /* check for exceptions */
   if (hr == DISP_E_EXCEPTION && info)
   {
       *info = (EXCEPINFO*)primAllocMemory( sizeof(EXCEPINFO));
       if (*info)
       {
          **info = excepInfo;
          if ((*info)->pfnDeferredFillIn)
              (*info)->pfnDeferredFillIn( *info );
       }
   }
   return hr;
}

void freeExcepInfo( EXCEPINFO* info )
{
   if (!info) return;

   if (info->bstrSource)        SysFreeString( info->bstrSource );
   if (info->bstrDescription)   SysFreeString( info->bstrDescription );
   if (info->bstrHelpFile)      SysFreeString( info->bstrHelpFile );
}

char* getExcepInfoMessage( EXCEPINFO* info )
{
#define    MAXMSG       255
#define    MAXSTR       80

    char member[MAXSTR+1] = "";
    char source[MAXSTR+1] = "";
    char wcode[MAXSTR+1]  = "";
    char description[MAXMSG+1] = "Exception occurred";
    char* msg;

    msg = primAllocMemory( MAXMSG+1 );
    if (!msg) return NULL;

    if (info)
    {
      if (info->bstrSource)
      {
         char sourceReg[MAXSTR];

         wcstombs( sourceReg, info->bstrSource, MAXSTR );
         if (ERROR_SUCCESS !=
                           RegQueryValueA( HKEY_CLASSES_ROOT, sourceReg, source, NULL ))
             wcstombs( source, info->bstrSource, MAXSTR );
      }

      if (info->wCode)
      {
         sprintf( wcode, "(%d)", info->wCode );
      }
      
      if (info->bstrDescription)
      {   
         wcstombs( description, info->bstrDescription, MAXMSG);
      }
      else if (info->scode)
      {
         strncpy( description, hresultString(info->scode), MAXMSG );
      }
   }

   if (info && info->bstrSource)
#ifdef __GNUC__
      sprintf(msg, "%s.%s:%s %s", source, member, wcode, description);
#else
      _snprintf( msg, MAXMSG, "%s.%s:%s %s",
                            source,member,wcode,description);
#endif
   else
#ifdef __GNUC__
      sprintf(msg, "%s %s", wcode, description);
#else
      _snprintf( msg, MAXMSG, "%s %s", wcode,description);
#endif
 
   return msg;
}

HRESULT dispatchGetMemberID( IDispatch* obj,
                             BSTR name, LCID lcid, DISPID* dispid )
{
   if (obj) return IDispatch_GetIDsOfNames( obj, &IID_NULL,
                                             &name, 1, lcid, dispid );
       else return E_POINTER;
}


/*-----------------------------------------------------------
-- Variant marshalling (we just lack VT_ARRAY support)
-----------------------------------------------------------*/

void freeVariants( int count, VARIANT* p )
{
   int i;
   if (!p) return;
   for (i = 0; i < count; i++)
      VariantClear(p+i);
}

int readVariantTag(VARIANT* pw) { return(pw->vt); }

#define READWRITEVAR(htp,ctp,tp,fld,def) \
                                void writeVar##htp( ctp x, VARIANT* v )   \
                                {                                       \
                                   WRITEVAR(tp,fld);                     \
                                }  \
                                HRESULT readVar##htp( VARIANT* v, ctp* p ) \
                                { \
                                   READVAR(tp,fld,def); \
                                }

#define WRITEVAR(tp,fld)     {  if (!v) return; \
                                VariantInit(v); \
                                v->vt = tp; \
                                v->fld = x; \
                             }

#define READVAR(tp,fld,def)  {  if (!p) return E_POINTER; \
                                if (!v) { *p = def; return E_POINTER;} \
                                if (v->vt == tp) {      \
                                  *p = v->fld;          \
                                  return S_OK;          \
                                } else {                \
                                  VARIANT w;            \
                                  HRESULT hr;           \
                                  VariantInit( &w );    \
                                  hr = VariantChangeType( &w, v, 0, tp ); \
                                  if (SUCCEEDED(hr)) *p = w.fld; \
                                                else *p = def;   \
                                  return hr;            \
                              }                         \
                            }


#define READWRITETEMPVAR(htp,ctp,tp,fld,def) \
                                void writeVar##htp( ctp x, VARIANT* v )   \
                                {                                       \
                                   WRITEVAR(tp,fld);                     \
                                }  \
                                HRESULT readVar##htp( VARIANT* v, ctp* p, VARIANT** w ) \
                                { \
                                   READTEMPVAR(tp,fld,def); \
                                }


#define READTEMPVAR(tp,fld,def) { if (w) *w = NULL;     \
                                if (!p) return E_POINTER; \
                                if (!v) { *p = def; return E_POINTER; } \
                                if (v->vt == tp) {      \
                                  *p = v->fld;          \
                                  return S_OK;          \
                                } else {                \
                                  HRESULT hr;           \
                                  *p = def;             \
                                  if (!w) return E_POINTER; \
                                  *w = primAllocMemory( sizeof(VARIANT)); \
                                  if (*w == NULL) return E_OUTOFMEMORY;  \
                                  VariantInit( *w );    \
                                  hr = VariantChangeType( *w, v, 0, tp ); \
                                  if (SUCCEEDED(hr)) *p = (*w)->fld; \
                                                else { primFreeMemory(*w); *w = NULL; } \
                                  return hr;            \
                              }                         \
                            }


READWRITEVAR( Short, int, VT_I2, iVal, 0 )
READWRITEVAR( Int, int, VT_I4, lVal, 0 )
READWRITEVAR( Word, unsigned int, VT_UI4, ulVal, 0 )
READWRITEVAR( Float, float, VT_R4, fltVal, 0.0 )
READWRITEVAR( Double, double, VT_R8, dblVal, 0.0 )
READWRITEVAR( Date, double, VT_DATE, date, 0.0 )
READWRITETEMPVAR( String, BSTR, VT_BSTR, bstrVal, NULL )
READWRITETEMPVAR( Dispatch, IDispatch*, VT_DISPATCH, pdispVal, NULL )
READWRITEVAR( Bool, BOOL, VT_BOOL, boolVal, 0 )
READWRITETEMPVAR( Unknown, IUnknown*, VT_UNKNOWN, punkVal, NULL )
READWRITEVAR( Byte, unsigned char, VT_UI1, bVal, 0 )
#if __W32API_MAJOR_VERSION == 1
READWRITEVAR( Error, int, VT_ERROR, n1.n2.n3.scode, 0 )
#else
READWRITEVAR( Error, int, VT_ERROR, scode, 0 )
#endif

void
writeVarOptional( VARIANT * v )
{
  if (!v) return;
  VariantInit ( v );
  v->vt       = VT_ERROR;
#if __W32API_MAJOR_VERSION == 1
  v->n1.n2.n3.scode = DISP_E_PARAMNOTFOUND;
#else
  v->scode = DISP_E_PARAMNOTFOUND;
#endif

}

/* ToDo: readVarOptional */

void 
writeVarSAFEARRAY( VARIANT* v, SAFEARRAY* sa, VARTYPE ovt )
{
  if (!v) return;
  VariantInit(v);
  v->vt     = VT_ARRAY | ovt;
  v->parray = sa;
}

HRESULT
readVarSAFEARRAY( VARIANT* v, SAFEARRAY** sa, VARTYPE ovt)
{
  if (!v) return E_POINTER;
  if (v->vt & VT_ARRAY || v->vt == VT_SAFEARRAY) {
    /* Will return same pointer, but let's follow the rules. */
    if (v->vt & VT_BYREF) {
      *sa = *(v->pparray);
    } else {
      *sa = v->parray;
    }
    return S_OK;
  } else {
        VARIANT w;
        HRESULT hr;

        VariantInit(&w);
        hr = VariantChangeType( &w, v, 0, VT_ARRAY | ovt);
        if (SUCCEEDED(hr)) {
           *sa = w.parray;
        } else  {
           *sa = NULL;
        }
        return hr;
  }
}

/* VT_EMPTY, VT_NULL, VT_CY, VT_VARIANT */

void writeVarEmpty( VARIANT* v )
{
   if (!v) return;
   VariantInit( v );
   v->vt = VT_EMPTY;
}

/* readEmpty; succeeds always */

void writeVarNull( VARIANT* v )
{
   if (!v) return;
   VariantInit( v );
   v->vt = VT_NULL;
}

HRESULT readVarNull( VARIANT* v )
{
  if (!v) return E_POINTER;
  if (v->vt == VT_NULL)
        return S_OK;
  else
  {
        VARIANT w;
        VariantInit( &w );
        return VariantChangeType( &w, v, 0, VT_NULL );
  }
}

void writeVarCurrency( int hi, unsigned int lo, VARIANT* v )
{
   if (!v) return;
   VariantInit(v);
   v->vt = VT_CY;
   v->cyVal.Hi  = hi;
   v->cyVal.Lo  = lo;
}

HRESULT readVarCurrency( VARIANT* v, int* hi, int* lo )
{
   if (!v) return E_POINTER;
   if (!hi) return E_POINTER;
   if (!lo) return E_POINTER;

   if (v->vt == VT_CY)
   {
        *hi = v->cyVal.Hi;
        *lo = v->cyVal.Lo;
        return S_OK;
   }
   else
   {
        VARIANT w;
        HRESULT hr;

        VariantInit(&w);
        hr = VariantChangeType( &w, v, 0, VT_CY);
        if (SUCCEEDED(hr))
        {
                *hi = w.cyVal.Hi;
                *lo = w.cyVal.Lo;
        }
        else
        {
                *hi = *lo = 0;
        }
        return hr;
   }
}

void writeVarWord64( unsigned int hi, unsigned int lo, VARIANT* v )
{
   ULONGLONG r;

   r = (ULONGLONG)hi;
   r >>= 32;
   r += (ULONGLONG)lo;

   if (!v) return;
   VariantInit(v);
   v->vt = VT_DECIMAL;
   v->decVal.Lo64  = r;
   v->decVal.Hi32  = 0;
   v->decVal.sign  = 0;
   v->decVal.scale = 0;
}

HRESULT readVarWord64( VARIANT* v, unsigned int* hi, unsigned int* lo )
{
   ULONGLONG r;

   if (!v) return E_POINTER;
   if (!hi) return E_POINTER;
   if (!lo) return E_POINTER;

   if (v->vt == VT_DECIMAL)
   {
        r   = v->decVal.Lo64;
        *lo = (unsigned long)r;
        r   <<= 32;
        *hi = (unsigned long)r;
        return S_OK;
   }
   else
   {
        VARIANT w;
        HRESULT hr;

        VariantInit(&w);
        hr = VariantChangeType( &w, v, 0, VT_DECIMAL);
        if (SUCCEEDED(hr))
        {
                r   = v->decVal.Lo64;
                *lo = (unsigned long)r;
                r   <<= 32;
                *hi = (unsigned long)r;
        }
        else
        {
                *hi = *lo = 0;
        }
        return hr;
   }
}


void writeVarVariant( VARIANT* x, VARIANT* v)
{
   if (!v) return;
   if (!x) return;

   VariantInit(v);
   VariantCopy( v, x );
}

HRESULT readVarVariant( VARIANT* v, VARIANT* x )
{
   if (!v)  return E_POINTER;
   if (!x)  return E_POINTER;

   VariantInit(x);
   return VariantCopy( x, v );
}

/* 
   First VARIANT* is uninitialised chunk of memory.
   Initialise and copy the second one into it.
*/
HRESULT
primCopyVARIANT ( VARIANT* p1
                , VARIANT* p2
                )
{
  VariantInit(p1);
  return (VariantCopy(p1,p2));
}

HRESULT
primVARIANTClear ( VARIANT* p1 )
{
  return VariantClear(p1);
}

/*
 * Converting a time_t to date/time representation used by
 * Automation.
 */
HRESULT
primClockToDate (int t, double *pt) 
{
  FILETIME ft;
  SYSTEMTIME st;
  LONGLONG l = Int32x32To64(t, 10000000) + 116444736000000000;

  ft.dwLowDateTime = (DWORD) l;
  ft.dwHighDateTime = l >>32;
  
  if (!FileTimeToSystemTime(&ft,&st)) {
    return E_FAIL;
  }
  
#if ( (!defined(__MINGW32__) && !defined(__CYGWIN__)) || \
       (__W32API_MAJOR_VERSION >= 1 && \
         (__W32API_MAJOR_VERSION > 1 || __W32API_MINOR_VERSION >= 1)) )
  /* If on a non-gnuwin platform or on one with a w32api version
     that's 1.1 or greater.
  */
  if (!SystemTimeToVariantTime(&st, pt)) {
    return E_FAIL;
  }
  return S_OK;
#else
  /* SystemTimeToVariantTime() isn't provided in the oleaut32
     import library until w32api 1.1 (or thereabouts, haven't 
     exactly pinpointed when this bug fixed).
   */
  return E_FAIL;
#endif
  
}
