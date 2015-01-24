/* This stuff will eventually be replaced by calls to the
   storage manager in the new GHC/Hugs RTS.   -- sof
*/

#if defined(__CYGWIN32__) || defined(__MINGW32__) || defined(__CYGWIN__)
#define __BASTARDIZED_WIN32__ 1
#endif

/*-----------------------------------------------------------
-- (c) 1998,  Daan Leijen, leijen@@fwi.uva.nl
-----------------------------------------------------------*/
/* #define DEBUG */

/* if you don't use COM, comment this out */
/* (pass the setting for this via command line instead.) */
/* #define COM */

/* define the DLL export macro */
#if defined(_BASTARDIZED_WIN32__)
#define DLLEXPORT(res)  __declspec(dllexport) res
#else
/* redef this default for your system */
#define DLLEXPORT(res)  extern res
#endif


#ifdef COM
#define COBJMACROS
#define CINTERFACE
#endif

#include <stdio.h>

#if !defined(__BASTARDIZED_WIN32__) && defined(COM)
#include <windows.h>
#else
# if defined(__BASTARDIZED_WIN32__)
  #include <windows.h>
# if defined(__BASTARDIZED_WIN32__) && defined(COM)
  #include "comPrim.h"
# endif
# endif
#include <stdlib.h>
#endif

#ifdef DEBUG
static int nallocs = 0;
static int nallocs_ = 0;
#endif

void primPointerCheck(void)
{
#ifdef DEBUG
    if (nallocs - nallocs_ != 0)
      printf( "pointer errors: allocs - frees = %i\n", nallocs - nallocs_ );
    nallocs_ = nallocs;
#endif    
}     

/*-----------------------------------------------------------
-- Free routines for foreign objects
-----------------------------------------------------------*/

DLLEXPORT(void) primFreeBSTR( void* p )
{
#ifdef COM
   if (p) SysFreeString( (BSTR)p );
#endif
}

DLLEXPORT(void) primNoFree( void* p )
{
#if 0
 char    msg[200];
 WCHAR* wmsg;
 HRESULT hr;

 sprintf(msg, "freeing: %p", p);
 MessageBox(NULL, msg, "primNoFree", MB_OK );

 hr = primGUIDToString(p, &wmsg);
 if (SUCCEEDED(hr)) {
    MessageBoxW(NULL, wmsg, L"primNoFree-clsid", MB_OK);
 }
#endif
}

void primFinalise (void* f, void* a)
{ ((void (*)(void *))f)(a); } 


/*-----------------------------------------------------------
-- Memory allocation
-----------------------------------------------------------*/

DLLEXPORT(void*) primAllocMemory( int size )
{
#ifdef COM

#ifdef DEBUG
    void* p;
    p = CoTaskMemAlloc( size );
    printf("alloc: %p, size = %i\n", p, size );
    if (p) nallocs++;
    return p;
#else
    return CoTaskMemAlloc(size);
#endif

#else
    return malloc(size);
#endif
}

/* COM version of freeing */
#ifdef COM
DLLEXPORT(void) primFreeMemory( void* p )
{
 #ifdef DEBUG    
   if (!p) printf( "freeing null pointer\n" );
 #endif
   if (!p) return;
 #ifdef DEBUG
   if (p) nallocs--;
   printf( "free: %p\n", p);
 #endif
   CoTaskMemFree( p );
}
#endif

#ifndef COM
DLLEXPORT(void) primFreeMemory( void* p )
{
 #ifdef DEBUG    
   if (!p) printf( "free null pointer\n" );
 #endif

   if (!p) return;

   free(p);
}
#endif /* !COM */

/* Strictly speaking, converting a function pointer to a void*
   is not guaranteed to be information preserving in ANSI C.
*/
void* finalFreeMemory() { return (void*)&primFreeMemory; }
void* finalNoFree()     { return (void*)&primNoFree;   }
void* finalFreeBSTR()   { return (void*)&primFreeBSTR; }
