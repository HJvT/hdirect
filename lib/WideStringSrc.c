/* define the DLL export macro */
#if defined(_WIN32) && !defined(__CYGWIN32__)
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

#if defined(_WIN32) && !defined(__CYGWIN32__)
#include <windows.h>
#define WCHAR wchar_t
#else
 #ifdef __CYGWIN32__
  #include <windows.h>
 #endif
#include <stdlib.h>
#define WCHAR wchar_t
#endif

#include "WideStringSrc.h"

#ifdef COM
#include "comPrim.h"
#else
#define UNI2STR(regstr, unistr) wcstombs (regstr, unistr, wcslen (unistr)+1)
#endif

/* wcslen() substitute implementation for cygwin'ers. */
#if defined(__CYGWIN32__) || defined(__CYGWIN__)

/* Troublesome to use the one that comes with 20.1 of cygwin */
#ifdef MB_CUR_MAX
#undef MB_CUR_MAX
#endif
#define MB_CUR_MAX 2

static int wcslen(WCHAR* wstr)
{
  char str[MB_CUR_MAX];
  char msg[80];
  WCHAR* wptr;
  int nbytes, i = 0;
  
  wptr = wstr;

  if ( wptr == NULL )
     return 0;

  while ( *wptr != L'\0' ) {
     nbytes = wctomb ( str, *wptr );
     i += nbytes; (char*)wptr += nbytes;
  }
  return i;
}
#endif


/* Return the length of a wide string big enough
   to hold the multi-byte string.
*/
unsigned int wideStringLen (char* str)
{
#ifdef _WIN32
  return MultiByteToWideChar(CP_ACP, 0, str, -1, NULL, 0);
#else
  /* Non Win32 specific version */
  WCHAR foo[300];  // Ho-ho, bulletproof!
  unsigned int len;

  if (!str) {
     return 0;
  } else {
     len = mbstowcs (foo, str, strlen(str));
     return ( (len < 0) ? 0 : len);
  }
#endif
}

/* ToDo: sort out source code deps. and make
   the return type HRESULT.
*/
int
wideToString ( WCHAR* wstr, char** pstr )
{
  int len;
  char* str;

  if (!pstr) {
#ifdef _WIN32
    return E_FAIL;
#else
    return -1;
#endif
  }

#if defined(_WIN32) && defined(COM)

  /* Compute how big a buffer we need to allocate for
     multi-byte string...
  */
  len = WideCharToMultiByte ( CP_ACP, 0, wstr, -1
			    , NULL, 0, NULL, NULL);
  if (len == 0) {
    return E_FAIL;
  }

  /* 
    Ask COM task allocator for some memory.
   */
  str = CoTaskMemAlloc(len * sizeof(char));
  if (str == NULL) {
    return E_FAIL;
  }

  WideCharToMultiByte(CP_ACP, 0, wstr, -1, str, len, NULL, NULL);
  *pstr = str;
  return S_OK;

#else

  len = wcslen(wstr);
  str=(char*)malloc(len+1);
  UNI2STR(str,wstr);
  *pstr = str;
  return 0;

#endif
}

/* Converting (null-terminated) strings into wide/Unicode ones */
int primStringToWide( char* str, unsigned int len, WCHAR* wstr , unsigned int wlen )
{
#ifdef _WIN32
  return MultiByteToWideChar(CP_ACP, 0, str, len, wstr, wlen);
#else
  int l;

  if ( wstr ) {
    if ( !str ) {
       *wstr = 0;
       return 0;
    } else {
       l = mbstowcs (wstr, str, wlen+1);
       return l;
    }
 } else {
    return (-1);
 }
#endif
}

/* The proto might not be in scope, so you might
   just get a warning from the C compiler.
   
   Reluctant to define the proto, as experience has
   shown this to be troublesome (there appears to be no
   universal agreement as to what the return type of
   wcslen() is.)

extern int wcslen (const WCHAR* wstr);
extern size_t wcslen (const WCHAR* wstr);

*/  

int
lenWideString (WCHAR* wstr )
{
  return wcslen(wstr);
}
