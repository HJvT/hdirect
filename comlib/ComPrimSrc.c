/* #define DEBUG */

/* If you don't want the 'stack' allocator, comment this out */
/* #define USESTACK */

#ifndef COM
#define COM
#endif

/* define the DLL export macro */
#if defined(_WIN32) && !defined(__CYGWIN32__) && !defined(__MINGW32__)
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

#ifdef _MSC_VER
#include <windows.h>
#else
#include <stdlib.h>
#include "comPrim.h"
#include "PointerSrc.h"
#endif

#define MAXSZ 255

const IID IID_NULL = {0x00000000L, 0x0000, 0x0000, {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}};
#ifndef _MSC_VER
const IID IID_IUnknown = {0x00000000L, 0x0000, 0x0000, {0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46}};
const IID IID_IClassFactory = {0x00000001L, 0x0000, 0x0000, {0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46}};
#endif

DLLEXPORT(char*) hresultString( HRESULT hr )
{
   static char msgbuf[256];
   int len;

   sprintf( msgbuf, "(0x%lx) ", hr );
   len = strlen(msgbuf);

   FormatMessageA(
        FORMAT_MESSAGE_FROM_SYSTEM,
        NULL,
        hr,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL),
        msgbuf + len,
        256,
        NULL
    );

    return msgbuf;
}


HRESULT primStringToGUID( WCHAR* guidStr, GUID* guid )
{
   if (guid) return CLSIDFromString( guidStr, guid );
        else  return E_OUTOFMEMORY;
}

HRESULT primGUIDToString( CLSID* guid, WCHAR** guidStr )
{
   CLSID* rclsid;
   HRESULT hr;

   rclsid = (CLSID*)guid;

   if (!rclsid)   rclsid = (CLSID*)&IID_NULL;
   if (guidStr) {
      hr = StringFromCLSID( rclsid, guidStr  );
   } else {
      hr = E_POINTER;
   }
   return hr;
}

HRESULT primProgIDFromCLSID( const CLSID* clsid, WCHAR** clsidStr )
{
   if (!clsid)   clsid = &IID_NULL;
   if (clsidStr) return ProgIDFromCLSID( clsid, clsidStr );
            else return E_POINTER;
}


/*-----------------------------------------------------------
-- Com library calls
-----------------------------------------------------------*/

static int com_already_initialized = 0;
static int com_already_uninitialized = 0;

HRESULT comInitialize(void)
{
   if (!com_already_initialized) {
      com_already_initialized = 1;
      return OleInitialize(NULL);
   } else {
      return S_OK;
   }
   
}

void shutdown__()
{
  OleUninitialize();
  com_already_initialized = 0;
  com_already_uninitialized = 0;
}

void comUnInitialize(void)
{
  if (!com_already_uninitialized) {
     com_already_uninitialized=1;
     atexit(shutdown__);
  }
}

/*-----------------------------------------------------------
-- Object Creation
-----------------------------------------------------------*/

/* CLSIDFromProgID is a bit too naive, so I wrote it myself */
HRESULT primCLSIDFromProgID( const char* progid, CLSID* clsid )
{
  HRESULT hr;
  char  name[MAXSZ];
  DWORD namesz = MAXSZ;
  BYTE  value[MAXSZ];
  DWORD valuesz = MAXSZ;
  WCHAR clsidName[MAXSZ];
  DWORD type   = REG_SZ;
  HKEY  key    = 0;
  HKEY  subkey = 0;

  if (!clsid)   return E_POINTER;
  if (!progid)  return E_POINTER;

  hr = HRESULT_FROM_WIN32(RegOpenKeyExA( HKEY_CLASSES_ROOT, progid, 0, KEY_READ, &key ));
  if (FAILED(hr)) return CO_E_CLASSSTRING;

  hr = HRESULT_FROM_WIN32(RegOpenKeyExA( key, "CurVer", 0, KEY_READ, &subkey ));
  if (SUCCEEDED(hr))
  {
     hr = HRESULT_FROM_WIN32(RegEnumValueA( subkey, 0, name, &namesz, NULL, &type, value, &valuesz));
     RegCloseKey( key );
     RegCloseKey( subkey );
     if (FAILED(hr)) return hr;

     hr = HRESULT_FROM_WIN32(RegOpenKeyExA( HKEY_CLASSES_ROOT, (char*)value, 0, KEY_QUERY_VALUE, &key ));
     if (FAILED(hr)) return CO_E_CLASSSTRING;

     namesz  = MAXSZ;
     valuesz = MAXSZ;
  }

  hr = HRESULT_FROM_WIN32(RegOpenKeyExA( key, "CLSID", 0, KEY_READ, &subkey ));
  if (FAILED(hr)) { RegCloseKey( key ); return CO_E_CLSREG_INCONSISTENT; };

  hr = HRESULT_FROM_WIN32(RegEnumValueA( subkey, 0, name, &namesz, NULL, &type,value,&valuesz));
  RegCloseKey( subkey );
  RegCloseKey( key );
  if (FAILED(hr)) return hr;

  mbstowcs( clsidName, (char*)value, MAXSZ );
  hr = CLSIDFromString( clsidName, clsid );

  return hr;
}

/* Newer COM libraries supply the functionality of
   bindObject() via CoGetObject(), but to avoid depending
   on that being around, we stick with our own implementation.
*/
HRESULT bindObject( const WCHAR* name, IID* iid, void** unk )
{
  HRESULT hr;
  IBindCtx    *bc;
  IMoniker    *mk;
  ULONG        count;

  if (!unk) return E_POINTER;
       else *unk = NULL;
  if (!iid) return E_POINTER;

  bc = NULL;
  mk = NULL;

  hr = CreateBindCtx(0, &bc);
  if (FAILED(hr)) return hr;

  hr = MkParseDisplayName(bc, name, &count, &mk);
  if (FAILED(hr)) { IUnknown_Release(bc); return hr; }

  hr = IMoniker_BindToObject( mk, bc, NULL, iid, unk );
  IUnknown_Release(mk);
  IUnknown_Release(bc);

  return hr;
}

typedef HRESULT (__stdcall * qi_methPtrTy)(void*,void*,void**);

HRESULT primQI ( /*[in]*/void*  methPtr
               , /*[in]*/void*  iptr
               , /*[in]*/void*  riid
               , /*[in]*/void** ppv
               )
{
 return ((qi_methPtrTy)methPtr)(iptr,riid,ppv);
}

typedef unsigned int (__stdcall * addRef_methPtrTy)(void*);
 
unsigned int
primAddRef ( /*[in]*/void*  methPtr
           , /*[in]*/void*  iptr
           )
{
 return ((addRef_methPtrTy)methPtr)(iptr);
}

typedef unsigned int (__stdcall * release_methPtrTy)(void*);
 
unsigned int
primRelease ( /*[in]*/void*  methPtr
            , /*[in]*/void*  iptr
            )
{
 return ((release_methPtrTy)methPtr)(iptr);
}

typedef unsigned int (__stdcall * enumNext_methPtrTy)(void* iptr, unsigned int celt, void* ptr, void* po);

HRESULT
primEnumNext(void* methPtr, void* iptr, unsigned int celt, void* ptr, void* po)
{
 return ((enumNext_methPtrTy)methPtr)(iptr,celt,ptr,po);

}

typedef unsigned int (__stdcall * enumSkip_methPtrTy)(void* iptr, unsigned int celt);

HRESULT
primEnumSkip(void* methPtr, void* iptr, unsigned int celt)
{
 return ((enumSkip_methPtrTy)methPtr)(iptr,celt);

}

typedef unsigned int (__stdcall * enumReset_methPtrTy)(void* iptr);

HRESULT
primEnumReset(void* methPtr, void* iptr)
{
 return ((enumReset_methPtrTy)methPtr)(iptr);
}

typedef unsigned int (__stdcall * enumClone_methPtrTy)(void* iptr,void* ppv);

HRESULT
primEnumClone(void* methPtr, void* iptr, void* ppv)
{
 return ((enumClone_methPtrTy)methPtr)(iptr,ppv);

}

typedef unsigned int (__stdcall * persistLoad_methPtrTy)(void* iptr,void* pszFileName,unsigned int dwMode);

HRESULT
primPersistLoad(void* methPtr, void* iptr, void* pszFileName, unsigned int dwMode)
{
 return ((persistLoad_methPtrTy)methPtr)(iptr,pszFileName,dwMode);
}

HRESULT primGetActiveObject( CLSID* clsid, IUnknown** unk )
{
  if (!unk) return E_POINTER;
       else *unk = NULL;
  return GetActiveObject( clsid, NULL, unk );
}

HRESULT primLoadRegTypeLib 
                        ( GUID* rguid
                        , short wMaj
                        , short wMin
                        , LCID lcid
                        , void** ppv
                        )
{
  HRESULT hr;

  hr = LoadRegTypeLib(rguid, wMaj, wMin, GetUserDefaultLCID(), (IUnknown**)ppv);
  return hr;  
}


void messageBox (char* str, char* t, unsigned long x)
{
 MessageBox ( NULL, str, t, x);
 return;
}

IID* primNullIID () { return (IID*)(&IID_NULL); }

DWORD
lOCALE_USER_DEFAULT()
{ 
 return(GetUserDefaultLCID());
}

BOOL  primComEqual( IUnknown* unk1, IUnknown* unk2 )
{
   IUnknown* obj1;
   IUnknown* obj2;
   BOOL    res;
   HRESULT hr;

   if (!unk1)  return (unk2 == NULL);
   if (!unk2)  return FALSE;

   hr = IUnknown_QueryInterface( unk1, &IID_IUnknown, (void**)&obj1 );
   if (FAILED(hr)) return FALSE;
   hr = IUnknown_QueryInterface( unk2, &IID_IUnknown, (void**)&obj2 );
   if (FAILED(hr)) {IUnknown_Release(obj1); return FALSE; }

   res = (obj1 == obj2);
   IUnknown_Release(obj1);
   IUnknown_Release(obj2);
   return res;
}

HRESULT
primCopyGUID (GUID* g1, GUID* g2)
{
  if (g2 == NULL || g1 == NULL) {
    return E_FAIL;
  } else {
    memcpy(g2,g1,sizeof(GUID));
    return S_OK;
  }
}

HRESULT
primNewGUID (GUID* g1)
{
  if (g1 == NULL) {
    return E_FAIL;
  } else {
    return CoCreateGuid(g1);
  }
}


/*
 *
 * CreateTypeLib() stub.
 *
 */
HRESULT primCreateTypeLib ( int i, LPOLESTR fname, void** ppv )
{
  /* Not much point really of defining this stub, since
   * should really be using the FFI, but a little bit
   * easier to debug this way until we know everything's reaonable
   * stable.
   */

  /* Unfortunately, the cygwin/mingw liboleaut32.a import library doesn't have an entry
   * for _CreateTypeLib@12, so you either have to generate your own version of it (which
   * I did), or fall back on CreateTypeLib().
   */
#ifdef _MSC
  return ( CreateTypeLib2(1/*SYS_WIN32*/, fname, ppv) );
#else
  return ( CreateTypeLib(1/*SYS_WIN32*/, fname, ppv) );
#endif
}

BSTR primQueryPathOfRegTypeLib ( GUID* rguid
                               , unsigned short maj
                               , unsigned short min
                               )
{
  BSTR bs;
  HRESULT hr;
  bs = SysAllocStringByteLen (NULL, 255);
  
  hr = QueryPathOfRegTypeLib ( rguid, maj, min, GetUserDefaultLCID(), &bs);
  
  if (FAILED(hr)) {
     SysFreeString(bs);
     return NULL;
  }
  return bs;
}

#define MAX_LEN_MOD_FNAME 2048

char*
getModuleFileName ( HANDLE hMod)
{
  char* buf = malloc(sizeof(char) * MAX_LEN_MOD_FNAME);
  DWORD len;

  len = GetModuleFileNameA(hMod, buf, MAX_LEN_MOD_FNAME);
  return buf;

}

/*-----------------------------------------------------------
-- Message pumping utils
-----------------------------------------------------------*/
void messagePump()
{
  MSG msg;

  while ( GetMessage(&msg, 0, 0, 0) ) {
    DispatchMessage(&msg);
  }
}

HANDLE
mkEvent()
{
  return CreateEvent(NULL, FALSE, FALSE, NULL);
}

void
waitForEvent (HANDLE h)
{
  HANDLE ha = h;
  DWORD dwR;
  MSG msg;

  while(1) {
    dwR = MsgWaitForMultipleObjects(1, &ha, FALSE, INFINITE, QS_ALLEVENTS);
    if (dwR == WAIT_OBJECT_0) {
       break;
    } else {
      while (PeekMessage(&msg,NULL, 0, 0, PM_REMOVE)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
      }
    }
  }
    
}

void
signalEvent(HANDLE h)
{
  SetEvent(h);
}

/* Add a WM_QUIT to the STA thread's message queue, causing the
   above pump to shutdown.
 */
void postQuitMsg()
{
 PostMessage(NULL, WM_QUIT, 0, 0); 
}

/*-----------------------------------------------------------
-- BSTR  <--> String operations:
-----------------------------------------------------------*/

HRESULT
stringToBSTR( /*[in,ptr]*/const char* pstrz
            , /*[out]*/ BSTR* pbstr
            )
{
  int i;

  if (!pbstr) {
    return E_FAIL;
  } else {
    *pbstr = NULL;
  }
  if (!pstrz) {
    return S_OK;
  }

  i = MultiByteToWideChar(CP_ACP, 0, pstrz, -1, NULL, 0);
  if ( i < 0 ) {
    return E_FAIL;
  }

  /* i is the length of the string *including* the null-terminator
   * (which we don't want in the BSTR).  So the length of the BSTR is
   * i-1.
   */
  *pbstr = SysAllocStringLen(NULL,i-1);
  if (*pbstr != NULL) {
    MultiByteToWideChar(CP_ACP, 0, pstrz, -1, *pbstr, i-1); 
    return S_OK;
  } else {
    return E_FAIL;
  }
}

int
bstrLen( /*[in]*/ BSTR bstr )
{
   if (!bstr) {
      return 0;
   }
   //   return wcstombs( NULL, bstr, SysStringLen(bstr)+1);
   return WideCharToMultiByte ( CP_ACP, 0, bstr, SysStringLen(bstr)
                              , NULL, 0, NULL, NULL);
}

HRESULT
bstrToStringLen ( BSTR bstr
                , int len
                , char* p
                )
{
  int i;
  if (!p) {
     return E_FAIL;
  } else {
     *p = '\0';
  }
  if (!bstr) {
     return E_FAIL;
  }
  //  i = wcstombs(p,bstr,len+1);
  i = WideCharToMultiByte ( CP_ACP, 0, bstr, SysStringLen(bstr)
                          , p, len, NULL, NULL);
  if ( i == 0 ) {
     return E_FAIL;
  } else {
     p[i]= '\0';
     return S_OK;
  }
}

/*
 * Wrapper for the interesting bits of GetVersionEx()
 */
void
primGetVersionInfo
    ( /*[out]*/unsigned long* maj
    , /*[out]*/unsigned long* min
    , /*[out]*/unsigned long* pid
    )
{
  OSVERSIONINFO oe;
  BOOL b;
  
  oe.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
  
  b = GetVersionEx(&oe);
  
#ifdef DEBUG
  if (!b) {
    fprintf(stderr, "GetVersionEx failed\n");
    exit(1);
  }
#endif
  /* Assume everything went well */
  *maj = oe.dwMajorVersion;
  *min = oe.dwMinorVersion;
  *pid = oe.dwPlatformId;

  return;
}

/*
 * IUnknown finaliser called by GC when ForeignObjs holding them 
 * are finalised.
 */

void
releaseIUnknown__(void* ip)
{
  DWORD d;
#ifdef DEBUG
  fprintf(stderr, "Releasing COM i-pointer 0x%p\n", ip); fflush(stderr);
#endif
  if (ip) {
    d = IUnknown_Release((IUnknown*)ip);
#ifdef DEBUG
    fprintf(stderr, "Released COM i-pointer 0x%p %d\n", ip, d); fflush(stderr);
#endif
  }
}

void*
addrOfReleaseIUnknown()
{
  /* Strictly speaking, converting a function pointer to a void*
     is not guaranteed to be information preserving in ANSI C.
  */
  return (void*)&releaseIUnknown__;
}
