/*
 To free ourselves from a couple of needless compiler/
 header file dependencies, we provide a hand-written
 set of definitions for IUnknown.
*/
#ifndef __COMPRIM_H__
#define __COMPRIM_H__

#define COBJMACROS
#include <windows.h>

#if defined(__MINGW32__) || defined(__CYGWIN32__)
/* Get at w32api version; assume both mingw and cygwin use it. */
#include <w32api.h>
#endif

#ifndef REFIID
typedef GUID IID;
#define REFIID const IID *
#endif

#ifndef REFCLSID
#define REFCLSID const CLSID *
#endif

#define FACILITY_WIN32                   7
#ifndef HRESULT_FROM_WIN32
#define HRESULT_FROM_WIN32(x)   (x ? ((HRESULT) (((x) & 0x0000FFFF) | (FACILITY_WIN32 << 16) | 0x80000000)) : 0 )
#endif

#ifndef SUCCEEDED
#define SUCCEEDED(hr)  ((hr) >= 0)
#endif

#if __W32API_MAJOR_VERSION == 1
typedef WCHAR OLECHAR;
typedef OLECHAR* LPOLESTR;
typedef const OLECHAR* LPCOLESTR;

typedef WCHAR* BSTR;
#endif

extern const IID IID_NULL;
extern const IID IID_IUnknown;
extern const IID IID_IClassFactory;

#if __W32API_MAJOR_VERSION == 1
typedef struct IUnknown IUnknown;

typedef struct IUnknownVtbl {
      HRESULT (STDCALL *QueryInterface) 
                           ( IUnknown* This
                           , REFIID riid
                           , void** ppvObject
                           );
      ULONG   (STDCALL *AddRef)  ( IUnknown* This );
      ULONG   (STDCALL *Release) ( IUnknown* This );
} IUnknownVtbl;

struct IUnknown { struct IUnknownVtbl* lpVtbl;};

#define IUnknown_QueryInterface(this,riid,ppvObject)	\
    (this)->lpVtbl->QueryInterface(this,riid,ppvObject)

#define IUnknown_AddRef(this)	(this)->lpVtbl->AddRef(this)

#define IUnknown_Release(this)	(this)->lpVtbl->Release(this)
#endif

#ifndef S_OK
#define S_OK                      (HRESULT)0x00000000L
#endif
#ifndef S_OK
#define S_FALSE                   (HRESULT)0x00000001L
#endif
#ifndef E_POINTER
#define E_POINTER                 (HRESULT)0x80004003L
#endif
#ifndef E_OUTOFMEMORY
#define E_OUTOFMEMORY             (HRESULT)0x8007000EL
#endif
#ifndef E_INVALIDARG
#define E_INVALIDARG              (HRESULT)0x80070057L
#endif
#ifndef E_NOINTERFACE
#define E_NOINTERFACE             (HRESULT)0x80004002L
#endif
#ifndef E_FAIL
#define E_FAIL                    (HRESULT)0x80004005L
#endif
#ifndef CO_E_CLASSSTRING
#define CO_E_CLASSSTRING          (HRESULT)0x800401F3L
#endif
#ifndef CO_E_CLSREG_INCONSISTENT
#define CO_E_CLSREG_INCONSISTENT  (HRESULT)0x8000401FL
#endif
#ifndef DISP_E_EXCEPTION
#define DISP_E_EXCEPTION          (HRESULT)0x80020009L
#endif
#ifndef DISP_E_UNKNOWNNAME
#define DISP_E_UNKNOWNNAME        (HRESULT)0x80020006L
#endif
#ifndef DISP_E_BADPARAMCOUNT
#define DISP_E_BADPARAMCOUNT      (HRESULT)0x8002000EL
#endif
#ifndef DISP_E_PARAMNOTFOUND
#define DISP_E_PARAMNOTFOUND      (HRESULT)0x80020004L
#endif
#ifndef CLASS_E_NOAGGREGATION
#define CLASS_E_NOAGGREGATION     (HRESULT)0x80040110L
#endif
#ifndef CLASS_E_CLASSNOTAVAILABLE
#define CLASS_E_CLASSNOTAVAILABLE (HRESULT)0x80040111L
#endif

#ifndef FAILED
#define FAILED(hr)   ((HRESULT)(hr)<0)
#endif

extern
HRESULT
STDCALL
OleInitialize ( void* pvReserved );

extern
void
STDCALL
OleUninitialize ();

extern
HRESULT
STDCALL
CoCreateInstance (
		  REFCLSID   rclsid,
		  IUnknown*  pUnkOuter,
		  DWORD      dwClsContext,
		  REFIID     riid,
		  void**     ppv);

#if __W32API_MAJOR_VERSION == 1
extern
HRESULT
STDCALL
CoCreateInstanceEx (
		  REFCLSID   rclsid,
		  IUnknown*  pUnkOuter,
		  DWORD      dwClsContext,
		  void*      pServerInfo,
		  ULONG      cmq,
		  void*      pResults);

extern
HRESULT
STDCALL
CoRegisterClassObject (
		  REFCLSID   rclsid,
		  IUnknown*  pUnkOuter,
		  DWORD      dwClsContext,
		  DWORD      flags,
		  DWORD**    pdwRegister);
#endif

extern
HRESULT
STDCALL
CoRevokeClassObject (
		  DWORD      dwRegister
                    );

extern
HRESULT
STDCALL
primCreateInstance (
		  CLSID*     clsid,
		  IUnknown*  pUnkOuter,
		  DWORD      dwClsContext,
		  IID*       iid,
		  void**     ppv);
extern
HRESULT
STDCALL
StringFromCLSID
             (REFCLSID rclsid,
	      WCHAR** lplpsz);

extern
int
STDCALL
StringFromGUID2
             ( const GUID* rguid
	     , LPOLESTR    lpsz
	     , int         cbMax
	     );

extern
HRESULT
STDCALL
ProgIDFromCLSID
             (REFCLSID rclsid,
	      WCHAR** lplpsz);

extern
HRESULT
STDCALL
CLSIDFromString
             (WCHAR* lpsz,
	      CLSID* pclsid);

/* Helper functions defined in ComPrimSrc.c */
extern HRESULT primStringToGUID( WCHAR* guidStr, GUID* guid );
extern char*   hresultString( HRESULT hr );
extern HRESULT primGUIDToString( CLSID* guid, WCHAR** guidStr );
extern DWORD   lOCALE_USER_DEFAULT ();
extern IID*    primNullIID();

extern HRESULT primQI (void* methPtr, void* iptr, void* rclsid, void** ppv);
extern unsigned int primAddRef (void* methPtr, void* iptr);
extern unsigned int primRelease(void* methPtr, void* iptr);

extern void releaseIUnknown__(void* iptr);
extern void* addrOfReleaseIUnknown();

extern HRESULT primEnumNext (void* methPtr, void* iptr, unsigned int celt, void* ptr, void* po);
extern HRESULT primEnumSkip (void* methPtr, void* iptr, unsigned int celt);
extern HRESULT primEnumReset (void* methPtr, void* iptr);
extern HRESULT primEnumClone (void* methPtr, void* iptr,void* ppv);

extern HRESULT primPersistLoad(void* methPtr, void* iptr, void* pszFileName, unsigned int dwMode);

extern HRESULT bstrToStringLen( BSTR bstr, int len, char* p );
extern int     bstrLen( BSTR bstr );
extern HRESULT stringToBSTR( const char* p, BSTR* pbstr );

#if __W32API_MAJOR_VERSION == 1
typedef struct IRunningObjectTable IRunningObjectTable;
typedef struct IEnumString IEnumString;
/* Close enough for our purposes.. */
typedef int BIND_OPTS;

typedef struct IBindCtx IBindCtx;

    typedef struct IBindCtxVtbl
    {
        HRESULT ( STDCALL *QueryInterface )
	           ( IBindCtx * This
		   , REFIID riid
		   , void **ppvObject
		   );
        
        ULONG ( STDCALL *AddRef )( IBindCtx * This );
        
        ULONG ( STDCALL *Release )( IBindCtx * This );
        
        HRESULT ( STDCALL *RegisterObjectBound )
	           ( IBindCtx * This
		   , IUnknown *punk
		   );
        
        HRESULT ( STDCALL *RevokeObjectBound )
	           ( IBindCtx * This
		   , IUnknown *punk
		   );
        
        HRESULT ( STDCALL *ReleaseBoundObjects )
	           ( IBindCtx * This );
        
        /* [local] */ HRESULT ( STDCALL *SetBindOptions )
	           ( IBindCtx * This
		   , BIND_OPTS *pbindopts
		   );
        
        /* [local] */ HRESULT ( STDCALL *GetBindOptions )
	           ( IBindCtx * This
		   , BIND_OPTS *pbindopts
		   );
        
        HRESULT ( STDCALL *GetRunningObjectTable )
	           ( IBindCtx * This
		   , IRunningObjectTable **pprot
		   );
        
        HRESULT ( STDCALL *RegisterObjectParam )
	           ( IBindCtx * This
		   , LPOLESTR pszKey
		   , IUnknown *punk
		   );
        
        HRESULT ( STDCALL *GetObjectParam )
	           ( IBindCtx * This
		   , LPOLESTR pszKey
		   , IUnknown **ppunk
		   );
        
        HRESULT ( STDCALL *EnumObjectParam )
	           ( IBindCtx * This
		   , IEnumString **ppenum
		   );
        
        HRESULT ( STDCALL *RevokeObjectParam )
	           ( IBindCtx * This
		   , LPOLESTR pszKey
		   );
    } IBindCtxVtbl;

struct IBindCtx { struct IBindCtxVtbl *lpVtbl; };

typedef struct IStream IStream;
typedef struct IEnumMoniker IEnumMoniker;

typedef struct IMoniker IMoniker;
    typedef struct IMonikerVtbl
    {
        HRESULT ( STDCALL *QueryInterface )
	           ( IMoniker * This
		   , REFIID riid
		   , void **ppvObject
		   );
        
        ULONG ( STDCALL *AddRef )( IMoniker * This );
        
        ULONG ( STDCALL *Release )( IMoniker * This );

        HRESULT ( STDCALL *GetClassID )
	           ( IMoniker * This
		   , CLSID *pClassID
		   );
        
        HRESULT ( STDCALL *IsDirty )( IMoniker * This );
        
        HRESULT ( STDCALL *Load )
	           ( IMoniker * This
		   , IStream *pStm
		   );
        
        HRESULT ( STDCALL *Save )
	           ( IMoniker * This
		   , IStream *pStm
		   , BOOL fClearDirty
		   );
        
        HRESULT ( STDCALL *GetSizeMax )
	           ( IMoniker * This
		   , ULARGE_INTEGER *pcbSize
		   );
        
        HRESULT ( STDCALL *BindToObject )
	           ( IMoniker * This
		   , IBindCtx *pbc
		   , IMoniker *pmkToLeft
		   , REFIID riidResult
		   , void **ppvResult
		   );
        
        HRESULT ( STDCALL *BindToStorage )
	           ( IMoniker * This
		   , IBindCtx *pbc
		   , IMoniker *pmkToLeft
		   , REFIID riid
		   , void **ppvObj
		   );
        
        HRESULT ( STDCALL *Reduce )
	           ( IMoniker * This
		   , IBindCtx *pbc
		   , DWORD dwReduceHowFar
		   , IMoniker **ppmkToLeft
		   , IMoniker **ppmkReduced
		   );
        
        HRESULT ( STDCALL *ComposeWith )
	           ( IMoniker * This
		   , IMoniker *pmkRight
		   , BOOL fOnlyIfNotGeneric
		   , IMoniker **ppmkComposite
		   );
        
        HRESULT ( STDCALL *Enum )
	           ( IMoniker * This
		   , BOOL fForward
		   , IEnumMoniker **ppenumMoniker
		   );
        
        HRESULT ( STDCALL *IsEqual )
	           ( IMoniker * This
		   , IMoniker *pmkOtherMoniker
		   );
        
        HRESULT ( STDCALL *Hash )
	           ( IMoniker * This
		   , DWORD *pdwHash
		   );
        
        HRESULT ( STDCALL *IsRunning )
	           ( IMoniker * This
		   , IBindCtx *pbc
		   , IMoniker *pmkToLeft
		   , IMoniker *pmkNewlyRunning
		   );
        
        HRESULT ( STDCALL *GetTimeOfLastChange )
	           ( IMoniker * This
		   , IBindCtx *pbc
		   , IMoniker *pmkToLeft
		   , FILETIME *pFileTime
		   );
        
        HRESULT ( STDCALL *Inverse )
	           ( IMoniker * This
		   , IMoniker **ppmk
		   );
        
        HRESULT ( STDCALL *CommonPrefixWith )
	           ( IMoniker * This
		   , IMoniker *pmkOther
		   , IMoniker **ppmkPrefix
		   );
        
        HRESULT ( STDCALL *RelativePathTo )
	           ( IMoniker * This
		   , IMoniker *pmkOther
		   , IMoniker **ppmkRelPath
		   );
        
        HRESULT ( STDCALL *GetDisplayName )
	           ( IMoniker * This
		   , IBindCtx *pbc
		   , IMoniker *pmkToLeft
		   , LPOLESTR *ppszDisplayName
		   );
        
        HRESULT ( STDCALL *ParseDisplayName )
	           ( IMoniker * This
		   , IBindCtx *pbc
		   , IMoniker *pmkToLeft
		   , LPOLESTR pszDisplayName
		   , ULONG *pchEaten
		   , IMoniker **ppmkOut
		   );
        
        HRESULT ( STDCALL *IsSystemMoniker )
	           ( IMoniker * This
		   , DWORD *pdwMksys
		   );
    } IMonikerVtbl;

struct IMoniker { struct IMonikerVtbl* lpVtbl; };

#define IMoniker_BindToObject(This,pbc,pmkToLeft,riidResult,ppvResult)	\
    (This)->lpVtbl -> BindToObject(This,pbc,pmkToLeft,riidResult,ppvResult)

typedef struct IEnumUnknown IEnumUnknown;

typedef struct IEnumUnknownVtbl
    {
        HRESULT ( STDCALL *QueryInterface )
	            ( IEnumUnknown * This
		    , REFIID riid
		    , void **ppvObject
		    );
        
        ULONG ( STDCALL *AddRef )( IEnumUnknown * This );
        
        ULONG ( STDCALL *Release )( IEnumUnknown * This );
        
        HRESULT ( STDCALL *Next )
	            ( IEnumUnknown * This
		    , ULONG celt
		    , IUnknown **rgelt
		    , ULONG *pceltFetched
		    );
        
        HRESULT ( STDCALL *Skip )
	            ( IEnumUnknown * This
		    , ULONG celt
		    );
        
        HRESULT ( STDCALL *Reset )(  IEnumUnknown * This );
        
        HRESULT ( STDCALL *Clone )( IEnumUnknown * This, IEnumUnknown **ppenum );

    } IEnumUnknownVtbl;

struct IEnumUnknown { IEnumUnknownVtbl* lpVtbl; };

extern
void*
STDCALL
CoTaskMemAlloc(unsigned int size);

void
STDCALL
CoTaskMemFree(LPVOID pv);

extern
HRESULT
STDCALL
GetActiveObject (CLSID* clsid, void* reserved, IUnknown** ppunk);

extern BSTR STDCALL SysAllocStringLen(OLECHAR* pch, unsigned int cch);
extern BSTR STDCALL SysAllocStringByteLen(char* pch, unsigned int len);
extern void STDCALL SysFreeString(BSTR bstr);
extern UINT STDCALL SysStringLen(BSTR bstr);
#endif

extern
HRESULT
STDCALL
MkParseDisplayName( IBindCtx* pbc
		  , LPCOLESTR szUserName
		  , unsigned long* eatern
		  , IMoniker** ppmk);

extern
HRESULT
STDCALL
CreateBindCtx (DWORD reserved, IBindCtx** ppbc);

#if __W32API_MAJOR_VERSION == 1
extern
HRESULT
STDCALL CreateTypeLib ( int syskind, LPCOLESTR szFile, void** ppv);

extern
HRESULT
STDCALL CoCreateGuid ( GUID* pguid );

extern
HRESULT
STDCALL CreateTypeLib2 ( int syskind, LPCOLESTR szFile, void** ppv);
#endif

#ifndef STR2UNI 
#define STR2UNI(unistr, regstr) mbstowcs (unistr, regstr, strlen (regstr)+1)
#define UNI2STR(regstr, unistr) wcstombs (regstr, unistr, wcslen (unistr)+1)
#endif 

#if __W32API_MAJOR_VERSION == 1
BOOL
STDCALL
IsEqualGUID (const GUID* g1, const GUID* g2);

HRESULT
STDCALL
LoadTypeLib
             ( LPOLESTR    lpsz
	     , IUnknown**  ppv
	     );

HRESULT
STDCALL
LoadTypeLibEx
             ( LPOLESTR    lpsz
	     , int         kind
	     , IUnknown**  ppv
	     );

HRESULT
STDCALL
LoadRegTypeLib
             ( GUID*       rguid
	     , WORD        wVerMajor
	     , WORD        wVerMinor
	     , LCID        lcid
	     , IUnknown**  ppv
	     );
#endif

extern
HRESULT
primLoadRegTypeLib 
                ( GUID* rguid
		, short wMaj
		, short wMin
		, LCID lcid
		, void** ppv
		);

#if __W32API_MAJOR_VERSION == 1
extern
HRESULT
STDCALL
QueryPathOfRegTypeLib
             ( GUID* rguid
	     , unsigned short maj
	     , unsigned short min
	     , LCID  lcid
	     , BSTR* pbstr
	     );

#endif

extern
BSTR
primQueryPathOfRegTypeLib
             ( GUID* rguid
	     , unsigned short maj
	     , unsigned short min
	     );

extern
char*
getModuleFileName
             ( HANDLE hMod
	     );

#if __W32API_MAJOR_VERSION == 1
extern
DWORD
WINAPI
GetModuleFileNameA
             ( HINSTANCE hMod
	     , LPSTR     lpFileName
	     , DWORD     size
	     );

extern
LCID
WINAPI
GetUserDefaultLCID
             ();

extern
INT
STDCALL
SystemTimeToVariantTime (void* lpSystemTime, double* pvarTime);
#endif

extern void    messageBox (char* str, char* t, unsigned long x);
extern HRESULT primCreateTypeLib ( int i, LPOLESTR fname, void** ppv );
extern BOOL    primComEqual( IUnknown* unk1, IUnknown* unk2 );
extern HRESULT primCopyGUID( GUID* g1, GUID* g2);
extern HRESULT primNewGUID( GUID* g1);
extern HRESULT bindObject( const WCHAR* name, IID* iid, void** unk );
extern HRESULT primProgIDFromCLSID( const CLSID* clsid, WCHAR** clsidStr );
extern HRESULT primCLSIDFromProgID( const char* progid, CLSID* clsid );
extern void    comUnInitialize(void);
extern HRESULT comInitialize(void);
extern HRESULT primStringToGUID( WCHAR* guidStr, GUID* guid );
extern HRESULT primGUIDToString( CLSID* guid, WCHAR** guidStr );

extern void    postQuitMsg();
extern void    messagePump();
extern void*   finalNoFree();

extern HANDLE  mkEvent();
extern void    waitForEvent(HANDLE h);
extern void    signalEvent(HANDLE h);

extern void primGetVersionInfo ( unsigned long*, unsigned long*, unsigned long*);

#endif /* __COMPRIM_H__ */
