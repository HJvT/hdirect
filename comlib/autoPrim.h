/*
 To free ourselves from a couple of needless compiler/
 header file dependencies, we provide a hand-written
 set of definitions for Automation related types
 and interfaces.
*/
#ifndef __AUTOPRIM_H__
#define __AUTOPRIM_H__

#include "comPrim.h"
#include "PointerSrc.h"

#if (defined(__CYGWIN32__) || defined(__MINGW32__)) && __W32API_MAJOR_VERSION == 1
typedef LONG DISPID;
#define DISPATCH_METHOD         0x1
#define DISPATCH_PROPERTYGET    0x2
#define DISPATCH_PROPERTYPUT    0x4
#define DISPATCH_PROPERTYPUTREF 0x8

#define	DISPID_VALUE	         (0)
#define	DISPID_UNKNOWN	        (-1)
#define	DISPID_PROPERTYPUT	(-3)
#define	DISPID_NEWENUM	        (-4)
#define	DISPID_EVALUATE	        (-5)
#define	DISPID_CONSTRUCTOR	(-6)
#define	DISPID_DESTRUCTOR	(-7)

typedef LONG SCODE;

enum VARENUM
    {	VT_EMPTY	= 0,
	VT_NULL	= 1,
	VT_I2	= 2,
	VT_I4	= 3,
	VT_R4	= 4,
	VT_R8	= 5,
	VT_CY	= 6,
	VT_DATE	= 7,
	VT_BSTR	= 8,
	VT_DISPATCH	= 9,
	VT_ERROR	= 10,
	VT_BOOL	= 11,
	VT_VARIANT	= 12,
	VT_UNKNOWN	= 13,
	VT_DECIMAL	= 14,
	VT_I1	= 16,
	VT_UI1	= 17,
	VT_UI2	= 18,
	VT_UI4	= 19,
	VT_I8	= 20,
	VT_UI8	= 21,
	VT_INT	= 22,
	VT_UINT	= 23,
	VT_VOID	= 24,
	VT_HRESULT	= 25,
	VT_PTR	= 26,
	VT_SAFEARRAY	= 27,
	VT_CARRAY	= 28,
	VT_USERDEFINED	= 29,
	VT_LPSTR	= 30,
	VT_LPWSTR	= 31,
	VT_FILETIME	= 64,
	VT_BLOB	= 65,
	VT_STREAM	= 66,
	VT_STORAGE	= 67,
	VT_STREAMED_OBJECT	= 68,
	VT_STORED_OBJECT	= 69,
	VT_BLOB_OBJECT	= 70,
	VT_CF	= 71,
	VT_CLSID	= 72,
	VT_BSTR_BLOB	= 0xfff,
	VT_VECTOR	= 0x1000,
	VT_ARRAY	= 0x2000,
	VT_BYREF	= 0x4000,
	VT_RESERVED	= 0x8000,
	VT_ILLEGAL	= 0xffff,
	VT_ILLEGALMASKED	= 0xfff,
	VT_TYPEMASK	= 0xfff
    };

typedef struct ITypeInfo ITypeInfo;

typedef struct tagEXCEPINFO 
{
  unsigned short wCode;
  unsigned short wReserved;
  BSTR bstrSource;
  BSTR bstrDescription;
  BSTR bstrHelpFile;
  unsigned long dwHelpContext;
  void* pvReserved;
  HRESULT (STDCALL *pfnDeferredFillIn) (struct tagEXCEPINFO*);
  SCODE scode;
} EXCEPINFO;

typedef unsigned short VARTYPE;
typedef short VARIANT_BOOL;
typedef VARIANT_BOOL _VARIANT_BOOL;
typedef double DATE;

#if 0
typedef unsigned long long ULONGLONG;
#endif

typedef struct tagCY {
    unsigned long Lo;
    long      Hi;
} CY;

typedef struct IDispatch IDispatch;

typedef struct tagDEC {
    USHORT wReserved;
    BYTE  scale;
    BYTE  sign;
    ULONG Hi32;
    unsigned long long Lo64;
} DECIMAL;

typedef struct tagSAFEARRAY SAFEARRAY;

typedef struct tagVARIANT VARIANT;

struct tagVARIANT 
{
  union {
   struct __tagVARIANT {
    VARTYPE vt;
    WORD wReserved1;
    WORD wReserved2;
    WORD wReserved3;
    union {
      LONG           lVal;
      BYTE           bVal;
      SHORT          iVal;
      FLOAT          fltVal;
      double         dblVal;
      VARIANT_BOOL   boolVal;
      _VARIANT_BOOL  bool;
      SCODE          scode;
      CY             cyVal;
      DATE           date;
      BSTR           bstrVal;
      IUnknown*      punkVal;
      IDispatch*     pdispVal;
      SAFEARRAY*     parray;
      BYTE*          pbVal;
      SHORT*         piVal;
      LONG*          plVal;
      FLOAT*         pfltVal;
      double*        pdblVal;
      VARIANT_BOOL*  pboolVal;
      _VARIANT_BOOL* pbool;
      SCODE*      pscode;
      CY*         pcyVal;
      DATE*       pdate;
      BSTR*       pbstrVal;
      IUnknown**  ppunkVal;
      IDispatch** ppdispVal;
      SAFEARRAY** pparray;
      VARIANT*    pvarVal;
      PVOID       byref;
      CHAR        cVal;
      USHORT      uiVal;
      ULONG       ulVal;
      INT         intVal;
      UINT        uintVal;
      DECIMAL*    pdecVal;
      CHAR*       pcVal;
      USHORT*     puiVal;
      ULONG*      pulVal;
      INT*        pintVal;
      UINT*       puintVal;
      } n3;
   } n2;
   DECIMAL decVal;
  } n1;
};

typedef VARIANT VARIANTARG;


typedef struct tagDISPPARAMS 
{
  VARIANTARG*  rgvarg;
  DISPID*      rgdispidNamedArgs;
  unsigned int cArgs;
  unsigned int cNamedArgs;
} DISPPARAMS;

typedef struct IDispatchVtbl
    {
        HRESULT ( STDCALL  *QueryInterface )
	   		( IDispatch  * This
			, REFIID riid
			, void  **ppvObject
			);
        
        ULONG ( STDCALL  *AddRef )( IDispatch  * This );
        
        ULONG ( STDCALL  *Release )( IDispatch  * This );
        
        HRESULT ( STDCALL  *GetTypeInfoCount )
			( IDispatch  * This
			, UINT  *pctinfo
			);
        
        HRESULT ( STDCALL *GetTypeInfo )
	                ( IDispatch  *This
			, UINT iTInfo
			, LCID lcid
			,ITypeInfo  **ppTInfo
			);
        
        HRESULT ( STDCALL  *GetIDsOfNames )
	                ( IDispatch  *This
			, REFIID riid
			, LPOLESTR  *rgszNames
			, UINT cNames
			, LCID lcid
			, DISPID  *rgDispId
			);
        
        HRESULT ( STDCALL  *Invoke )
	                ( IDispatch  *This
			, DISPID dispIdMember
			, REFIID riid
			, LCID lcid
			, WORD wFlags
			, DISPPARAMS  *pDispParams
			, VARIANT  *pVarResult
			, EXCEPINFO  *pExcepInfo
			,UINT  *puArgErr
			);

    } IDispatchVtbl;

struct IDispatch { struct IDispatchVtbl *lpVtbl; };
#endif


#define IDispatch_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)

#define IDispatch_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)

#if __W32API_MAJOR_VERSION == 1
extern void STDCALL VariantInit(VARIANTARG* pvarg);
extern HRESULT STDCALL VariantClear(VARIANTARG* pvarg);
extern HRESULT STDCALL VariantCopy(VARIANTARG* pvArgDest, VARIANTARG* pvArgSrc);
extern HRESULT STDCALL VariantChangeType( VARIANTARG* pvargDest
				, VARIANTARG* pvarSrc
				, unsigned short wFlags
				, VARTYPE vt
				);
#endif

extern int dispErrorUNKNOWNNAME(HRESULT hr);
extern int dispErrorEXCEPTION(HRESULT hr);
extern HRESULT readVarVariant( VARIANT* v, VARIANT* x );
extern int readVariantTag( VARIANT* v );
extern void writeVarVariant( VARIANT* x, VARIANT* v);
extern HRESULT readVarCurrency( VARIANT* v, int* hi, int* lo );
extern void writeVarCurrency( int hi, unsigned int lo, VARIANT* v );
extern HRESULT readVarWord64( VARIANT* v, unsigned int* hi, unsigned int* lo );
extern void writeVarWord64( unsigned int hi, unsigned int lo, VARIANT* v );
extern HRESULT readVarNull( VARIANT* v );
extern void writeVarNull( VARIANT* v );
extern void writeVarEmpty( VARIANT* v );
extern void writeVarOptional( VARIANT* v );
extern void writeVarSAFEARRAY ( VARIANT* v, SAFEARRAY* sa, VARTYPE vt);
extern HRESULT readVarSAFEARRAY  ( VARIANT* v, SAFEARRAY** sa, VARTYPE vt);

#define READWRITEPROTO(htype,ctype) \
extern void writeVar##htype( ctype x, VARIANT* v); \
extern HRESULT readVar##htype( VARIANT* v, ctype* p)

#define READWRITETEMPPROTO(htype,ctype) \
extern void writeVar##htype( ctype x, VARIANT* v); \
extern HRESULT readVar##htype( VARIANT* v, ctype* p, VARIANT** w)

READWRITEPROTO(Short,int);
READWRITEPROTO(Int,int);
READWRITEPROTO(Word,unsigned int);
READWRITEPROTO(Float,float);
READWRITEPROTO(Double, double);
READWRITEPROTO(Date, double);
READWRITETEMPPROTO(String, BSTR);
READWRITETEMPPROTO(Dispatch, IDispatch*);
READWRITEPROTO(Bool, BOOL);
READWRITETEMPPROTO(Unknown, IUnknown*);
READWRITEPROTO(Byte, unsigned char);
READWRITEPROTO(Error, int);

extern void freeVariants( int count, VARIANT* p );
extern HRESULT 
dispatchGetMemberID( IDispatch* obj, BSTR name, LCID lcid, DISPID* dispid );
extern char* getExcepInfoMessage( EXCEPINFO* info );
extern void freeExcepInfo( EXCEPINFO* info );
extern HRESULT 
dispatchInvoke( IDispatch* obj, DISPID dispid, LCID lcid,
                BOOL isfunction, unsigned flags,
                int cargs, int cargsout,
                VARIANT* args, VARIANT* argsout,
                EXCEPINFO** info );

extern
HRESULT
STDCALL
SafeArrayDestroy
 	( /*[in]*/SAFEARRAY* psa 
	);

extern HRESULT primCopyVARIANT  ( VARIANT* p1, VARIANT* p2 );
extern HRESULT primVARIANTClear ( VARIANT* p1 );

extern HRESULT primClockToDate (/*[in]*/int ct, /*[out]*/double* pT);

#endif /* __AUTOPRIM_H__ */

