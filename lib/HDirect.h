/* --------------------------------------------------------------------------
 * GreenCard / HaskellDirect include file.
 *
 * Hugs 98 is Copyright (c) Mark P Jones, Alastair Reid and the Yale
 * Haskell Group 1994-99, and is distributed as Open Source software
 * under the Artistic License; see the file "Artistic" that is included
 * in the distribution for details.
 *
 * sof 4/99 - changed to make it useable with HaskellDirect.
 * 
 * $RCSfile: HDirect.h,v $
 * $Revision: 1.4 $
 * $Date: 2003/08/18 16:46:42 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 *
 *                                  WARNING
 *
 * Most of the code in this file must exactly match corresponding definitions
 * in the Hugs source code.
 *
 * We have chosen to copy this code over to avoid the need to #include huge
 * chunks of the Hugs internal definitions (which sometimes conflict with
 * Xlib, Win32 or other libraries which we might also have to #include).
 *
 *
 * sof 4/99 - removed #include of config.h and options.h, since its only use
 *            appeared to be to pick up whether the host C compiler supported
 *            prototypes or not. Most do nowadays.
 * ------------------------------------------------------------------------*/

#ifndef __HDIRECT_H__
#define __HDIRECT_H__

#if 1 /* PROTOTYPES */   /* To enable use of prototypes whenever possible */
#define Args(x) x
#else
#define Args(x) ()
#endif

typedef unsigned char      hugs_uint8_t;
typedef unsigned short     hugs_uint16_t;
typedef unsigned int       hugs_uint32_t;
typedef signed   char      hugs_int8_t;
typedef signed   short     hugs_int16_t;
typedef signed   int       hugs_int32_t;
# ifdef _MSC_VER
typedef unsigned __int64   hugs_uint64_t;
typedef          __int64   hugs_int64_t;
# else
typedef unsigned long long hugs_uint64_t;
typedef signed   long long hugs_int64_t;
# endif

typedef int            HsInt;        
typedef hugs_int8_t    HsInt8;         
typedef hugs_int16_t   HsInt16;        
typedef hugs_int32_t   HsInt32;        
typedef unsigned int   HsWord;       
typedef hugs_uint8_t   HsWord8;        
typedef hugs_uint16_t  HsWord16;       
typedef hugs_uint32_t  HsWord32;       

/* 
 * Here we deviate from the FFI specification:
 * If we make them both float, then there's no way to pass a double
 * to C which means we can't call common C functions like sin.
 */           
typedef float          HsFloat;      
typedef double         HsDouble;     

typedef hugs_int64_t   HsInt64;        
typedef hugs_uint64_t  HsWord64;       
typedef char           HsChar;
typedef int            HsBool;         
typedef void*          HsAddr;       
typedef void*          HsPtr;          
typedef void           (*HsFunPtr)(void);
typedef void*          HsForeignPtr;   
typedef void*          HsStablePtr;  

typedef int   HugsStackPtr;
typedef int   HugsStablePtr;
typedef void* HugsForeign;


#define primFun(name)	 static void name(HugsStackPtr hugs_root)
#define hugs_returnIO(n) hugs->returnIO(hugs_root,n)
#define hugs_returnId(n) hugs->returnId(hugs_root,n)

/* These declarations must exactly match those in storage.h */

typedef void (*Prim) Args((HugsStackPtr)); /* primitive function	   */

extern struct hugs_primitive {		/* table of primitives		   */
    char*  ref;				/* primitive reference string	   */
    int	   arity;			/* primitive function arity	   */
    Prim   imp;				/* primitive implementation	   */
} primitives[];

struct hugs_primInfo {
    void                   (*controlFun) Args((int));
    struct hugs_primitive  *primFuns;
    struct hugs_primInfo   *nextPrimInfo;
};

/* This is an exact copy of the declaration found in storage.h */

typedef struct {

  /* evaluate next argument */
  HsInt          (*getInt)         (void);
  HsWord         (*getWord)        (void);
  HsAddr    	 (*getAddr)        (void);
  HsFloat        (*getFloat)       (void);
  HsDouble       (*getDouble)      (void);
  HsChar         (*getChar)        (void);
  HugsForeign    (*getForeign)     (void);
  HugsStablePtr  (*getStablePtr)   (void); /* deprecated */

  /* push part of result   */
  void           (*putInt)         (HsInt);
  void      	 (*putWord)        (HsWord);
  void      	 (*putAddr)        (HsAddr);
  void           (*putFloat)       (HsFloat);
  void           (*putDouble)      (HsDouble);
  void           (*putChar)        (HsChar);
  void      	 (*putForeign)     (HugsForeign, void (*)(HugsForeign));
  void           (*putStablePtr)   (HugsStablePtr); /* deprecated */

  /* return n values in IO monad or Id monad */
  void      	 (*returnIO)       (HugsStackPtr, int);
  void      	 (*returnId)       (HugsStackPtr, int);
  int      	 (*runIO)          (int);

  /* free a stable pointer */	    			 
  void      	 (*freeStablePtr)  (HugsStablePtr); /* deprecated */

  /* register the prim table */	    			 
  void      	 (*registerPrims)  (struct hugs_primInfo*);
			   
  /* garbage collect */
  void		 (*garbageCollect) (void);

  /* API3 additions follow */
  HugsStablePtr  (*lookupName)     (char*, char*);
  void           (*ap)             (int);
  void           (*getUnit)        (void);
  void*          (*mkThunk)        (HsFunPtr, HugsStablePtr);
  void           (*freeThunk)      (void*);
  HsBool         (*getBool)        (void);
  void           (*putBool)        (HsBool);

  /* API4 additions follow */
  HsInt8         (*getInt8)        (void);
  HsInt16        (*getInt16)       (void);
  HsInt32        (*getInt32)       (void);
  HsInt64        (*getInt64)       (void);
  HsWord8        (*getWord8)       (void);
  HsWord16       (*getWord16)      (void);
  HsWord32       (*getWord32)      (void);
  HsWord64       (*getWord64)      (void);
  HsPtr          (*getPtr)         (void);
  HsFunPtr       (*getFunPtr)      (void);
  HsForeignPtr   (*getForeignPtr)  (void);

  void           (*putInt8)        (HsInt8);
  void           (*putInt16)       (HsInt16);
  void           (*putInt32)       (HsInt32);
  void           (*putInt64)       (HsInt64);
  void           (*putWord8)       (HsWord8);
  void           (*putWord16)      (HsWord16);
  void           (*putWord32)      (HsWord32);
  void           (*putWord64)      (HsWord64);
  void           (*putPtr)         (HsPtr);
  void           (*putFunPtr)      (HsFunPtr);
  void           (*putForeignPtr)  (HsForeignPtr);

  HugsStablePtr  (*makeStablePtr4) (void);
  void           (*derefStablePtr4)(HugsStablePtr);

  void           (*putStablePtr4)  (HsStablePtr);
  HsStablePtr    (*getStablePtr4)  (void);
  void      	 (*freeStablePtr4) (HsStablePtr);

  int      	 (*runId)          (int);
} HugsAPI4;

HugsAPI4 *hugs; /* pointer to virtual function table */

/* 
  copyBytes() is needed when dealing with 
  functions that return structs

  Note: we're (intentionally!) relying on memcpy() to handle
  malloc() failure for us.
*/
#define copyBytes(len,struct_ptr) \
    memcpy((char*)malloc(len*sizeof(char)),(char*)(struct_ptr), len)

/* Copied verbatim from prelude.h */

#ifdef _MSC_VER /* Microsoft Visual C++ */
#define DLLIMPORT(rty) __declspec(dllimport) rty
#define DLLEXPORT(rty) __declspec(dllexport) rty
#elif defined __BORLANDC__ 
#define DLLIMPORT(rty) rty far _import
#define DLLEXPORT(rty) rty far _export
#else 
#define DLLIMPORT(rty) rty
#define DLLEXPORT(rty) rty
#endif /* Don't need to declare DLL exports */

#endif /* __HDIRECT_H__ */
