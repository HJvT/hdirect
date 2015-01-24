/*
 Using the C compiler, figure out properties of
 basic types on a platform, outputting them
 into a .hs file.
 */
#include <stdio.h>

/* Basic COM types. */
typedef char* BSTR; /* Not right, but right enough! */

typedef struct tagSAFEARRAYBOUND {
    unsigned long cElements;
    long  lLbound;
} SAFEARRAYBOUND;

typedef struct tagSAFEARRAY {
    unsigned short cDims;
    unsigned short fFeatures;
    unsigned long  cbElements;
    unsigned long  cLocks;
    void*          pvData;
    SAFEARRAYBOUND rgsabound[1];
} SAFEARRAY;

#define alignment(TYPE) ((long)((char *)&((struct{char c; TYPE d;}*)0)->d - (char *) 0))

#define FLOAT_SIZE  sizeof(float)
#define FLOAT_ALIGN alignment(float)
#define DOUBLE_SIZE sizeof(double)
#define DOUBLE_ALIGN alignment(double)
#define SHORT_SIZE   sizeof(short int)
#define SHORT_ALIGN  alignment(short int)
#define LONG_SIZE   sizeof(long int)
#define LONG_ALIGN  alignment(long int)
#define LONGLONG_SIZE   sizeof(long long int)
#define LONGLONG_ALIGN  alignment(long long int)
#define USHORT_SIZE   sizeof(unsigned short int)
#define USHORT_ALIGN  alignment(unsigned short int)
#define ULONG_SIZE   sizeof(unsigned long int)
#define ULONG_ALIGN  alignment(unsigned long int)
#define ULONGLONG_SIZE   sizeof(unsigned long long int)
#define ULONGLONG_ALIGN  alignment(unsigned long long int)
#define UCHAR_SIZE   sizeof(unsigned char)
#define UCHAR_ALIGN  alignment(unsigned char)
#define SCHAR_SIZE   sizeof(signed char)
#define SCHAR_ALIGN  alignment(signed char)
#define DATA_PTR_SIZE   sizeof(long *)
#define DATA_PTR_ALIGN  alignment(long *)
#define BSTR_SIZE   sizeof(BSTR)
#define BSTR_ALIGN  alignment(BSTR)
#define SAFEARRAY_SIZE   sizeof(SAFEARRAY)
#define SAFEARRAY_ALIGN  alignment(SAFEARRAY)
#define STRUCT_ALIGN  alignment(struct _foo {char x; char y;})

void
dump_def(char* nm, int val1, int val2)
{
  printf("%s_SIZE :: Int\n", nm);
  printf("%s_SIZE = %d\n", nm,val1);
  printf("%s_ALIGN_MODULUS :: Int\n", nm);
  printf("%s_ALIGN_MODULUS = %d\n", nm, val2);
}

int
main()
{
  printf("-- This file was created by mkNativeInfo. Do not edit by hand.\n\n");

  printf("module NativeInfo where\n\n");

  dump_def("fLOAT",  FLOAT_SIZE, FLOAT_ALIGN);
  dump_def("dOUBLE", DOUBLE_SIZE, DOUBLE_ALIGN);

  dump_def("sHORT", SHORT_SIZE, SHORT_ALIGN);

  dump_def("lONG", LONG_SIZE, LONG_ALIGN);

  dump_def("lONGLONG", LONGLONG_SIZE, LONGLONG_ALIGN);

  dump_def("uSHORT", USHORT_SIZE, USHORT_ALIGN);

  dump_def("uLONG", ULONG_SIZE, ULONG_ALIGN);

  dump_def("uLONGLONG", ULONGLONG_SIZE, ULONGLONG_ALIGN);

  dump_def("uCHAR", UCHAR_SIZE, UCHAR_ALIGN);

  dump_def("sCHAR", SCHAR_SIZE, SCHAR_ALIGN);

  dump_def("dATA_PTR", DATA_PTR_SIZE, DATA_PTR_ALIGN);

  dump_def("bSTR", BSTR_SIZE, BSTR_ALIGN);

  dump_def("sAFEARRAY", SAFEARRAY_SIZE, SAFEARRAY_ALIGN);

  printf("sTRUCT_ALIGN_MODULUS ::Int\n");
  printf("sTRUCT_ALIGN_MODULUS = %d\n", STRUCT_ALIGN);

  return(0);
}
