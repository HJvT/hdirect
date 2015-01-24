#ifndef __POINTERSRC_H__
#define __POINTERSRC_H__

extern void   primPointerCheck();
extern void   primNoFree(void* p);
extern void   primFinalise(void* f, void* a);
extern void*  primAllocMemory(int size);
extern void   primFreeMemory(void* p);
extern void   primFreeBSTR(void* p);
extern void*  finalFreeMemory();
extern void*  finalFreeBSTR();
extern void*  finalFreeObject();
extern void*  finalNoFree();

#endif /* __POINTERSRC_H__ */
