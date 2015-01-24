
#include "autoPrim.h"
#include "SafeArrayPrim.h"

/* Finaliser for SAFEARRAYs */
void
primSafeArrayDestroy(void* p)
{
  SafeArrayDestroy((SAFEARRAY*)p);
}
