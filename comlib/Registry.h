#ifndef __REGISTRY_H__
#define __REGISTRY_H__

extern
HRESULT
primRegAddEntry ( int hive, const char* path, const char* val);

extern
HRESULT
primRegRemoveEntry ( int hive, const char* path, const char* val, int kind);

#if 0
extern
HRESULT
RegisterServer ( HMODULE  hMod
	       , REFCLSID rclsid
	       , const char* name
	       , const char* verProgID
	       , const char* progID
	       );
extern
HRESULT
UnregisterServer 
               ( HANDLE hMod
	       , REFCLSID rclsid
	       , const char* name
	       , const char* verProgID
	       , const char* progID
	       );

#endif


#endif
