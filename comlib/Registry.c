/*
  Misc helper functions for accessing the registry.
*/
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "comPrim.h"

#if __CYGWIN32__
int wcslen(WCHAR* wstr)
{
  return WideCharToMultiByte(CP_ACP, WC_DEFAULTCHAR, wstr, (-1), NULL, 0, NULL, NULL);
}
#endif

/* 
 RegAddEntry() adds a key + string value at

      HKEY_CLASSES_ROOT\path\subKey1\subKey2

*/
HRESULT
RegAddEntry(int hive,
            const char* path,
            const char* subKey1,
            const char* subKey2,
            const char* value)
{
   HKEY hKey;
   char keyBuffer[1024]; /* Sigh! */
   long result;
   HANDLE the_hive;

   switch(hive) {
    case 0: the_hive = HKEY_CLASSES_ROOT; break;
    case 1: the_hive = HKEY_CURRENT_USER; break;
    case 2: the_hive = HKEY_LOCAL_MACHINE; break;
    case 3: the_hive = HKEY_USERS; break;
    case 4: the_hive = HKEY_CURRENT_CONFIG; break;
   default:
     MessageBox (NULL, "RegAddEntry: weird hive", "RegAddEntry", MB_OK | MB_ICONINFORMATION );
     return E_FAIL;
   }

   /* construct complete path in buffer. */
   strcpy(keyBuffer, path) ;
   if (subKey1 != NULL) {
        strcat(keyBuffer, "\\") ;
        strcat(keyBuffer, subKey1 ) ;
   }

   if (subKey2 != NULL) {
        strcat(keyBuffer, "\\") ;
        strcat(keyBuffer, subKey2 ) ;
   }

   /* Create and open key and subkey. */
   result = RegCreateKeyEx(the_hive,
                           keyBuffer, 
                           0, NULL, REG_OPTION_NON_VOLATILE,
                           KEY_ALL_ACCESS, NULL, 
                           &hKey, NULL) ;
   if (result != ERROR_SUCCESS) {
        return S_FALSE;
   }

   /* Set the value */
   if (value != NULL) {
      RegSetValueEx(hKey, NULL, 0, REG_SZ, 
                    (BYTE *)value, 
                    strlen(value)+1) ;
   }

   RegCloseKey(hKey) ;
   return S_OK;
}

/* Called by Haskell code */
HRESULT
primRegAddEntry ( int hive, const char* path, const char* val)
{
 return RegAddEntry( hive, path, NULL, NULL, val);
}

#define REMOVE_KEY    1
#define REMOVE_VALUE  0

/*
 RegRemoveEntry() adds a key + string value at

      HKEY_CLASSES_ROOT\path\subKey1\subKey2
*/
HRESULT
RegRemoveEntry
         ( int hive
         , const char* path
         , const char* subKey1
         , const char* subKey2
         , int deleteKey
         )
{
   HKEY hKey;
   LONG res;
   char keyBuffer[1024]; /* Sigh! */
   HANDLE the_hive;

   switch(hive) {
    case 0: the_hive = HKEY_CLASSES_ROOT; break;
    case 1: the_hive = HKEY_CURRENT_USER; break;
    case 2: the_hive = HKEY_LOCAL_MACHINE; break;
    case 3: the_hive = HKEY_USERS; break;
    case 4: the_hive = HKEY_CURRENT_CONFIG; break;
   default:
     MessageBox (NULL, "RegAddEntry: weird hive", "RegAddEntry", MB_OK | MB_ICONINFORMATION );
     return E_FAIL;
   }

   /* construct complete path in buffer. */
   strcpy(keyBuffer, path);

   if (subKey1 != NULL) {
        strcat(keyBuffer, "\\") ;
        strcat(keyBuffer, subKey1 ) ;
   }

   res = RegOpenKeyEx 
             ( the_hive
             , keyBuffer
             , 0
             , KEY_SET_VALUE
             , &hKey
             );
   if ( res != ERROR_SUCCESS ) {
     MessageBox (NULL, "RegOpenKeyEx() failed", "RegRemoveEntry", MB_OK | MB_ICONINFORMATION );
     return S_FALSE;
   }
   if ( deleteKey ) {
     res = RegDeleteKey (hKey, subKey2 );
   } else {
     res = RegDeleteValue (hKey, subKey2 );
   }
   RegCloseKey(hKey) ;
   return ( res == ERROR_SUCCESS ? S_OK : S_FALSE );
}

/* Called by Haskell code */
HRESULT
primRegRemoveEntry ( int hive, const char* path, const char* val, int kind)
{
 return RegRemoveEntry( hive, path, NULL, val, kind);
}

#if 0
HRESULT
RegisterServer ( HMODULE  hMod
               , REFCLSID rclsid
               , const char* name
               , const char* verProgID
               , const char* progID
               )
{
   char    modBuffer[1024]; /* Sigh! */
   WCHAR   wstr[45];
   char*   clsid_str;
   DWORD   st;
   int     i;
   HRESULT hr;

   st = GetModuleFileName (hMod, modBuffer, 1024);

   if ( st == 0 ) {
     MessageBox (NULL, "GetModuleFileName() failed", "Msg", MB_OK | MB_ICONINFORMATION );
     return S_FALSE;
   }

   i = StringFromGUID2 ( rclsid, wstr, 45);
   if (i == 0) {
     MessageBox (NULL, "StringFromGUID2() failed", "RegisterServer", MB_OK | MB_ICONINFORMATION );
     return S_FALSE;
   }
   clsid_str = (char*)malloc((wcslen(wstr)+1)*sizeof(char));
   UNI2STR(clsid_str,wstr);

   /* 
      HKCR\CLSID\{clsid}\{name}
      HKCR\CLSID\{clsid}\InProcServer32\{dll-name}
      HKCR\CLSID\{clsid}\ProgId\{prog-id}
      HKCR\CLSID\{clsid}\VersionIndependentProgId\{vprog-id}

      HKCR\{prog-id}\CLSID\{clsid}
      HKCR\{vprog-id}\CLSID\{clsid}

   */
   hr = RegAddEntry(0, "CLSID", clsid_str , "", name);
   if (FAILED(hr)) return hr;
   hr = RegAddEntry(0, "CLSID", clsid_str , "InprocServer32", modBuffer);
   if (FAILED(hr)) return hr;
   hr = RegAddEntry(0, "CLSID", clsid_str , "ProgID", progID);
   if (FAILED(hr)) return hr;
   hr = RegAddEntry(0, "CLSID", clsid_str , "VersionIndependentProgID", verProgID);
   if (FAILED(hr)) return hr;
   hr = RegAddEntry(0, progID, "CLSID", NULL, clsid_str);
   if (FAILED(hr)) return hr;
   hr = RegAddEntry(0, verProgID, "CLSID", NULL, clsid_str);
   if (FAILED(hr)) return hr;

   return S_OK;   
}
#endif

#if 0
HRESULT
UnregisterServer 
               ( HMODULE  hMod
               , REFCLSID rclsid
               , const char* name
               , const char* verProgID
               , const char* progID
               )
{ 
   char    modBuffer[1024]; /* Sigh! */
   WCHAR   wstr[45];
   char*   clsid_str;
   DWORD   st;
   int     i;
   HRESULT hr;

   st = GetModuleFileName (hMod, modBuffer, 1024);

   if ( st == 0 ) {
     MessageBox (NULL, "GetModuleFileName() failed", "Msg", MB_OK | MB_ICONINFORMATION );
     return S_FALSE;
   }

   i = StringFromGUID2 ( rclsid, wstr, 45);
   if (i == 0) {
     MessageBox (NULL, "StringFromGUID2() failed", "RegisterServer", MB_OK | MB_ICONINFORMATION );
     return S_FALSE;
   }
   clsid_str = (char*)malloc((wcslen(wstr)+1)*sizeof(char));
   UNI2STR(clsid_str,wstr);

   hr = RegRemoveEntry(0, "CLSID", clsid_str , "InprocServer32", REMOVE_KEY);
   if (FAILED(hr)) return hr;
   hr = RegRemoveEntry(0, "CLSID", clsid_str , "ProgID", REMOVE_KEY);
   if (FAILED(hr)) return hr;
   hr = RegRemoveEntry(0, "CLSID", clsid_str , "VersionIndependentProgID", REMOVE_KEY);
   if (FAILED(hr)) return hr;
   hr = RegRemoveEntry(0, "CLSID", NULL , clsid_str, REMOVE_KEY);
   if (FAILED(hr)) return hr;
   return S_OK;
}
#endif
