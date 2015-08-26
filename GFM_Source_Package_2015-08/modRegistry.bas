Attribute VB_Name = "modRegistry"
Public Const READ_CONTROL = &H20000

Public Const STANDARD_RIGHTS_ALL = &H1F0000
Public Const STANDARD_RIGHTS_EXECUTE = (READ_CONTROL)
Public Const STANDARD_RIGHTS_READ = (READ_CONTROL)
Public Const STANDARD_RIGHTS_REQUIRED = &HF0000
Public Const STANDARD_RIGHTS_WRITE = (READ_CONTROL)
Public Const SYNCHRONIZE = &H100000

'Registry Access through the Win32 API

Public Const HKEY_CLASSES_ROOT = &H80000000
Public Const HKEY_CURRENT_CONFIG = &H80000005
Public Const HKEY_CURRENT_USER = &H80000001
Public Const HKEY_LOCAL_MACHINE = &H80000002
Public Const HKEY_USERS = &H80000003

'Return values for registry access functions

Public Const ERROR_SUCCESS = 0&                ' Function call successful
Public Const ERROR_FILE_NOT_FOUND = 2&         ' Registry path does not exist
Public Const ERROR_ACCESS_DENIED = 5&          ' Requested permissions not available
Public Const ERROR_INVALID_HANDLE = 6&         ' Invalid handle or top-level key
Public Const ERROR_BAD_NETPATH = 53            ' Network path not found
Public Const ERROR_INVALID_PARAMETER = 87      ' Bad parameter to a Win32 API function
Public Const ERROR_CALL_NOT_IMPLEMENTED = 120& ' Function valid only in WinNT/2000?XP
Public Const ERROR_INSUFFICIENT_BUFFER = 122   ' Buffer too small to hold data
Public Const ERROR_BAD_PATHNAME = 161          ' Registry path does not exist
Public Const ERROR_MORE_DATA = 234&            ' More data available
Public Const ERROR_NO_MORE_ITEMS = 259&        ' Invalid enumerated value
Public Const ERROR_BADDB = 1009                ' Corrupted registry
Public Const ERROR_BADKEY = 1010               ' Invalid registry key
Public Const ERROR_CANTOPEN = 1011&            ' Cannot open registry key
Public Const ERROR_CANTREAD = 1012&            ' Cannot read from registry key
Public Const ERROR_CANTWRITE = 1013&           ' Cannot write to registry key
Public Const ERROR_REGISTRY_RECOVERED = 1014&  ' Recovery of part of registry successful
Public Const ERROR_REGISTRY_CORRUPT = 1015&    ' Corrupted registry
Public Const ERROR_REGISTRY_IO_FAILED = 1016&  ' Input/output operation failed
Public Const ERROR_NOT_REGISTRY_FILE = 1017&   ' Input file not in registry file format
Public Const ERROR_KEY_DELETED = 1018&         ' Key already deleted
Public Const ERROR_KEY_HAS_CHILDREN = 1020&    ' Key has subkeys & cannot be deleted

Public Const KEY_CREATE_LINK = &H20
Public Const KEY_CREATE_SUB_KEY = &H4
Public Const KEY_ENUMERATE_SUB_KEYS = &H8
Public Const KEY_NOTIFY = &H10
Public Const KEY_QUERY_VALUE = &H1
Public Const KEY_READ = ((STANDARD_RIGHTS_READ Or KEY_QUERY_VALUE _
                        Or KEY_ENUMERATE_SUB_KEYS Or KEY_NOTIFY) _
                        And (Not SYNCHRONIZE))
Public Const KEY_SET_VALUE = &H2
Public Const KEY_WRITE = ((STANDARD_RIGHTS_WRITE Or KEY_SET_VALUE _
                         Or KEY_CREATE_SUB_KEY) And (Not SYNCHRONIZE))
Public Const KEY_EXECUTE = ((KEY_READ) And (Not SYNCHRONIZE))
Public Const KEY_ALL_ACCESS = ((STANDARD_RIGHTS_ALL Or KEY_QUERY_VALUE _
                              Or KEY_SET_VALUE Or KEY_CREATE_SUB_KEY _
                              Or KEY_ENUMERATE_SUB_KEYS Or KEY_NOTIFY _
                              Or KEY_CREATE_LINK) And (Not SYNCHRONIZE))

Public Declare Function RegOpenKeyEx Lib "advapi32.dll" _
  Alias "RegOpenKeyExA" (ByVal hKey As Long, ByVal lpSubKey As String, _
  ByVal ulOptions As Long, ByVal samDesired As Long, phkResult As Long) As Long

Public Declare Function RegQueryValueEx Lib "advapi32.dll" _
  Alias "RegQueryValueExA" (ByVal hKey As Long, ByVal lpValueName As String, _
  ByVal lpReserved As Long, lpType As Long, lpData As Any, lpcbData As Long) As Long
  ' Note that if you declare the lpData parameter as String, you must pass it By Value.

Public Declare Function RegCloseKey Lib "advapi32.dll" (ByVal hKey As Long) As Long

Public Function GetRegStringVal(hRootKey As Long, sSubKeyPath As String, _
                                sValueName As String, bSuccess As Boolean) As String
  Dim hSubKey As Long
  Dim lSize As Long
  Dim sValue As String
  bSuccess = False
  If RegOpenKeyEx(hRootKey, sSubKeyPath, 0, KEY_READ, hSubKey) _
     <> ERROR_SUCCESS Then Exit Function
  If RegQueryValueEx(hSubKey, sValueName, 0, REG_SZ, ByVal vbNull, lSize) _
     <> ERROR_MORE_DATA Then
    RegCloseKey hSubKey
    Exit Function
  End If
  sValue = Space(lSize + 1)
  If RegQueryValueEx(hSubKey, sValueName, 0, REG_SZ, ByVal sValue, lSize) _
     <> ERROR_SUCCESS Then
    RegCloseKey hSubKey
    Exit Function
  End If
  RegCloseKey hSubKey
  sValue = Left(sValue, lSize - 1)
  GetRegStringVal = sValue
  bSuccess = True
End Function
