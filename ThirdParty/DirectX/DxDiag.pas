{$I DirectX.inc}

unit DxDiag;

interface

uses
  Windows;

// This identifier is passed to IDxDiagProvider::Initialize in order to ensure that an
// application was built against the correct header files. This number is
// incremented whenever a header (or other) change would require applications
// to be rebuilt. If the version doesn't match, IDxDiagProvider::Initialize will fail.
// (The number itself has no meaning.)
const
	DXDIAG_DX9_SDK_VERSION = 111;


{****************************************************************************
 *
 * DxDiag Errors
 *
 ****************************************************************************}

const
	DXDIAG_E_INSUFFICIENT_BUFFER = $8007007A;  // HRESULT_FROM_WIN32(ERROR_INSUFFICIENT_BUFFER)


{****************************************************************************
 *
 * DxDiag CLSIDs
 *
 ****************************************************************************}

const	
	CLSID_DxDiagProvider: TGUID = '{A65B8071-3BFE-4213-9A5B-491DA4461CA7}';

	
{****************************************************************************
 *
 * DxDiag Interface IIDs
 *
 ****************************************************************************}
const	
	IID_IDxDiagProvider: TGUID = '{9C6B4CB0-23F8-49CC-A3ED-45A55000A6D2}';
	IID_IDxDiagContainer: TGUID = '{7D0F462F-4064-4862-BC7F-933E5058C10F}';
	


{****************************************************************************
 *
 * DxDiag Structures
 *
 ****************************************************************************}

type

  PDXDIAG_INIT_PARAMS = ^TDXDIAG_INIT_PARAMS;
  _DXDIAG_INIT_PARAMS = record
    dwSize : DWORD;                  // Size of this structure.
    dwDxDiagHeaderVersion : DWORD;   // Pass in DXDIAG_DX9_SDK_VERSION.  This verifies 
                                     // the header and dll are correctly matched.
    bAllowWHQLChecks : BOOL;         // If true, allow dxdiag to check if drivers are 
                                     // digital signed as logo'd by WHQL which may 
                                     // connect via internet to update WHQL certificates.
    pReserved : POINTER;             // Reserved. Must be nil.
  end;
  TDXDIAG_INIT_PARAMS = _DXDIAG_INIT_PARAMS;


type
  IDxDiagProvider = interface;
  IDxDiagContainer = interface;


  IDxDiagProvider = interface(IUnknown)
    ['{9C6B4CB0-23F8-49CC-A3ED-45A55000A6D2}']
    (*** IDxDiagProvider methods ***)
    function Initialize(pParams : PDXDIAG_INIT_PARAMS) : HResult; stdcall;
    function GetRootContainer(out ppInstance : IDxDiagContainer) : HResult; stdcall;
  end;


  IDxDiagContainer = interface(IUnknown)
    ['{7D0F462F-4064-4862-BC7F-933E5058C10F}']
    (*** IDxDiagContainer methods ***)
    function GetNumberOfChildContainers(var pdwCount : DWORD) : HResult; stdcall;
    function EnumChildContainerNames(dwIndex : DWORD; pwszContainer : PWideChar; cchContainer : DWORD ) : HResult; stdcall;
    function GetChildContainer(pwszContainer : PWideChar; out ppInstance : IDxDiagContainer) : HResult; stdcall;
    function GetNumberOfProps(var pdwCount : DWORD) : HResult; stdcall;
    function EnumPropNames(dwIndex : DWORD; pwszPropName : PWideChar; cchPropName : DWORD) : HResult; stdcall;
    function GetProp(pwszPropName : PWChar; out pvarProp : OleVariant) : HResult; stdcall;
  end;

implementation

end.