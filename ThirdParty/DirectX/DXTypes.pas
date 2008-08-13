{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  File:       extracted from various DirectX SDK include files              *}
{*                                                                            *}
{*  Content:    DirectX 9.0 headers common types                              *}
{*                                                                            *}
{*  Direct3DX 9.0 Delphi adaptation by Alexey Barkovoy                        *}
{*  E-Mail: clootie@ixbt.com                                                  *}
{*                                                                            *}
{*  Modified: 28-Jul-2004                                                     *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*     http://clootie.narod.ru/delphi                                         *}
{*                                                                            *}
{******************************************************************************}
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is DXTypes.pas.                                            }
{                                                                              }
{******************************************************************************}
unit DXTypes;

interface

uses Windows;

type
  // TD3DValue is the fundamental Direct3D fractional data type
  D3DVALUE = Single;
  {$EXTERNALSYM D3DVALUE}
  TD3DValue = D3DVALUE;
  PD3DValue = ^TD3DValue;

  D3DCOLOR = {$IFDEF TYPE_IDENTITY}type {$ENDIF}DWord;
  {$EXTERNALSYM D3DCOLOR}
  TD3DColor = D3DCOLOR;
  PD3DColor = ^TD3DColor;

  _D3DVECTOR = packed record
    x: Single;
    y: Single;
    z: Single;
  end {_D3DVECTOR};
  {$EXTERNALSYM _D3DVECTOR}
  D3DVECTOR = _D3DVECTOR;
  {$EXTERNALSYM D3DVECTOR}
  TD3DVector = _D3DVECTOR;
  PD3DVector = ^TD3DVector;


// ==================================================================
// Here comes generic Windows types for Win32 / Win64 compatibility
//

  //
  // The INT_PTR is guaranteed to be the same size as a pointer.  Its
  // size with change with pointer size (32/64).  It should be used
  // anywhere that a pointer is cast to an integer type. UINT_PTR is
  // the unsigned variation.
  //
  {$EXTERNALSYM INT_PTR}
  {$EXTERNALSYM UINT_PTR}
  {$EXTERNALSYM LONG_PTR}
  {$EXTERNALSYM ULONG_PTR}
  {$IFDEF WIN64}
  INT_PTR = Int64;
  UINT_PTR = Int64; // Any new type will appear here ???
  LONG_PTR = Int64;
  ULONG_PTR = Int64;
  {$ELSE}
  INT_PTR = Longint;
  UINT_PTR = Longword;
  LONG_PTR = Longint;
  ULONG_PTR = Longword;
  {$ENDIF}
  PINT_PTR = ^INT_PTR;
  PUINT_PTR = ^UINT_PTR;
  PLONG_PTR = ^LONG_PTR;
  PULONG_PTR = ^ULONG_PTR;

  //
  // SIZE_T used for counts or ranges which need to span the range of
  // of a pointer.  SSIZE_T is the signed variation.
  //
  SIZE_T = ULONG_PTR;
  {$EXTERNALSYM SIZE_T}
  PSIZE_T = ^SIZE_T;

implementation

end.

