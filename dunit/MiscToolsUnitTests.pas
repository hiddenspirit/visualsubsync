unit MiscToolsUnitTests;

interface

uses
  MiscToolsUnit,
  TestFrameWork;

type
  TMiscToolsTests = class(TTestCase)
  private

  protected

  published

    // Test methods
    procedure TestWideStringFind;

  end;

implementation

procedure TMiscToolsTests.TestWideStringFind;
begin
  CheckEquals(0, WideStringFind(1, '',''), 'Empty pattern in empty string');
  CheckEquals(0, WideStringFind(1, '','a'), 'Not empty pattern in empty string');
  CheckEquals(1, WideStringFind(1, 'a',''), 'Empty pattern in not empty string');
  CheckEquals(1, WideStringFind(1, 'aBcD eFgh iJk','aBc'), 'Start of string');
  CheckEquals(12, WideStringFind(1, 'aBcD eFgh iJk','Jk'), 'End of string');
  CheckEquals(0, WideStringFind(1, 'aBcD eFgh iJk','Jkl'), 'End of string2');
  CheckEquals(7, WideStringFind(1, 'aBcD eFgh iJk','Fg'), 'Middle of string');
  CheckEquals(0, WideStringFind(1, 'aBcD eFgh iJk','FG', False), 'Case insensitive');
  CheckEquals(0, WideStringFind(1, 'aBcD eFgh iJk','aBc', False, True), 'Whole word start of string');
  CheckEquals(1, WideStringFind(1, 'aBcD eFgh iJk','aBcD', False, True), 'Whole word start of string2');
  CheckEquals(11, WideStringFind(1, 'aBcD eFgh iJk','iJk', False, True), 'Whole word end of string');
  CheckEquals(6, WideStringFind(1, 'aBcD eFgh iJk','eFgh', False, True), 'Whole word middle of string');
  CheckEquals(11, WideStringFind(2, 'aBcD eFgh aBcD','aBcD', False, True), 'Offset > 1');
end;

initialization
  TestFramework.RegisterTest('MiscToolsUnitTests Suite',
    TMiscToolsTests.Suite);

end.
 