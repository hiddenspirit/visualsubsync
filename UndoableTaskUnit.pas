unit UndoableTaskUnit;

interface

type
  TUndoableTask = class
  public
    procedure DoTask; virtual; abstract;
    function GetName : WideString; virtual; abstract;
    procedure UndoTask; virtual; abstract;
  end;

implementation

end.
 