unit SSAParser;

interface

uses Classes;

type
  SSAParser = class
  end;

  FormatedParser = class
  private
    keywordLst : TStringList;
  public
    function ParseFormatLine(line: WideString) : Integer;
    function ParseFormatedLine(line : WideString) : Integer;
  end;
  
implementation

uses SysUtils;

function FormatedParser.ParseFormatLine(line: WideString) : Integer;
var p : integer;
begin
  Delete(line,1, Length('Format:'));
  while True do
  begin
    p := Pos(',', line);
    if p = 0 then
    begin
      keywordLst.Add(Trim(line));
      Break;
    end;
    keywordLst.Add(Trim(Copy(line, 1, p - 1)));
    Delete(line,1,p);
  end;  
end;

function FormatedParser.ParseFormatedLine(line: WideString) : Integer;
begin

end;


end.
