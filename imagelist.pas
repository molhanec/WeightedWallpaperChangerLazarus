unit ImageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TImageList = class
  public
    Filenames: TStringList;
    constructor Create;
    destructor Destroy; override;
    function Load(Folder: string): boolean;
    procedure UpdateWallpaper;
  end;

implementation

uses LazFileUtils, StrUtils;

constructor TImageList.Create;
begin
  self.Filenames := TStringList.Create;
end;

destructor TImageList.Destroy;
begin
  FreeAndNil(Filenames);
end;

function TImageList.Load(Folder: string): boolean;
var
   sr: TSearchRec;
begin
  Result := false;
  Filenames.Clear;
  if FindFirstUTF8(IncludeTrailingPathDelimiter(Folder) + '*', faReadOnly + faArchive, sr) <> 0 then Exit;
  try
    repeat
      if AnsiEndsText('.jpg', sr.Name) or AnsiEndsText('.jpeg', sr.Name) then Filenames.Add(sr.Name);
    until FindNextUTF8(sr) <> 0;
  finally
    FindCloseUTF8(sr);
  end;
  Result := Filenames.Count > 0;
end;

procedure TImageList.UpdateWallpaper;
begin
end;

end.

