unit ImageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type
  TImageList = class
  public
    FilenameGroups: TFPObjectHashTable;
    constructor Create;
    destructor Destroy; override;
    function Load(Folder: string): boolean;
    procedure UpdateWallpaper;
    procedure ClearFilenameGroups;
    procedure FreeFilenameGroupsIterator(Item: TObject; const Key: string; var Continue: Boolean);
  end;

implementation

uses LazFileUtils, StrUtils;

constructor TImageList.Create;
begin
  self.FilenameGroups := TFPObjectHashTable.Create;
end;

procedure TImageList.FreeFilenameGroupsIterator(Item: TObject; const Key: string; var Continue: Boolean);
begin
  FreeAndNil(Item);
  Continue := true;
end;

procedure TImageList.ClearFilenameGroups;
begin
  FilenameGroups.Iterate(@FreeFilenameGroupsIterator);
  FilenameGroups.Clear;
end;

destructor TImageList.Destroy;
begin
  FreeAndNil(FilenameGroups);
end;

procedure AddImage(filename: string; FilenameGroups: TFPObjectHashTable);
var
   c: char;
   prefix: string;
   filenames: TStringList;
begin
  prefix := '';
  for c in filename do begin
    if (c <= ' ') or (c = '.') then break;
    prefix := prefix + c;
  end;
  if prefix = '' then raise Exception.Create('Cannot extract prefix for filename: ' + filename);
  filenames := FilenameGroups[prefix] as TStringList;
  if filenames = nil then begin
    filenames := TStringList.Create;
    FilenameGroups[prefix] := filenames;
  end;
  filenames.Add(filename);
end;

function TImageList.Load(Folder: string): boolean;
var
   sr: TSearchRec;
begin
  Result := false;
  ClearFilenameGroups;
  FilenameGroups.Clear;
  if FindFirstUTF8(IncludeTrailingPathDelimiter(Folder) + '*', faReadOnly + faArchive, sr) <> 0 then Exit;
  try
    repeat
      if AnsiEndsText('.jpg', sr.Name) or AnsiEndsText('.jpeg', sr.Name) then AddImage(sr.Name, FilenameGroups);
    until FindNextUTF8(sr) <> 0;
  finally
    FindCloseUTF8(sr);
  end;
  Result := FilenameGroups.Count > 0;
end;

procedure TImageList.UpdateWallpaper;
begin
end;

end.

