unit ImageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  TFilenameGroup = class
  public
    Count: integer;
    Name: string;
    Filenames: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TCount = class
  public
    Count: integer;
    constructor Create;
  end;

  TIndex = class
  public
    Index: integer;
    constructor Create(index_: integer);
  end;

  TImageList = class
  public
    FilenameGroups: TFPObjectHashTable;
    constructor Create;
    destructor Destroy; override;
    function Load(Folder: string): boolean;
    procedure UpdateWallpaper;
  private
    RootFolder: string;
    SelectedGroups: TFPObjectList;
    MinCount: integer;
    procedure MinFinder(Item: TObject; const {%H-}Key: string; var Continue: Boolean);
  end;

implementation

uses LazFileUtils, StrUtils, Windows, Dialogs;

constructor TFilenameGroup.Create;
begin
  Count := 0;
  Name := '';
  Filenames := TStringList.Create;
end;

constructor TCount.Create;
begin
  Count := 0;
end;

constructor TIndex.Create(index_: integer);
begin
  Index := index_;
end;

destructor TFilenameGroup.Destroy;
begin
  FreeAndNil(Filenames);
end;

constructor TImageList.Create;
begin
  FilenameGroups := TFPObjectHashTable.Create;
  RootFolder := '';
  Randomize;
end;

destructor TImageList.Destroy;
begin
  FreeAndNil(FilenameGroups);
end;

procedure AddImage(filename: string; FilenameGroups: TFPObjectHashTable);
var
  c: char;
  prefix: string;
  group: TFilenameGroup;
begin
  prefix := '';
  for c in filename do begin
    if (c <= ' ') or (c = '.') then break;
    prefix := prefix + c;
  end;
  if prefix = '' then raise Exception.Create('Cannot extract prefix for filename: ' + filename);
  group := FilenameGroups[prefix] as TFilenameGroup;
  if group = nil then begin
    group := TFilenameGroup.Create;
    group.name := prefix;
    FilenameGroups[prefix] := group;
  end;
  group.Filenames.AddObject(filename, TCount.Create);
end;

function TImageList.Load(Folder: string): boolean;
var
   sr: TSearchRec;
begin
  Result := false;
  RootFolder := Folder;
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

procedure TImageList.MinFinder(Item: TObject; const {%H-}Key: string; var Continue: Boolean);
var
  Count: integer;
begin
  Count := (Item as TFilenameGroup).Count;
  if Count < MinCount then begin
    MinCount := Count;
    SelectedGroups.Clear;
  end;
  if Count = MinCount then
    SelectedGroups.Add(Item);
  Continue := true;
end;

procedure TImageList.UpdateWallpaper;
var
  index: integer;
  group: TFilenameGroup;
  selectedFilenames: TFPObjectList;
  Count: integer;
  ImagePath: UnicodeString;
begin
  SelectedGroups := TFPObjectList.Create(false);
  MinCount := MaxInt;
  try
    FilenameGroups.Iterate(@MinFinder);
    index := random(SelectedGroups.Count);
    group := SelectedGroups[index] as TFilenameGroup;
    Inc(group.Count);

    selectedFilenames := TFPObjectList.Create;
    MinCount := MaxInt;
    try
      for index := 0 to group.Filenames.Count - 1 do begin
        Count := (group.Filenames.Objects[index] as TCount).Count;
        if Count < MinCount then begin
          MinCount := Count;
          selectedFilenames.Clear;
        end;
        if Count = MinCount then
          selectedFilenames.Add(TIndex.Create(index));
      end;
      index := random(selectedFilenames.Count);
      index := (selectedFilenames.Items[index] as TIndex).Index;
      Inc((group.Filenames.Objects[index] as TCount).Count);
      ImagePath := UnicodeString(IncludeTrailingPathDelimiter(RootFolder) + group.Filenames.Strings[index]);
//      ShowMessage(ImagePath);
      if not SystemParametersInfoW(SPI_SETDESKWALLPAPER, 0, pChar(ImagePath), SPIF_SENDCHANGE) then begin
        raise Exception.Create(IntToStr(GetLastError()));
      end
    finally
      selectedFilenames.Free;
    end;
  finally
    SelectedGroups.Free;
  end;
end;

end.

