unit Settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSettings = class
  private
    ConfigFilePath: string;
    FImagesFolder: string;
    procedure SetImagesFolder(folder: string);
  public
    constructor Create;
    procedure Load;
    procedure Save;
    property ImagesFolder: string read FImagesFolder write SetImagesFolder;
  end;

implementation

uses ConfigFile, IniFiles;

const
  ConfigDirName = 'WeightedWallpaperChanger';
  SectionName = 'Global Settings';

constructor TSettings.Create;
begin
  ConfigFilePath := GetConfigFile(ConfigDirName);
end;

procedure TSettings.Load;
var
  inifile: TIniFile;
begin
  if not FileExists(ConfigFilePath) then Exit;
  inifile := TIniFile.Create(ConfigFilePath);
  try
    ImagesFolder := inifile.ReadString(SectionName, 'ImagesDirectory', '');
  finally
    inifile.Free;
  end
end;

procedure TSettings.Save;
var
  inifile: TIniFile;
begin
  if ImagesFolder = '' then Exit;
  if not FileExists(ConfigFilePath) then CreateConfigDirs(ConfigDirName);
  inifile := TIniFile.Create(ConfigFilePath);
  try
    inifile.WriteString(SectionName, 'ImagesDirectory', FImagesFolder);
  finally
    inifile.Free;
  end
end;

procedure TSettings.SetImagesFolder(folder: string);
begin
  FImagesFolder := folder;
  Save;
end;

end.

