{ Licence: LGPL with linking expcetion similar to FreePascal's RTL }

unit ConfigFile;

{$mode objfpc}{$H+}

interface

{
 For Windows use roaming appdata directory. It will be something like:
 c:\users\username\appdata\roaming\appname\appname.cfg
 For UNIX style OSes. First tries to do:
 /home/username/.appname
 If it cannot find home directory than use FreePascal's
 GetAppConfigDir + /.appname
}
function GetConfigFile(const AppName: string): string;
procedure CreateConfigDirs(const AppName: string);

implementation

{$IFDEF MSWindows}

{ The code is just rip-off of the windows sysutils }

uses windows, sysutils {Exception};

const
 CSIDL_FLAG_CREATE             = $8000; { (force creation of requested folder if it doesn't exist yet)     }
 CSIDL_APPDATA                 = $001A; { %USERPROFILE%\Application Data (roaming)                         }

Type
  PFNSHGetFolderPath = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;

var
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;

Procedure InitDLL;
Var
  P : Pointer;
begin
  P := nil;
  CFGDLLHandle:=LoadLibrary('shell32.dll');
  if (CFGDLLHandle<>0) then
    begin
    P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
    If (P=Nil) then
      begin
      FreeLibrary(CFGDLLHandle);
      CFGDllHandle:=0;
      end
    else
      SHGetFolderPath:=PFNSHGetFolderPath(P);
    end;
  If (P=Nil) then
    begin
    CFGDLLHandle:=LoadLibrary('shfolder.dll');
    if (CFGDLLHandle<>0) then
      begin
      P:=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
      If (P=Nil) then
        begin
        FreeLibrary(CFGDLLHandle);
        CFGDllHandle:=0;
        end
      else
        ShGetFolderPath:=PFNSHGetFolderPath(P);
      end;
    end;
  If (@ShGetFolderPath=Nil) then
    Raise Exception.Create('Could not determine SHGetFolderPath Function');
end;

Function GetSpecialDir(ID :  Integer) : String;
Var
  APath : Array[0..MAX_PATH] of char;
begin
  Result := '';
  if CFGDLLHandle = 0 then
    InitDLL;
  If SHGetFolderPath <> Nil then
    begin
    if SHGetFolderPath(0,ID or CSIDL_FLAG_CREATE,0,0,@APATH[0])=S_OK then
      Result:=IncludeTrailingPathDelimiter(StrPas(@APath[0]));
    end;
end;

function GetConfigFile(const AppName: string): string;
begin
  Result := GetSpecialDir(CSIDL_APPDATA);
  if Result = '' then begin
    Result := GetAppConfigDir(false);
  end;
  Result := IncludeTrailingPathDelimiter(Result) + AppName + '\' + AppName + '.cfg';
end;

procedure CreateConfigDirs(const AppName: string);
var
  Result: string;
begin
  Result := GetSpecialDir(CSIDL_APPDATA);
  if Result = '' then begin
    Result := GetAppConfigDir(false);
  end;
  Result := IncludeTrailingPathDelimiter(Result) + AppName;
  if not DirectoryExists(Result) then begin
    CreateDir(Result);
  end;
end;

Finalization
 if CFGDLLHandle<>0 then
   FreeLibrary(CFGDllHandle);

{$ELSE}

uses SysUtils;

function GetConfigFile(const AppName: string): string;
begin
  Result := GetEnvironmentVariable('HOME');
  if Result = '' then begin
     Result := GetAppConfigDir(false);
  end;
  Result := IncludeTrailingPathDelimiter(Result) + '.' + AppName;
end;

procedure CreateConfigDirs(const AppName: string);
begin
// do nothing
end;

{$ENDIF}

end.

