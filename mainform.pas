unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  ActnList, StdCtrls, Settings, ImageList;

type

  { TMainForm }

  TMainForm = class(TForm)
    Filenames: TListBox;
    ReloadImagesButton: TButton;
    SelectFolderButton: TButton;
    ExitAction: TAction;
    ActionList: TActionList;
    ImagesFolder: TLabeledEdit;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    Timer: TTimer;
    TrayIconPopupMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure ReloadImagesButtonClick(Sender: TObject);
    procedure SelectFolderButtonClick(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
  private
    CanClose: boolean;
    Settings: TSettings;
    ImageList: TImageList;
    procedure MinimizeToTray;
    procedure ShowSettings;
    procedure LoadImageList;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  MinimizeToTray;
  CanClose := self.CanClose;
end;

procedure TMainForm.ExitActionExecute(Sender: TObject);
begin
  CanClose := true;
  Close;
end;

procedure TMainForm.SelectFolderButtonClick(Sender: TObject);
begin
  if Settings.ImagesFolder <> '' then SelectDirectoryDialog.InitialDir := Settings.ImagesFolder;
  if SelectDirectoryDialog.Execute then begin
    Settings.ImagesFolder := SelectDirectoryDialog.FileName;
    ShowSettings;
    LoadImageList;
  end;
end;

procedure TMainForm.ReloadImagesButtonClick(Sender: TObject);
begin
  LoadImageList;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CanClose := false;
  ShowInTaskBar := stNever;
  Settings := TSettings.Create;
  Settings.Load;
  ShowSettings;
  LoadImageList;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Settings);
  FreeAndNil(ImageList);
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then begin
    MinimizeToTray;
  end;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if ImageList <> nil then ImageList.UpdateWallpaper
  else Timer.Enabled := False;
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  if ShowInTaskBar = stNever then begin
    Show;
    ShowInTaskBar := stDefault;
  end else begin
    MinimizeToTray;
  end;
end;

procedure TMainForm.MinimizeToTray;
begin
  Hide;
  WindowState := wsNormal;
  ShowInTaskBar := stNever;
end;

procedure TMainForm.ShowSettings;
begin
  ImagesFolder.Text := Settings.ImagesFolder;
end;

procedure TMainForm.LoadImageList;
begin
  Timer.Enabled := False;
  FreeAndNil(ImageList);
  if Settings.ImagesFolder = '' then Exit;
  ImageList := TImageList.Create;
  if ImageList.Load(Settings.ImagesFolder) then begin
    ImageList.UpdateWallpaper;
    Filenames.Items := ImageList.Filenames;
    Timer.Enabled := True;
  end else begin
    FreeAndNil(ImageList);
  end;
end;

end.

