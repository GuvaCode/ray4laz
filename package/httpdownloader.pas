unit HttpDownloader;

    {$mode objfpc}{$H+}

interface

uses
  ButtonPanel, Classes, ComCtrls, Controls, Dialogs, ExtCtrls,
  Forms, fphttpclient, opensslsockets, StdCtrls, SysUtils;

procedure ShowHttpDownloader(const ACaption, AURL: String; AStream: TStream);
procedure ShowHttpDownloader(const ACaption, AURL, AFileName: String);

implementation

type

  THttpDownloader = class(TForm)
  private
    FPanel: TPanel;
    FProgressBar: TProgressBar;
    FRaiseError: String;
  public
    constructor Create(AOwner: TComponent); override;
  end;

type

  THttpDownloaderThread = class(TThread)
  private
    FURL: String;
    FContentLength, FContentPosition: Int64;
    FUserStream: TStream;
    FHttpClient: TFPHTTPClient;
    FHttpDownloader: THttpDownloader;
  public
    constructor Create(AHttpForm: THttpDownloader; const AURL: String;
      AUserStream: TStream);
  protected
    procedure Execute; override;
    procedure SynchronizedClose;
    procedure SynchronizedWrite;
  end;

type

  THttpDownloaderStreamProxy = class(TStream)
  strict private
    FThread: THttpDownloaderThread;
    FStream: TStream;
  public
    constructor Create(AThread: THttpDownloaderThread; AStream: TStream);
  public
    function Write(const ABuffer; ACount: Longint): Longint; override;
  end;

constructor THttpDownloaderStreamProxy.Create(AThread: THttpDownloaderThread;
  AStream: TStream);
begin
  inherited Create;
  FThread := AThread;
  FStream := AStream;
end;

function THttpDownloaderStreamProxy.Write(const ABuffer;
  ACount: Longint): Longint;
begin
  Inc(FThread.FContentPosition, ACount);
  if FThread.CheckTerminated then begin
    raise Exception.Create('Download terminated by user');
  end;
  Result := FStream.Write(ABuffer, ACount);
  FThread.Synchronize(@FThread.SynchronizedWrite);
end;

constructor THttpDownloaderThread.Create(AHttpForm: THttpDownloader;
  const AURL: String; AUserStream: TStream);
begin
  FHttpDownloader := AHttpForm;
  FURL := AURL;
  FUserStream := AUserStream;
  inherited Create(False);
end;

procedure THttpDownloaderThread.SynchronizedClose;
begin
  FHttpDownloader.Close;
end;

procedure THttpDownloaderThread.SynchronizedWrite;
begin
  if FHttpDownloader.FProgressBar.Style = pbstMarquee then begin
    FHttpDownloader.FProgressBar.Style := pbstNormal;
    FContentLength :=
      StrToIntDef(FHttpClient.ResponseHeaders.Values['content-length'], 0);
    FHttpDownloader.FProgressBar.Max := FContentLength;
    FHttpDownloader.FProgressBar.Visible := FContentLength > 0;
  end;
  if FHttpDownloader.FProgressBar.Visible then begin
    FHttpDownloader.FPanel.Caption :=
      Format('Download @ %d%% (%d of %d bytes)',
      [FContentPosition * 100 div FContentLength, FContentPosition,
      FContentLength]);
    FHttpDownloader.FProgressBar.Position := FContentPosition;
  end else begin
    FHttpDownloader.FPanel.Caption :=
      Format('Downloaded %d bytes', [FContentPosition]);
  end;
end;

procedure THttpDownloaderThread.Execute;
var
  LStream: TStream;
begin
  try
    try
      LStream := THttpDownloaderStreamProxy.Create(Self, FUserStream);
      try
        FHttpClient := TFPHTTPClient.Create(nil);
        try
          FHttpClient.AllowRedirect := True;
          FHttpClient.Get(FURL, LStream);
        finally
          FreeAndNil(FHttpClient);
        end;
      finally
        FreeAndNil(LStream);
      end;
    except
      on LException: Exception do begin
        FHttpDownloader.FRaiseError := LException.Message;
      end;
    end;
  finally
    Synchronize(@SynchronizedClose);
  end;
end;

constructor THttpDownloader.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Width := 512;
  Height := 128;
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  ChildSizing.TopBottomSpacing := 4;
  ChildSizing.VerticalSpacing := 4;
  ChildSizing.LeftRightSpacing := 4;
  ChildSizing.HorizontalSpacing := 4;
  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Style := pbstMarquee;
  FProgressBar.Smooth := True;
  FProgressBar.Align := alBottom;
  FProgressBar.Parent := Self;
  with TButtonPanel.Create(Self) do begin
    ShowBevel := False;
    ShowButtons := [pbCancel];
    BorderSpacing.Around := 0;
    Align := alBottom;
    Parent := Self;
  end;
  FPanel := TPanel.Create(Self);
  FPanel.Caption := 'Waiting for connection';
  FPanel.BorderStyle := bsSingle;
  FPanel.BevelOuter := bvNone;
  FPanel.Align := alClient;
  FPanel.Parent := Self;
end;

procedure ShowHttpDownloader(const ACaption, AURL: String; AStream: TStream);
var
  LHttpForm: THttpDownloader;
begin
  LHttpForm := THttpDownloader.Create(Application);
  try
    LHttpForm.Caption := ACaption;
    with THttpDownloaderThread.Create(LHttpForm, AURL, AStream) do begin
      try
        LHttpForm.ShowModal;
        Terminate;
        WaitFor;
      finally
        Free;
      end;
    end;
    if LHttpForm.FRaiseError <> EmptyStr then begin
      raise Exception.Create(LHttpForm.FRaiseError);
    end;
  finally
    FreeAndNil(LHttpForm);
  end;
end;

procedure ShowHttpDownloader(const ACaption, AURL, AFileName: String);
var
  LStream: TStream;
begin
  LStream := TFileStream.Create(AFileName, fmCreate);
  try
    ShowHttpDownloader(ACaption, AURL, LStream);
  finally
    FreeAndNil(LStream);
  end;
end;

end.
