program FileCompress7z;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.IOUtils,
  System.Math,
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  System.Types,
  OtlParallel,
  OtlCommon,
  OtlCollections,
  OtlTask,
  FileCompress7z.Utils in 'FileCompress7z.Utils.pas';

type
  TFileCompress7z = class(TObject)
  strict private
    FCriticalSection: TCriticalSection;
    FRunningTasks: Boolean;
    FRootPath: string;
    FSearchPattern: string;
    procedure LockingWriteLn(const ALine: string);
    procedure ExecuteAndWait(const ACommandLine: string);
    function GetFileNameOnly(const AFilename: string): string;
    procedure CompressFile(const ARootDirectory, AFilename: string);
    procedure FilterByDirectories(const AFiles: TStringList; const ARootDirectory: string);
  public
    constructor Create(const ARootPath, ASearchPattern: string);
    destructor Destroy; override;

    procedure Execute;
  end;

procedure TFileCompress7z.LockingWriteLn(const ALine: string);
begin
  if not Assigned(FCriticalSection) then
    Exit;

  FCriticalSection.Acquire;
  try
    WriteLn(ALine);
  finally
    FCriticalSection.Release;
  end;
end;

procedure TFileCompress7z.Execute;
var
  LFiles: TStringList;
begin
  LFiles := TStringList.Create;
  try
    LFiles.AddStrings(TDirectory.GetFiles(FRootPath, FSearchPattern, TSearchOption.soTopDirectoryOnly));

    FilterByDirectories(LFiles, FRootPath);

    if LFiles.Count > 0 then
    begin
      FRunningTasks := True;

      Parallel.ForEach(LFiles).NumTasks(GetMaxThreadCount).NoWait.OnStopInvoke(
        procedure
        begin
          FCriticalSection.Acquire;
          try
            FRunningTasks := False;
          finally
            FCriticalSection.Release;
          end;
        end)
        .Execute(
        procedure(const AFileName: TOmniValue)
        var
          LCurrentFile: string;
        begin
          LCurrentFile := AFileName;

          CompressFile(FRootPath, ExtractFileName(LCurrentFile));
        end
      );

      while True do
      begin
        Sleep(100);
        ProcessMessages;

        FCriticalSection.Acquire;
        try
          if not FRunningTasks then
            Break;
        finally
          FCriticalSection.Release;
        end;
      end;
    end
    else
    begin
      LockingWriteLn('No files found from directory "' + FRootPath + '" with search pattern "' + FSearchPattern + '"');
      Exit;
    end;
  finally
    LFiles.Free;
  end;
end;

procedure TFileCompress7z.ExecuteAndWait(const ACommandLine: string);
var
  LStartupInfo: TStartupInfo;
  LProcessInformation: TProcessInformation;
  LCommandLine: string;
  LExitCode: DWORD;
  LCreationFlags: DWORD;
begin
  LCommandLine := Trim(ACommandLine);

  FillChar(LStartupInfo, SizeOf(LStartupInfo), 0);

  LStartupInfo.cb := SizeOf(TStartupInfo);
  LStartupInfo.wShowWindow := SW_SHOW;

  LCreationFlags := NORMAL_PRIORITY_CLASS or CREATE_NEW_CONSOLE;

  if CreateProcess(nil, PChar(LCommandLine), nil, nil, True, LCreationFlags, nil, nil, LStartupInfo,
    LProcessInformation) then
  try
    var LMsg: TMsg;

    repeat
      Sleep(10);

      ProcessMessages;

      GetExitCodeProcess(LProcessInformation.hProcess, LExitCode);
    until LExitCode <> STILL_ACTIVE;
  finally
    CloseHandle(LProcessInformation.hProcess);
    CloseHandle(LProcessInformation.hThread);
  end
  else
    RaiseLastOSError;
end;

function TFileCompress7z.GetFileNameOnly(const AFilename: string): string;
var
  LExtension: string;
begin
  Result := ExtractFileName(AFilename);
  LExtension := ExtractFileExt(Result);

  if not LExtension.IsEmpty then
    Result := Copy(Result, 1, Result.Length - LExtension.Length)
  else
    Result := Result
end;

constructor TFileCompress7z.Create(const ARootPath, ASearchPattern: string);
begin
  inherited Create;

  FRootPath := ARootPath;
  FSearchPattern := ASearchPattern;
  FCriticalSection := TCriticalSection.Create;

  // Need one or more calls to stabilize, it seems....
  TotalCpuUsage;
end;

destructor TFileCompress7z.Destroy;
begin
  FreeAndNil(FCriticalSection);

  inherited Destroy;
end;

procedure TFileCompress7z.CompressFile(const ARootDirectory, AFilename: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LFileNameOnly: string;
  LDestinationDir: string;
  LCommandLine: string;
begin
  LFileNameOnly := GetFileNameOnly(AFilename);
  LDestinationDir := ARootDirectory + LFileNameOnly;

  FCriticalSection.Acquire;
  try
    LCommandLine := EXE_7Z + ' ' + 'a -mx9 -md256m -mfb128 -mmt=off -v500m "'
      + IncludeTrailingPathDelimiter(LDestinationDir) + LFileNameOnly + '.7z" "'
      + ARootDirectory + AFilename + '"';

    WriteLn('Executing: ' + LCommandLine + '...');
  finally
    FCriticalSection.Release;
  end;

  WaitForSystemStatus(3500, 80.00, 80.00);

  ExecuteAndWait(LCommandLine);
end;

procedure TFileCompress7z.FilterByDirectories(const AFiles: TStringList; const ARootDirectory: string);
var
  LIndex: Integer;
  LFileNameOnly: string;
  LDestinationDir: string;
begin
  for LIndex := AFiles.Count - 1 downto 0 do
  begin
    LFileNameOnly := GetFileNameOnly(AFiles[LIndex]);
    LDestinationDir := ARootDirectory + LFileNameOnly;

    if not DirectoryExists(LDestinationDir) then
      ForceDirectories(LDestinationDir)
    else if not DirEmpty(LDestinationDir) then
    begin
      WriteLn('Destination dir not empty: ' + LDestinationDir.QuotedString('"'));
      AFiles.Delete(LIndex);
    end;
  end;
end;

var
  LRootFolder: string;
  LSearchPattern: string;
  LFileCompress: TFileCompress7z;
begin
  try
    LRootFolder := IncludeTrailingPathDelimiter(ParamStr(1));
    LSearchPattern := ParamStr(2);

    if DirectoryExists(LRootFolder) and not LSearchPattern.IsEmpty then
    begin
      LFileCompress := TFileCompress7z.Create(LRootFolder, LSearchPattern);
      try
        LFileCompress.Execute;
      finally
        LFileCompress.Free;
      end;
    end
    else
    begin
      PrintHelp;
      Exit;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName + ': ' + E.Message);
  end;
end.
