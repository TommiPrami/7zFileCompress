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
  System.Diagnostics,
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
    function Lock: Boolean; inline;
    procedure Unlock; inline;
    procedure LockingWriteLn(const ALine: string);
    procedure CompressFile(const ARootDirectory, AFilename: string);
    procedure FilterByDirectories(const AFiles: TStringList; const ARootDirectory: string);
  public
    constructor Create(const ARootPath, ASearchPattern: string);
    destructor Destroy; override;

    procedure Execute;
  end;

function TFileCompress7z.Lock: Boolean;
begin
  Result := False;

  if not Assigned(FCriticalSection) then
    Exit;

  FCriticalSection.Acquire;

  Result := True;
end;

procedure TFileCompress7z.Unlock;
begin
  FCriticalSection.Release;
end;

procedure TFileCompress7z.LockingWriteLn(const ALine: string);
begin

  if Lock then
  try
    WriteLn(ALine);
  finally
    Unlock;
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
      var LStopWatch := TStopWatch.StartNew;

      FRunningTasks := True;

      Parallel.ForEach(LFiles)
        .NumTasks(GetMaxThreadCount)
        .OnStop(
          procedure
          begin
            if Lock then
            try
              FRunningTasks := False;
            finally
              Unlock;
            end;
          end)
        .NoWait
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
        Sleep(200);
        ProcessMessages;

        if Lock then
        try
          if not FRunningTasks then
            Break;
        finally
          Unlock;
        end;
      end;

      LStopWatch.Stop;
      LockingWriteLn(' Elapsed time: ' + LStopWatch.Elapsed.ToString);
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

  if Lock then
  try
    LCommandLine := EXE_7Z + ' ' + 'a -mx7 -md128m -mmt=off -v1000m "'
      + IncludeTrailingPathDelimiter(LDestinationDir) + LFileNameOnly + '.7z" "'
      + ARootDirectory + AFilename + '"';

    WriteLn('Executing: ' + LCommandLine + '...');
  finally
    Unlock
  end;

  WaitForSystemStatus(3500, 80.00, 80.00);

  ExecuteAndWait(LCommandLine, fcpcIdle);
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
