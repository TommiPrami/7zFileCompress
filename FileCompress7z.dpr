program FileCompress7z;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows, Winapi.Messages, System.IOUtils, System.Math, System.Classes, System.SyncObjs, System.SysUtils,
  System.Types, OtlParallel, OtlCommon, OtlCollections, OtlTask;

const
  MSG_STATUS = WM_USER;

var
  GCriticalSection: TCriticalSection;
  FLastIdleTime: Int64;
  FLastKernelTime: Int64;
  FLastUserTime: Int64;

function FileTimeToInt64(const FileTime: TFileTime): Int64;
begin
  Result := Int64(FileTime.dwHighDateTime) shl 32 or FileTime.dwLowDateTime;
end;

function TotalCpuUsage: Double;
var
  LIdleTime, LKernelTime, LUserTime: TFileTime;
  LIdleDiff, LKernelDiff, LUserDiff, LTotalCpuTime: Int64;
begin
  if GetSystemTimes(LIdleTime, LKernelTime, LUserTime) then
  begin
    LIdleDiff := FileTimeToInt64(LIdleTime) - FLastIdleTime;
    LKernelDiff := FileTimeToInt64(LKernelTime) - FLastKernelTime;
    LUserDiff := FileTimeToInt64(LUserTime) - FLastUserTime;

    LTotalCpuTime := LKernelDiff + LUserDiff;

    FLastIdleTime := FileTimeToInt64(LIdleTime);
    FLastKernelTime := FileTimeToInt64(LKernelTime);
    FLastUserTime := FileTimeToInt64(LUserTime);

    if LTotalCpuTime > 0 then
      Result := 100.0 - ((LIdleDiff * 100.0) / LTotalCpuTime)
    else
      Result := 0.00;
  end
  else
    Result := 0.00;
end;

procedure InitializeGlobals;
begin
  GCriticalSection := nil;

  // Seems to need at least one call to return any sane info
  TotalCpuUsage;
end;

procedure LockingWriteLn(const ALine: string);
begin
  if not Assigned(GCriticalSection) then
    Exit;

  GCriticalSection.Acquire;
  try
    WriteLn(ALine);
  finally
    GCriticalSection.Release;
  end;
end;

function GetMaxThreadCount(const AMaxThreadCount: Integer): Integer;
begin
  Result := EnsureRange(Round(CPUCount * 0.69696969696969), 1, CPUCount);

  Result := Min(Result, AMaxThreadCount);
end;

procedure ExecuteAndWait(const ACommandLine: string);
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

      while PeekMessage(LMsg, 0, 0, 0, PM_REMOVE) do
      begin
        if LMsg.Message = WM_QUIT then
          Exit;

        TranslateMessage(LMsg);
        DispatchMessage(LMsg);
      end;

      GetExitCodeProcess(LProcessInformation.hProcess, LExitCode);
    until LExitCode <> STILL_ACTIVE;
  finally
    CloseHandle(LProcessInformation.hProcess);
    CloseHandle(LProcessInformation.hThread);
  end
  else
    RaiseLastOSError;
end;

function GetFileNameOnly(const AFilename: string): string;
var
  LExtension: string;
begin
  LExtension := ExtractFileExt(AFilename);

  if not LExtension.IsEmpty then
    Result := Copy(AFilename, 1, AFilename.Length - LExtension.Length)
  else
    Result := AFilename
end;

function DirEmpty(const ADirectory: string): Boolean;
var
  LFiles: TStringDynArray;
begin
  LFiles := TDirectory.GetFiles(ADirectory, '*.*', TSearchOption.soTopDirectoryOnly);

  Result := Length(LFiles) = 0;
end;

function GetAvailableMemoryPercentage: Integer;
var
  LMemoryStatus: TMemoryStatus;
begin
  // Retrieve the memory status once
  LMemoryStatus.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(LMemoryStatus);

  Result := LMemoryStatus.dwMemoryLoad;
end;

procedure WaitForSystemStatus;
var
  LRepeatCounter: Integer;
begin
  LRepeatCounter := 0; // Give some time to other processes to wake up

  while (LRepeatCounter <= 35) or ((TotalCpuUsage > 80.00) or (GetAvailableMemoryPercentage > 80.00)) do
  begin
    Sleep(100);
    Inc(LRepeatCounter);
  end;
end;

procedure CompressFile(const ARootDirectory, AFilename: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LFileNameOnly: string;
  LDestinationDir: string;
  LCommandLine: string;
begin
  LFileNameOnly := GetFileNameOnly(AFilename);
  LDestinationDir := ARootDirectory + LFileNameOnly;

  GCriticalSection.Acquire;
  try
    if not DirectoryExists(LDestinationDir) then
      ForceDirectories(LDestinationDir)
    else if not DirEmpty(LDestinationDir) then
    begin
      WriteLn('Destination dir not empty: ' + LDestinationDir.QuotedString('"'));
      Exit;
    end;

    LCommandLine := EXE_7Z + ' ' + 'a -mx9 -md256m -mfb128 -mmt=off -v500m "'
      + IncludeTrailingPathDelimiter(LDestinationDir) + LFileNameOnly + '.7z" "'
      + ARootDirectory + AFilename + '"';

    WriteLn('Executing: ' + LCommandLine + '...');
  finally
    GCriticalSection.Release;
  end;

  WaitForSystemStatus;

  ExecuteAndWait(LCommandLine);
end;

procedure ProcessMessages;
var
  LMsg: TMsg;
begin
  while Integer(PeekMessage(LMsg, 0, 0, 0, PM_REMOVE)) <> 0 do
  begin
    TranslateMessage(LMsg);
    DispatchMessage(LMsg);
  end;
end;

procedure PrintHelp;
begin
  // TODO: Add info what app supposed to do

  GCriticalSection.Acquire;
  try
    WriteLn('FileCompress7z RootPath SearchPattern');
    WriteLn('  FileCompress7z "C:\Temp\" *.largefile');
    WriteLn('');
  finally
    GCriticalSection.Release;
  end;
end;

var
  LRootFolder: string;
  LSearchPattern: string;
  LFiles: TStringList;
begin
  InitializeGlobals;

  LFiles := TStringList.Create;
  GCriticalSection := TCriticalSection.Create;
  try
    try
      LRootFolder := IncludeTrailingPathDelimiter(ParamStr(1));
      LSearchPattern := ParamStr(2);

      if DirectoryExists(LRootFolder) and not LSearchPattern.IsEmpty then
      begin
        //
        LFiles.AddStrings(TDirectory.GetFiles(LRootFolder, LSearchPattern, TSearchOption.soTopDirectoryOnly));

        if LFiles.Count > 0 then
        begin
          Parallel.ForEach(LFiles).NoWait.Execute(
            procedure(const AFileName: TOmniValue)
            var
              LCurrentFile: string;
            begin
              LCurrentFile := AFileName;

              CompressFile(LRootFolder, ExtractFileName(LCurrentFile));
            end
          );

          while True do
          begin
            Sleep(100);
            ProcessMessages;
          end;
        end
        else
        begin
          LockingWriteLn('No files found from directory "' + LRootFolder + '" with search pattern "' + LSearchPattern + '"');
          Exit;
        end;
      end
      else
      begin
        PrintHelp;
        Exit;
      end;
    except
      on E: Exception do
        LockingWriteLn(E.ClassName + ': ' + E.Message);
    end;
  finally
    FreeAndNil(GCriticalSection);
    LFiles.Free;
  end;
end.
