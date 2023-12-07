program FileCompress7z;

{$APPTYPE CONSOLE}

// Disable the "new" RTTI to make exe smaller
{$WEAKLINKRTTI ON}

{$IF DECLARED(TVisibilityClasses)}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

{$R *.res}

uses
  Winapi.Windows, System.IOUtils, System.Math, System.Classes, System.SyncObjs, System.SysUtils, System.Types, System.Threading;

var
  GCriticalSection: TCriticalSection;
  FLastIdleTime: Int64;
  FLastKernelTime: Int64;
  FLastUserTime: Int64;

procedure ZeroOutGlobals;
begin
  GCriticalSection := nil;

  FLastIdleTime := 0;
  FLastKernelTime := 0;
  FLastUserTime := 0;
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

function GetMaxThreadCount: Integer;
begin
  Result := EnsureRange(Round(CPUCount * 0.69696969696969), 1, CPUCount);
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
    repeat
      Sleep(10);
      (*
      while PeekMessage(Msg, 0, 0, 0, pm_Remove) do
      begin
        if Msg.Message = wm_Quit then
          Halt(Msg.WParam);
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
      *)
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

function FileTimeToInt64(const FileTime: _FILETIME): Int64;
begin
  Result := Int64(FileTime.dwHighDateTime) shl 32 or FileTime.dwLowDateTime;
end;

function TotalCpuUsage: Double;
var
  IdleTime, KernelTime, UserTime: _FILETIME;
  IdleDiff, KernelDiff, UserDiff, TotalDiff: Int64;
begin
  // Call GetSystemTimes to get the current system times
  if GetSystemTimes(IdleTime, KernelTime, UserTime) then
  begin
    // Convert FILETIME to Int64
    IdleDiff := FileTimeToInt64(IdleTime) - FLastIdleTime;
    KernelDiff := FileTimeToInt64(KernelTime) - FLastKernelTime;
    UserDiff := FileTimeToInt64(UserTime) - FLastUserTime;

    // Calculate the total difference
    TotalDiff := KernelDiff + UserDiff;

    // Update the last values for the next iteration
    FLastIdleTime := FileTimeToInt64(IdleTime);
    FLastKernelTime := FileTimeToInt64(KernelTime);
    FLastUserTime := FileTimeToInt64(UserTime);

    // Calculate the CPU percentage
    if TotalDiff > 0 then
      Result := 100.0 - ((IdleDiff * 100.0) / TotalDiff)
    else
      Result := 0.0;
  end
  else
    Result := 0.0;
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
  LRepeatCounter := 0;

  while (LRepeatCounter < 35) and ((TotalCpuUsage > 80.00) or (GetAvailableMemoryPercentage > 80.00)) do
  begin
    Sleep(100);
    Inc(LRepeatCounter); // Give some time to other processes to wake up
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
  LFiles: TStringDynArray;
  LThreadPoool: TThreadPool;
begin
  ZeroOutGlobals;

  GCriticalSection := TCriticalSection.Create;
  try
    try
      LRootFolder := IncludeTrailingPathDelimiter(ParamStr(1));
      LSearchPattern := ParamStr(2);

      if DirectoryExists(LRootFolder) and not LSearchPattern.IsEmpty then
      begin
        //
        LFiles := TDirectory.GetFiles(LRootFolder, LSearchPattern, TSearchOption.soTopDirectoryOnly);

        if Length(LFiles) > 0 then
        begin
          LThreadPoool := TThreadPool.Create;
          try
            LThreadPoool.SetMaxWorkerThreads(GetMaxThreadCount);

            TParallel.&for(Low(LFiles), High(LFiles),
              procedure(AFileIndex: Integer)
              var
                LCurrentFile: string;
              begin
                LCurrentFile := LFiles[AFileIndex];

                CompressFile(LRootFolder, ExtractFileName(LCurrentFile));
              end,
              LThreadPoool
            );
          finally
            LThreadPoool.Free;
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
  end;
end.
