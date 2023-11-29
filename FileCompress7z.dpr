program FileCompress7z;

{$APPTYPE CONSOLE}

// Disable the "new" RTTI to make exe smaller
{$WEAKLINKRTTI ON}

{$IF DECLARED(TVisibilityClasses)}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

{$R *.res}

uses
  Winapi.Windows, System.IOUtils, System.Math, System.SysUtils, System.Types;

procedure ExecuteAndWait(const ACommandLine: string);
var
  LStartupInfo: TStartupInfo;
  LProcessInformation: TProcessInformation;
  LCommandLine: string;
  LExitCode: DWORD;
begin
  LCommandLine := Trim(ACommandLine);

  FillChar(LStartupInfo, SizeOf(LStartupInfo), 0);

  LStartupInfo.cb := SizeOf(TStartupInfo);
  LStartupInfo.wShowWindow := SW_SHOW;

  if CreateProcess(nil, PChar(LCommandLine), nil, nil, True, 0, nil, nil, LStartupInfo,
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

function Get7zThreadCount: Integer;
begin
  Result := EnsureRange(Round(CPUCount * 0.47494937998792065033), 1, CPUCount - 1);
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

  if not DirectoryExists(LDestinationDir) then
    ForceDirectories(LDestinationDir)
  else if not DirEmpty(LDestinationDir) then
  begin
    WriteLn('Destination dir not empty: ' + LDestinationDir.QuotedString('"'));
    Exit;
  end;

  LCommandLine := EXE_7Z + ' ' + 'a -mx9 -md128m -mfb128 -mmt=' + Get7zThreadCount.ToString + ' -v500m "'
    + IncludeTrailingPathDelimiter(LDestinationDir) + LFileNameOnly + '.7z" "'
    + ARootDirectory + AFilename + '"';
  WriteLn('Executing: ' + LCommandLine + '...');

  ExecuteAndWait(LCommandLine);
end;


procedure PrintHelp;
begin
  // TODO: Add info what app supposed to do

  WriteLn('FileCompress7z RootPath SearchPattern');
  WriteLn('  FileCompress7z "C:\Temp\" *.largefile');
  WriteLn('');
end;

var
  LRootFolder: string;
  LSearchPattern: string;
  LFiles: TStringDynArray;
  LCurrentFile: string;
begin
  try
    LRootFolder := IncludeTrailingPathDelimiter(ParamStr(1));
    LSearchPattern := ParamStr(2);

    if DirectoryExists(LRootFolder) and not LSearchPattern.IsEmpty then
    begin
      //
      LFiles := TDirectory.GetFiles(LRootFolder, LSearchPattern, TSearchOption.soTopDirectoryOnly);

      if Length(LFiles) > 0 then
      begin
        for LCurrentFile in LFiles do
        begin
          CompressFile(LRootFolder, ExtractFileName(LCurrentFile));
        end;
      end
      else
      begin
        WriteLn('No files found from directory "' + LRootFolder + '" with search pattern "' + LSearchPattern + '"');
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
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
