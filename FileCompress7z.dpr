program FileCompress7z;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows, System.IOUtils, System.SysUtils, System.Types;

procedure ExecuteAndWait(const ACommandLine: string);
var
  LStartupInfo: TStartupInfo;
  LProcessInformation: TProcessInformation;
  LCommandLine: string;
begin
  LCommandLine := Trim(ACommandLine);

  FillChar(LStartupInfo, SizeOf(LStartupInfo), 0);

  LStartupInfo.cb := SizeOf(TStartupInfo);
  LStartupInfo.wShowWindow := SW_HIDE;

  if CreateProcess(nil, PChar(LCommandLine), nil, nil, True, CREATE_NO_WINDOW, nil, nil, LStartupInfo,
    LProcessInformation) then
  try

    // loop every 10 ms
    while WaitForSingleObject(LProcessInformation.hProcess, 10) > 0 do
    begin
      (*
      repeat
        while PeekMessage(Msg, 0, 0, 0, pm_Remove) do
        begin
          if Msg.Message = wm_Quit then Halt(Msg.WParam);
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
        GetExitCodeProcess(ProcessInfo.hProcess,lpExitCode);
      until lpExitCode <> Still_Active;
      *)
    end;
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

procedure CompressFile(const ARootDirectory, AFilename: string);
const
  EXE_7Z = 'C:\Program Files\7-Zip\7z.exe';
var
  LFileNameOnly: string;
begin
  LFileNameOnly := GetFileNameOnly(AFilename);

  if not DirectoryExists(ARootDirectory + LFileNameOnly) then
    ForceDirectories(ARootDirectory + LFileNameOnly);

  ExecuteAndWait(EXE_7Z + ' ' + 'a -mx9 -md128m -mfb128 -mmt=off -v500m "'
    + IncludeTrailingPathDelimiter(ARootDirectory + LFileNameOnly) + LFileNameOnly + '.7z" "'
    + ARootDirectory + AFilename + '"');
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
