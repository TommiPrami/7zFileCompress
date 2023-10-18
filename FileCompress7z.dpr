program FileCompress7z;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.IOUtils;

procedure PrintHelp;
begin
  WriteLn('FileCompress7z RootPath SearchPattern');
  WriteLn('  FileCompress7z "C:\Temp\" *.largefile');
  WriteLn('');
end;

var
  LRootFolder: string;
  LFileType: string;
begin
  try
    LRootFolder := IncludeTrailingPathDelimiter(ParamStr(1));
    LFileType := ParamStr(2);

    if FileExists(LRootFolder) and not LFileType.IsEmpty then
    begin
      //
      TDirectory.GetFiles(LRootFolder, LFileType, TSearchOption.soTopDirectoryOnly);

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
