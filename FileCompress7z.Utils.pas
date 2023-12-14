unit FileCompress7z.Utils;

interface

uses
  Winapi.Messages, Winapi.Windows, System.Diagnostics;

  function DirEmpty(const ADirectory: string): Boolean;
  function FileTimeToInt64(const FileTime: TFileTime): Int64;
  function GetAvailableMemoryPercentage: Integer;
  function GetMaxThreadCount: Integer;
  function TotalCpuUsage: Double;
  procedure PrintHelp;
  procedure ProcessMessages;
  procedure WaitForSystemStatus(const AWaiMillisecs: Integer; const AMaxTotalCpuUsagePercentage, AMaxAValilableMemoryPercentage: Double);

implementation

uses
  System.Types, System.Math, System.IOUtils;

const
  CPU_FACTOR: Double = 0.69696969696969;

var
  GLastIdleTime: Int64;
  GLastKernelTime: Int64;
  GLastUserTime: Int64;

function DirEmpty(const ADirectory: string): Boolean;
var
  LFiles: TStringDynArray;
begin
  LFiles := TDirectory.GetFiles(ADirectory, '*.*', TSearchOption.soTopDirectoryOnly);

  Result := Length(LFiles) = 0;
end;

function FileTimeToInt64(const FileTime: TFileTime): Int64;
begin
  Result := Int64(FileTime.dwHighDateTime) shl 32 or FileTime.dwLowDateTime;
end;

procedure SetGlobalTimes(const ALastIdleTime, ALastKernelTime, ALastUserTime: Int64);
begin
  GLastIdleTime := ALastIdleTime;
  GLastKernelTime := ALastKernelTime;
  GLastUserTime := ALastUserTime;
end;

function TotalCpuUsage: Double;
var
  LIdleTime, LKernelTime, LUserTime: TFileTime;
  LIdleDiff, LKernelDiff, LUserDiff, LTotalCpuTime: Int64;
begin
  if GetSystemTimes(LIdleTime, LKernelTime, LUserTime) then
  begin
    LIdleDiff := FileTimeToInt64(LIdleTime) - GLastIdleTime;
    LKernelDiff := FileTimeToInt64(LKernelTime) - GLastKernelTime;
    LUserDiff := FileTimeToInt64(LUserTime) - GLastUserTime;

    LTotalCpuTime := LKernelDiff + LUserDiff;

    SetGlobalTimes(FileTimeToInt64(LIdleTime), FileTimeToInt64(LKernelTime), FileTimeToInt64(LUserTime));

    if LTotalCpuTime > 0 then
      Result := 100.0 - ((LIdleDiff * 100.0) / LTotalCpuTime)
    else
      Result := 0.00;
  end
  else
    Result := 0.00;
end;

function GetMaxThreadCount: Integer;
begin
  Result := EnsureRange(Round(CPUCount * CPU_FACTOR), 1, CPUCount);
end;

procedure ProcessMessages;
var
  LMsg: TMsg;
begin
  while PeekMessage(LMsg, 0, 0, 0, PM_REMOVE) do
  begin
    if LMsg.Message = WM_QUIT then
      Exit;

    TranslateMessage(LMsg);
    DispatchMessage(LMsg);
  end;
end;

function GetAvailableMemoryPercentage: Integer;
var
  LMemoryStatus: TMemoryStatus;
begin
  LMemoryStatus.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(LMemoryStatus);

  Result := LMemoryStatus.dwMemoryLoad;
end;

procedure WaitForSystemStatus(const AWaiMillisecs: Integer; const AMaxTotalCpuUsagePercentage, AMaxAValilableMemoryPercentage: Double);
const
  SLEEP_TIME = 100;
var
  LStopWatch: TStopwatch;
begin
  LStopWatch := TStopwatch.StartNew;

  // TODO: this potentiaally can hang the whole app, so not too smart :) But let's see does it help or not
  while (LStopWatch.Elapsed.TotalMilliseconds <= AWaiMillisecs) or ((TotalCpuUsage > AMaxTotalCpuUsagePercentage)
    or (GetAvailableMemoryPercentage > AMaxAValilableMemoryPercentage)) do
  begin
    Sleep(SLEEP_TIME);
  end;
end;

procedure PrintHelp;
begin
  // TODO: Add info what app supposed to do

  WriteLn('FileCompress7z RootPath SearchPattern');
  WriteLn('  FileCompress7z "C:\Temp\" *.largefile');
  WriteLn('');
end;

initialization

  SetGlobalTimes(0, 0, 0);

end.
