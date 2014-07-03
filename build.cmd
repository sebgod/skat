@setlocal enabledelayedexpansion

:: For the C# compiler, we MUST NOT use the UTF-8 codepage
@set newCP=850
@for /F "usebackq tokens=2 delims=:" %%A in (`chcp`) do @(
    set oldCP=%%A
    set oldCP=!oldCP: =!
    set oldCP=!oldCP:.=!
)
@if %oldCP% NEQ %newCP% chcp %newCP% 1>nul


@if defined MERCURY_HOME (
   call :SET_HOME MMC
) else (
   call :FIND_IN_PATH mercury.bat MMC
)

@if defined MMC goto :MAKE
@echo Cannot find Mercury compiler executable, MERCURY_HOME=%MERCURY_HOME%
@exit /b 1

:MAKE
@pushd %~dp0
@make MMC=%MMC% %*
@set MAKE_RESULT=%ERRORLEVEL%
@popd

:: Restoring the previous codepage
@if %oldCP% NEQ %newCP% chcp %oldCP% 1>nul
@exit /b %MAKE_RESULT%

:SET_HOME
    @endlocal && ( set %1="%MERCURY_HOME%\bin\mmc" ) && exit /b 0

:FIND_IN_PATH
    @set RESULT=%~dp$PATH:1
    @endlocal && ( set %2=%RESULT%mmc ) && exit /b 0
