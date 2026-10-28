@echo off
ocaml coda.ml <NUL
IF ERRORLEVEL 1 EXIT /b %ERRORLEVEL%

for %%F in ("data\*.cod") do (
echo /// %%~nxF
ocaml coda.ml <%%F
IF ERRORLEVEL 1 EXIT /b %ERRORLEVEL%
)

echo pass