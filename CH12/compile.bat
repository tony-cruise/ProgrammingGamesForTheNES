@del megablast.o
@del megablast.nes
@del megablast.map.txt
@del megablast.labels.txt
@del megablast.nes.ram.nl
@del megablast.nes.0.nl
@del megablast.nes.1.nl
@del *.dbg
@echo.
@echo Compiling...
\cc65\bin\ca65 megablast.s -g -o megablast.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
\cc65\bin\ld65 -o megablast.nes -C megablast.cfg megablast.o -m megablast.map.txt -Ln megablast.labels.txt --dbgfile megablast.dbg
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Success!
@GOTO endbuild
:failure
@echo.
@echo Build error!
:endbuild