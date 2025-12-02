@echo off
title Publicar aplicación Shiny en GitHub
color 0A

REM === CONFIGURACIÓN ===
set "RUTA_APP=E:\Escritorio 2025\RevAutoSearch"
set "USUARIO=Vicentemed"
set "REPO=RevAutoSearch"
set "RAMA=main"

REM === CAMBIAR DIRECTORIO ===
cd /d "%RUTA_APP%"
if errorlevel 1 (
    echo [ERROR] No se pudo acceder a la carpeta "%RUTA_APP%"
    pause
    exit /b
)

echo =========================================================
echo 🚀 INICIANDO PUBLICACIÓN DE LA APP SHINY EN GITHUB
echo Carpeta del proyecto: %RUTA_APP%
echo Repositorio remoto:   https://github.com/%USUARIO%/%REPO%.git
echo =========================================================
echo.

REM === VERIFICAR QUE GIT ESTE INSTALADO ===
git --version >nul 2>&1
if errorlevel 1 (
    echo [ERROR] Git no está instalado o no está en el PATH.
    echo Descárgalo desde: https://git-scm.com/downloads
    pause
    exit /b
)

REM === INICIALIZAR GIT SI ES NECESARIO ===
if not exist ".git" (
    echo 📁 Inicializando repositorio Git local...
    git init
)

REM === AGREGAR ARCHIVOS ===
echo 📂 Añadiendo archivos nuevos o modificados...
git add .

REM === GENERAR FECHA Y HORA ===
for /f "tokens=1-3 delims=/ " %%a in ('date /t') do set FECHA=%%c-%%a-%%b
for /f "tokens=1-2 delims=: " %%a in ('time /t') do set HORA=%%a-%%b

REM === CREAR COMMIT CON FECHA ===
set "MSG=Actualización automática %FECHA% %HORA%"
echo 💾 Creando commit: "%MSG%"
git commit -m "%MSG%" >nul 2>&1

REM === CONFIGURAR REMOTO SI FALTA ===
git remote -v | find "origin" >nul 2>&1
if errorlevel 1 (
    echo 🔗 Configurando enlace remoto con GitHub...
    git remote add origin https://github.com/%USUARIO%/%REPO%.git
)

REM === ASEGURAR NOMBRE DE RAMA ===
git branch -M %RAMA%

REM === HACER PUSH A GITHUB ===
echo 🌍 Subiendo cambios al repositorio remoto...
git push -u origin %RAMA%

if errorlevel 1 (
    
)

echo.
echo ✅ Publicación completada exitosamente.
echo Puedes ver tu aplicación en:
echo 🔗 https://github.com/%USUARIO%/%REPO%
echo.
pause
