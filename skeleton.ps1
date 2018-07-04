dotnet new sln

$libdir = "Sudoku.Lib"
$libproj = "$libdir\$libdir.fsproj"
$exedir = "Sudoku.Repl"
$exeproj = "$exedir\$exedir.fsproj"
$testdir = "Sudoku.Tests"
$testproj = "$testdir\$testdir.fsproj"
 
New-Item -ItemType Directory $libdir
Push-Location
Set-Location $libdir
dotnet new classlib -lang F#
Pop-Location
dotnet sln add $libproj

mkdir $exedir
cd $exedir
dotnet new console -lang F#
dotnet add reference "..\$libproj"
cd ..
dotnet sln add $exeproj

mkdir $testdir
cd $testdir
dotnet new mstest -lang F#
dotnet add reference "..\$libproj"
cd ..
dotnet sln add $testproj
