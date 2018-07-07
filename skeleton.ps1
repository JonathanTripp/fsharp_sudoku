$sln = "Sudoku"
$libdir = "Sudoku.Lib"
$libproj = "$libdir\$libdir.fsproj"
$exedir = "Sudoku.Repl"
$exeproj = "$exedir\$exedir.fsproj"
$testdir = "Sudoku.Tests"
$testproj = "$testdir\$testdir.fsproj"

dotnet new sln -o $sln

Push-Location
Set-Location $sln
 
#New-Item -ItemType Directory $libdir
#Push-Location
#Set-Location $libdir
dotnet new classlib -lang F# -o $libdir
#Pop-Location
dotnet sln add $libproj

dotnet new console -lang F# -o $exedir
dotnet add $exeproj reference $libproj
dotnet sln add $exeproj

dotnet new mstest -lang F# -o $testdir
dotnet add $testproj reference $libproj
dotnet sln add $testproj

Pop-Location
