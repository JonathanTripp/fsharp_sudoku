$sln = "Sudoku"
$libcompatdir = "SudokuLibCompat"
$libcompatproj = "$libcompatdir\$libcompatdir.fsproj"
$libcoredir = "SudokuLibCore"
$libcoreproj = "$libcoredir\$libcoredir.fsproj"
$libhintsdir = "SudokuLibHints"
$libhintsproj = "$libhintsdir\$libhintsdir.fsproj"
$exedir = "SudokuRepl"
$exeproj = "$exedir\$exedir.fsproj"
$testdir = "SudokuTests"
$testproj = "$testdir\$testdir.fsproj"

dotnet new sln -o $sln

Push-Location
Set-Location $sln
 
#New-Item -ItemType Directory $libdir
#Push-Location
#Set-Location $libdir
dotnet new classlib -lang F# -o $libcompatdir
#Pop-Location
dotnet sln add $libcompatproj

dotnet new classlib -lang F# -o $libcoredir
dotnet add $libcoreproj reference $libcompatproj
dotnet sln add $libcoreproj

dotnet new classlib -lang F# -o $libhintsdir
dotnet add $libhintsproj reference $libcompatproj
dotnet add $libhintsproj reference $libcoreproj
dotnet sln add $libhintsproj

dotnet new console -lang F# -o $exedir
dotnet add $exeproj reference $libcompatproj
dotnet add $exeproj reference $libcoreproj
dotnet add $exeproj reference $libhintsproj
dotnet sln add $exeproj

dotnet new mstest -lang F# -o $testdir
dotnet add $testproj reference $libcompatproj
dotnet add $testproj reference $libcoreproj
dotnet add $testproj reference $libhintsproj
dotnet sln add $testproj

Pop-Location
