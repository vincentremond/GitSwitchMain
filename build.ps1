$ErrorActionPreference = "Stop"

dotnet tool restore
dotnet build

AddToPath .\GitSwitchMain\bin\Debug\
