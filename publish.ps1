param (
    [Parameter(Mandatory=$true)]
    $Version
)
nuget pack .\TimeSeries\TimeSeries.fsproj -Build -Properties Configuration=Release -Version $Version
cp '*.nupkg' \\db-1\dropzone\packages
