### DesktopSearch.ps1 -- Search WDS from command line
#
# Author: Lennart Borgman
# Last-Updated: 2010-12-21 Tue

# .SYNOPSIS
# Use Windows Search from command line.
#
# .DESCRIPTION
# Use Windows Search from command line.
#
# .INPUTS
# None. You cannot pipe objects to this script.
#
# .LINK
# Available from http://EmacsWiki.org/NxhtmlMode/

param (
    # Where to search.
    $roots,

    [string[]]
    # What to search for.
    $strings
)


$ErrorActionPreference = "Continue"
$bufferSize = new-object System.Management.Automation.Host.Size 160,72
$host.UI.RawUI.BufferSize = $bufferSize
$host.UI.RawUI.WindowSize = $bufferSize
$ErrorActionPreference = "Stop"

function myout ($obj) {
    out-string -inputObject $obj -stream -width 200
}
# "(get-host).ui"
# (get-host).ui
# ""
# "(get-host).ui.rawui"
# (get-host).ui.rawui
# ""

function Search-WindowsDesktop-2 ($fields, $filter, $strings)
{
    $connection = New-Object -comObject "ADODB.Connection"
    $recordSet = New-Object -comObject "ADODB.RecordSet"

    $null = $connection.Open("Provider=Search.CollatorDSO;Extended Properties='Application=Windows';") > $null
    #out-string -inputobject $connection
    #out-string -inputobject $recordset.Open

    $ofs = ","
    $query = "SELECT $fields"
    $query += " FROM SYSTEMINDEX $filter"
    $null = $recordSet.Open($query, $connection)

    if ($recordSet.EOF) { return }
    $recordSet.MoveFirst()

    $textfiles = ".org",".txt"

    while (-not $recordSet.EOF) {
        #$result = New-Object Object
        $file = ""
        foreach ($field in $fields) {
            #$result | Add-Member NoteProperty $field $recordSet.Fields.Item($field).Value
            $val = $recordSet.Fields.Item($field).Value
            #$v = "-----"+$val
            #$v
            if ($file.Equals("")) { $val = "\" + $val }
            $file = $val + $file
        }
        $v = "-----"+$file
        #$v
        $istext = $False
        foreach ($ext in $textfiles) {
            if ($file.EndsWith($ext)) { $istext = $True }
        }
        if ($istext) {
            myout ("Text file " + $file + " matches:")
            #$res = "text file "+ $file
            #out-host -inputobject $file
            $res = ""
            foreach ($s in $strings) {
                #Select-String -Path $file.ToString() -Pattern $s
                $r = Select-String -Path $file -Pattern $s
                $max = 100
                foreach ($row in $r) {
                    myout $row
                    $row = $row.ToString()
                    $row = "  " + $row.Substring($file.Length)
                    # "========== length: " + $row.Length
                    if ($row.Length -gt $max) {
                        #"++++++++++ too long: " + $row.Length
                        $row = $row.Substring(0, $max)
                    }
                    myout $row
                }
            }
        } else {
            myout ("Binary file " + $file + " matches")
        }
        #$result
        $recordSet.MoveNext()
    }

    $null = $recordSet.Close()
    $null = $connection.Close()

    $connection = $null
    $recordSet = $null
}
function Search-WindowsDesktop-1 ($filter, $strings)
{
    Search-WindowsDesktop-2 "System.FileName", "System.ItemFolderPathDisplay" $filter $strings
}
function Search-WindowsDesktop ($roots, $strings)
{
    $sfs =
    foreach ($str in $strings) {
        "Contains('""" + $str + """')"
    }
    #$fs
    $filter = "WHERE " + ( $sfs -join " AND " )
    if ($roots) {
        $ffs =
        foreach ($root in $roots) {
            "System.ItemFolderPathDisplay LIKE '" + $root + "%'"
        }
        $filter += " AND (" + ( $ffs -join " OR ") + ")"
    }
    myout $filter
    myout ""
    Search-WindowsDesktop-1 $filter $strings
}

# It looks liek $Args can not be used if a param (...) section gives
# the parameters names.
#
# $Args.Length
# $Args
# $roots
# $strings
#if ($Args.Length -gt 0) {

myout $roots
myout $strings
if ($strings) {
    #Search-WindowsDesktop $Args[0] $Args[1]
    if ($Args[2]) {
        help DesktopSearch
    } else {
        # There is a problem with parameter coming from a cmd
        # shell. They will show up as single string in the first array
        # slot.  Test for this and split if it looks like this is the
        # best.  This procedure means that a "," itself can not be
        # included in the search.
        myout ("strings=" + $strings)
        if ($strings.Length -eq 1) {
            myout "l=1!"
            $strings = $strings[0].Split(",")
            # $strings.Trim()
            #[string[]]$newstr = []
            foreach ($s in $strings) {
                [string[]]$newstr += $s.Trim()
            }
            myout $newstr
            # myout $strings
        }
        # Sending \ in filenames fails sometimes so let us assume that
        # we always recieves /.  We must then change / => \ for
        # matching.
        foreach ($r in $roots) {
            [string[]]$newrts += $r.Replace("/", "\")
        }
        Search-WindowsDesktop $newrts $newstr
    }
} else {
    if ($roots) {
        help DesktopSearch
    }}

# process {"Checking pipe, got: $_" }
# some error here

# "at the end of desktopsearch.ps1"
# exit
