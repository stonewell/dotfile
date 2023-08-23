extern-wrapped e [...params] {
    mut emacsclient = ""
    if ($nu.os-info.name == "windows") and (($env.EmacsBaseDir? | default '__not_exists__') | path exists) {
        $emacsclient = ([$env.EmacsBaseDir, 'bin', 'emacsclient'] | path join)
    } else {
        $emacsclient = ((which emacsclient).path | path parse | path join).0
    }

    mut runemacs = ""
    if ($nu.os-info.name == "windows") and (($env.EmacsBaseDir? | default '__not_exists__') | path exists) {
        $runemacs = ([$env.EmacsBaseDir, 'bin', 'runemacs'] | path join)
    } else {
        if ($nu.os-info.name == "windows") {
            $runemacs = ((which runemacs).path | path parse | path join).0
        } else {
            $runemacs = ((which emacs).path | path parse | path join).0
        }
    }

    run-external $emacsclient "-n" $"--alternate-editor=($runemacs)" $params
}

extern-wrapped vi [...params] {
    run-external "nvim" $params
}

extern-wrapped vs [...params] {
    mut vscode = ""
    if $nu.os-info.name == "windows" {
        $vscode = (run-external --redirect-stdout --trim-end-newline scoop "prefix" "vscode"|path parse|path join code)
    } else {
        $vscode = ((which code).path | path parse | path join)
    }

    run-external $"($vscode)" $params
}
