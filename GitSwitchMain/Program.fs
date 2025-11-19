open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open LibGit2Sharp
open Pinicola.FSharp
open Pinicola.FSharp.RegularExpressions
open Pinicola.FSharp.SpectreConsole

let gitDirectory =
    let currentDirectory = Directory.GetCurrentDirectory()

    match (Repository.Discover(currentDirectory) |> Option.ofObj) with
    | None ->
        AnsiConsole.markupLine "[red]No git repository found in the current directory or its parents.[/]"
        Environment.Exit(1)
        failwith "Unreachable"
    | Some dir -> dir

type GitCredentials = {
    Username: string
    Password: string
}

type BranchIdentifier = | FriendlyName of string

let (|IsTracking|_|) (remoteBranches: Branch list) (branch: Branch) =
    if branch.IsTracking then
        let trackedBranch = branch.TrackedBranch

        match
            remoteBranches
            |> List.filter (fun b -> b.CanonicalName = trackedBranch.CanonicalName)
        with
        | [] -> Some None
        | [ b ] -> Some(Some b)
        | _ -> failwith "Multiple remote branches found with the same canonical name."
    else
        None

let (|IsNotTracking|_|) (branch: Branch) =
    if not branch.IsTracking then Some() else None

let fixAzureRegex = Regex(@"https:\/\/(?:\w+@)?dev\.azure\.com\/(?<Org>\w+)\/")

let getGitCredentials url =

    let url = url |> Regex.replace fixAzureRegex @"https://${Org}.visualstudio.com/"
    let uri = Uri(url)
    let host = uri.Host

    let startInfo = ProcessStartInfo()
    startInfo.FileName <- "git"
    startInfo.Arguments <- "credential fill"
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo.RedirectStandardInput <- true
    startInfo.UseShellExecute <- false
    startInfo.CreateNoWindow <- true

    let p = Process.Start(startInfo)

    p.StandardInput.WriteLine($"protocol={uri.Scheme}")
    p.StandardInput.WriteLine($"host={host}")
    p.StandardInput.WriteLine()

    let output =
        let rec readLines acc =
            match p.StandardOutput.ReadLine() |> Option.ofObj with
            | Some line -> readLines (line :: acc)
            | None -> acc |> List.rev

        readLines []

    p.WaitForExit()

    let mappedOutput =
        output
        |> List.map (fun line ->
            match line |> String.splitMax '=' 2 with
            | [ key; value ] -> key, value
            | _ -> failwith "Invalid credential format"
        )
        |> Map.safeOfSeq

    let userName = mappedOutput |> Map.find "username"
    let password = mappedOutput |> Map.find "password"

    {
        Username = userName
        Password = password
    }

using
    (new Repository(gitDirectory))
    (fun repo ->

        let originRemote = repo.Network.Remotes |> Seq.exactlyOne

        let credentials = getGitCredentials originRemote.Url

        // Fetch

        AnsiConsole.status ()
        |> Status.start
            $"Fetching from remote: {originRemote.Name}"
            (fun _ ->
                let fetchOptions = FetchOptions()
                fetchOptions.Prune <- true
                fetchOptions.TagFetchMode <- TagFetchMode.All

                fetchOptions.CredentialsProvider <- (fun _ _ _ -> UsernamePasswordCredentials(Username = credentials.Username, Password = credentials.Password))

                repo.Network.Fetch(originRemote.Name, [], fetchOptions)
            )

        AnsiConsole.markupLineInterpolated $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Fetch completed successfully from [blue]{originRemote.Name}[/]."

        // Checkout main branch

        let potentialMainBranches = [
            "refs/heads/main"
            "refs/heads/master"
        ]

        let readBranches () =

            let remoteBranches, localBranches =
                repo.Branches |> List.ofSeq |> List.partition _.IsRemote

            let mainBranch, otherLocalBranches =
                localBranches
                |> List.removeExactlyOne (fun b -> potentialMainBranches |> List.contains b.CanonicalName)

            (remoteBranches, mainBranch, otherLocalBranches)

        let _, mainBranch, _ = readBranches ()

        if repo.Head.CanonicalName = mainBranch.CanonicalName then
            AnsiConsole.markupLineInterpolated $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Already on main branch: [blue]{mainBranch.FriendlyName}[/]."
        else
            AnsiConsole.status ()
            |> Status.start $"Checking out main branch: {mainBranch.FriendlyName}" (fun _ -> Commands.Checkout(repo, mainBranch) |> ignore)

            AnsiConsole.markupLineInterpolated $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Checked out branch: [blue]{mainBranch.FriendlyName}[/]."

        // Pull latest changes

        let shouldPull =
            let remoteMainBranch = mainBranch.TrackedBranch |> Option.ofObj

            match remoteMainBranch with
            | None -> failwith "No remote tracking branch found for main branch."
            | Some remoteBranch -> remoteBranch.Tip.Sha <> mainBranch.Tip.Sha

        if not shouldPull then
            AnsiConsole.markupLineInterpolated $"[grey]{DateTimeOffset.Now}[/] [yellow]✓[/] No changes to pull for branch: [blue]{mainBranch.FriendlyName}[/]."
        else
            AnsiConsole.status ()
            |> Status.start
                $"Pulling latest changes for branch: {mainBranch.FriendlyName}"
                (fun _ ->
                    let pullOptions = PullOptions()
                    pullOptions.FetchOptions <- FetchOptions()
                    pullOptions.FetchOptions.Prune <- true

                    pullOptions.FetchOptions.CredentialsProvider <- (fun _ _ _ -> UsernamePasswordCredentials(Username = credentials.Username, Password = credentials.Password))

                    pullOptions.MergeOptions <- MergeOptions()
                    pullOptions.MergeOptions.FastForwardStrategy <- FastForwardStrategy.FastForwardOnly

                    let signature = repo.Config.BuildSignature(DateTimeOffset.Now)

                    Commands.Pull(repo, signature, pullOptions) |> ignore
                )

            AnsiConsole.markupLineInterpolated $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Pulled changes successfully for branch: [blue]{mainBranch.FriendlyName}[/]."

        // Check for orphan branches

        let rec cleanOrphanBranches (checkedBranches: BranchIdentifier list) =

            let remoteBranches, _, otherLocalBranches = readBranches () // Re-read branches after fetch and pull

            let otherLocalBranchesToCheck =
                otherLocalBranches
                |> List.filter (fun b ->
                    not (
                        checkedBranches
                        |> List.exists (fun c ->
                            match c with
                            | FriendlyName name -> name = b.FriendlyName
                        )
                    )
                )

            match otherLocalBranchesToCheck with
            | [] -> ()
            | localBranch :: _ ->

                match localBranch with
                | IsNotTracking ->
                    AnsiConsole.markupLineInterpolated
                        $"[grey]{DateTimeOffset.Now}[/] [yellow]✓[/] Orphan branch detected: [blue]{localBranch.FriendlyName}[/]. It is not tracking any remote branch."

                | IsTracking remoteBranches trackedBranch ->
                    match trackedBranch with
                    | Some remoteTrackedBranch ->

                        let trimmedTrackedName =
                            remoteTrackedBranch.FriendlyName
                            |> Regex.replace (Regex($@"^{remoteTrackedBranch.RemoteName}/")) ""

                        AnsiConsole.markupLineInterpolated
                            $"[grey]{DateTimeOffset.Now}[/] [blue]?[/] Local branch [blue]{localBranch.FriendlyName}[/] is tracking a non-existent remote branch [blue]{trimmedTrackedName}[/]. Do you want to delete this orphan branch?"
                    | None ->
                        let confirmText =
                            SpectreConsoleString.fromInterpolated
                                $"[grey]{DateTimeOffset.Now}[/] [blue]?[/] Local branch [blue]{localBranch.FriendlyName}[/] is tracking a non-existent remote branch. Do you want to delete this orphan branch?"

                        let delete = AnsiConsole.confirm confirmText

                        if delete then
                            AnsiConsole.status ()
                            |> Status.start $"Deleting orphan branch: {localBranch.FriendlyName}" (fun _ -> repo.Branches.Remove(localBranch.FriendlyName))

                            AnsiConsole.markupLineInterpolated $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Deleted orphan branch: [blue]{localBranch.FriendlyName}[/]."
                        else
                            AnsiConsole.markupLineInterpolated
                                $"[grey]{DateTimeOffset.Now}[/] [yellow]✓[/] Skipped deletion of orphan branch: [blue]{localBranch.FriendlyName}[/]."

                | _ -> failwith $"Unreachable case in branch checking for branch: {localBranch.FriendlyName}"

                // Continue checking other branches
                let newCheckedBranches = FriendlyName localBranch.FriendlyName :: checkedBranches
                cleanOrphanBranches newCheckedBranches

        cleanOrphanBranches []

    )
