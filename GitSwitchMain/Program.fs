open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open LibGit2Sharp
open Pinicola.FSharp
open Pinicola.FSharp.RegularExpressions
open Pinicola.FSharp.SpectreConsole

let currentDirectory = Directory.GetCurrentDirectory()

type GitCredentials = {
    Username: string
    Password: string
}

let fixAzureRegex = Regex(@"https:\/\/(?:\w+@)?dev\.azure\.com\/(?<Org>\w+)\/")

let getGitCredentials url =

    let url = fixAzureRegex |> Regex.replace url @"https://${Org}.visualstudio.com/"
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
    (new Repository(currentDirectory))
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

                fetchOptions.CredentialsProvider <-
                    (fun _ _ _ ->
                        UsernamePasswordCredentials(Username = credentials.Username, Password = credentials.Password)
                    )

                repo.Network.Fetch(originRemote.Name, [], fetchOptions)
            )

        AnsiConsole.markupLineInterpolated $"[green]Fetch completed successfully.[/] [grey]{DateTimeOffset.Now}[/]"

        // Checkout main branch

        let potentialMainBranches = [
            "refs/heads/main"
            "refs/heads/master"
        ]

        let remoteBranches, localBranches =
            repo.Branches |> List.ofSeq |> List.partition _.IsRemote

        let mainBranch, otherLocalBranches =
            localBranches
            |> List.removeExactlyOne (fun b -> potentialMainBranches |> List.contains b.CanonicalName)

        if repo.Head.CanonicalName = mainBranch.CanonicalName then
            AnsiConsole.markupLineInterpolated
                $"[green]Already on main branch: {mainBranch.FriendlyName}.[/] [grey]{DateTimeOffset.Now}[/]"
        else
            AnsiConsole.status ()
            |> Status.start
                $"Checking out main branch: {mainBranch.FriendlyName}"
                (fun _ -> Commands.Checkout(repo, mainBranch) |> ignore)

            AnsiConsole.markupLineInterpolated
                $"[green]Checked out branch: {mainBranch.FriendlyName}.[/] [grey]{DateTimeOffset.Now}[/]"

        // Pull latest changes

        let shouldPull =
            let remoteMainBranch = mainBranch.TrackedBranch |> Option.ofObj

            match remoteMainBranch with
            | None -> failwith "No remote tracking branch found for main branch."
            | Some remoteBranch -> remoteBranch.Tip.Sha <> mainBranch.Tip.Sha

        if not shouldPull then
            AnsiConsole.markupLineInterpolated
                $"[yellow]No changes to pull for branch: {mainBranch.FriendlyName}.[/] [grey]{DateTimeOffset.Now}[/]"
        else
            AnsiConsole.status ()
            |> Status.start
                $"Pulling latest changes for branch: {mainBranch.FriendlyName}"
                (fun _ ->
                    let pullOptions = PullOptions()
                    pullOptions.FetchOptions <- FetchOptions()
                    pullOptions.FetchOptions.Prune <- true

                    pullOptions.FetchOptions.CredentialsProvider <-
                        (fun _ _ _ ->
                            UsernamePasswordCredentials(
                                Username = credentials.Username,
                                Password = credentials.Password
                            )
                        )

                    pullOptions.MergeOptions <- MergeOptions()
                    pullOptions.MergeOptions.FastForwardStrategy <- FastForwardStrategy.FastForwardOnly

                    let signature = repo.Config.BuildSignature(DateTimeOffset.Now)

                    Commands.Pull(repo, signature, pullOptions) |> ignore
                )

            AnsiConsole.markupLineInterpolated
                $"[green]Pulled changes successfully for branch: {mainBranch.FriendlyName}.[/] [grey]{DateTimeOffset.Now}[/]"

        // Check for orphan branches
        for localBranch in otherLocalBranches do
            if not localBranch.IsTracking then
                AnsiConsole.markupLineInterpolated
                    $"[yellow]Orphan branch detected: {localBranch.FriendlyName}. It is not tracking any remote branch.[/] [grey]{DateTimeOffset.Now}[/]"
            else
                let trackedBranch = localBranch.TrackedBranch

                let trackedBranchFound =
                    remoteBranches
                    |> List.exists (fun b -> b.CanonicalName = trackedBranch.CanonicalName)

                if trackedBranchFound then
                    AnsiConsole.markupLineInterpolated
                        $"[green]Branch {localBranch.FriendlyName} is tracking remote branch {trackedBranch.FriendlyName}.[/] [grey]{DateTimeOffset.Now}[/]"
                else
                    AnsiConsole.markupLineInterpolated
                        $"[red]Branch {localBranch.FriendlyName} is tracking a non-existent remote branch: {trackedBranch.FriendlyName}.[/] [grey]{DateTimeOffset.Now}[/]"

                    let delete = AnsiConsole.confirm (Raw "Do you want to delete this branch?")

                    if delete then
                        AnsiConsole.status ()
                        |> Status.start
                            $"Deleting orphan branch: {localBranch.FriendlyName}"
                            (fun _ ->
                                repo.Branches.Remove(localBranch)
                            )

                        AnsiConsole.markupLineInterpolated
                            $"[green]Deleted orphan branch: {localBranch.FriendlyName}.[/] [grey]{DateTimeOffset.Now}[/]"
                    else
                        AnsiConsole.markupLineInterpolated
                            $"[yellow]Skipped deletion of orphan branch: {localBranch.FriendlyName}.[/] [grey]{DateTimeOffset.Now}[/]"

    )
