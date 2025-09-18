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

        AnsiConsole.markupLineInterpolated
            $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Fetch completed successfully from [blue]{originRemote.Name}[/]."

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
            AnsiConsole.markupLineInterpolated
                $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Already on main branch: [blue]{mainBranch.FriendlyName}[/]."
        else
            AnsiConsole.status ()
            |> Status.start
                $"Checking out main branch: {mainBranch.FriendlyName}"
                (fun _ -> Commands.Checkout(repo, mainBranch) |> ignore)

            AnsiConsole.markupLineInterpolated
                $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Checked out branch: [blue]{mainBranch.FriendlyName}[/]."

        // Pull latest changes

        let shouldPull =
            let remoteMainBranch = mainBranch.TrackedBranch |> Option.ofObj

            match remoteMainBranch with
            | None -> failwith "No remote tracking branch found for main branch."
            | Some remoteBranch -> remoteBranch.Tip.Sha <> mainBranch.Tip.Sha

        if not shouldPull then
            AnsiConsole.markupLineInterpolated
                $"[grey]{DateTimeOffset.Now}[/] [yellow]✓[/] No changes to pull for branch: [blue]{mainBranch.FriendlyName}[/]."
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
                $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Pulled changes successfully for branch: [blue]{mainBranch.FriendlyName}[/]."

        // Check for orphan branches
        let remoteBranches, _, otherLocalBranches = readBranches () // Re-read branches after fetch and pull

        for localBranch in otherLocalBranches do
            if not localBranch.IsTracking then
                AnsiConsole.markupLineInterpolated
                    $"[grey]{DateTimeOffset.Now}[/] [yellow]✓[/] Orphan branch detected: [blue]{localBranch.FriendlyName}[/]. It is not tracking any remote branch."
            else
                let trackedBranch = localBranch.TrackedBranch

                let trackedBranchFound =
                    remoteBranches
                    |> List.exists (fun b -> b.CanonicalName = trackedBranch.CanonicalName)

                if trackedBranchFound then
                    AnsiConsole.markupLineInterpolated
                        $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Branch [blue]{localBranch.FriendlyName}[/] is tracking remote branch [blue]{trackedBranch.FriendlyName}[/]."
                else
                    let confirmText =
                        SpectreConsoleString.fromInterpolated
                            $"[blue]{localBranch.FriendlyName}[/] is tracking a non-existent remote branch [blue]{trackedBranch.FriendlyName}[/]. Do you want to delete this orphan branch?"

                    let delete = AnsiConsole.confirm confirmText

                    if delete then
                        AnsiConsole.status ()
                        |> Status.start
                            $"Deleting orphan branch: {localBranch.FriendlyName}"
                            (fun _ -> repo.Branches.Remove(localBranch.FriendlyName))

                        AnsiConsole.markupLineInterpolated
                            $"[grey]{DateTimeOffset.Now}[/] [green]✓[/] Deleted orphan branch: [blue]{localBranch.FriendlyName}[/]."
                    else
                        AnsiConsole.markupLineInterpolated
                            $"[grey]{DateTimeOffset.Now}[/] [yellow]✓[/] Skipped deletion of orphan branch: [blue]{localBranch.FriendlyName}[/]."

    )
