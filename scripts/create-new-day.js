
async function app(github, context, exec) {
    const dateTimeStr = new Date().toLocaleString("nb-NO", { timeZone: "Europe/Oslo" })
    const parts = dateTimeStr.split(".")
    const day =  Number(parts[0]) + 1
    const month = Number(parts[1])
    const year = Number(parts[2].substring(0,4))

    if( year == 2022 && month == 12){
        const targetBrahch = getTargetBranch(day)
        await createNewBranchAndPushItToRemote(exec, targetBrahch)
        const files = fileContents(day)
        await addFile(exec, github, context, `day${day}/input.txt`, `day${day}/input.txt`, targetBrahch)
        await addFile(exec, github, context, `day${day}/test-input.txt`, `day${day}/test-input.txt`, targetBrahch))
    }   
}

const fileContents = (day) => [
    { 
        "path": `day${day}/input.txt`,
        "content": `day${day}/input.txt`

    },
    { 
        "path": `day${day}/test-input.txt`,
        "content": "test-input"

    },
    { 
        "path": `day${day}/day${day}.fsx`,
        "content": `
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let task1 = "task 1"
let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
        `
    }
]

const getTargetBranch = (day) => `day-${day}`

async function createNewBranchAndPushItToRemote(exec, targetBranch) {
    try {
        await exec.exec("git", ["checkout", "-b", targetBranch])
        await exec.exec("git", ["push", "origin", targetBranch, "-f"])
    } catch (error) {
        console.log(error)
    }
}

async function addFile(exec, github, context, filePath, fileContent, targetBranch) {
    await github.rest.repos.createOrUpdateFileContents({
        owner: context.repo.owner,
        repo: context.repo.repo,
        path: filePath,
        message: `Added file ${filePath}`,
        content: Buffer.from(fileContent).toString('base64'),
        branch: targetBranch
    })
}

module.exports = async (github, context, exec) => {
    return app(github, context, exec)
}