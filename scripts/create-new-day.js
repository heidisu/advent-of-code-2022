
const { Console } = require('console')
var crypto = require('crypto')

async function app(github, context, exec) {
    console.log("DATE------in app")
    const dateTimeStr = new Date().toLocaleString("us-EN", { timeZone: "Europe/Oslo" })
    const date = new Date(dateTimeStr)
    const year = date.getFullYear()
    const month =date.getMonth() + 1
    const day = date.getDay() + 1

    console.log("DATE-------", dateTimeStr, date, year, month, day)
    if( year == 2022 && month == 12){
        const targetBrahch = getTargetBranch(day)
        await createNewBranchAndPushItToRemote(exec, targetBrahch)
        const files = files(day)
        files.forEach((file) => addFile(github, context, file.path, file.content, targetBrahch))
    }   
}

const files = (day) => [
    { 
        "path": `day${day}/input.txt`,
        "content": "input"

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

async function addFile(github, context, filePath, fileContent, targetBranch) {
    await github.rest.repos.createOrUpdateFileContents({
        owner: context.repo.owner,
        repo: context.repo.repo,
        path: filePath,
        message: `Added file ${filePath}`,
        content: btoa(fileContent),
        branch: targetBranch
    })
}

module.exports = async (github, context, exec) => {
    return app(github, context, exec)
}