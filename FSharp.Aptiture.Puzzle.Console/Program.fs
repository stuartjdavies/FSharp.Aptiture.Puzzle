open System
open FSharp.Aptiture.Puzzle.Solution
open FSharp.Aptiture.Puzzle.Solution.Subject

[<EntryPoint>]
let main argv = 
    let sem = { Semester.T.Number = 1; Semester.T.Year=2016 }                      
    let subjects = [ Programming(ProgrammingSubject.create "Java Programming" sem)
                     Programming(ProgrammingSubject.create "C# Programming" sem)
                     Programming(ProgrammingSubject.create "PHP Programmming" sem)
                     Design(DesignSubject.create "Graphic Design" sem)
                     Design(DesignSubject.create "Web Design" sem)
                     Design(DesignSubject.create "3D Design" sem)
                     Literature(LiteratureSubject.create "English Literature" sem) ]                
    let r = new System.Random()              
    let genStudents = [ for x in 1 .. 55 do yield Student.generate(r)  ]
 
    let updatedSubs, logs = RoundRobinSubjectEnroller.assignStudentsToSubjects 4 genStudents subjects

    logs |> Log.display

    Console.ReadLine() |> ignore
    0 // return an integer exit code
