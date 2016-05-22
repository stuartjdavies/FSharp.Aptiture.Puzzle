module FSharp.Aptiture.Puzzle.Solution

open System

type DomainMessage =
     | InvalidEntranceRating 
     | InvalidStudentId
     | SubjectIsFull
     | FailedProgrammingValidationRule1 // When programming subjects are more than 50% full, the student's entrance ranking must be greater than 70 for them to be accepted.
     | FailedLiteratureValidationRule1 // When literature subjects are more than 50% full, people on scholarships will no longer be accepted.
     | FailedDesignValidationRule1 // When design subjects are more than 70% full, only international students will be accepted.
     | InvalidSubjectName
     | InvalidSemestor   
     | EnrolmentExsits  
     | AssignedStudentToSubject
     override this.ToString() = 
            match this with
            | InvalidEntranceRating ->  "Students must have an entrance ranking between 0 and 100)."
            | InvalidStudentId ->  "Student Id must be between 5 and 20 characters "
            | SubjectIsFull ->  "Subject is full"
            | FailedProgrammingValidationRule1 -> "When programming subjects are more than 50% full, the student's entrance ranking must be greater than 70 for them to be accepted."
            | FailedLiteratureValidationRule1 -> "When literature subjects are more than 50% full, people on scholarships will no longer be accepted."
            | FailedDesignValidationRule1 ->  "When design subjects are more than 70% full, only international students will be accepted."
            | InvalidSubjectName -> "Subject name should between 0 and 20 characters"
            | InvalidSemestor -> "Invalid Semestor (0 > Semestor Number < 2) and (2010 < year > 2020)"
            | EnrolmentExsits -> "Student has already enroled in subject"
            | AssignedStudentToSubject -> "Assigned Student to Subject"

module Student = 
        module EntranceRanking = 
            type T = int    
            let create (value : int) : Choice<T, DomainMessage> =
                    if (value >= 0 && value <= 100) then 
                        Choice1Of2(value) 
                    else 
                        Choice2Of2(InvalidEntranceRating)

        module StudentID = 
             type T = string
             let create(id : string) : Choice<T, DomainMessage> =
                    if (id.Length > 4 && id.Length <= 20) then 
                        Choice1Of2(id) 
                    else 
                        Choice2Of2(InvalidStudentId)

             let generate id = Guid.NewGuid().ToString()

        type T = { ID : StudentID.T; 
                   IsInternational : bool; 
                   HasScholarship : bool; 
                   EntraceRanking : EntranceRanking.T }
                   override this.ToString() = 
                           sprintf "SubjectID=%s, IsInternational=%s, HasScholarship=%s, EntraceRanking=%d" 
                                    this.ID (this.IsInternational.ToString()) (this.HasScholarship.ToString()) this.EntraceRanking
        
        let create (id : string) 
                   (isInternational : bool)
                   (hasScholarship : bool)
                   (entranceRanking : int) =
                    match StudentID.create(id) with
                    | Choice1Of2(success) -> match(EntranceRanking.create(entranceRanking)) with
                                             | Choice1Of2(ranking) -> Choice1Of2({ T.ID = success; 
                                                                                     IsInternational = isInternational;
                                                                                     HasScholarship = hasScholarship;
                                                                                     EntraceRanking = ranking })
                                             | Choice2Of2(f2) -> Choice2Of2(f2)
                    | Choice2Of2(failure) -> Choice2Of2(failure)                  
                        
        let generate(r : System.Random) =                
                    let isInter = r.Next(0, 2) |> (function | 1 -> true | _ -> false)
                    let hasScholarship = r.Next(0, 2) |> (function | 1 -> true | _ -> false)
                    let ranking = r.Next(0, 10)             
                    { T.ID = Guid.NewGuid().ToString();
                        IsInternational = isInter;
                        HasScholarship = hasScholarship; 
                        EntraceRanking = ranking }

module Subject =    
        let getPercentFull count (maxSize : int) = (float count) / (float maxSize) * 100.0    
    
        module Semester = 
                 type T = { Number : int; Year : int }             
             
                 let create (number : int) (year : int) =
                        if number > 0 && number < 3 &&
                           year > 2010 && year < 2020 then
                           Choice2Of2(InvalidSemestor)
                        else
                           Choice1Of2({ T.Number=number; Year=year })

        module SubjectName = 
                 type T = string
                 let create(name : string) : Choice<T, DomainMessage> =
                     if (name.Length > 4 && name.Length <= 30) then 
                        Choice1Of2(name)
                     else 
                        Choice2Of2(InvalidSubjectName)
                                                     
        type SubjectInfo = { Name : SubjectName.T; Students : Student.T list; Semester : Semester.T }                                                                                   
  
        let doesStudentExist (student : Student.T) (students : Student.T list) =
                students |> List.exists(fun item -> item = student)            

        module ProgrammingSubject = 
                type T = SubjectInfo 
                let MAX_SUBJECT_SIZE = 20       
        
                let create (name : SubjectName.T) (semester : Semester.T) = { T.Name = name; Semester=semester; Students=[] } 
        
                let assignStudent (student : Student.T) (subject : T) =
                        if subject.Students.Length > 20 then
                            Choice2Of2(SubjectIsFull)
                        else if getPercentFull subject.Students.Length 20 > 50.0 && student.IsInternational = false then                        
                            Choice2Of2(FailedProgrammingValidationRule1)                
                        else if doesStudentExist student subject.Students then
                            Choice2Of2(EnrolmentExsits)
                        else
                            Choice1Of2({ subject with Students = student::subject.Students })
                 
        module LiteratureSubject = 
                type T = SubjectInfo   
                let MAX_SUBJECT_SIZE = 10     
        
                let create (name : string) (semester : Semester.T) = { T.Name = name; Semester=semester; Students=[] } 

                let assignStudent (student : Student.T) (subject : T) =
                        if subject.Students.Length > 10 then
                            Choice2Of2(SubjectIsFull)
                        else if getPercentFull subject.Students.Length MAX_SUBJECT_SIZE > 50.0 && student.HasScholarship = true then                        
                            Choice2Of2(FailedLiteratureValidationRule1)                
                        else if doesStudentExist student subject.Students then
                            Choice2Of2(EnrolmentExsits)
                        else
                            Choice1Of2({ subject with Students = student::subject.Students })

        module DesignSubject = 
                type T = SubjectInfo   
                let MAX_SUBJECT_SIZE = 10
        
                let create (name : string) (semester : Semester.T) = { T.Name = name; Semester=semester; Students=[] }                 
        
                let assignStudent (student : Student.T) (subject : T) =
                        if subject.Students.Length > 10 then
                            Choice2Of2(SubjectIsFull)                    
                        else if getPercentFull subject.Students.Length MAX_SUBJECT_SIZE > 70.0 && student.IsInternational = false then                        
                            Choice2Of2(FailedDesignValidationRule1)
                        else if doesStudentExist student subject.Students then
                            Choice2Of2(EnrolmentExsits)                                                        
                        else
                            Choice1Of2({ subject with Students = student::subject.Students })

        type T = | Programming of ProgrammingSubject.T
                 | Design of LiteratureSubject.T
                 | Literature of DesignSubject.T                 
                 override this.ToString() = 
                    match this with
                    | Programming(p) -> sprintf "Programming, %s, %d, %d" p.Name p.Semester.Number p.Semester.Year
                    | Design(d) -> sprintf "Design, %s, %d, %d" d.Name d.Semester.Number d.Semester.Year
                    | Literature(l) -> sprintf "Literature, %s, %d, %d" l.Name l.Semester.Number l.Semester.Year
                  
        let assignStudent student (subject : T) =                                 
                    let mapChoice1Of2 f (c : Choice<'T, 'A>) =
                            match c with 
                            | Choice1Of2(one) -> Choice1Of2(f(one)) 
                            | Choice2Of2(two) -> Choice2Of2(two)   

                    match subject with
                    | Programming(p) -> ProgrammingSubject.assignStudent student p |> mapChoice1Of2(fun r -> Programming(r))
                    | Design(d) -> DesignSubject.assignStudent student d |> mapChoice1Of2(fun r -> Design(r))
                    | Literature(l) -> LiteratureSubject.assignStudent student l |> mapChoice1Of2(fun r -> Literature(r))
        
module RoundRobinSubjectEnroller =
        let private assignStudent student (subjects : Subject.T list) = 
                let rec aux log acc subs =                                                                                                
                            match subs with
                            | [] -> acc, log                                                         
                            | h::t -> match Subject.assignStudent student h with
                                        | Choice1Of2(success) -> acc @ success::t, (student, h, AssignedStudentToSubject)::log
                                        | Choice2Of2(EnrolmentExsits) -> aux log (h::acc) t     
                                        | Choice2Of2(failure) -> aux ((student, h, failure)::log) (h::acc) t     
                aux [] [] subjects                                           

        let private assignStudentNumberOfSubjects count student subjects =
                let rec aux counter log subs =
                            if counter = 0 then
                                subs, log
                            else
                                let newSubs, logEntries = assignStudent student subs                  
                                aux (counter - 1) (logEntries @ log) newSubs 
                aux count [] subjects

        let assignStudentsToSubjects count students subjects = 
                let rec aux log studs subs =
                            match studs with
                            | [] -> subs, log
                            | h::t -> let newSubs, logEntries = assignStudentNumberOfSubjects count h subs
                                      aux (logEntries @ log) t newSubs                             
                aux [] students subjects

module Log =         
         let display (log : (Student.T * Subject.T * DomainMessage) list) =                      
                for (student, subject, msg) in log do  
                   printfn "Subject: %s\rStudent: %s\rDomainMessage: %s\r\r" (subject.ToString()) (student.ToString()) (msg.ToString())        
                
                let numberOfStudents = log |> List.distinctBy(fun (s, _,_) -> s.ID) |> List.length
                let errors = log |> List.sumBy(fun (_,_,m) -> match(m) with | AssignedStudentToSubject -> 0 | _ -> 1)
                let assignments = log.Length - errors
                
                printfn "Summary"
                printfn "-------"                               
                printfn "Students: %d" numberOfStudents
                printfn "Errors: %d" errors
                printfn "Assignments: %d" assignments 