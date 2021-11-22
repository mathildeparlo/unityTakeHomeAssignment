module jobQueue

open System
open System.Threading

let now = DateTime.Now

/// <summary>                   A class representing a job                                  </summary>
/// <param name="Id">           An integer that uniquely identifies a job                   </param>
/// <param name="Duration">     The amount of seconds the job takes to run (rep. as int)    </param>
/// <param name="HasPriority">  When true, the job takes precedence over non-priority jobs  </param>
/// <param name="Submitted">    A date/time for when the job was submitted                  </param> 
type Job (Id: int, Duration: int, HasPriority: bool, Submitted: DateTime) =
  
  member this.Id = Id
  member this.Duration = Duration
  member this.HasPriority = HasPriority
  member this.Submitted = Submitted


let mutable jobQueue = []

/// <summary>             Schedules a given job by adding it to a list  </summary>
/// <param name="job">    The job to add to the queue                   </param>
/// <returns>             Updates global variable queue, returns a unit </returns>
let scheduleJob (job : Job) : unit =

  jobQueue <- jobQueue @ [job]


/// <summary>           Helper function that dermines how to use List.sortWith  </summary>
/// <param name="job1"> First job for comparison                                </param>
/// <param name="job2"> Second job for comparison                               </param>
/// <returns>           An int indication which job takes precedence            </returns>
let timeCompare (job1: Job) (job2: Job) : int =
  
  if job1.HasPriority && not job2.HasPriority then
    -1
  elif job2.HasPriority && not job1.HasPriority then
    1
  else
    DateTime.Compare (job1.Submitted, job2.Submitted)


/// <summary>             Picks the next appropriate job and runs it    </summary>
/// <param name="queue">  A list of tupples each representing a job     </param>
/// <returns>             A print statement emulating the run of a job  </returns>
let runNextJob (queue : list<Job>) : unit =

  let timeSortedQueue = queue |> List.sortWith timeCompare

  let mutable tempQueue = []

  let rec NextJob (q: list<Job>) : unit = 

    match q with
      | h::t -> if DateTime.Compare (h.Submitted, DateTime.Now.AddSeconds -5.0) <> 1 then
                  jobQueue <- tempQueue @ t
                  printfn "\nRunning job %A and waiting %A seconds\n" h.Id h.Duration
                  Thread.Sleep(h.Duration * 1000)
                else
                  if h.HasPriority then
                    printfn "\nThere was no priority job in the queue older than %A. Waiting..." (DateTime.Now.AddSeconds -5.0)
                  else 
                    printfn "\nThere was job in the queue older than %A. Waiting..." (DateTime.Now.AddSeconds -5.0)
                  Thread.Sleep(5000)
                  NextJob q

      | [] -> printfn "\nThere are no scheduled jobs"

  NextJob timeSortedQueue


/// ----main----

/// Creating jobs
let job1 = Job(1, 3, true, DateTime.Now)
let job2 = Job(2, 2, false, DateTime.Now)
let job3 = Job(3, 5, false, DateTime.Now)
let job4 = Job(4, 4, false, new DateTime(2021, 11, 22, 10, 10, 10))
let job5 = Job(5, 1, true, DateTime.Now)

// Scheduling jobs for jobQueue
scheduleJob job1
scheduleJob job2
scheduleJob job3
scheduleJob job4
scheduleJob job5

// Running jobQueue
for i in 0..jobQueue.Length - 1 do
  runNextJob jobQueue