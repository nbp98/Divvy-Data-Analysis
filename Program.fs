//
// F# program to analyze Divvy daily ride data.
//
// << NEEL PATEL >>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #04
//

#light
module project04


//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [176;74;1252;21;595;1986;1]; ... ]

// The values are station id (from), station id (to), bike
// id, starting hour (0..23), trip duration (secs), birth
// year (0=>not specified), and gender (0=>not specified, 
// 1=>identifies as male, 2=>identifies as female).
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides
 
// printstarts 
// will print n amount of stars 
//
let rec printstars n = 
    match n with
    | 0 -> ()
    | 1 -> printf "*"
    | _ -> printf "*" 
           printstars (n-1) 

// curYear
// Get the current year: 2019
//
let curYear = System.DateTime.Now.Year

// findPercent 
// Genric function that takes two ints and makes them type float and does math to find their percentage
//  
let findPercent x y = ((float)x/ (float)y)*100.00

// acessElem
// Access the nth item in a list
// 
let acessElem L (n) =  L |> List.map (List.item (n)) 

//genderCount
// Recursive functions takes in a value v and adds 1 each occurance of v 
// where v can equal 1 or 2  male/female
let rec genderCount (v, L)  =
    match L with 
    | []                   ->  0
    | hd::tl when v = hd   ->  1 + genderCount(v, tl)
    | hd::tl               -> genderCount (v, tl) 

// noZero and subYear
// noZero will filter out all the element in the birthyear that were 0 to avoid including not specified birthyears
//subYear will subtract the curYear (2019) with a fun n which will be called later 
let noZero A = List.filter (fun a -> a > 0) A
let subYear = fun n -> if (n > 0) then 
                          (curYear - n)
                       else 
                          0       


// getAge
// pattern match so getAge  will call function f which will be subYear in this case
let rec getAge f L =
    match L with 
    | [] -> []
    | hd::tl  ->  f hd :: getAge f tl  

//findAVG
// will return 0.0 if num and dem are 0 else find the avg
let findAVG x y = if (x = 0 && y = 0) then
                    0.0
                  else    
                    ((float)x/ (float)y)   
                  


//filterDuration 
// Will filter the list after making comparisons in this case we will input parameters when calling in main
let fliterDuration x y L  = 
    List.filter (fun n -> n > x && n <= y ) L

//startTimes
// pattern match to count every instance of n in the list. In this case n will be the start time (0...23)

let rec startTimes n L =
    match L with 
    | []                      ->  0
    | hd::tl when hd = n      ->  1 + startTimes n tl 
    | hd::tl                  -> startTimes n tl

//startPrint
//  will print out start time info so it looks cleaner in main and dont have to use printfn 23 times. Also includes stars
let startPrint n L =
    //printfn"%A: %A %A" n (printstars ((startTimes n L)/10)) (startTimes n L)  
    printf" %A: " n
    printstars ((startTimes n L)/10)
    printf "%A" (startTimes n L)
    printfn ""


  //                                                
  //
  //
  //           **********************************
  //           **********************************
  //                           MAIN
  //           **********************************
  //           **********************************
  //
  //
  //
[<EntryPoint>]

let main argv =
    //
    // input file name, then input divvy ride data and build
    // a list of lists:
    // 
    printf "filename> "
    let filename = System.Console.ReadLine()
    let contents = System.IO.File.ReadLines(filename)
    let ridedata = ParseInput contents
    //printfn "%A" ridedata

    ////////////////////////////////////
    // ******* TOTAL # RIDERS ******* //
    ////////////////////////////////////
    let N = List.length ridedata
    printfn ""
    printfn "# of riders: %A" N
    printfn ""

    ///////////////////////////
    //******* GENDER ******* //
    ///////////////////////////
    let genList = acessElem ridedata (6) 
    let numMale = genderCount (1, genList)
    let numFemale = genderCount (2, genList)
    let P1 = findPercent numMale N
    let P2 = findPercent numFemale N
    printfn "%% of riders identifying as male: %A (%A%%)" numMale P1 
    printfn "%% of riders identifying as female: %A (%A%%)" numFemale P2
    printfn ""

    /////////////////////////////////
    // ******* Average Age ******* //
    /////////////////////////////////


    let ageList = acessElem ridedata (5) 




    let AA = getAge subYear ageList
    let A = noZero AA
    let AAlen = List.length A
    let AAsum = List.sum AA
    //let avg = (float(AAsum)/float(AAlen))
    //let avg = List.averageBy (fun a -> float a) AA
    let avg = findAVG AAsum AAlen
    printfn "Average age: %A" avg
    printfn ""     


    ////////////////////////////////////
    // ******* Ride Durations ******* //
    ////////////////////////////////////
    let durList = acessElem ridedata (4)

    let L1 = fliterDuration 0 1800 durList
    let L2 = fliterDuration 1800 3600 durList
    let L3 = fliterDuration 3600 7200 durList
    let L4 = fliterDuration 7200 10000000  durList

    let N1 = List.length L1
    let N2 = List.length L2
    let N3 = List.length L3
    let N4 = List.length L4

    let P1 = findPercent N1 N
    let P2 = findPercent N2 N
    let P3 = findPercent N3 N
    let P4 = findPercent N4 N
   
    printfn "** Ride Durations: "
    printfn " 0..30 mins: %A (%A%%)" N1 P1
    printfn " 30..60 mins: %A (%A%%)" N2 P2
    printfn " 60..120 mins: %A (%A%%)" N3 P3
    printfn " > 2 hours: %A (%A%%)" N4 P4

    
    /////////////////////////////////////////
    //******* START TIME HISTOGRAM ******* //
    /////////////////////////////////////////

    let startList = acessElem ridedata (3)
    printfn ""
    printfn "** Ride Start Time Histogram: " 

    startPrint 0 startList
    startPrint 1 startList
    startPrint 2 startList
    startPrint 3 startList
    startPrint 4 startList
    startPrint 5 startList
    startPrint 6 startList
    startPrint 7 startList
    startPrint 8 startList
    startPrint 9 startList
    startPrint 10 startList
    startPrint 11 startList
    startPrint 12 startList
    startPrint 13 startList
    startPrint 14 startList
    startPrint 15 startList
    startPrint 16 startList
    startPrint 17 startList
    startPrint 18 startList
    startPrint 19 startList
    startPrint 20 startList
    startPrint 21 startList
    startPrint 22 startList
    startPrint 23 startList




    0



      


     




   



   





    

    