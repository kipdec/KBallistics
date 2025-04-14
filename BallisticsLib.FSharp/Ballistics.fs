namespace BallisticsLib.FSharp

open System
open LUTFuncs

module Ballistics =
    open System.IO
    /// The gravity constant (fps) 
    let g = 32.185
    /// Gives the mach for a given velocity v
    let mach v = v / 1116.4 

    /// Gets the drag for a given velocity
    let dragFunc v = 
        let m = mach v
        let r = LUTFuncs.cd m
        match r with
        | Some x -> x
        | None -> 0.0 // Should probably return none and handle the errror. Our table is comprehensive, so shouldn't happen
    
    /// (De) Acceleration of Drag - using G7 formula I found
    let adrag v bc =
        g / bc * dragFunc v

    /// (De) Acceleration of Drag - using a different G7 formula I found
    let adrag2 v bc gr cal=
        let bc1 = (gr / 7000.0) / (cal ** 2.0 * bc)
        g / bc * dragFunc v
    
    /// Acceleration along the x-axis
    let ax v bc = - adrag v bc
    /// Acceleration along the x-axis
    let ax2 v bc gr cal = - adrag2 v bc gr cal
    /// Acceleration along the y-axis
    let ay v bc vy = (adrag v bc) * (vy / v)
    /// Acceleration along the y-axis
    let ay2 v bc vy gr cal = (adrag2 v bc gr cal) * (vy / v)
    /// Next velocity x from acceleration & delta x
    let vx_next v bc step = v + (ax v bc) * (step / v) 
    /// Next velocity x from acceleration & delta x
    let vx_next2 v bc step gr cal = v + (ax2 v bc gr cal )
    /// Next velocity y from acceleration & delta x
    let vy_next v bc vy (step : float) = vy + (ay v bc vy - g) * (step / v)
    /// Next velocity y from acceleration & delta x
    let vy_next2 v bc vy (step : float) gr cal =
        vy + (ay2 v bc vy gr cal - g) * (step / v)

    /// Time taken for a step at a given velocity
    let dti step (v : float) = step / v

    /// <summary>Recursive function that calculates the drop for a bullet through step-wise integration</summary>
    /// <param name="x">Position X (ft)</param>
    /// <param name="y">Position Y (ft)</param>
    /// <param name="vx">Velocity X (fps)</param>
    /// <param name="vy">Velocity Y (fps)</param>
    /// <param name="bc">G7 Ballistic Coefficient</param>
    /// <param name="s">Step (ft)</param>
    /// <param name="target">Target Distance</param>
    /// <returns>Drop (ft)</returns>
    let rec step x y vx vy bc s target=
        let dt = dti s vx
        let vx1 = vx_next vx bc s
        let vy1 = vy_next vx bc vy s
        let x1 = x + vx1 * dt
        let y1 = y + vy1 * dt

        //printfn $"dt: {dt} vx1: {vx1} vy1: {vy1} x1: {x1} y1: {y1}"
        //printfn $"{x1 / 3.0},{vx1},{y1 * 12.0}"

        if x1 > target then
            y1
        else
            step x1 y1 vx1 vy1 bc s target

    /// <summary>Recursive function that calculates the drop for a bullet through step-wise integration</summary>
    /// <returns></returns>
    /// <param name="x">Position X (ft)</param>
    /// <param name="y">Position Y (ft)</param>
    /// <param name="vx">Velocity X (fps)</param>
    /// <param name="vy">Velocity Y (fps)</param>
    /// <param name="bc">G7 Ballistic Coefficient</param>
    /// <param name="s">Step (ft)</param>
    /// <param name="target">Target Distance</param>
    /// <param name="gr">Grains</param>
    /// <param name="cal">Caliber (in)</param>
    /// <returns>Drop (ft)</returns>
    let rec step2 x y vi vy bc s target gr cal=
        let dt = dti s vi
        let vx1 = vx_next2 vi bc s gr cal
        let vy1 = vy_next2 vi bc vy s gr cal
        let x1 = x + vx1 * dt
        let y1 = y + vy1 * dt

        //printfn $"{x1 / 3.0},{vx1},{y1 * 12.0}"

        if x1 > target then
            y1
        else
            step x1 y1 vx1 vy1 bc s target
    
    let step_adv x vx vy bc s target zero sightHeight =
        let z = step x -sightHeight vx vy bc s zero
        let impact = step x -sightHeight vx vy bc s target

        impact - z

    let step_adv2 x vx vy bc s target zero sightHeight gr cal=
        let z = step2 x -sightHeight vx vy bc s zero gr cal
        let impact = step2 x -sightHeight vx vy bc s target gr cal

        impact - z
    
    let mils din ryd =  
        din * 27.778 / ryd
    
    let moa din ryd =
        din / (1.047 * (ryd / 100.0))

    type BallisticResult = {
        Range: float
        Drop: float
        Mils: float
        MOA: float
    }  
    let stepCalc x vx vy bc s target zero sightHeight =
        let drop = step_adv x vx vy bc s target zero sightHeight
        let din = drop * 12.0
        let ryd = target / 3.0
        {
            Range = ryd;
            Drop = din;
            Mils = - mils din ryd;
            MOA = -moa din ryd;
        }
        
    let stepMils2 x vx vy bc s target zero sightHeight gr cal=
        let drop = step_adv2 x vx vy bc s target zero sightHeight gr cal
        - mils (drop * 12.0) (target / 3.0)
    
    /// Calculates the rsquared vals of two lists
    let rsquare predictedList actualList = 
        let actual = actualList |> List.map snd
        let predicted = predictedList |> List.map snd
        if List.length actual <> List.length predicted then
            failwith "Must be same length"
        
        let yBar = List.average actual
        let bot = actual |> List.sumBy (fun y -> (y - yBar) ** 2.0)
        let top = List.zip actual predicted |> List.sumBy (fun (y, yHat) -> ( y - yHat) ** 2.0)

        1.0 - (top / bot)
    
    /// Generates a Distance (yd), Drop (in) LUT using `step`
    let genLUT l =
        let distances  = l |> List.map fst
        let newList = distances |> List.map (fun d -> d, 12.0 * (step_adv 0 2600 0 1 10 (d * 3.0) 100 0))
        newList

    /// Generates a Distance (yd), Drop (in) LUT using `step2`
    let genLUT2 l =
        let distances  = l |> List.map fst
        let newList = distances |> List.map (fun d -> d, 12.0 * step_adv2 0 2600 0 1 10 (d * 3.0) 100 0 140 0.264)
        newList
    
    /// Gets the r^2 value for our prediction using `step` and the known Hornady values
    let rsquaredStepHornady = 
        let actual = Map.toList LUTS.hg71
        let predicted = genLUT actual
        rsquare predicted actual

    /// Gets the r^2 value for our prediction using `step` and the known Hornady values
    let rsquaredStep2Hornady = 
        let actual = Map.toList LUTS.hg71
        let predicted = genLUT2 actual
        rsquare predicted actual
    
    let calcToLines v bc rYd sYd zeroYd sightHeight =
        let rFt = rYd * 3.0
        let zeroFt = zeroYd * 3.0
        let stepFt = sYd * 3.0
        let steps = [stepFt .. stepFt .. rFt]

        let lines = 
            steps |> List.map (fun s -> stepCalc 0 v 0 bc 10 s zeroFt sightHeight) 
        
        lines
    
    let simpleCalc v bc zeroYd rYd =
        let rFt = rYd * 3.0
        let zeroFt = zeroYd * 3.0

        stepCalc 0 v 0 bc 10 rFt zeroFt 0
    
    let f (s : float) =
        s.ToString("F2") 
    
    let writeCSV path (lines : list<float * float * float * float>)  (header: bool option) =
        let useHeader = defaultArg header true
        let h = "Range(yd),Drop(in),Mils,MOA"

        let strLines =
            lines |> List.map (fun (r, drop, mils, moa) -> $"{f r},{f drop},{f mils},{f moa}")

        let allLines =
            if useHeader then
                h::strLines
            else 
                strLines
        
        File.WriteAllLines(path, allLines)
        
        

            

        





    

    



    
    
    
