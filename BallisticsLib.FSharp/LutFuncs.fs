namespace BallisticsLib.FSharp

open LUTS

/// Functions that act on the LUTs
module LUTFuncs =
    /// <summary>Interpolate a lut to get values in between</summary>
    /// <param name="lut">The LUT to interpolate</param>
    /// <param name="target">The target "key"</param>
    /// <returns>Value @ "key" or None if it can't do it</returns>
    let interpolate (lut: Map<float, float>) (target : float) = 
        let keys = Map.toList lut |> List.map fst |> List.sort

        match List.tryFindBack (fun k -> k <= target) keys,
            List.tryFind (fun k -> k >= target) keys with
        | Some x0, Some x1 when x0 <> x1 ->
            let y0 = lut.[x0]
            let y1 = lut.[x1]
            let y = y0 + (target - x0) * (y1 - y0) / (x1 - x0)
            Some y
        | Some x0, _ -> Some lut.[x0]
        | _ -> None
    
    /// Flips a map around - Mach is on the left, CD on the right
    let flipMap (m: Map<float, float>) : Map<float, float> =
        m |> Map.toList |> List.map (fun (k, v) -> (v, k)) |> Map.ofList

    
    /// <summary>Gets the CD for a given Mach</summary>
    /// <param name="mach">The Mach to use to look up the CD</param>
    /// <returns>The CD value - either real or interpolated</returns>
    let cd mach =
        interpolate (flipMap g7lut) mach