namespace BallisticsLib.FSharp

// =((41.68/_V0)/((1/(1+C4))-(1/(_FC-(0.75+0.00006*C4)*_N1*C4))))^2
// = tof / ()

/// Derived from Pejsa's excel sheet: http://www.jacksonrifles.com/files/pejsa%20ballistics.xls
module Pejsa =
    /// Time of Flight
    let tof (v0 : float) step_f = step_f / v0

    /// Mayowski Constant 
    let mayowski = 246.0

    /// Haven't determined what this is
    let f bc v0 =
        bc * mayowski * v0 ** 0.45
    
    /// euler
    let e = 2.71828

    /// e^pow
    let epow pow =
        e ** pow

    /// Haven't determined what this is
    let fc bc v0 temp alt pr pr0 =
        let f_1 = f bc v0
        f_1 * (460.0 + temp) / (519.0- alt / 280.0) * epow (alt / 31654.0) * (2.0 - pr / pr0)
    
    /// Haven't determined what this is
    let retard_coeff = 0.5
    
    /// <summary>The solution for Pejsa</summary>
    /// <param name="v0">Initial Velocity</param>
    /// <param name="r">Target distance</param>
    /// <param name="fc">The FC func</param>
    /// <returns></returns>
    let sol (v0: float) (r: float) (fc: float)  =
        let top = 41.68 / v0
        let innerLeft = 1.0 / (1.0 + r)
        let dragTerm = (0.75 + 0.00006 * r) * 0.5 * r
        let innerRight = 1.0 / (fc - dragTerm)
        let delta = innerLeft - innerRight
        (top / delta) ** 2.0
    
    /// <summary>The solution with a zero</summary>
    /// <param name="v0">Initial Velocity</param>
    /// <param name="r">Target Distance</param>
    /// <param name="fc">The fc func</param>
    /// <param name="zero">Zero Distance</param>
    /// <returns></returns>
    let solZero v0 r fc zero =
        let zeroS = sol v0 zero fc
        let impact = sol v0 r fc

        -impact + zeroS * r/zero 




    
    





