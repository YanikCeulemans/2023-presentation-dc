// define some measures
[<Measure>]
type cm

[<Measure>]
type inches

[<Measure>]
type feet =
   // add a conversion function
   static member toInches(feet : float<feet>) : float<inches> =
      feet * 12.0<inches/feet>

// define some values
let meter = 100.0<cm>
let yard = 3.0<feet>

//convert to different measure
let yardInInches = feet.toInches(yard)

// can't mix and match!
// yard + meter