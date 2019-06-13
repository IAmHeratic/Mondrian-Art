//
// F# program to generate random Mondrian Art.
//
// Jose E. Rodriguez
// U. of Illinois, Chicago
// CS 341, Spring 2018
// Project #05
//

#light

//
// randomInt LB UB
//
// generates random integer in range LB..UB, inclusive.
//
// NOTE: if you want repeatable random numbers for testing,
// uncomment "let seed = 0".  If you want random images 
// every time, uncomment the other "let seed = ..." line.
//
let seed = System.DateTime.Now.Millisecond
//let seed = 0
let ranInt = new System.Random(seed)
let randomInt LB UB =
  if LB <= UB then
     ranInt.Next(LB, UB+1)
  else
     LB


// returns the red value to be used to color a rectangle
let redValue r =

    if (r < 9 ) then
       // red
       255
    else if (r < 17) then
       // sky blue
       135
    else if (r < 25) then
       // yellow
       255
    else
       // white
       255


// returns the green value to be used to color a rectangle
let greenValue r =

    if (r < 9 ) then
       // red
       0
    else if (r < 17) then
       // sky blue
       206
    else if (r < 25) then
       // yellow
       255
    else
       // white
       255


// returns the blue value to be used to color a rectangle
let blueValue r =

    if (r < 9 ) then
       // red
       0
    else if (r < 17) then
       // sky blue
       250
    else if (r < 25) then
       // yellow
       0
    else
       // white
       255


// Generates a randomly colored rectangle in HTML SVG format
let randomRect2 x1 y1 x2 y2 = 
  
  let r = randomInt 1 100
  let red   = redValue r
  let green = greenValue r
  let blue  = blueValue r

  let html = 
       "<rect x=" + (string x1) +
       " y=" + (string y1) + 
       " width=" + (string (x2-x1+1)) + 
       " height=" + (string (y2-y1+1)) + 
       " stroke=\"black\" stroke-width=\|3" +
       " fill=\"rgb(" + 
       (string red) + "," +
       (string green) + "," +
       (string blue) + ")\" />\n"
  html



// Returns true if the width of the region is greater than half the
// width of the original canvas
let regionIsWider x1 x2 canvasWidth =
    if (x2-x1) > (canvasWidth/2) then true else false


// Returns true if the height of the region is greater than half the
// height of the original canvas
let regionIsTaller y1 y2 canvasHeight =
    if (y2-y1) > (canvasHeight/2)  then true else false


// Checks if the regions width is greater than half the canvas
// original width and if the regions height is greater than half
// the canvas original height
let regionIsTallerAndWider x1 y1 x2 y2 canvasW canvasH =
    if (regionIsTaller y1 y2 canvasH) && (regionIsWider x1 x2 canvasW) then true else false


//
// _mondrian x1 y1 x2 y2 canvasWidth canvasHeight
//
// Recursive helper function that randomly generates an image
// for the area denoted by the rectange (x1,y1) and (x2,y2),
// where (x1,y1) is the upper-left corner and (x2,y2) is the 
// lower-right corner.  The image is in HTML SVG format.
//
let rec _mondrian x1 y1 x2 y2 canvasWidth canvasHeight = 
   
   if (regionIsTallerAndWider x1 y1 x2 y2 canvasWidth canvasHeight) then

     // split into 4 regions (vert/horiz splits, chosen randomly)
     let width  = (float (x2-x1))
     let height = (float (y2-y1))
     
     let verticalLB = (int ((float x1) + (width * 0.33)))
     let verticalUB = (int ((float x1) + (width * 0.33)))     
     let horizontalLB = (int ((float y1) + (height * 0.33)))
     let horizontalUB = (int ((float y1) + (height * 0.67)))

     let verticalSplitPt = randomInt verticalLB verticalUB
     let horizontalSplitPt = randomInt horizontalLB horizontalUB

     let topLeft = (_mondrian x1 y1 (verticalSplitPt) (horizontalSplitPt)  canvasWidth canvasHeight)
     let topRight = (_mondrian (verticalSplitPt) y1 x2 (horizontalSplitPt) canvasWidth canvasHeight)
     let bottomLeft = (_mondrian x1 (horizontalSplitPt) (verticalSplitPt) y2 canvasWidth canvasHeight)
     let bottomRight = (_mondrian (verticalSplitPt) (horizontalSplitPt) x2 y2 canvasWidth canvasHeight)
     
     topLeft + topRight + bottomLeft + bottomRight

   else if (regionIsWider x1 x2 canvasWidth) then
     
     // split into 2 regions with a vertical split chosen randomly
     let width  = (float (x2-x1))
     
     let verticalLB = (int ((float x1) + (width * 0.33)))
     let verticalUB = (int ((float x1) + (width * 0.33)))

     let verticalSplitPt = randomInt verticalLB verticalUB
     
     let leftRect  = (_mondrian x1 y1 (verticalSplitPt) y2 canvasWidth canvasHeight)
     let rightRect = (_mondrian (verticalSplitPt) y1 x2 y2 canvasWidth canvasHeight)

     leftRect + rightRect

   else if (regionIsTaller y1 y2 canvasHeight) then

     // split into 2 regions with a horizontal split chosen randomly
     let height = (float (y2-y1))
         
     let horizontalLB = (int ((float y1) + (height * 0.33)))
     let horizontalUB = (int ((float y1) + (height * 0.67)))
     
     let horizontalSplitPt = randomInt horizontalLB horizontalUB
     
     let bottomRect = (_mondrian x1 (horizontalSplitPt) x2 y2 canvasWidth canvasHeight)
     let topRect    = (_mondrian x1 y1 x2 (horizontalSplitPt) canvasWidth canvasHeight)

     topRect + bottomRect

   else

     // Generate random height/width
     let width  = (x2 - x1)
     let height = (y2 - y1)
     let randWidth = randomInt 120 (int ((float width) * 1.5))
     let randHeight = randomInt 120 (int ((float height) * 1.5))

     if ((randWidth < width) && (randHeight < height)) then
     
        // split into 4 regions (vert/horiz splits, chosen randomly)
        let verticalLB = (int ((float x1) + ((float width) * 0.33)))
        let verticalUB = (int ((float x1) + ((float width) * 0.33)))     
        let horizontalLB = (int ((float y1) + ((float height) * 0.33)))
        let horizontalUB = (int ((float y1) + ((float height) * 0.67)))

        let verticalSplitPt = randomInt verticalLB verticalUB
        let horizontalSplitPt = randomInt horizontalLB horizontalUB


        let topLeft = (_mondrian x1 y1 (verticalSplitPt) (horizontalSplitPt)  canvasWidth canvasHeight)
        let topRight = (_mondrian (verticalSplitPt) y1 x2 (horizontalSplitPt) canvasWidth canvasHeight)
        let bottomLeft = (_mondrian x1 (horizontalSplitPt) (verticalSplitPt) y2 canvasWidth canvasHeight)
        let bottomRight = (_mondrian (verticalSplitPt) (horizontalSplitPt) x2 y2 canvasWidth canvasHeight)

        topLeft + topRight + bottomLeft + bottomRight

     else if (randWidth < width) then
     
        // split into 2 regions, vertical line chosen randomly
        let verticalLB = (int ((float x1) + ((float width) * 0.33)))
        let verticalUB = (int ((float x1) + ((float width) * 0.33)))     
        
        let verticalSplitPt = randomInt verticalLB verticalUB
     
        let leftRect = (_mondrian x1 y1 (verticalSplitPt) y2 canvasWidth canvasHeight)
        let rightRect = (_mondrian (verticalSplitPt) y1 x2 y2 canvasWidth canvasHeight)

        leftRect + rightRect

     else if (randHeight < height) then
     
        // split into 2 regions, horizontal line chosen randomly
        let horizontalLB = (int ((float y1) + ((float height) * 0.33)))
        let horizontalUB = (int ((float y1) + ((float height) * 0.67)))

        let horizontalSplitPt = randomInt horizontalLB horizontalUB

        let bottomRect = (_mondrian x1 (horizontalSplitPt) x2 y2 canvasWidth canvasHeight)
        let topRect = (_mondrian x1 y1 x2 (horizontalSplitPt) canvasWidth canvasHeight)

        bottomRect + topRect

     else 

        // fill the region with a random color!
        let html = randomRect2 x1 y1 x2 y2
        html


//
// mondrian canvasWidth canvasHeight
//
// Randomly generates an image in the spirit of Piet Mondrian.
// Returns an HTML document containing an SVG image of the given
// canvas width and height.  
//
// SVG: https://www.w3schools.com/html/html5_svg.asp
//
let mondrian canvasWidth canvasHeight = 
  let prefix = "<html>\n<head></head>\n<body>\n" +
               "<svg width=\"" + (string canvasWidth) + 
               "\" height=\"" + (string canvasHeight) + "\">\n"
  //
  let image = _mondrian 0 0 (canvasWidth-1) (canvasHeight-1) canvasWidth canvasHeight
  //
  let suffix = "</svg>\n</body>\n</html>\n"
  let html = prefix + image + suffix
  html


//
// main:
//
[<EntryPoint>]
let main argv =
  printfn "** Starting **"
  //
  let width = 1024
  let height = 768
  let filename = "..\\..\\..\\CS_341\MondrianArt\MondrianArt\mondrian.html"  // same folder as F# code:
  //
  printfn "** Generating image... "
  let html = mondrian width height
  System.IO.File.WriteAllText(filename, html) 
  //
  printfn "** Done **"
  0
