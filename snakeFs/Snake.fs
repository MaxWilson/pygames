module Snake

open Fable.Core
open Fable.Python
open Fable.Core.PyInterop

let turtle: obj = importAll "turtle"
let drawSquare(x:int, y:int, size:int, color: string): unit = import "square" "freegames"
type Vector = { mutable x: int; mutable y: int }
  with
  override this.ToString() = $"({this.x}, {this.y})"
  static member (+) (this, rhs: Vector) =
    { x = this.x + rhs.x; y = this.y + rhs.y }
let vector (x,y) = { x = x; y = y }

let rand = System.Random()
let randRange (a, b) = a + rand.Next(1 + b - a)

let mutable food = vector(0,0)
let mutable snake = [vector(10, 0)]
let mutable aim = vector(0, -10)

/// Change snake direction
let change(x,y) =
  aim <- vector(x,y)

/// Return True if head inside boundaries
let inside (head: Vector) =
  -200 < head.x && head.x < 190 && -200 < head.y && head.y < 190

/// Move snake forward one segment
let rec move() =
  let head = snake |> List.last
  let newhead = head + aim
  if not (inside newhead) || (List.contains newhead snake) then
    drawSquare(head.x, head.y, 9, "red")
    turtle?update()
  else
    snake <- snake@[newhead]
    if newhead = food then
      let x = randRange(-15, 15) * 10
      let y = randRange(-15, 15) * 10
      food <- vector(x, y)
    else
      snake <- snake[0..snake.Length-1]
    turtle?clear()
    for bodySegment in snake do
      drawSquare(bodySegment.x, bodySegment.y, 9, "black")
    drawSquare(food.x, food.y, 9, "green")
    turtle?update()
    turtle?ontimer(move, 100)

turtle?setup(420, 420, 370, 0)
turtle?hideturtle()
turtle?tracer(false)
turtle?listen()
turtle?onkey(fun () -> change(10, 0)) "Right"
turtle?onkey(fun () -> change(-10, 0)) "Left"
turtle?onkey(fun () -> change(0, 10)) "Up"
turtle?onkey(fun () -> change(0, -10)) "Down"
move()
turtle?``done``()
