package snake

import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle

import scala.concurrent.Future
import scala.util.Random

object SnakeFx extends JFXApp3 {

  object Word {
    val width = 800
    val height = 800
  }

  val initialSnake: List[(Double, Double)] = List(
    (250, 200),
    (225, 200),
    (2200, 200)
  )

  import scala.concurrent.ExecutionContext.Implicits.global
  def gameLoop(update: () => Unit): Unit =
    Future {
      update()
      Thread.sleep(1000/25 * 2)
    }.flatMap(_ => Future(gameLoop(update)))

  case class State(snake: List[(Double, Double)],
                   food: (Double, Double)) {
    def newState(dir: Int): State = {
      val (x, y) = snake.head
      val (newX, newY) = dir match {
        case 1 => (x, y - 25)
        case 2 => (x, y + 25)
        case 3 => (x - 25, y)
        case 4 => (x + 25, y)
        case _ => (x, y)
      }

      val newSnake: List[(Double, Double)] =
        if(newX < 0 || newX >= Word.width || newY < 0 || newY >= Word.height || snake.tail.contains(newX, newY))
          initialSnake
        else if (food == (newX, newY))
          food :: snake
        else
          (newX, newY) :: snake.init

      val newFood: (Double, Double) =
        if(food == (newX, newY))
          randomFood()
        else
          food

      State(newSnake, newFood)
    }

    def rectangles: List[Rectangle] = rect(food._1, food._2, Red) :: snake.map {
      case (x, y) => rect(x,y,Green)
    }
  }
  def randomFood(): (Double, Double) =
    (Random.nextInt(24) * 25, Random.nextInt(24) * 25)

  def rect(xr: Double, yr: Double, color: Color) = new Rectangle {
    x = xr
    y = yr
    width = 25
    height = 25
    fill = color
  }

  override def start(): Unit = {
    val state = ObjectProperty(State(initialSnake, randomFood()))
    val frame = IntegerProperty(0)
    val direction = IntegerProperty(4)

    frame.onChange {
      state.update(state.value.newState(direction.value))
    }
    stage = new JFXApp3.PrimaryStage {
      width = Word.width
      height = Word.height
      scene = new Scene {
        fill = White
        content = state.value.rectangles
        onKeyPressed = key => key.getText match {
          case "w" => direction.value = 1
          case "s" => direction.value = 4
          case "a" => direction.value = 3
          case "z" => direction.value = 2
          case _ => direction.value = direction.value
        }

        frame.onChange {
          Platform.runLater {
            content = state.value.rectangles
          }
        }
      }
    }
    gameLoop(() => frame.update(frame.value + 1))
  }
}
