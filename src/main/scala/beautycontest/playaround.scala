package beautycontest
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import scala.util.Random
import scalatags.JsDom.all._

import scala.scalajs.js

object playaround {
  def main(canvas: html.Canvas): Unit = {
    val renderer = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    renderer.fillStyle = "#55A0ED"
    renderer.fillRect(0, 0, canvas.width, canvas.height)

    renderer.fillStyle = "black"
    var down = false
    canvas.onmousedown = (_: dom.MouseEvent) => down = true
    canvas.onmouseup = (_: dom.MouseEvent) => down = false

    canvas.onmousemove = {
      (e: dom.MouseEvent) =>
        val rect = canvas.getBoundingClientRect()
        if (down) renderer.fillRect(e.clientX - rect.left, e.clientY - rect.top, 10, 10)
    }

  }
}

object HelloWorld0 {
  def main(target: html.Div, targetCanvas: html.Canvas) = {

    val box = input(
      `type`:="text",
      placeholder:="Type here!"
    ).render

    val output = span.render

    box.onkeyup = (e: dom.Event) => {
      output.textContent =
        box.value.toUpperCase

      println(e.asInstanceOf[dom.KeyboardEvent].key)
      if (e.asInstanceOf[dom.KeyboardEvent].key == "Enter") {
        println (output.textContent)

        if (output.textContent == "HELLO") {
          val (animalA, animalB) = ("fox", "dog")
          target.appendChild(
            div(
              h1("Hello World!"),
              p(
                "The quick brown ", b(animalA),
                " jumps over the lazy ",
                i(animalB), "."
              )
            ).render
          )
        }

        if (output.textContent == "DRAW") {
          playaround.main(targetCanvas);
        }

        if (output.textContent == "CLOCK") {
          val renderer = targetCanvas.getContext("2d")
            .asInstanceOf[dom.CanvasRenderingContext2D]

          targetCanvas.width = targetCanvas.parentElement.clientWidth
          targetCanvas.height = targetCanvas.parentElement.clientHeight

          val gradient = renderer.createLinearGradient(
            targetCanvas.width / 2 - 100, 0, targetCanvas.width/ 2 + 100, 0
          )
          gradient.addColorStop(0,"red")
          gradient.addColorStop(0.5,"green")
          gradient.addColorStop(1,"blue")
          renderer.fillStyle = gradient
          //renderer.fillStyle = "black"

          renderer.textAlign = "center"
          renderer.textBaseline = "middle"

          def render() = {
            val date = new js.Date()
            renderer.clearRect(
              0, 0, targetCanvas.width, targetCanvas.height
            )

            renderer.font = "75px sans-serif"
            renderer.fillText(
              Seq(
                date.getHours(),
                date.getMinutes(),
                date.getSeconds()
              ).mkString(":"),
              targetCanvas.width / 2,
              targetCanvas.height / 2
            )
          }
          dom.window.setInterval(render _, 1000)

        }

      }
    }

    target.appendChild(
      div(
        h1("Capital Box!"),
        p(
          "Type here and " +
            "have it capitalized!"
        ),
        div(box),
        div(output)
      ).render
    )
  }
}
