import scala.swing._

object HW extends SimpleSwingApplication { 
  def top = new MainFrame {
    title = "Hello, World!"
    contents = new Button {
      text = "Click Me!"
    }
  }
}
