import javax.imageio._
import javax.swing._
import java.io._
import java.awt.image._

object FImage { 

  case class Pixel( a : Int,r : Int, g : Int, b : Int) 
  case class FImage(w : Int, h : Int, data : Seq[Seq[Pixel]])


  def getBuff(s : String) : BufferedImage = 
    ImageIO.read(new File(s))

  def getData( s : String) : FImage = {
    val img = getBuff(s)
    val h = img.getHeight()
    val w = img.getWidth()
    val argb = img.getRGB(0,0,w,h, null, 0,w) 
    FImage(w, h, 
      (0 until h) map (i => argb.slice(i*w, (i+1)*w) map intToPix : Seq[Pixel]) 
      )
  }

  def asImage(i : FImage) : BufferedImage = {
    var buff = new BufferedImage(i.w, i.h,BufferedImage.TYPE_INT_ARGB)  
    val data = i.data.flatMap(_ map pixToInt).toArray
    buff.setRGB(0,0,i.w-1, i.h-1, data, 0, i.w)
    buff
  }

  def intToPix(rgb : Int) : Pixel = Pixel(
      (rgb >> 24) & 0xFF,
      (rgb >> 16) & 0xFF,
      (rgb >>  8) & 0xFF,
      (rgb      ) & 0xFF)
  
  def pixToInt(v : Pixel) : Int = ( 
    ((v.a &0x0ff)<<24)|
    ((v.r &0x0ff)<<16)|
    ((v.g &0x0ff)<<8 )|
    (v.b  &0x0ff)  
  )

  def show(i : FImage) =JOptionPane.showMessageDialog(null, new JLabel(new ImageIcon(asImage(i))))

  def save(i : FImage)(s : String) = 
    ImageIO.write(asImage(i), "jpg", new File(s))
}

