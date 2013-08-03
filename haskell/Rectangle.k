class rectangle as this
  public
    width; height
    area = width * height 
    flipped : rectangle
  unifies
    flipped.width = height
    flipped.height = width
    flipped.flipped = this

class square : rectangle
  unifies
    width = height

class colored rectangle
  public
    color
    flipped : colored rectangle
  unifies
    color = flipped.color

data Red
data Blue

r = new colored square with width = 12; color = Red
