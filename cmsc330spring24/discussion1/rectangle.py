num_rectangles = 0

class Rectangle:
   def __init__(self, width, height):
      self.width = width
      self.height = height

      global num_rectangles
      num_rectangles += 1

   def get_area(self):
      return self.width * self.height

   def get_num_rectangles():
      return num_rectangles