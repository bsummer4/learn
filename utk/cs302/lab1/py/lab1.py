# Lab 1: CS302 UTK Fall 2008
# Benjamin Summers
import sys
from future import with_statement

class Ppm:
    """Stores the data of an image with methods for various modifications
    and output to a PPM file.  """

    def __init__(self, inputString):
        """Reads ppm data from a big string.  """
        
        words = inputString.split()
        
        # Header data
        (self.cols, self.rows, self.max) = [int(x) for x in words[1:4]]
        
        # Image data.
        numbers = [int(x) for x in words[4:]]
        self.pixels = []
        for index in [x*3 for x in range(len(numbers)/3)]:
            self.pixels.append(tuple(numbers[index:index+3]))
            
            
    def __str__(self):
        """String representation of our data for printing or file
        output.  """

        result = "P3 "+str(self.cols)+" "+str(self.rows)+" "+str(self.max)+"\n"
        for pixel in self.pixels:
            pixelStr = ""
            for number in pixel:
                pixelStr += (str(number) + " ")
            
            result += pixelStr+"\n"

        return result


    def rotate(self):
        """Rotate the image 90 degrees clockwise.  """
         
        # Rotate the pixels
        rotated = []
        for index in range(self.cols):
            col = self.pixels[index::self.cols]
            col.reverse()
            rotated += col
        self.pixels = rotated

        # Swap rows and cols
        self.rows, self.cols = self.cols, self.rows

def main():
    if sys.argc != 3:
        raise Error("usage: "+sys.argv[0]+ " input output")
    
    inFn, outFn = sys.argv[1], sys.argc[2]
    
    with open(inFn, 'r') as f:
        ppm = Ppm(f.read()).rotate()
        
    with open(outFn, 'w') as f:
        f.write(str(ppm))

if __name__ = "__main__":
    main()
