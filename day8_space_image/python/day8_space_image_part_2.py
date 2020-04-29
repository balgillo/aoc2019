import sys
import io

class SpaceImageLayer:
    def __init__(self, width, height):
        self.width = width
        self.height = height
        self.rows = []

    def load(self, input):
        self.rows = []
        row_index = 0
        next_row_index = 1
        while next_row_index * self.width <= len(input):
            self.rows.append( 
                input[row_index * self.width : next_row_index * self.width])
            row_index = next_row_index
            next_row_index += 1

    def merge_with(self, layer_below):
        newrows = []
        for rowindex in range(0, self.height):
            self_row = self.rows[rowindex]
            other_layer_row = layer_below.rows[rowindex]
            newrow = ""
            for colindex in range(0, self.width):
                self_char = self_row[colindex]
                other_layer_char = other_layer_row[colindex]
                newrow += other_layer_char if self_char == "2" else self_char
            newrows.append(newrow)
        ret = SpaceImageLayer(self.width, self.height)
        ret.rows = newrows
        return ret

    def __str__(self):
        ret = ""
        for row in self.rows:
            for c in row:
                ret += "   " if c == "0" else "[*]"
            ret += "\n"
        return ret


class SpaceImage:
    def __init__(self, width, height, input):
        self.width = width
        self.height = height
        pixels_per_layer = width * height
        self.layers = []
        layer_index = 0
        next_layer_index = 1
        while next_layer_index * pixels_per_layer <= len(input):
            layer = SpaceImageLayer(width, height)
            layer.load(input[layer_index * pixels_per_layer : next_layer_index * pixels_per_layer])
            self.layers.append(layer)
            layer_index = next_layer_index
            next_layer_index += 1

    def merge_layers(self):
        ret = self.layers[0]
        for l in self.layers[1:]:
            ret = ret.merge_with(l)
        return ret


image_width = int(sys.argv[1])
image_height = int(sys.argv[2])
file_path = sys.argv[3]
image = None
with io.open(file_path, "r") as f:
    line = f.readline()
    if line:
        image = SpaceImage(image_width, image_height, line)
    else:
        raise Exception("No image in file!")
print(image.merge_layers())
