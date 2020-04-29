use std::clone::Clone;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::mem::swap;

fn main() {
    let args: Vec<String> = env::args().collect();
    let image_width = usize::from_str_radix(&args[1], 10).expect("Can't parse first param as width");
    let image_height = usize::from_str_radix(&args[2], 10).expect("Can't parse second param as height");
    let file_path = &args[3];
    let image = load_image(image_width, image_height, &file_path);
    println!("{}", image.merge_layers());
}


fn load_image(width: usize, height: usize, file_path: &str) -> SpaceImage {
    let mut builder = SpaceImageBuilder::new(width, height);
    let mut f = File::open(file_path).expect("Unable to open file");
    let mut buf: [u8; 1] = [0];

    loop {
        match f.read(&mut buf) {
            Ok(1) => {
                match buf[0] as char {
                    '0' => builder.append_pixel(PixelColour::Black),
                    '1' => builder.append_pixel(PixelColour::White),
                    '2' => builder.append_pixel(PixelColour::Transparent),
                    '3' | '4' | '5' | '6' | '7' | '8' | '9' => builder.append_pixel(PixelColour::Other),
                    _ => ()  // ignore line breaks, whitespace etc.
                };
            },
            Ok(0) => {
                break;
            }
            Ok(_) | Err(_) => {
                panic!("File read failed");
            }
        }
    }
    match builder.build_image() {
        Ok(image) => {
            image
        }
        Err(s) => {
            panic!(format!("Bad image! {}", s));
        }
    }
}


struct SpaceImageBuilder {
    width: usize,
    height: usize,
    complete_layers: Vec<SpaceImageLayer>,
    wip_layer: Vec<Vec<PixelColour>>,
    wip_row: Vec<PixelColour>
}

impl SpaceImageBuilder {
    fn new(width: usize, height: usize) -> SpaceImageBuilder {
        SpaceImageBuilder{width: width, height: height,
            complete_layers: Vec::new(), wip_layer: Vec::new(),
            wip_row: Vec::new()}
    }

    fn append_pixel(&mut self, pixel: PixelColour) {
        self.wip_row.push(pixel);
        if self.wip_row.len() == self.width {
            let mut r = Vec::new();
            swap(&mut self.wip_row, &mut r);
            self.wip_layer.push(r);
            if self.wip_layer.len() == self.height {
                self.complete_layers.push(SpaceImageLayer::new(&self.wip_layer));
                self.wip_layer.clear();
            }
        }
    }

    fn build_image(&mut self) -> Result::<SpaceImage, String> {
        if self.wip_layer.is_empty() && self.wip_row.is_empty() {
            let mut layers = Vec::new();
            swap(&mut self.complete_layers, &mut layers);
            Ok(SpaceImage::new(self.width, self.height, layers))
        } else {
            Err(format!("Partial layer! {:?}", self.wip_row))
        }
    }

    fn build_layer(&mut self) -> Result::<SpaceImageLayer, String> {
        if self.wip_layer.is_empty() && self.wip_row.is_empty() && self.complete_layers.len() == 1 {
            Ok(self.complete_layers.pop().expect("Empty vector of length 1?!"))
        } else {
            Err("Not a single complete layer!".to_string())
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum PixelColour {
    Black,
    White,
    Transparent,
    Other
}

impl fmt::Display for PixelColour {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            PixelColour::Black => "0",
            PixelColour::White => "1",
            PixelColour::Transparent => "2",
            PixelColour::Other => "x",
        })
    }    
}


struct SpaceImage {
    width: usize,
    height: usize,
    layers: Vec<SpaceImageLayer>,
}

impl SpaceImage {
    fn new(width: usize, height: usize, mut layers: Vec<SpaceImageLayer>) -> SpaceImage {
        layers.shrink_to_fit();
        SpaceImage{width: width, height: height, layers: layers}
    }

    fn merge_layers(&self) -> SpaceImage {
        let mut builder = SpaceImageBuilder::new(self.width, self.height);
        for c in self.layers[0].iter() {
            builder.append_pixel(c);
        }
        let mut merged_layer = builder.build_layer().expect("Error merging layers");
        for under_layer in self.layers.iter().skip(1) {
            let mut builder = SpaceImageBuilder::new(self.width, self.height);
            for (over_pixel, under_pixel) in merged_layer.iter().zip(under_layer.iter()) {
                builder.append_pixel(match (over_pixel, under_pixel) {
                    (PixelColour::Transparent, c) => c,
                    (c, _) => c,
                });
            }
            merged_layer = builder.build_layer().expect("Error merging layers");
        }
        SpaceImage{width: self.width, height: self.height, layers: vec![merged_layer]}
    }
}

impl fmt::Display for SpaceImage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for l in &self.layers {
            for r in &l.rows {
                for c in r {
                    write!(f, "{}", match c {
                        PixelColour::Black => "   ",
                        PixelColour::White => "[*]",
                        PixelColour::Transparent => " . ",
                        PixelColour::Other => "???",
                    })?;
                }
                write!(f, "\n")?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}


struct SpaceImageLayer {
    rows: Vec<Vec<PixelColour>>,
}

static NO_PIXELS: [PixelColour; 0] = [];

impl SpaceImageLayer {
    fn new(rows: &Vec<Vec<PixelColour>>) -> SpaceImageLayer {
        let mut myrows = Vec::with_capacity(rows.len());
        for row in rows {
            let mut myrow = row.clone();
            myrow.shrink_to_fit();
            myrows.push(myrow);
        }
        SpaceImageLayer{rows: myrows}
    }

    fn iter(&self) -> SpaceImageLayerPixelIterator {
        let mut row_iterator = self.rows.iter();
        match row_iterator.next() {
            None => SpaceImageLayerPixelIterator{col_iterator: NO_PIXELS.iter(), row_iterator: row_iterator},
            Some(row) => SpaceImageLayerPixelIterator{col_iterator: row.iter(), row_iterator: row_iterator}
        }
    }
}


struct SpaceImageLayerPixelIterator<'a> {
    col_iterator: std::slice::Iter<'a, PixelColour>,
    row_iterator: std::slice::Iter<'a, Vec<PixelColour>>
}

impl<'a> Iterator for SpaceImageLayerPixelIterator<'a> {
    type Item = PixelColour;

    fn next(&mut self) -> Option<PixelColour> {
        match self.col_iterator.next() {
            None => {
                match self.row_iterator.next() {
                    None => None,
                    Some(v) => {
                        self.col_iterator = v.iter();
                        match self.col_iterator.next() {
                            None => None,
                            Some(c) => Some(*c)
                        }
                    }
                }
            }
            Some(c) => Some(*c)
        }
    }
}