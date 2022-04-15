use goldberg::*;

#[test]
fn test_stmts() {
    let result = goldberg_stmts! {
        {
            fn fake_random(value: u32) -> u32 {
                match value {
                    1 => 42u32,
                    2 => 64u32,
                    3 => 31337u32,
                    _ => 0u32,
                }
            }

            let x: u32 = fake_random(1);
            let y: u32 = fake_random(2);
            let z: u32;

            z = x ^ y;
            z ^= fake_random(3);
            z += 420u32;

            z
        }
    };

    assert_eq!(result, 31655);
}

#[test]
fn test_advent() {
    goldberg_stmts! {
        use std::collections::HashSet;

        #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
        struct Coordinate(usize, usize);
        impl Coordinate {
            fn fold_x(&self, bifurcation: usize) -> Self {
                Self(self.0-((self.0 - bifurcation) * 2),self.1)
            }
            fn fold_y(&self, bifurcation: usize) -> Self {
                Self(self.0,self.1-((self.1 - bifurcation) * 2))
            }
        }

        #[derive(Copy, Clone, Eq, PartialEq, Debug)]
        enum Axis {
            X,
            Y,
        }
        impl Axis {
            fn from_string(s: &String) -> Self {
                match s.as_str() {
                    "x" => Self::X,
                    "y" => Self::Y,
                    _ => panic!("bad axis!"),
                }
            }
        }

        fn read_origami(data: String) -> Result<(HashSet<Coordinate>, Vec<(Axis,usize)>), ()> {
            let mut coords: HashSet<Coordinate> = HashSet::<Coordinate>::new();
            let mut instructions: Vec<(Axis, usize)> = Vec::<(Axis,usize)>::new();
            let split_char: String = String::from("\n");
            let lines: Vec<String> = data.split(split_char.as_str()).map(|x| x.to_string()).collect();
            let mut index: usize = 0usize;
            
            'coords: for i in 0..lines.len() {
                index = i;
                
                let line: String = lines[index].clone();
                let size: usize = line.len();
                if size == 0 || size == 1 { break 'coords; }

                let coord: Vec<usize> = line.trim().split(",").map(|x| x.parse().unwrap()).collect();
                coords.insert(Coordinate(coord[0], coord[1]));
            }

            if coords.len() == 0 { return Err(()); }

            'folds: for i in index+1..lines.len() {
                let line: String = lines[i].clone();
                let size: usize = line.len();
                
                if size == 0 { break 'folds; }
                if size == 1 { continue 'folds; }

                let instruction: Vec<String> = line.trim().replace("fold along ","").split("=").map(|x| x.to_string()).collect();
                instructions.push((Axis::from_string(&instruction[0]),instruction[1].parse().unwrap()));
            }

            if instructions.len() == 0 { Err(()) }
            else { Ok((coords, instructions)) }
        }

        fn fold(coords: &HashSet<Coordinate>, axis: Axis, bifurcation: usize) -> HashSet<Coordinate> {
            let affected_coords: HashSet<Coordinate> = match axis {
                Axis::X => {
                    coords.iter().filter(|x| x.0 > bifurcation).cloned().collect()
                },
                Axis::Y => {
                    coords.iter().filter(|x| x.1 > bifurcation).cloned().collect()
                },
            };

            let unaffected_coords: HashSet<Coordinate> = coords.difference(&affected_coords).cloned().collect();

            let folded_coords: HashSet<Coordinate> = match axis {
                Axis::X => {
                    affected_coords.iter().map(|x| x.fold_x(bifurcation)).collect()
                },
                Axis::Y => {
                    affected_coords.iter().map(|x| x.fold_y(bifurcation)).collect()
                }
            };

            folded_coords.union(&unaffected_coords).cloned().collect()
        }

        fn print_dots(coords: &HashSet<Coordinate>) -> String {
            let mut mx: usize = 0usize;
            let mut my: usize = 0usize;

            for coord in coords {
                let nx: usize = coord.0;
                let ny: usize = coord.1;

                if nx+1>mx { mx=nx+1; }
                if ny+1>my { my=ny+1; }
            }

            let mut buffer: Vec<Vec<char>> = vec![vec![' '; mx]; my];
            
            for coord in coords {
                let (x,y) = (coord.0,coord.1);

                buffer[y][x] = '#';
            }

            let lines: Vec<String> = buffer.iter().map(|x| x.into_iter().collect()).collect();

            lines.join("\n")
        }
    }

    let file_data = std::fs::read("tests/origami-input.txt").unwrap();
    let string_data = std::str::from_utf8(file_data.as_slice()).unwrap();
    let origami_data_result = read_origami(string_data.to_string());

    assert!(origami_data_result.is_ok());

    let (mut coords, instructions) = origami_data_result.unwrap();

    for (axis, bifurcation) in instructions {
        coords = fold(&coords, axis, bifurcation);
    }

    let result = print_dots(&coords);

    let file_data = std::fs::read("tests/origami-output.txt").unwrap();
    let string_data = std::str::from_utf8(file_data.as_slice()).unwrap().trim_end();

    assert_eq!(string_data, result);
}

#[test]
fn test_int() {
    let value = goldberg_int!(0xDEADBEEFu32);

    assert_eq!(value, 0xDEADBEEF);
}

#[test]
fn test_string() {
    let string = String::from(goldberg_string!("This is an encrypted string!"));

    assert_eq!(string, "This is an encrypted string!");
}
