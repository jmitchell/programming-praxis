#![allow(dead_code)]

pub mod sudoku {
    const WIDTH: usize = 9;
    const HEIGHT: usize = 9;

    #[derive(Copy,Clone,Debug)]
    pub enum Value {
        V1, V2, V3, V4, V5, V6, V7, V8, V9
    }

    impl Value {
        fn decode(n: &u8) -> Option<Value> {
            use self::Value::*;

            match *n {
                1 => Some(V1),
                2 => Some(V2),
                3 => Some(V3),
                4 => Some(V4),
                5 => Some(V5),
                6 => Some(V6),
                7 => Some(V7),
                8 => Some(V8),
                9 => Some(V9),
                _ => None
            }
        }
    }
    
    #[derive(Copy,Clone,Debug)]    
    pub enum Cell {
        Given(Value),
        Deduced(Value),
        Unknown
    }

    impl Cell {
        fn decode_given(n: &u8) -> Cell {
            match Value::decode(n) {
                Some(v) => Cell::Given(v),
                None => Cell::Unknown
            }
        }
    }

    #[derive(Clone,Debug)]
    pub struct Puzzle {
        grid: [[Cell; WIDTH]; HEIGHT]
    }

    impl Puzzle {
        pub fn new(cells: [[Cell; WIDTH]; HEIGHT]) -> Puzzle {
            Puzzle {
                grid: cells
            }
        }

        pub fn decode(encoded_grid: [[u8; WIDTH]; HEIGHT]) -> Puzzle {
            let mut grid = [[Cell::Unknown; WIDTH]; HEIGHT];
            for (y, row) in grid.iter_mut().enumerate() {
                for (x, cell) in row.iter_mut().enumerate() {
                    *cell = Cell::decode_given(&encoded_grid[y][x])
                }
            }
            Puzzle::new(grid)
        }
    }
}

#[cfg(test)]
mod test {
    use sudoku::Puzzle;

    #[test]
    fn initialize_puzzle() {
        Puzzle::decode(
            [[7, 0, 0, 1, 0, 0, 0, 0, 0],
             [0, 2, 0, 0, 0, 0, 0, 1, 5],
             [0, 0, 0, 0, 0, 6, 3, 9, 0],
             [2, 0, 0, 0, 1, 8, 0, 0, 0],
             [0, 4, 0, 0, 9, 0, 0, 7, 0],
             [0, 0, 0, 7, 5, 0, 0, 0, 3],
             [0, 7, 8, 5, 0, 0, 0, 0, 0],
             [5, 6, 0, 0, 0, 0, 0, 4, 0],
             [0, 0, 0, 0, 0, 1, 0, 0, 2]]);
    }
}
