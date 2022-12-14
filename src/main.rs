use std::collections::BTreeSet;

macro_rules! parse_input {
	($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

struct Actions {
	actions: Vec<String>,
}

impl Actions {
	fn new() -> Self {
		Actions { actions: Vec::new() }
	}

	fn add(&mut self, action: String) {
		self.actions.push(action + ";");
	}

	fn flush(&mut self) {
		if self.actions.is_empty() {
			self.actions.push("WAIT;".to_string());
		}

		for action in &self.actions {
			print!("{action}");
		}
		println!("");
	}
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct Point {
	x: usize,
	y: usize,
}

#[derive(Copy, Clone, PartialEq)]
enum Owner {
	Ally,
	Enemy,
	Neutral,
}

#[derive(Copy, Clone)]
struct Patch {
	position: Point,

	scrap: usize,
	owner: Owner,
	units: usize,
	recycler: bool,
	can_build: bool,
	can_spawn: bool,
	in_range_of_recycler: bool,
}

impl Patch {
	fn from(x: usize, y: usize) -> Self {
		Patch {
			position: Point { x: x, y: y },
			scrap: 0,
			owner: Owner::Neutral,
			units: 0,
			recycler: false,
			can_build: false,
			can_spawn: false,
			in_range_of_recycler: false
		}
	}

	fn pos(&self) -> Point {
		self.position
	}
}

fn main() {
	let inputs = get_inputs();
	let width = parse_input!(inputs[0], usize);
	let height = parse_input!(inputs[1], usize);

	let mut grid: Vec<Vec<Patch>> = (0..height).map(|_| (0..width).map(|_| Patch::from(width, height)).collect()).collect();

	loop {
		let mut actions:Actions = Actions::new();
		let mut my_matter = update_input(&mut grid, width, height);

		let mut targets: Vec<(usize, usize)> = Vec::new();
		for y in 0..height {
			for x in 0..width {
				let patch = &mut grid[y][x];

				// Build recycler if noone in range
				if patch.owner == Owner::Ally && !patch.in_range_of_recycler && patch.units == 0 && my_matter >= 10 {
					actions.add(format!("BUILD {} {}", x, y));
					my_matter -= 10;
					patch.recycler = true;
				}

				// Move away from owned tiles
				if patch.owner == Owner::Ally {
					if x >= 2 && x < width && y >= 2 && y < height { targets.push((x - 2, y - 2)); }
					if x + 2 < width && y >= 2 && y < height { targets.push((x + 2, y - 2)); }
					if x >= 2 && x < width && y + 2 < height { targets.push((x - 2, y + 2)); }
					if x + 2 < width && y + 2 < height { targets.push((x + 2, y + 2)); }
				}
				if patch.owner == Owner::Ally {
					for _ in 0..patch.units {
						if !targets.is_empty() {
							let target = targets.pop().unwrap();
							actions.add(format!("MOVE 1 {} {} {} {}", x, y, target.0, target.1));
						}
					}
				}
			}
		}

		actions.flush();
	}
}

fn nearest_unowned(grid: &mut Vec<Vec<Patch>>, point: Point) {
	let mut edges = vec![point; 1];
	let mut visited: BTreeSet<Point> = edges.iter().map(|p| p.clone()).collect();
	
	loop {
		let mut new: Vec<Point> = Vec::new();
		for edge in &edges {
			let nb = neighbors(grid, *edge);
			for point in nb {
				if !visited.contains(&point) {
					new.push(point);
				}
			}
			visited.insert(*edge);
		}
		edges = new;
	}
}

fn neighbors(grid: &mut Vec<Vec<Patch>>, point: Point) -> Vec<Point> {
	let mut nb = Vec::new();
	let width = grid.first().unwrap().len();
	let height = grid.len();

	if point.x + 1 < width { nb.push(Point { x: point.x + 1, y: point.y })}
	if point.x >= 1 { nb.push(Point { x: point.x - 1, y: point.y })}
	if point.y + 1 < height { nb.push(Point { x: point.x, y: point.y + 1 })}
	if point.y >= 1 { nb.push(Point { x: point.x + 1, y: point.y - 1})}
	nb
}

// Input
fn update_input(grid: &mut Vec<Vec<Patch>>, width: usize, height: usize) -> i32 {
	let inputs = get_inputs();

	let my_matter = parse_input!(inputs[0], i32);
	let opp_matter = parse_input!(inputs[1], i32);

	for y in 0..height {
		for x in 0..width {
			let inputs = get_inputs();

			grid[y][x].scrap = parse_input!(inputs[0], i32) as usize;
			grid[y][x].owner = match parse_input!(inputs[1], i32) { 1 => Owner::Ally, 0 => Owner::Enemy, _ => Owner::Neutral };
			grid[y][x].units = parse_input!(inputs[2], i32) as usize;
			grid[y][x].recycler = parse_input!(inputs[3], i32) == 1;

			grid[y][x].can_build = parse_input!(inputs[4], i32) == 1;
			grid[y][x].can_spawn = parse_input!(inputs[5], i32) == 1;
			grid[y][x].in_range_of_recycler = parse_input!(inputs[6], i32) == 1;
		}
	}

	my_matter
}

fn get_inputs() -> Vec<String> {
	let mut input_line = String::new();
	std::io::stdin().read_line(&mut input_line).unwrap();
	input_line.split(" ").map(|s| s.to_string()).collect()
}
