use std::{collections::{BTreeSet, HashSet}, fmt, cmp::min};
use rand::{thread_rng, seq::SliceRandom};

macro_rules! parse_input {
	($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

macro_rules! MOVE {
	($actions:expr, $amount:expr, $from:expr, $to:expr) => {
		$actions.add(format!("MOVE {} {} {} {} {}", $amount, $from.x, $from.y, $to.x, $to.y))
	};
}

macro_rules! BUILD {
	($actions:expr, $pos:expr) => {
		$actions.add(format!("BUILD {} {}", $pos.x, $pos.y))
	};
}

macro_rules! SPAWN {
	($actions:expr, $amount:expr, $pos:expr) => {
		$actions.add(format!("SPAWN {} {} {}", $amount, $pos.x, $pos.y))
	};
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

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Point {
	x: usize,
	y: usize,
}

impl Point {
	fn from(x: usize, y: usize) -> Self {
		Point { x: x, y: y }
	}
}

impl fmt::Display for Point {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "[{}, {}]", self.x, self.y)
	}
}

impl fmt::Debug for Point {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "[{}, {}]", self.x, self.y)
	}
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Owner {
	Ally,
	Enemy,
	Neutral,
}

#[derive(Copy, Clone, Debug)]
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
			position: Point::from(x, y),
			scrap: 0,
			owner: Owner::Neutral,
			units: 0,
			recycler: false,
			can_build: false,
			can_spawn: false,
			in_range_of_recycler: false
		}
	}
}

impl fmt::Display for Patch {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "[{}, {}]", self.position.x, self.position.y)
	}
}

fn main() {
	const COST: usize = 10;
	let inputs = get_inputs();
	let width = parse_input!(inputs[0], usize);
	let height = parse_input!(inputs[1], usize);

	let mut grid: Vec<Vec<Patch>> = (0..height).map(|j| (0..width).map(|i| Patch::from(i, j)).collect()).collect();

	loop {
		let mut actions: Actions = Actions::new();
		let mut my_matter = update_input(&mut grid, width, height);
	
		let mut units: Vec<Point> = Vec::new();
		let mut ally_tiles: Vec<Patch> = Vec::new();
		let mut ally_tiles_in_range_of_recycler = 0;
	
		// Map data
		for y in 0..height {
			for x in 0..width {
				let patch = &mut grid[y][x];

				// Update scrap
				if patch.in_range_of_recycler {
					patch.scrap -= 1;
					if patch.scrap == 0 {
						patch.can_build = false;
						patch.can_spawn = false;
						patch.owner = Owner::Neutral;
						patch.units = 0;
					}
				}
				// Save unit positions
				if patch.owner == Owner::Ally {
					ally_tiles.push(*patch);
					if patch.units > 0 {
						units.push(patch.position);
				 	}
					if patch.in_range_of_recycler {
						ally_tiles_in_range_of_recycler += 1;
					}
				}

			}
		}

		ally_tiles.sort_by(|lhs, rhs| {
			let lhs_sum = adjacent(&grid, lhs.position).into_iter()
				.fold(lhs.scrap, |sum, p| {
					sum + grid[p.y][p.x].scrap
				});
			let rhs_sum = adjacent(&grid, rhs.position).into_iter()
				.fold(rhs.scrap, |sum, p| {
					sum + grid[p.y][p.x].scrap
				});
			lhs_sum.cmp(&rhs_sum)
		});

		ally_tiles = ally_tiles.into_iter().rev().collect();

		// Moving units
		for unit in units {
			for _ in 0..grid[unit.y][unit.x].units {

				let target = nearest_where(&grid, unit, |p| {
					p.owner != Owner::Ally && !p.recycler
				});

				if target.is_none() {
					break ;
				}

				let point = target.unwrap();
				let patch = &mut grid[unit.y][unit.x];
				if patch.owner == Owner::Enemy {
					if patch.units > 0 {
						patch.units -= 1;
					} else {
						patch.owner = Owner::Ally;
					}
				} else {
					patch.owner = Owner::Ally;
				}

				MOVE!(actions, 1, unit, point);
			}
		}

		// Building recyclers
		let free_tiles = ally_tiles.len() - ally_tiles_in_range_of_recycler;
		let build_budget = free_tiles - min(free_tiles, 20);
		for _ in 0..(build_budget / COST) {
			for patch in &ally_tiles {
				let nearby = near(&grid, patch.position, 2);
			
				if my_matter >= COST && patch.can_build && !nearby.iter().any(|p| grid[p.y][p.x].recycler) {
					let adj = adjacent(&grid, patch.position);
				
					if adj.iter().all(|p| grid[p.y][p.x].units == 0 || grid[p.y][p.x].owner != Owner::Ally) {
						BUILD!(actions, patch.position);
						grid[patch.position.y][patch.position.x].recycler = true;
						my_matter -= COST;
						break ;
					}
				}
			}
		}
	
		let middle = nearest_where(&grid, Point::from(width / 2, height / 2), |p| {
			p.can_spawn && p.scrap > 0
		});
	
		if middle.is_some() {
			let point = middle.unwrap();

			if my_matter >= COST {
				SPAWN!(actions, my_matter / COST, point);
			}
		}

		actions.flush();
	}
}

fn near(grid: &Vec<Vec<Patch>>, point: Point, dist: usize) -> Vec<Point> {
	let mut points: HashSet<Point> = vec![point; 1].into_iter().collect();

	for _ in 0..dist {
		let mut new: Vec<Point> = Vec::new();
		for p in &points {
			new.extend(adjacent(grid, *p).into_iter());
		}
		points.extend(new.into_iter());
	}
	
	points.into_iter().collect()
}

fn nearest_where<F>(grid: &Vec<Vec<Patch>>, point: Point, f: F) -> Option<Point> where
	F: Fn(&Patch) -> bool {
	
	let mut edges = vec![point; 1];
	let mut visited: BTreeSet<Point> = vec![point; 1].into_iter().collect();
	
	while !edges.is_empty() {
		let mut new: Vec<Point> = Vec::new();
		for edge in &edges {
			let patch = &grid[edge.y][edge.x];
			if f(patch) {
				return Some(edge.clone())
			}
			let nb = adjacent_movable(grid, *edge);
			for point in nb {
				if !visited.contains(&point) {
					new.push(point);
				}
			}
			visited.insert(*edge);
		}
		edges = new;
	}
	None
}

fn adjacent_movable(grid: &Vec<Vec<Patch>>, point: Point) -> Vec<Point> {
	adjacent(grid, point).into_iter().filter(|p| grid[p.y][p.x].scrap > 0).collect()
}

fn adjacent(grid: &Vec<Vec<Patch>>, point: Point) -> Vec<Point> {
	adjacent_tiles(point).into_iter().filter_map(|tile| {
		let row = grid.iter().nth(tile.1 as usize)?;
		row.iter().nth(tile.0 as usize)?;
		Some(Point { x: tile.0 as usize, y: tile.1 as usize })
	}).collect()
}

fn adjacent_tiles(point: Point) -> Vec<(i32, i32)> {
	let mut adj: Vec<(i32, i32)> = vec![
		(point.x as i32 + 1, point.y as i32),
		(point.x as i32, point.y as i32 + 1),
		(point.x as i32 - 1, point.y as i32),
		(point.x as i32, point.y as i32 - 1)
	];
	adj.shuffle(&mut thread_rng());
	adj
}

// Input
fn update_input(grid: &mut Vec<Vec<Patch>>, width: usize, height: usize) -> usize {
	let inputs = get_inputs();

	let my_matter = parse_input!(inputs[0], i32);
	let _opp_matter = parse_input!(inputs[1], i32);

	for y in 0..height {
		for x in 0..width {
			let inputs = get_inputs();

			grid[y][x].scrap = parse_input!(inputs[0], i32) as usize;
			grid[y][x].owner = match parse_input!(inputs[1], i32) {
				1 => Owner::Ally, 0 => Owner::Enemy, _ => Owner::Neutral
			};
			grid[y][x].units = parse_input!(inputs[2], i32) as usize;
			grid[y][x].recycler = parse_input!(inputs[3], i32) == 1;

			grid[y][x].can_build = parse_input!(inputs[4], i32) == 1;
			grid[y][x].can_spawn = parse_input!(inputs[5], i32) == 1;
			grid[y][x].in_range_of_recycler = parse_input!(inputs[6], i32) == 1;
		}
	}

	my_matter as usize
}

fn get_inputs() -> Vec<String> {
	let mut input_line = String::new();
	std::io::stdin().read_line(&mut input_line).unwrap();
	input_line.split(" ").map(|s| s.to_string()).collect()
}
