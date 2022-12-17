use std::{collections::{BTreeSet, HashSet}, cmp::max, fmt, ops::{Add, AddAssign, Div, Sub, DivAssign}};
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
struct IVec2 {
	x: i32,
	y: i32,
}

impl IVec2 {
	fn new(x: i32, y: i32) -> Self {
		IVec2 { x: x, y: y }
	}

	fn perp(self: &Self) -> Self {
		IVec2 { x: self.y, y: -self.x }
	}

	fn abs(self: &Self) -> i32 {
		self.x.abs() + self.y.abs()
	}
}

impl Add for IVec2 {
	type Output = IVec2;

	fn add(self, rhs: Self) -> Self::Output {	
		IVec2 { x: self.x + rhs.x, y: self.y + rhs.y }
	}
}

impl AddAssign for IVec2 {
	fn add_assign(&mut self, rhs: Self) {
		self.x += rhs.x;
		self.y += rhs.y;
	}
}

impl Sub for IVec2 {
	type Output = IVec2;

	fn sub(self, rhs: Self) -> Self::Output {
		IVec2 { x: self.x - rhs.x, y: self.y - rhs.y }
	}
}

impl Div<i32> for IVec2 {
	type Output = IVec2;

	fn div(self, rhs: i32) -> Self::Output {
		IVec2 { x: self.x / rhs, y: self.y / rhs }
	}
}

impl DivAssign<i32> for IVec2 {
	fn div_assign(&mut self, rhs: i32) {
		self.x /= rhs;
		self.y /= rhs;
	}
}

impl fmt::Display for IVec2 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "[{}, {}]", self.x, self.y)
	}
}

impl fmt::Debug for IVec2 {
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
	position: IVec2,
	scrap_total: usize,

	scrap: usize,
	owner: Owner,
	units: usize,
	recycler: bool,
	can_build: bool,
	can_spawn: bool,
	in_range_of_recycler: bool,
}

impl Patch {
	fn from(x: i32, y: i32) -> Self {
		Patch {
			position: IVec2::new(x, y),
			scrap_total: 0,
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

fn main() {
	const COST: usize = 10;
	let inputs = get_inputs();
	let width = parse_input!(inputs[0], usize);
	let height = parse_input!(inputs[1], usize);

	let mut grid: Vec<Vec<Patch>> = (0..height as i32).map(|j: i32| {
		(0..width as i32).map(|i: i32| {
			Patch::from(i, j)
		}).collect()
	}).collect();

	loop {
		let mut actions: Actions = Actions::new();
		let mut ally_matter = update_input(&mut grid, width, height);
	
		let mut ally_units: Vec<IVec2> = Vec::new();
		let mut ally_tiles: Vec<IVec2> = Vec::new();
		let mut ally_recyclers: Vec<IVec2> = Vec::new();
		
		let mut enemy_units: Vec<IVec2> = Vec::new();
	
		// Map data
		for y in 0..height {
			for x in 0..width {
				let adj: Vec<Patch> = adjacent(&grid, IVec2::new(x as i32, y as i32)).into_iter().map(|p| get(&grid, p).clone()).collect();
			
				let patch = &mut grid[y][x];

				patch.scrap_total = adj.iter().fold(patch.scrap, |sum, p| sum + p.scrap);

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

				if patch.units > 0 {
					if patch.owner == Owner::Ally {
						ally_units.push(patch.position);
					} else {
						enemy_units.push(patch.position);
					}
				}

				if patch.owner == Owner::Ally {
					ally_tiles.push(patch.position);
					if patch.recycler {
						ally_recyclers.push(patch.position);
				}
			}

			}
		}

		ally_tiles.sort_by(|lhs, rhs| {
			get(&grid, *lhs).scrap_total.cmp(&get(&grid, *rhs).scrap_total)
		});

		let ally_gravity = ally_units.iter().fold(IVec2::new(0, 0), |sum, point| sum + *point) / max(ally_units.len() as i32, 1);
		let enemy_gravity = enemy_units.iter().fold(IVec2::new(0, 0), |sum, point| sum + *point) / max(enemy_units.len() as i32, 1);
		let center = (ally_gravity + enemy_gravity) / 2;

		let perp = (center - ally_gravity).perp();

		let frontline_start = center + perp;
		let frontline_end = center - perp;

		let frontline = line(frontline_start, frontline_end);

		// Moving units
		for unit in ally_units {
			for _ in 0..get(&grid, unit).units {

				let target = nearest_where(&grid, unit, |p| p.owner != Owner::Ally);

				if target.is_none() {
					break ;
				}

				let point = target.unwrap();
				let patch = get_mut(&mut grid, point);

				if patch.units > 0 && (point - unit).abs() == 1 {
					break ;
				}
			
				if patch.units > 0 {
					patch.units -= 1;
				} else {
					patch.owner = Owner::Ally;
				}
				MOVE!(actions, 1, unit, point);
			}
		}

		// Building recyclers
		for tile in &ally_tiles {
			let patch = get(&grid, *tile).clone();
		
			// Frontline
			if ally_matter >= COST && patch.can_build && frontline.contains(tile) {
				let patch = get_mut(&mut grid, *tile);
				
				BUILD!(actions, tile);
				
				patch.can_spawn = false;
				patch.recycler = true;
				ally_matter -= COST;
				break ;
			}

			// Efficient patches
			let nearby_recycler = near(&grid, *tile, 2).iter().any(|p| get(&grid, *p).recycler);
			if ally_matter >= COST && patch.can_build && patch.scrap >= 5 && patch.scrap_total >= 40 && !nearby_recycler {
				let patch = get_mut(&mut grid, *tile);
			
				BUILD!(actions, tile);
			
				patch.can_spawn = false;
				patch.recycler = true;
				ally_matter -= COST;
			}
		}

		// Spawning next to unowned tiles
		for tile in &ally_tiles {
			let patch = get(&grid, *tile);
		
			if patch.can_spawn &&  ally_matter >= COST && patch.units == 0 {
				if adjacent_movable(&grid, *tile).iter().any(|p| get(&grid, *p).owner != Owner::Ally) {
					SPAWN!(actions, 1, tile);
					ally_matter -= COST;
				} 
			}
		}

		actions.flush();
	}
}

fn get<'a>(grid: &'a Vec<Vec<Patch>>, point: IVec2) -> &'a Patch {
	&grid[point.y as usize][point.x as usize]
}

fn get_mut<'a>(grid: &'a mut Vec<Vec<Patch>>, point: IVec2) -> &'a mut Patch {
	&mut grid[point.y as usize][point.x as usize]
}

fn line(mut start: IVec2, end: IVec2) -> Vec<IVec2> {
	let mut points: Vec<IVec2> = vec![start];

	while start != end {
		let mut diff = end - start;

		if diff.x.abs() < diff.y.abs() {
			diff = IVec2::new(0, diff.y.clamp(-1, 1));
		} else {
			diff = IVec2::new(diff.x.clamp(-1, 1), 0);
		}
		start += diff;
		points.push(start);
	}

	points
}

fn near(grid: &Vec<Vec<Patch>>, point: IVec2, dist: usize) -> Vec<IVec2> {
	let mut points: HashSet<IVec2> = vec![point; 1].into_iter().collect();

	for _ in 0..dist {
		let mut new: Vec<IVec2> = Vec::new();
		for p in &points {
			new.extend(adjacent(grid, *p).into_iter());
		}
		points.extend(new.into_iter());
	}
	
	points.into_iter().collect()
}

fn nearest_where<F>(grid: &Vec<Vec<Patch>>, point: IVec2, f: F) -> Option<IVec2> where
	F: Fn(&Patch) -> bool {
	
	let mut edges = vec![point; 1];
	let mut visited: BTreeSet<IVec2> = vec![point; 1].into_iter().collect();
	
	while !edges.is_empty() {
		let mut new: Vec<IVec2> = Vec::new();
		for edge in &edges {
			let patch = get(grid, *edge);
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

fn adjacent_movable(grid: &Vec<Vec<Patch>>, point: IVec2) -> Vec<IVec2> {
	adjacent(grid, point).into_iter().filter(|p| get(grid, *p).scrap > 0 && !get(grid, *p).recycler).collect()
}

fn adjacent(grid: &Vec<Vec<Patch>>, point: IVec2) -> Vec<IVec2> {
	adjacent_points(point).into_iter().filter_map(|tile| {
		let row = grid.iter().nth(tile.1 as usize)?;
		row.iter().nth(tile.0 as usize)?;
		Some(IVec2 { x: tile.0, y: tile.1 })
	}).collect()
}

fn adjacent_points(point: IVec2) -> Vec<(i32, i32)> {
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

	let ally_matter = parse_input!(inputs[0], i32);
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

	ally_matter as usize
}

fn get_inputs() -> Vec<String> {
	let mut input_line = String::new();
	std::io::stdin().read_line(&mut input_line).unwrap();
	input_line.split(" ").map(|s| s.to_string()).collect()
}
