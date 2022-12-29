use std::{collections::{BTreeSet, HashSet}, cmp::max, fmt, ops::{Add, AddAssign, Div, Sub, DivAssign}};
use rand::{thread_rng, seq::SliceRandom};
use chrono::Utc;

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

macro_rules! MESSAGE {
	($actions:expr, $msg:expr) => {
		$actions.add(format!("MESSAGE {}", $msg))
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
	const ZERO: Self = Self { x:0, y: 0 };

	fn new(x: i32, y: i32) -> Self {
		IVec2 { x: x, y: y }
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
	scrap_total: i32,

	scrap: i32,
	owner: Owner,
	units: i32,
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

#[derive(Debug)]
struct Border {
	tile: IVec2,
	enemies: i32,
}

// impl Border {
// 	fn new() -> Self {
// 		Self { tile: IVec2::ZERO, enemies: 0 }
// 	}
// }

struct Player {
	matter: i32,
	recyclers: i32,
	tiles: Vec<IVec2>,
	units: HashSet<IVec2>,
	border: Vec<Border>,
	gravity: IVec2,
}

impl Player {
	fn new() -> Self {
		Self {
			matter: 0,
			recyclers: 0,
			tiles: Vec::new(),
			units: HashSet::new(),
			border: Vec::new(),
			gravity: IVec2::ZERO,
		}
	}
}

const COST: i32 = 10;
const RATIO: i32 = 8;

fn main() {
	let inputs = get_inputs();
	let width = parse_input!(inputs[0], usize);
	let height = parse_input!(inputs[1], usize);

	let mut grid: Vec<Vec<Patch>> = (0..height).map(|j| {
		(0..width).map(|i| {
			Patch::from(i as i32, j as i32)
		}).collect()
	}).collect();

	loop {
		let mut ally: Player = Player::new();
		let mut enemy: Player = Player::new();
		let mut neutral: Player = Player::new();
		let mut actions: Actions = Actions::new();

		let matter = update_input(&mut grid, width, height);

		let now = Utc::now().timestamp_millis();

		ally.matter = matter.0;
		enemy.matter = matter.1;
	
		// Map data
		for y in 0..height {
			for x in 0..width {
				let patch = &mut grid[y][x];
			
				let player = match patch.owner {
					Owner::Ally => &mut ally,
					Owner::Enemy => &mut enemy,
					Owner::Neutral => &mut neutral,
				};

				// Mark owned tiles
				player.tiles.push(patch.position);

				// Mark units
				if patch.units > 0 {
					player.units.insert(patch.position);
				}

				// Count recyclers
				if patch.recycler {
					player.recyclers += 1;
				}
			}
		}

		ally.gravity = ally.units.iter().fold(IVec2::ZERO, |sum, point| sum + *point) / max(ally.units.len() as i32, 1);
		enemy.gravity = enemy.units.iter().fold(IVec2::ZERO, |sum, point| sum + *point) / max(enemy.units.len() as i32, 1);
	
		// Adjacency dependant
		for tile in &ally.tiles {
			let adj: Vec<Patch> = adjacent_movable(&grid, *tile).into_iter().map(|p| get(&grid, p).clone()).collect();
			let patch = get_mut(&mut grid, *tile);
		
			// Calculate recyclable scrap
			patch.scrap_total = adj.iter().fold(patch.scrap, |sum, p| sum + p.scrap);

			// Mark border tiles
			let borders: Vec<&Patch> = adj.iter().filter(|p| p.owner != patch.owner).collect();
			if !borders.is_empty() {
				let opposed = borders.iter().fold(0, |sum, p| sum + p.units);
			
				let border = Border { tile: *tile, enemies: opposed };
			
				ally.border.push(border);
			}
		}

		// Sort allied tiles by total scrap for recyclers
		ally.tiles.sort_by(|lhs, rhs| {
			get(&grid, *lhs).scrap_total.cmp(&get(&grid, *rhs).scrap_total).reverse()
		});

		// Sort allied border tiles by distance to enemy
		ally.border.sort_by(|lhs, rhs| {
			(lhs.tile - enemy.gravity).abs().cmp(&((rhs.tile - enemy.gravity).abs()))
		});

		// Moving units
		for unit in ally.units {
			let allies = get(&grid, unit).units;
				
			for _ in 0..allies {
				let target = nearest_where(&grid, unit, |p| p.owner != Owner::Ally, enemy.gravity);

				if target.is_some() {
					let target = target.unwrap();
					let patch = get_mut(&mut grid, target);

					if patch.units > 0 {
						patch.units -= 1;
					} else {
						patch.owner = Owner::Ally;
					}

					MOVE!(actions, 1, unit, target);
				}		
			}
		}

		// TODO: detect stalemate, defending with units

		// Blocking enemy units with recyclers
		for border_tile in &ally.border {
			let tile = border_tile.tile;
			let enemies = border_tile.enemies;
			let patch = get_mut(&mut grid, tile);
			
			if ally.matter >= COST && patch.can_build 
				&& enemies > 0
			{
				BUILD!(actions, tile);
			
				patch.can_build = false;
				patch.can_spawn = false;
				patch.recycler = true;
				ally.matter -= COST;
			}
		}

		// Building recyclers on efficient patches
		for tile in &ally.tiles {
			let adj: Vec<Patch> = adjacent_movable(&grid, *tile).into_iter().map(|p| get(&grid, p).clone()).collect();
			let patch = get(&grid, *tile);

			let tiles_cost = 1 + adj.iter().filter(|a| a.scrap <= patch.scrap).count() as i32;
			let efficiency = patch.scrap_total / tiles_cost;

			if ally.matter >= COST && patch.can_build
				&& patch.scrap_total > COST
				&& efficiency >= COST
				&& !near(&grid, *tile, 2).iter().any(|p| get(&grid, *p).recycler)
			{
				let patch = get_mut(&mut grid, *tile);
			
				eprintln!("{}", patch.scrap_total);
				eprintln!("{tiles_cost}");
			
				BUILD!(actions, tile);
			
				patch.can_build = false;
				patch.can_spawn = false;
				patch.recycler = true;
				ally.matter -= COST;
			}
		}

		// Spawning on border tiles
		for border in &ally.border {
			let tile = border.tile;
			let enemies = border.enemies;
			let patch = get_mut(&mut grid, tile);

			if ally.matter >= COST && patch.can_spawn {
				let available = ally.matter / COST;
				let diff: i32 = enemies - patch.units;
			
				let amount = diff.clamp(1, available);
			
				SPAWN!(actions, amount, tile);
				ally.matter -= amount * COST;
			}
		}

		let time = Utc::now().timestamp_millis() - now;

		MESSAGE!(actions, format!("{} ms", time as f64));

		actions.flush();
	}
}

fn get<'a>(grid: &'a Vec<Vec<Patch>>, point: IVec2) -> &'a Patch {
	&grid[point.y as usize][point.x as usize]
}

fn get_mut<'a>(grid: &'a mut Vec<Vec<Patch>>, point: IVec2) -> &'a mut Patch {
	&mut grid[point.y as usize][point.x as usize]
}

fn near(grid: &Vec<Vec<Patch>>, point: IVec2, dist: usize) -> Vec<IVec2> {
	let mut points: HashSet<IVec2> = vec![point].into_iter().collect();

	for _ in 0..dist {
		let mut new: Vec<IVec2> = Vec::new();
	
		for p in &points {
			new.extend(adjacent(grid, *p).into_iter());
		}
		points.extend(new.into_iter());
	}
	
	points.into_iter().collect()
}

fn nearest_where<F>(grid: &Vec<Vec<Patch>>, point: IVec2, f: F, enemy: IVec2) -> Option<IVec2> where
	F: Fn(&Patch) -> bool {
	
	let mut edges = vec![point];
	let mut visited: BTreeSet<IVec2> = vec![point].into_iter().collect();
	let mut answer: Vec<IVec2> = Vec::new();
	
	while !edges.is_empty() && answer.is_empty() {
		let mut new: Vec<IVec2> = Vec::new();

		for edge in &edges {
			let patch = get(grid, *edge);
		
			if f(patch) {
				answer.push(*edge);
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

	answer.into_iter().min_by(|lhs, rhs| {
		(enemy - *lhs).abs().cmp(&(enemy - *rhs).abs())
	})
}

fn adjacent(grid: &Vec<Vec<Patch>>, point: IVec2) -> Vec<IVec2> {
	let width = grid[0].len() as i32;
	let height = grid.len() as i32;

	let mut adj: Vec<IVec2> = vec![
		IVec2::new(point.x + 1, point.y),
		IVec2::new(point.x, point.y + 1),
		IVec2::new(point.x - 1, point.y),
		IVec2::new(point.x, point.y - 1)
	];
	adj.shuffle(&mut thread_rng());
	adj.into_iter().filter_map(|tile| {
		if tile.x >= 0 && tile.x < width && tile.y >= 0 && tile.y < height {
			Some(tile)
		} else {
			None
		}
	}).collect()
}

fn adjacent_movable(grid: &Vec<Vec<Patch>>, point: IVec2) -> Vec<IVec2> {
	adjacent(grid, point).into_iter().filter(|p| get(grid, *p).scrap > 0 && !get(grid, *p).recycler).collect()
}

// Input
fn update_input(grid: &mut Vec<Vec<Patch>>, width: usize, height: usize) -> (i32, i32) {
	let inputs = get_inputs();

	let ally_matter = parse_input!(inputs[0], i32);
	let enemy_matter = parse_input!(inputs[1], i32);

	for y in 0..height {
		for x in 0..width {
			let inputs = get_inputs();

			grid[y][x].scrap = parse_input!(inputs[0], i32);
			grid[y][x].owner = match parse_input!(inputs[1], i32) {
				1 => Owner::Ally, 0 => Owner::Enemy, _ => Owner::Neutral
			};
			grid[y][x].units = parse_input!(inputs[2], i32);
			grid[y][x].recycler = parse_input!(inputs[3], i32) == 1;

			grid[y][x].can_build = parse_input!(inputs[4], i32) == 1;
			grid[y][x].can_spawn = parse_input!(inputs[5], i32) == 1;
			grid[y][x].in_range_of_recycler = parse_input!(inputs[6], i32) == 1;
		}
	}

	(ally_matter, enemy_matter)
}

fn get_inputs() -> Vec<String> {
	let mut input_line = String::new();
	std::io::stdin().read_line(&mut input_line).unwrap();
	input_line.split(" ").map(|s| s.to_string()).collect()
}
