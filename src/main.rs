use std::{collections::{BTreeSet, HashSet}, cmp::max, fmt, ops::{Add, AddAssign, Div, Sub, DivAssign, Mul}};
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

	fn abs(self) -> i32 {
		self.x.abs() + self.y.abs()
	}

	fn as_f32(self) -> Vec2 {
		Vec2 { x: self.x as f32 + 0.5, y: self.y as f32 + 0.5 }
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

#[derive(Clone, Copy, Debug)]
struct Vec2 {
	x: f32,
	y: f32,
}

impl Vec2 {
	fn dot(self, other: &Self) -> f32 {
		self.x * other.x + self.y + other.y
	}

	fn perp(self) -> Self {
		Self { x: self.y, y: -self.x }
	}

	fn len(self) -> f32 {
		f32::sqrt(self.x * self.x + self.y * self.y)
	}

	fn normalize(self) -> Self {
		let inverse = 1.0 / self.len();

		Self { x: self.x * inverse, y: self.y * inverse }
	}
}

impl Add for Vec2 {
	type Output = Vec2;

	fn add(self, rhs: Self) -> Self::Output {	
		Vec2 { x: self.x + rhs.x, y: self.y + rhs.y }
	}
}

impl Sub for Vec2 {
	type Output = Vec2;

	fn sub(self, rhs: Self) -> Self::Output {
		Vec2 { x: self.x - rhs.x, y: self.y - rhs.y }
	}
}

impl Mul<f32> for Vec2 {
	type Output = Vec2;

	fn mul(self, rhs: f32) -> Self::Output {
		Vec2 { x: self.x * rhs, y: self.y * rhs }
	}
}

impl Div<f32> for Vec2 {
	type Output = Vec2;

	fn div(self, rhs: f32) -> Self::Output {
		Vec2 { x: self.x / rhs, y: self.y / rhs }
	}
}

impl fmt::Display for Vec2 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "[{}, {}]", self.x, self.y)
	}
}

#[derive(Debug)]
struct Line {
	origin: Vec2,
	direction: Vec2,
}

impl Line {
	fn distance(self: &Self, point: Vec2) -> f32 {
		let pa =  self.origin - point;
		let shortest = pa - self.direction * (pa.dot(&self.direction));
		shortest.len()
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

struct Player {
	matter: i32,
	tiles: Vec<IVec2>,
	units: Vec<IVec2>,
	border: Vec<(IVec2, i32)>,
	gravity: IVec2,
}

impl Player {
	fn new() -> Self {
		Self {
			matter: 0,
			tiles: Vec::new(),
			units: Vec::new(),
			border: Vec::new(),
			gravity: IVec2::ZERO,
		}
	}
}

const COST: i32 = 10;
const MAX: i32 = 25;

fn main() {
	let inputs = get_inputs();
	let width = parse_input!(inputs[0], usize);
	let height = parse_input!(inputs[1], usize);

	let mut grid: Vec<Vec<Patch>> = (0..height).map(|j| {
		(0..width).map(|i| {
			Patch::from(i as i32, j as i32)
		}).collect()
	}).collect();

	let mut unreachable: HashSet<IVec2> = HashSet::new();

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
					player.units.push(patch.position);
				}
			}
		}

		// Adjacency dependant
		for tile in &ally.tiles {
			let adj: Vec<Patch> = adjacent_movable(&grid, *tile).into_iter().map(|p| get(&grid, &p).clone()).collect();
			let patch = get_mut(&mut grid, tile);
		
			// Calculate recyclable scrap
			patch.scrap_total = adj.iter().fold(patch.scrap, |sum, p| sum + p.scrap);

			// Mark defensive tiles
			if adj.iter().any(|a| a.owner != Owner::Ally) {
				let enemies = adj.iter().filter(|p| p.owner == Owner::Enemy).fold(0, |sum, p| sum + p.units);
			
				ally.border.push((*tile, enemies));
			}
		}

		ally.gravity = ally.units.iter().fold(IVec2::ZERO, |sum, point| sum + *point) / max(ally.units.len() as i32, 1);
		enemy.gravity = enemy.units.iter().fold(IVec2::ZERO, |sum, point| sum + *point) / max(enemy.units.len() as i32, 1);
	
		let frontline = Line {
			origin: (ally.gravity.as_f32() + enemy.gravity.as_f32()) / 2.0,
			direction: (enemy.gravity.as_f32() - ally.gravity.as_f32()).perp().normalize()
		};

		// Sort allied tiles by total scrap for recyclers
		ally.tiles.sort_by(|lhs, rhs| {
			get(&grid, lhs).scrap_total.cmp(&get(&grid, rhs).scrap_total).reverse()
		});

		// Sort allied border tiles by distance to frontline
		ally.border.sort_by(|lhs, rhs| {
			frontline.distance(lhs.0.as_f32()).partial_cmp(&frontline.distance(rhs.0.as_f32())).unwrap_or(std::cmp::Ordering::Equal)
		});

		// Sort allied units by distance to frontline
		ally.units.sort_by(|lhs, rhs| {
			frontline.distance(lhs.as_f32()).partial_cmp(&frontline.distance(rhs.as_f32())).unwrap_or(std::cmp::Ordering::Equal)
		});
	
		// Defending
		for (tile, enemies) in ally.border.iter().filter(|b| b.1 > 0) {
			let enemies = *enemies;
			let patch = get_mut(&mut grid, tile);
			let mut spawned = 0;

			if ally.matter >= COST && patch.can_build
				&& enemies > 1
			{
				BUILD!(actions, tile);
			
				patch.can_build = false;
				patch.can_spawn = false;
				patch.recycler = true;
				ally.matter -= COST;
			}

			if ally.matter >= COST && patch.can_spawn {
				spawned = enemies.clamp(1, ally.matter / COST);
			
				SPAWN!(actions, spawned, tile);
			
				ally.matter -= spawned * COST;
			}

			if enemies > spawned {
				patch.units -= enemies - spawned;
			}
		}

		let distance = (enemy.gravity - ally.gravity).abs();
		let ratio = MAX - distance;

		// Building recyclers on efficient patches
		for tile in &ally.tiles {
			let adj: Vec<Patch> = adjacent_movable(&grid, *tile).into_iter().map(|p| get(&grid, &p).clone()).collect();
			let patch = get(&grid, tile);

			let tiles_cost = 1 + adj.iter().filter(|a| a.scrap <= patch.scrap).count() as i32;
			let efficiency = patch.scrap_total / tiles_cost;
			let gain = adj.iter().fold(patch.scrap, |sum, p| sum + p.scrap.clamp(0, patch.scrap));

			if ally.matter >= COST && patch.can_build
				&& gain > COST
				&& efficiency >= ratio
				&& !near(&grid, *tile, 2).iter().any(|p| get(&grid, p).recycler)
			{
				let patch = get_mut(&mut grid, tile);
			
				BUILD!(actions, tile);
			
				patch.can_build = false;
				patch.can_spawn = false;
				patch.recycler = true;
				ally.matter -= COST;
			}
		}

		// Spawning on border tiles
		for (tile, _) in &ally.border {
			let patch = get(&grid, tile);
		
			if ally.matter >= COST && patch.can_spawn
				&& patch.units == 0 
				&& !unreachable.contains(tile)
			{
				SPAWN!(actions, 1, tile);
				ally.matter -= COST;

			}
		}

		// Moving units
		for unit in &ally.units {
			if unreachable.contains(unit) {
				continue;
			}
		
			let can_reach_enemy = !nearest_where(&grid, unit, |p| p.owner == Owner::Enemy).is_empty();

			if !can_reach_enemy {
				unreachable.insert(*unit);
			}
		
			let allies = get(&grid, unit).units;

			for _ in 0..allies {
				let targets = nearest_where(&grid, unit, |p| p.owner != Owner::Ally);

				if !targets.is_empty() {
					let target = targets.iter().min_by(|lhs, rhs| {
						frontline.distance(lhs.as_f32()).partial_cmp(&frontline.distance(rhs.as_f32())).unwrap_or(std::cmp::Ordering::Equal)
					}).unwrap();

					let patch = get_mut(&mut grid, target);

					if can_reach_enemy {
						if patch.units > 0 {
							patch.units -= 1;
						} else {
							patch.owner = Owner::Ally;
						}
					}

					MOVE!(actions, 1, unit, target);
				}		
			}
		}

		let time = Utc::now().timestamp_millis() - now;

		MESSAGE!(actions, format!("{} ms", time as f64));

		actions.flush();
	}
}

fn get<'a>(grid: &'a Vec<Vec<Patch>>, point: &IVec2) -> &'a Patch {
	&grid[point.y as usize][point.x as usize]
}

fn get_mut<'a>(grid: &'a mut Vec<Vec<Patch>>, point: &IVec2) -> &'a mut Patch {
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

fn nearest_where<F>(grid: &Vec<Vec<Patch>>, point: &IVec2, f: F) -> Vec<IVec2> where
	F: Fn(&Patch) -> bool {
	
	let mut edges = vec![point.clone()];
	let mut visited: BTreeSet<IVec2> = vec![point.clone()].into_iter().collect();
	let mut answer: Vec<IVec2> = Vec::new();
	
	while !edges.is_empty() && answer.is_empty() {
		let mut new: Vec<IVec2> = Vec::new();

		for edge in &edges {
			let patch = get(grid, edge);
		
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

	answer
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
	adjacent(grid, point).into_iter().filter(|p| get(grid, p).scrap > 0 && !get(grid, p).recycler).collect()
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
