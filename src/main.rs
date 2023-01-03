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

	fn abs(self: &Self) -> i32 {
		self.x.abs() + self.y.abs()
	}

	fn as_f32(self) -> Vec2 {
		Vec2 { x: self.x as f32, y: self.y as f32 }
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
	const ZERO: Self = Self { x: 0.0, y: 0.0 };

	fn dot(self, other: &Self) -> f32 {
		self.x * other.x + self.y * other.y
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

	fn round(self: &mut Self) {
		self.x = self.x.round();
		self.y = self.y.round();
	}
}

impl Add for Vec2 {
	type Output = Vec2;

	fn add(self, rhs: Self) -> Self::Output {	
		Vec2 { x: self.x + rhs.x, y: self.y + rhs.y }
	}
}

impl AddAssign for Vec2 {
	fn add_assign(&mut self, rhs: Self) {
		self.x += rhs.x;
		self.y += rhs.y;
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
	fn new() -> Self {
		Self { origin: Vec2::ZERO, direction: Vec2::ZERO }
	}

	fn distance(self: &Self, point: Vec2) -> f32 {
		let pa =  point - self.origin;
		let shortest = pa - self.direction * pa.dot(&self.direction);
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
	pos: IVec2,
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
			pos: IVec2::new(x, y),
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

#[derive(Clone, Debug)]
struct Border {
	pos: IVec2,
	allies: i32,
	enemies: i32,
	supporting: bool,
	tiles: Vec<IVec2>,
}

impl Border {
	fn new(pos: &IVec2) -> Self {
		Self {
			pos: *pos,
			allies: 0,
			enemies: 0,
			supporting: false,
			tiles: Vec::new(),
		}
	}
}

struct Player {
	matter: i32,
	recyclers: i32,
	tiles: Vec<IVec2>,
	units: Vec<IVec2>,
	border: Vec<Border>,
	gravity: IVec2,
}

impl Player {
	fn new() -> Self {
		Self {
			matter: 0,
			recyclers: 0,
			tiles: Vec::new(),
			units: Vec::new(),
			border: Vec::new(),
			gravity: IVec2::ZERO,
		}
	}
}

const COST: i32 = 10;
const RATIO: i32 = 15;

fn main() {
	let inputs = get_inputs();
	let width = parse_input!(inputs[0], usize);
	let height = parse_input!(inputs[1], usize);

	let mut grid: Vec<Vec<Patch>> = (0..height).map(|j| {
		(0..width).map(|i| {
			Patch::from(i as i32, j as i32)
		}).collect()
	}).collect();

	let mut frontline = Line::new();
	let mut first_turn = true;
	let mut trench: Vec<IVec2> = Vec::new();

	loop {
		let mut ally = Player::new();
		let mut enemy = Player::new();
		let mut neutral = Player::new();
		let mut actions = Actions::new();

		let matter = update_input(&mut grid, width, height);
	
		let mut timestamps = vec![Utc::now().timestamp_millis()];

		ally.matter = matter.0;
		enemy.matter = matter.1;
	
		// Map data
		for y in 0..height {
			for x in 0..width {
				let patch = &mut grid[y][x];
			
				if patch.scrap == 0 {
					continue;
				}
			
				let player = match patch.owner {
					Owner::Ally => &mut ally,
					Owner::Enemy => &mut enemy,
					Owner::Neutral => &mut neutral,
				};

				// Mark owned tiles
				player.tiles.push(patch.pos);

				// Mark units
				for _ in 0..patch.units {
					player.units.push(patch.pos);
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
			let adj: Vec<Patch> = adjacent_movable(&grid, tile).into_iter().map(|p| get(&grid, &p).clone()).collect();
			let patch = get_mut(&mut grid, tile);
		
			// Calculate recyclable scrap
			patch.scrap_total = adj.iter().fold(patch.scrap, |sum, p| sum + p.scrap);

			// Mark border tiles
			if adj.iter().any(|p| p.owner != patch.owner) {
				let mut border = Border::new(tile);
			
				let tiles = adj.iter().filter(|p| p.owner == Owner::Enemy);

				border.tiles = tiles.clone().map(|tile| tile.pos).collect();
				border.enemies = tiles.fold(0, |sum, p| sum + p.units);

				if border.tiles.len() == 1 {
					border.supporting = adjacent_movable(&grid, &border.tiles[0]).into_iter()
						.filter(|t| *t != *tile)
							.any(|t| get(&grid, &t).owner == Owner::Ally);
				}

				ally.border.push(border);
			}
		}

		for a in &ally.border {
			eprintln!("{a:?}");
		}

		// Sort allied tiles by total scrap for recyclers
		ally.tiles.sort_by(|lhs, rhs| {
			get(&grid, lhs).scrap_total.cmp(&get(&grid, rhs).scrap_total).reverse()
		});

		// Sort allied border tiles by units difference, then distance to enemy
		ally.border.sort_by(|lhs, rhs| {
			let mut cmp = (&lhs.enemies - &lhs.allies).cmp(&(&rhs.enemies - &rhs.allies)).reverse();

			if cmp == std::cmp::Ordering::Equal {
				cmp = (lhs.pos - enemy.gravity).abs().cmp(&((rhs.pos - enemy.gravity).abs()));
			}
			
			cmp
		});

		// Sort allied units by distance to frontline
		ally.units.sort_by(|lhs, rhs| {
			let first = bfs(&grid, lhs, |tile| trench.contains(&tile.pos)).1;
			let second = bfs(&grid, rhs, |tile| trench.contains(&tile.pos)).1;
			
			first.cmp(&second)
		});
	
		timestamps.push(Utc::now().timestamp_millis());

		if first_turn {
			first_turn = false;

			let mut center = (ally.gravity.as_f32() + enemy.gravity.as_f32()) / 2.0;

			if ((ally.gravity.x - enemy.gravity.x) % 2).abs() == 1 || ((ally.gravity.y - enemy.gravity.y) % 2).abs() == 1 {
				center += (ally.gravity.as_f32() - center).normalize() / 2.0;
				center.round();
			}

			frontline.origin = center;
			frontline.direction = (ally.gravity.as_f32() - enemy.gravity.as_f32()).perp().normalize();

			for tile in &neutral.tiles {
				if frontline.distance(tile.as_f32()) < 0.55 {
					trench.push(*tile);
				}
			}
		
			timestamps.push(Utc::now().timestamp_millis());
		}

		// Remove claimed trench tiles
		trench.retain(|tile| !ally.tiles.contains(tile));
		
		eprintln!("{trench:?}");
	
		timestamps.push(Utc::now().timestamp_millis());
	
		// Defending
		for border in ally.border.iter_mut().filter(|b| b.enemies > 0 && b.tiles.len() == 1) {
			let patch = get_mut(&mut grid, &border.pos);
			
			if ally.matter >= COST && patch.can_build && border.enemies > 1 {
				BUILD!(actions, border.pos);
			
				patch.can_build = false;
				patch.can_spawn = false;
				patch.recycler = true;
				ally.matter -= COST;
			}

			if ally.matter >= COST && patch.can_spawn{
				let diff = border.enemies - border.allies;
				let amount = diff.clamp(0, ally.matter / COST);
			
				if amount > 0 {
					SPAWN!(actions, amount, border.pos);

					border.allies += amount;
					ally.matter -= amount * COST;
				}
			}
		}

		timestamps.push(Utc::now().timestamp_millis());

		// Building recyclers on efficient patches
		for tile in &ally.tiles {
			let adj: Vec<Patch> = adjacent_movable(&grid, tile).into_iter().map(|p| get(&grid, &p).clone()).collect();
			let patch = get(&grid, tile);

			let tiles_cost = 1 + adj.iter().filter(|a| a.scrap <= patch.scrap).count() as i32;
			let efficiency = patch.scrap_total / tiles_cost;
			let gain = adj.iter().fold(patch.scrap, |sum, p| sum + p.scrap.clamp(0, patch.scrap));

			if ally.matter >= COST && patch.can_build
				&& gain > COST
				&& efficiency >= RATIO
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

		timestamps.push(Utc::now().timestamp_millis());

		// Spawning on border tiles
		for border in ally.border.iter_mut() {
			let patch = get(&grid, &border.pos);

			if ally.matter >= COST && patch.can_spawn {
				SPAWN!(actions, 1, border.pos);
	
				border.allies += 1;
				ally.matter -= COST;

			}
		}

		timestamps.push(Utc::now().timestamp_millis());

		let mut claimed: HashSet<IVec2> = HashSet::new();

		for unit in &ally.units {		
			let border = ally.border.iter_mut().find(|border| border.pos == *unit);
		
			if border.is_some() {
				let border = border.unwrap();

				if border.enemies > border.allies && border.tiles.len() == 1 && !border.supporting {
					eprintln!("{border:?}");
				
					border.allies += 1;
					continue;
				}

				if border.supporting {
					MOVE!(actions, 1, unit, border.tiles.first().unwrap());
					continue;
				}
			}

			let targets = bfs(&grid, unit, |tile| {
				trench.contains(&tile.pos) && !claimed.contains(&tile.pos)
			}).0;

			if !targets.is_empty() {
				let target = targets.first().unwrap();
			
				eprintln!("{unit} -> {target}");

				claimed.insert(*target);

				if *unit != *target {
					MOVE!(actions, 1, unit, target);
				}
			}
		}

		timestamps.push(Utc::now().timestamp_millis());

		// for unit in &ally.units {
		// 	let allies = get(&grid, unit).units;
				
		// 	for _ in 0..allies {
		// 		let mut targets = bfs(&grid, unit, |p| {
		// 			p.owner != Owner::Ally && !claimed.contains(&p.pos)
		// 		});

		// 		if targets.is_empty() {
		// 			targets = bfs(&grid, unit, |p| {
		// 				p.owner != Owner::Ally
		// 			});
		// 		}

		// 		if !targets.is_empty() {	
		// 			let target = targets.iter().min_by(|&lhs, &rhs| {
		// 				(*lhs - enemy.gravity).abs().cmp(&((*rhs - enemy.gravity).abs()))
		// 			}).unwrap();

		// 			let patch = get_mut(&mut grid, target);

		// 			if patch.units > 0 {
		// 				patch.units -= 1;
		// 			} else {
		// 				claimed.insert(*target);
		// 			}
				
		// 			MOVE!(actions, 1, unit, target);
		// 		}
		// 	}
		// }

		timestamps.push(Utc::now().timestamp_millis());

		let mut message = timestamps.iter().enumerate().skip(1).fold(String::new(), |sum, (index, &time)| {
			format!("{sum}{}:", (time - timestamps[index - 1]).to_string())
		});

		message.pop();
	
		MESSAGE!(actions, message);

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
			new.extend(adjacent(grid, p).into_iter());
		}
		points.extend(new.into_iter());
	}
	
	points.into_iter().collect()
}

fn bfs<F>(grid: &Vec<Vec<Patch>>, point: &IVec2, f: F) -> (Vec<IVec2>, i32)
where
	F: Fn(&Patch) -> bool {
	
	let mut edges = vec![point.clone()];
	let mut visited: BTreeSet<IVec2> = vec![point.clone()].into_iter().collect();
	let mut answer: Vec<IVec2> = Vec::new();
	let mut i = -1;
	
	while !edges.is_empty() && answer.is_empty() {
		let mut new: Vec<IVec2> = Vec::new();

		for edge in &edges {
			let patch = get(grid, edge);
		
			if f(patch) {
				answer.push(*edge);
			}
			let nb = adjacent_movable(grid, edge);
			for point in nb {
				if !visited.contains(&point) {
					new.push(point);
				}
			}
			visited.insert(*edge);
		}
		edges = new;
		i += 1;
	}

	(answer, i)
}

fn adjacent(grid: &Vec<Vec<Patch>>, point: &IVec2) -> Vec<IVec2> {
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

fn adjacent_movable(grid: &Vec<Vec<Patch>>, point: &IVec2) -> Vec<IVec2> {
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
