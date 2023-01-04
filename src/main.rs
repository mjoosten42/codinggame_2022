use std::{collections::{BTreeSet, HashSet}, fmt, ops::{Add, AddAssign, Div, Sub, DivAssign}};
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
		self.actions.push(action);
	}

	fn flush(&mut self) {
		if self.actions.is_empty() {
			self.actions.push("WAIT".to_string());
		}

		for action in &self.actions {
			print!("{action};");
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
		IVec2 { x, y }
	}

	fn from(x: usize, y: usize) -> Self {
		IVec2 { x: x as i32, y: y as i32 }
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
		write!(f, "[{:>2}, {:>2}]", self.x, self.y)
	}
}

impl fmt::Debug for IVec2 {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "[{:>2}, {:>2}]", self.x, self.y)
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
	ally: i32,
	enemy: i32,
	value: i32,

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
			ally: 0,
			enemy: 0,
			value: 0,
		
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
	fn new(pos: IVec2) -> Self {
		Self {
			pos,
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
}

impl Player {
	fn new() -> Self {
		Self {
			matter: 0,
			recyclers: 0,
			tiles: Vec::new(),
			units: Vec::new(),
			border: Vec::new(),
		}
	}
}

const COST: i32 = 10;

fn main() {
	let inputs = get_inputs();
	let width = parse_input!(inputs[0], usize);
	let height = parse_input!(inputs[1], usize);

	let mut grid: Vec<Vec<Patch>> = (0..height).map(|j| {
		(0..width).map(|i| {
			Patch::from(i as i32, j as i32)
		}).collect()
	}).collect();

	let mut first_turn = true;

	loop {
		let mut ally = Player::new();
		let mut enemy = Player::new();
		let mut neutral = Player::new();
		let mut actions = Actions::new();
		let mut minimum_gain = COST;

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

		// Adjacency dependant
		for tile in &ally.tiles {
			let adj: Vec<Patch> = adjacent_patches(&grid, tile);
			let patch = get_mut(&mut grid, tile);
		
			// Calculate recyclable scrap
			patch.scrap_total = adj.iter().fold(patch.scrap, |sum, p| sum + p.scrap);

			// Mark border tiles
			if !patch.recycler && adj.iter().any(|p| p.owner == Owner::Enemy) {
				let mut border = Border::new(*tile);
			
				let tiles = adj.iter().filter(|p| p.owner == Owner::Enemy);

				border.allies = patch.units;
				border.tiles = tiles.clone().map(|tile| tile.pos).collect();
				border.enemies = tiles.fold(0, |sum, p| sum + p.units);

				if border.tiles.len() == 1 {
					border.supporting = adjacent(&grid, border.tiles.first().unwrap()).into_iter()
						.filter(|t| *t != *tile)
							.any(|t| get(&grid, &t).owner == Owner::Ally);
				}

				ally.border.push(border);
			}
		}

		timestamps.push(Utc::now().timestamp_millis());

		if first_turn {
			let ally_spawn = ally.units.iter().fold(IVec2::ZERO, |sum, point| sum + *point) / ally.units.len() as i32;
			let enemy_spawn = enemy.units.iter().fold(IVec2::ZERO, |sum, point| sum + *point) / enemy.units.len() as i32;

			bfs_for_each(&mut grid, &ally_spawn, |patch, i| patch.ally = i);
			let max = bfs_for_each(&mut grid, &enemy_spawn, |patch, i| patch.enemy = i);

			for y in 0..height {
				for x in 0..width {
					let patch = get_mut(&mut grid, &IVec2::from(x, y));

					patch.value = (max - patch.enemy) * patch.ally;	
		
					if patch.scrap > 0 {
						eprint!("{:>3} ", patch.value);
					} else {
						eprint!("    ");
					}
				}
				eprintln!("");
			}

			timestamps.push(Utc::now().timestamp_millis());
		}

		// Recylers
		{
			let mut new_recyclers: HashSet<IVec2> = HashSet::new();
		
			for border in &ally.border {
				let patch = get_mut(&mut grid, &border.pos);
				
				if ally.matter >= COST && patch.can_build && border.enemies > 1 {
					BUILD!(actions, border.pos);
				
					patch.can_build = false;
					patch.can_spawn = false;
					patch.recycler = true;
					ally.matter -= COST;

					new_recyclers.insert(border.pos);
				}
			}

			ally.border.retain(|border| !new_recyclers.contains(&border.pos));

			// Sort allied tiles by value, to avoid blocking allied units
			ally.tiles.sort_by(|lhs, rhs| {
				get(&grid, lhs).value.cmp(&get(&grid, rhs).value)
			});

			// Building recyclers on efficient patches
			for tile in &ally.tiles {
				let patch = get(&grid, tile);
				let adj: Vec<Patch> = adjacent_patches(&grid, tile);
	
				let tiles_cost = adj.iter().fold(1, |sum, a| sum + (a.scrap <= patch.scrap) as i32);
				let efficiency = patch.scrap_total / tiles_cost;
				let gain = adj.iter().filter(|p| !p.in_range_of_recycler).fold(patch.scrap, |sum, p| sum + p.scrap.clamp(0, patch.scrap));
	
				if ally.matter >= COST && patch.can_build
					&& gain > 2 * COST
					&& efficiency >= COST
					&& adj.iter().all(|p| p.scrap >= 2)
				{
					let patch = get_mut(&mut grid, tile);
				
					BUILD!(actions, tile);
				
					patch.can_build = false;
					patch.can_spawn = false;
					patch.recycler = true;
					ally.matter -= COST;
				}
			}
		}

		timestamps.push(Utc::now().timestamp_millis());

		// Spawning
		ally.border.sort_by(|lhs, rhs| {
			get(&grid, &lhs.pos).value.cmp(&get(&grid, &rhs.pos).value).reverse()
		});

		// Next to enemies
		for border in &mut ally.border {
			let patch = get_mut(&mut grid, &border.pos);

			while ally.matter >= COST && patch.can_spawn
				&& border.tiles.len() == 1
				&& border.allies < border.enemies
			{		
				SPAWN!(actions, 1, border.pos);

				border.allies += 1;
				ally.matter -= COST;
			}
		}


		ally.tiles.sort_by(|lhs, rhs| {
			get(&grid, lhs).value.cmp(&get(&grid, rhs).value).reverse()
		});

		for tile in &ally.tiles {
			let patch = get(&grid, tile);

			if ally.matter >= COST && patch.can_spawn {
				SPAWN!(actions, 1, tile);

				ally.matter -= COST;

				if let Some(border) = ally.border.iter_mut().find(|border| border.pos == *tile) {
					border.allies += 1;
				}
			}
		}

		timestamps.push(Utc::now().timestamp_millis());
	
		for unit in &ally.units {
			let adj = adjacent_patches(&grid, unit);

			if let Some(border) = ally.border.iter_mut().find(|border| border.pos == *unit) {
				if border.allies <= border.enemies && border.tiles.len() == 1 && !border.supporting {
					continue ;
				}
			}

			if let Some(patch) = adj.iter().max_by(|lhs, rhs| lhs.value.cmp(&rhs.value)) {
				MOVE!(actions, 1, unit, patch.pos);

				if let Some(border) = ally.border.iter_mut().find(|border| border.pos == *unit) {
					border.allies -= 1;
					
					if let Some(to) = ally.border.iter_mut().find(|border| border.pos == patch.pos) {
						to.allies += 1;
					}
				}

				// eprintln!("{unit} -> {}", patch.pos);
			}
		}
	
		timestamps.push(Utc::now().timestamp_millis());

		first_turn = false;
	
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

fn bfs<F>(grid: &Vec<Vec<Patch>>, point: &IVec2, f: F) -> (Vec<IVec2>, i32)
where
	F: Fn(&Patch) -> bool {
	
	let mut edges = vec![point.clone()];
	let mut visited: BTreeSet<IVec2> = vec![point.clone()].into_iter().collect();
	let mut answer: Vec<IVec2> = Vec::new();
	let mut i = 0;
	
	while !edges.is_empty() && answer.is_empty() {
		let mut new: Vec<IVec2> = Vec::new();

		for edge in &edges {
			let patch = get(grid, edge);
		
			if f(patch) {
				answer.push(*edge);
			}
			let nb = adjacent(grid, edge);
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

	answer.sort();
	answer.dedup();
	(answer, i)
}

fn bfs_for_each<F>(grid: &mut Vec<Vec<Patch>>, point: &IVec2, mut f: F) -> i32
where
	F: FnMut(&mut Patch, i32)
{
	let mut edges = vec![point.clone()];
	let mut visited: HashSet<IVec2> = vec![point.clone()].into_iter().collect();
	let mut i = 0;
	
	while !edges.is_empty() {
		let mut new: Vec<IVec2> = Vec::new();

		for edge in edges {
			let patch = get_mut(grid, &edge);

			f(patch, i);

			for point in adjacent(grid, &edge) {
				if !visited.contains(&point) {
					new.push(point);
					visited.insert(point);
				}
			}
		}
		edges = new;
		i += 1;
	}

	i
}

fn adjacent_all(grid: &Vec<Vec<Patch>>, point: &IVec2) -> Vec<IVec2> {
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

fn adjacent(grid: &Vec<Vec<Patch>>, point: &IVec2) -> Vec<IVec2> {
	adjacent_all(grid, point).into_iter().filter(|p| get(grid, p).scrap > 0 && !get(grid, p).recycler).collect()
}

fn adjacent_patches(grid: &Vec<Vec<Patch>>, point: &IVec2) -> Vec<Patch> {
	adjacent(grid, point).iter().map(|p| get(grid, p).clone()).collect()
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
