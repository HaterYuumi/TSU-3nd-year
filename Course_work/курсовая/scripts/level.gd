extends Node3D

@export var modules: Array[PackedScene] = []
@export var coin_scene: PackedScene

var amnt = 30
var rng = RandomNumberGenerator.new()
var offset = 6

var initObs = 0

# Called when the node enters the scene tree for the first time.
func _ready():
	rng.randomize()
	for n in amnt:
		spawnModule(n*offset)

const Start_x = 10

func spawnModule(n):
	if initObs > 10:
		rng.randomize()
		var num = rng.randi_range(0,modules.size()-1)
		var instance = modules[num].instantiate()
		instance.position.x = n
		add_child(instance)
	else:
		var instance = modules[0].instantiate()
		instance.position.x = n
		add_child(instance)
		spawn_coin_in_module(n)
		initObs += 1

func spawn_coin_in_module(x_pos: float):
	var num_coins = rng.randi_range(1,3)
	var lanes = [-3, 0, 3]
	
	for i in range(num_coins):
		var lane = lanes[rng.randi_range(0, lanes.size() - 1)]
		var coin = coin_scene.instantiate()
		coin.position = Vector3(x_pos, 0.5, lane)
		add_child(coin)
