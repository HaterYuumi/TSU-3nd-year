extends Area3D

#const SPEED = 15.0
var rotation_speed = 2.0
var lane = 0

func set_lane(value):
	lane = value
	position.z = lane

func _ready():
	body_entered.connect(_on_body_entered)

func _physics_process(delta):
	# Move coin towards player
	position.x -= GameManager.current_speed * delta
	
	# Rotate for visual effect
	rotate_y(rotation_speed * delta)

func _on_body_entered(body):
	var player = body
	while player and not player.has_method("collect_coin"):
		player = player.get_parent()
	if player:
		player.collect_coin()
		queue_free()
