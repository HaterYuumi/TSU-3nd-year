extends Node3D

@onready var level = $"../"
#var speed = 15

func _physics_process(delta):
	var current_speed = GameManager.current_speed
	position.x -= current_speed * delta
	if position.x < -15:
		#level.spawnModule(position.x*(level.amnt*level.offset))
		queue_free()
