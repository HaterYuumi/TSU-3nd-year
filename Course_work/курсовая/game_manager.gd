extends Node

var base_speed: float = 15.0
var current_speed: float = base_speed

@export var speed_weight_factor: float = 0.1

func update_speed(weight: float) -> void:
	current_speed = base_speed / (1.0 + weight * speed_weight_factor)
	current_speed = max(current_speed, 5.0)
