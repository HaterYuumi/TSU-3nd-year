extends CharacterBody3D

const JUMP_VEL = 7.0
const JUMP_WEIGHT_FACTOR = 0.15
const GRAVITY_WEIGHT_FACTOR = 0.1

@onready var death_sensor = $DeathSensor

var positions = [-3,0,3]
var curPos = 1

var swipeLength = 100
var startSwipe: Vector2
var curSwipe: Vector2
var swiping = false

var threshold = 20
var swipeDir = 0

var gravity = ProjectSettings.get_setting("physics/3d/default_gravity")

var coins_collected = 0
var weight: float = 0.0

func _process(delta):
	swipe()
	if swipeDir == 1:
		if curPos < 2:
			curPos += 1
			swipeDir = 0
	elif swipeDir == -1:
		if curPos > 0:
			curPos -= 1
			swipeDir = 0
			
	position.z = lerpf(position.z, positions[curPos],delta*30)
	
	if death_sensor.is_colliding():
		death()
	
	var gravity_scale = 1.0 + weight * GRAVITY_WEIGHT_FACTOR
	velocity.y -= gravity*gravity_scale*delta
	move_and_slide()
	
func swipe():
	if Input.is_action_just_pressed("press"):
		if !swiping:
			swiping = true
			startSwipe = get_viewport().get_mouse_position()
			
	if Input.is_action_pressed("press"):
		if swiping:
			curSwipe = get_viewport().get_mouse_position()
			if startSwipe.distance_to(curSwipe) >= swipeLength:
				
				if abs(startSwipe.y-curSwipe.y) < threshold:
					if startSwipe.x-curSwipe.x < 0:
						swipeDir = 1
					else:
						swipeDir = -1
				if abs(startSwipe.x-curSwipe.x) < threshold:
					if startSwipe.y-curSwipe.y > 0 and is_on_floor():
						var effective_jump = JUMP_VEL / (1.0 + weight * JUMP_WEIGHT_FACTOR)
						velocity.y = effective_jump
						
				swiping = false
	else:
		swiping = false

func death():
	get_tree().reload_current_scene()
	

func collect_coin():
	coins_collected += 1
	weight += 0.2
	GameManager.update_speed(weight)
	print("Coins: ", coins_collected, " Weight: ", weight)
