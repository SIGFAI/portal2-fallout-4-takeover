// Fallout 4 Takeover: the recorded demo. Each step shows one feature of the mod, in the order of clip.txt.
// Geometry is relative to the player's spot on the stage (the stage faces +x with the portal panels ahead).

::D <- { p0 = null, f = null, s = null, blue = null, orange = null, a = null, b = null }

::DemoDir <- function(yawDeg) {
	local r = yawDeg / 57.29578
	return Vector(cos(r), sin(r), 0)
}

// Wall point hit by a ray from the player's eyes in a horizontal direction.
::DemoWall <- function(dir) {
	local host = SigfHost()
	local eye = host.EyePosition()
	local tr = TraceLine(eye, eye + dir * 3000, host)
	return eye + dir * (3000.0 * tr)
}

::DemoFloor <- function(p) {
	local g = ::SigfFloorAt(p)
	return g != null ? g : p
}

// 0 s: the takeover. Pip-Boy green, HUD, narrator, statue, radio, butler bot.
SigfDemo(0.2, function() {
	::D.p0 = SigfHost().GetOrigin()
	local yaw = ::SigfYaw
	::D.f = ::DemoDir(yaw)
	::D.s = Vector(-::D.f.y, ::D.f.x, 0)
	::D.orange = ::DemoWall(::DemoDir(yaw + 42.0))
	SigfPortalGun()
	SigfLook(4, yaw)
	local p = ::D.p0
	local f = ::D.f
	local s = ::D.s
	// caps on the player's path (walking through them picks them up)
	local caps = []
	for (local i = 0; i < 3; i++) caps.append(::DemoFloor(p + f * (120 + 85 * i) + s * (-14 + 14 * i)))
	::FoIntro(caps)
	// supply crates (cubes) standing on both sides
	SigfCube(p + f * 300 + s * -230 + Vector(0, 0, 40), 0, 0, null)
	SigfCube(p + f * 330 + s * 230 + Vector(0, 0, 40), 0, 0, null)
	// the camera starts on the giant bobblehead, then turns to the caps on the path
	SigfIn(1.0, function() { SigfLookAt(::D.p0 + ::D.f * 400 + ::D.s * 210 + Vector(0, 0, 90)) })
})

// 3 s: Nuka bottles fall from the ceiling
SigfDemo(3.0, function() {
	local p = ::D.p0
	for (local i = 0; i < 3; i++) ::FoBottle(p + ::D.f * (230 + 80 * i) + ::D.s * (-90 + 90 * i) + Vector(0, 0, 380))
})

// 6.8 s: the camera turns to the caps, then the player walks through them: the CAPS counter goes up
SigfDemo(6.8, function() { SigfLookAt(::D.p0 + ::D.f * 330 + Vector(0, 0, -20)) })
SigfDemo(7.8, function() { SigfWalk(1.6) })

// 10.6 s: a radroach crawls in; the butler bot torches it
SigfDemo(10.6, function() {
	local p = SigfHost().GetOrigin()
	SigfCaption("RADROACH!", 2.5)
	SigfLookAt(p + ::D.f * 400 + Vector(0, 0, -20))
	::FoSpawnRoach(::DemoFloor(p + ::D.f * 400 + ::D.s * 60))
	// the second one dozes next to the future portal exit
	::FoSpawnRoach(::DemoFloor(::D.orange - ::D.s * 240), 60.0)
})

// 16 s: real portals: blue on the far panel, orange on the side panel
SigfDemo(16.2, function() {
	SigfCaption("PORTALS STILL WORK", 2.5)
	::D.blue = ::DemoWall(::D.f)
	SigfFirePortal(false, ::D.blue)
})
SigfDemo(17.6, function() {
	SigfFirePortal(true, ::D.orange)
})

// 19.4 s: a Vault-Tec crate goes into the blue portal and comes out of the orange one, flattening the sleeper
SigfDemo(19.4, function() {
	local host = SigfHost()
	SigfCaption("CRATE THROUGH THE PORTAL", 3)
	SigfLookAt(::D.blue * 0.55 + ::D.orange * 0.45)
	local from = host.EyePosition() + ::D.f * 210 + Vector(0, 0, -45)
	local to = ::D.blue - ::D.f * 20
	SigfCube(from, 0, 0, function(c):(from, to) {
		local d = to - from
		local flat = sqrt(d.x * d.x + d.y * d.y)
		local t = flat / 1100.0
		local v = Vector(d.x / t, d.y / t, (d.z + 300.0 * t * t) / t)
		SigfIn(1.3, function():(c, v) { if (::FoOk(c)) SigfPush(c, v) })
	})
})

// 24.5 s: the mini nuke falls through a rift and keeps its speed
SigfDemo(24.8, function() {
	local p = SigfHost().GetOrigin()
	::D.a = ::DemoFloor(p + ::D.f * 480 + ::D.s * -140)
	::D.b = ::DemoFloor(p + ::D.f * 560 + ::D.s * 120)
	::FoRift(::D.a, ::D.b, 18.0, { radius = 130, boost = 0.5, cooldown = 3.0, onEnter = function(ent, from, to) {
		if (ent.GetModelName().find("fatman") != null) SigfText("THROUGH THE PORTAL!", -1, 0.62, 2, "120 255 120", 4)
	} })
	SigfLookAt(::D.a + Vector(0, 0, 150))
	::FoNuke(::D.a)
})
SigfDemo(26.2, function() { SigfLookAt(::D.b + Vector(0, 0, 300)) })

// 36 s: V.A.T.S.: more roaches, slow motion, critical hits
SigfDemo(35.5, function() {
	local p = SigfHost().GetOrigin()
	SigfLookAt(p + ::D.f * 420 + Vector(0, 0, -10))
	for (local i = 0; i < 3; i++) ::FoSpawnRoach(::DemoFloor(p + ::D.f * 430 + ::D.s * (-170 + 170 * i)))
})
SigfDemo(40.0, function() { ::FoVats() })

// 47 s: encore. Bottles rain again, roaches swarm, a second nuke lands without any portal.
SigfDemo(47.0, function() {
	local p = SigfHost().GetOrigin()
	SigfCaption("FALLOUT 4 TAKEOVER", 4)
	SigfLookAt(p + ::D.f * 420 + ::D.s * 140 + Vector(0, 0, 10))
	for (local i = 0; i < 4; i++) ::FoBottle(p + ::D.f * (260 + 70 * i) + ::D.s * (-120 + 80 * i) + Vector(0, 0, 360))
})
SigfDemo(51.0, function() {
	local p = SigfHost().GetOrigin()
	for (local i = 0; i < 3; i++) ::FoSpawnRoach(::DemoFloor(p + ::D.f * 480 + ::D.s * (-200 + 200 * i)))
	SigfCaption("MORE RADROACHES", 2.5)
})
SigfDemo(57.0, function() {
	local p = SigfHost().GetOrigin()
	SigfLookAt(p + ::D.f * 450 + Vector(0, 0, 120))
	::FoNuke(::DemoFloor(p + ::D.f * 450 + ::D.s * -40))
})
SigfDemo(66.0, function() {
	SigfCaption("WAR NEVER CHANGES", 4)
	SigfLookAt(::D.p0 + ::D.f * 450 + Vector(0, 0, 160))
})
