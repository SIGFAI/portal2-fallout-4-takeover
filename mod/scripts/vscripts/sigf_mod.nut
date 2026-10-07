// Fallout 4 Takeover: the test chamber becomes a Commonwealth wasteland ruin.
// Pip-Boy green haze and HUD, a giant vault bobblehead, Nuka bottles and caps, a butler bot, radroaches,
// V.A.T.S. slow motion and mini nukes that fall through portals. All models and sounds are generated for this mod.

::FO <- {
	caps = 0, rads = 0, hp = 100, xp = 0, kills = 0, level = 1,
	roaches = [], pickups = [], handy = null, statue = null, bomb = null,
	last = {}, fade = null, shake = null, tickOn = false, vats = false, nukeBusy = false, nextRoach = 0.0,
	hurtAt = 0.0, burn = [], turrets = [], cloud = null, cloudEnt = null, riftEnts = [], rift = null, crated = {}, cratesOn = false, crateAt = 0.0
}

// ---------- small helpers ----------

::FoYaw <- function(from, to) { return atan2(to.y - from.y, to.x - from.x) * 57.29578 }
::FoDist <- function(a, b) { return (a - b).Length() }
::FoFlat <- function(a, b) { return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y)) }
::FoOk <- function(e) { return e != null && e.IsValid() }
::FoMdl <- function(n) { return "models/sigf/" + n + ".mdl" }

// Spot on the floor at random inside the room, away from the player.
::FoSpot <- function(minDist) {
	local host = SigfHost()
	for (local i = 0; i < 10; i++) {
		local p = SigfGround()
		if (::FoFlat(p, host.GetOrigin()) > minDist) return p
	}
	return SigfGround()
}

::FoSay <- function(text, sec) { SigfText(text, -1, 0.80, sec, "255 240 150", 2) }

::FoSound <- function(n) { SigfSound("sigf/" + n + ".wav") }

// ---------- screen: Pip-Boy green, HUD ----------

::FoAtmosphere <- function() {
	SigfCmd("r_screenoverlay sigf/ov_pip")
	SigfCmd("fog_override 1; fog_enable 1; fog_color 70 100 50; fog_colorskybox 70 100 50; fog_start 250; fog_end 2200; fog_maxdensity 0.45")
	SigfCmd("mat_ambient_light_r 0.10; mat_ambient_light_g 0.16; mat_ambient_light_b 0.08")
}

::FoCompass <- function() {
	local f = SigfHost().GetForwardVector()
	local yaw = atan2(f.y, f.x) * 57.29578
	if (yaw < 0) yaw += 360.0
	local names = ["E", "NE", "N", "NW", "W", "SW", "S", "SE"]
	// heading: yaw 0 = east, 90 = north
	local idx = ((yaw + 22.5) / 45.0).tointeger() % 8
	local mid = names[idx]
	local left = names[(idx + 1) % 8]
	local right = names[(idx + 7) % 8]
	return left + "   .   " + mid + "   .   " + right
}

::FoHud <- function() {
	local hpBars = ""
	local n = (::FO.hp / 10.0).tointeger()
	for (local i = 0; i < 10; i++) hpBars += (i < n) ? "|" : "."
	SigfText("HP " + ::FO.hp.tointeger() + "/100  [" + hpBars + "]", 0.05, 0.90, 1.2, "110 255 110", 1)
	SigfText("CAPS " + ::FO.caps + "   RADS " + ::FO.rads + "   LVL " + ::FO.level, 0.50, 0.90, 1.2, "110 255 110", 3)
	SigfText(::FoCompass(), -1, 0.035, 1.2, "110 255 110", 0)
}

// Full-screen flash: color "255 255 255", alpha 0..255, fade time, hold time
::FoFlash <- function(color, alpha, fadeSec, holdSec) {
	local e = ::FO.fade
	if (!::FoOk(e)) return
	e.__KeyValueFromString("rendercolor", color)
	e.__KeyValueFromInt("renderamt", alpha)
	e.__KeyValueFromFloat("duration", fadeSec)
	e.__KeyValueFromFloat("holdtime", holdSec)
	EntFireByHandle(e, "Fade", "", 0.0, null, null)
}

::FoShake <- function(amp, sec) {
	local e = ::FO.shake
	if (!::FoOk(e)) return
	e.__KeyValueFromFloat("amplitude", amp)
	e.__KeyValueFromFloat("duration", sec)
	EntFireByHandle(e, "StartShake", "", 0.0, null, null)
}

// ---------- stats ----------

::FoCaps <- function(n, pos) {
	::FO.caps += n
	::FoSound("caps")
	SigfText("+" + n + " CAPS", -1, 0.62, 1.2, "255 220 80", 4)
}

::FoXp <- function(n) {
	::FO.xp += n
	local need = ::FO.level * 30
	if (::FO.xp >= need) {
		::FO.xp -= need
		::FO.level++
		::FoSound("levelup")
		SigfCaption("LEVEL UP!  LVL " + ::FO.level, 3)
		local perks = ["PERK: PORTAL GUN MASTERY", "PERK: NUKA-GRAVITY", "PERK: RAD RESISTANT", "PERK: LONE WANDERER", "PERK: GUN NUT (NO GUN)"]
		SigfText(perks[::FO.level % perks.len()], -1, 0.34, 3, "255 220 80", 4)
	}
}

::FoRads <- function(n) {
	::FO.rads += n
	if (::FO.rads > 999) ::FO.rads = 999
	if (n > 5) ::FoSound("geiger")
}

// ---------- spawning ----------

// A spinning pickup cap floating above the floor.
::FoCap <- function(pos, value) {
	SigfProp(::FoMdl("cap"), pos + Vector(0, 0, 36), 0, function(e):(value) {
		EntFireByHandle(e, "DisableMotion", "", 0.0, null, null)
		::FO.pickups.append({ e = e, kind = "cap", value = value, born = Time(), spin = RandomFloat(0, 360) })
	})
}

// Soda bottle: falls from above, can be flung through portals; walking into it drinks it.
::FoBottle <- function(pos) {
	SigfProp(::FoMdl("nuka"), pos, 0, function(e) {
		e.SetAngles(RandomInt(-30, 30), RandomInt(0, 360), RandomInt(-30, 30))
		::FO.pickups.append({ e = e, kind = "bottle", value = 25, born = Time(), spin = 0.0 })
	})
}

::FoStatue <- function() {
	local host = SigfHost()
	local f = SigfForward()
	local s = Vector(-f.y, f.x, 0)
	local p = ::SigfFloorAt(host.GetOrigin() + f * 400 + s * 210)
	if (p == null) p = host.GetOrigin() + f * 300
	SigfProp(::FoMdl("bobble"), p + Vector(0, 0, 14), 0, function(e):(p, host) {
		::FO.statue = e
		e.SetAngles(0, ::FoYaw(p, host.GetOrigin()), 0)
		EntFireByHandle(e, "DisableMotion", "", 0.0, null, null)
		e.SetOrigin(p)
	})
}

::FoHandy <- function() {
	local host = SigfHost()
	local p = host.GetOrigin() + SigfForward() * 160 + Vector(0, 0, 90)
	SigfProp(::FoMdl("handy"), p, 0, function(e) {
		::FO.handy = { e = e, ang = 200.0, say = 0.0, torch = 0.0, bob = 0.0 }
		EntFireByHandle(e, "DisableMotion", "", 0.3, null, null)
	})
}


// ---------- cubes become vault-tec supply crates ----------

::FoCrates <- function() {
	if (!::FO.cratesOn) return
	for (local c = Entities.FindByClassname(null, "prop_weighted_cube"); c != null; c = Entities.FindByClassname(c, "prop_weighted_cube")) {
		local id = c.entindex()
		if (id in ::FO.crated) continue
		::FO.crated[id] <- true
		c.SetModel(::FoMdl("crate"))
	}
}

// ---------- main tick (20 times per second) ----------

::FoPickups <- function(now) {
	local host = SigfHost()
	local hp = host.GetOrigin()
	local keep = []
	foreach (p in ::FO.pickups) {
		if (!::FoOk(p.e)) continue
		local o = p.e.GetOrigin()
		local done = false
		if (p.kind == "cap") {
			p.spin += 9.0
			local bob = sin((now - p.born) * 3.0) * 4.0
			p.e.SetAngles(0, p.spin, 0)
			p.e.SetOrigin(Vector(o.x, o.y, p.e.GetOrigin().z))
			local d = ::FoDist(o + Vector(0, 0, -20), hp)
			if (!::SigfDemoMode && d < 450 && d > 80) {
				// loot magnet: caps near the player fly to them (the free-running show has a player who stands still)
				local dir = (hp + Vector(0, 0, 30)) - o
				dir.Norm()
				p.e.SetOrigin(o + dir * 7.0)
			}
			if (d < 80) { ::FoCaps(p.value, o); done = true }
		} else if (p.kind == "bottle") {
			if (::FoDist(o, hp + Vector(0, 0, 30)) < 70) {
				::FoSound("bottle_pop")
				::FO.hp = ::FO.hp + 25 > 100 ? 100 : ::FO.hp + 25
				::FoRads(-15)
				if (::FO.rads < 0) ::FO.rads = 0
				SigfText("NUKA-POP!  HP +25  RADS -15", -1, 0.62, 1.5, "255 120 90", 4)
				::FoXp(5)
				done = true
			}
		}
		if (done) EntFireByHandle(p.e, "Kill", "", 0.0, null, null)
		else keep.append(p)
	}
	::FO.pickups = keep
}

::FoHandyTick <- function(now) {
	local h = ::FO.handy
	if (h == null || !::FoOk(h.e)) return
	local host = SigfHost()
	local hp = host.GetOrigin()
	h.ang += 1.6
	local r = h.ang / 57.29578
	local target = hp + Vector(cos(r) * 170, sin(r) * 170, 95 + sin(now * 2.2) * 10)
	local cur = h.e.GetOrigin()
	local nxt = cur + (target - cur) * 0.12
	h.e.SetOrigin(nxt)
	h.e.SetAngles(sin(now * 1.7) * 6, ::FoYaw(nxt, hp), sin(now * 2.3) * 5)
}

::FoTick <- function() {
	local now = Time()
	if (!::SigfLive) return
	::FoPickups(now)
	::FoHandyTick(now)
	::FoEnemyTick(now)
	if (now > ::FO.crateAt) { ::FO.crateAt = now + 0.5; ::FoCrates() }
	::FoBombTick(now)
	::FoCloudTick(now)
	::FoRiftTick(now)
}

::FoEnemyTick <- null
IncludeScript("sigf_fo_enemy")
IncludeScript("sigf_fo_nuke")

::FoStartTick <- function() {
	local old = Entities.FindByName(null, "fo_clock")
	if (old != null) old.Destroy()
	local t = Entities.CreateByClassname("logic_timer")
	t.__KeyValueFromString("targetname", "fo_clock")
	t.ValidateScriptScope()
	t.GetScriptScope().FoOnTimer <- function() { ::FoTick() }
	t.ConnectOutput("OnTimer", "FoOnTimer")
	EntFireByHandle(t, "RefireTime", "0.05", 0.0, null, null)
	EntFireByHandle(t, "Enable", "", 0.0, null, null)
}

// ---------- the show ----------

SigfAutoCam(false)

// Boot: runs always (also in the recorded demo).
SigfAfter(0.3, function() {
	// precache the crate model (SetModel only takes precached models), then swap every cube
	SigfProp(::FoMdl("crate"), SigfHost().GetOrigin() + Vector(0, 0, 500), 2.5, null)
	SigfIn(1.0, function() { ::FO.cratesOn = true })
	::FoAtmosphere()
	::FoStartTick()
	::FoCloudInit()
	::FoRiftInit()
	SigfSpawn("env_fade", Vector(0, 0, 0), {}, 0, function(e) { ::FO.fade = e; e.__KeyValueFromInt("spawnflags", 1) })
	SigfSpawn("env_shake", Vector(0, 0, 0), {}, 0, function(e) {
		::FO.shake = e
		e.__KeyValueFromInt("spawnflags", 5)
		e.__KeyValueFromFloat("radius", 3000.0)
		e.__KeyValueFromFloat("frequency", 40.0)
	})
})

SigfEvery(0.5, function() { ::FoHud() })

// The opening: narrator, statue, radio, butler bot, caps. Called by the show below, or by the demo script.
::FoIntro <- function(capSpots = null) {
	::FoSound("narrator")
	SigfCaption("WAR. WAR NEVER CHANGES.", 4)
	SigfIn(0.6, function() {
		if (!::FoOk(::FO.statue)) ::FoStatue()
	})
	SigfIn(2.6, function() { SigfCaption("VAULT-TEC WELCOMES YOU", 3) })
	SigfIn(1.6, function():(capSpots) {
		SigfSound("sigf/radio_loop.wav")
		SigfText("NOW PLAYING: ATOMIC SWING RADIO", -1, 0.72, 4, "255 230 120", 2)
		if (capSpots == null) { for (local i = 0; i < 5; i++) ::FoCap(::FoSpot(100), 12) }
		else { foreach (c in capSpots) ::FoCap(c, 12) }
	})
	SigfIn(4.2, function() {
		if (::FO.handy == null || !::FoOk(::FO.handy.e)) ::FoHandy()
		::FoSound("handy_hello")
		::FoSay("\"Good morning! Would you care for a Nuka-pop? Splendid!\"", 4.2)
	})
}

// The free-running show (not in the recorded demo: that one has its own script).
if (!::SigfDemoMode) {
	SigfAfter(0.4, function() { ::FoIntro() })

	SigfAfter(11.0, function() {
		SigfCaption("RADROACHES!", 2.5)
		for (local i = 0; i < 2; i++) ::FoSpawnRoach(::FoSpot(420))
	})

	SigfEvery(6.0, function() {
		if (::FO.pickups.len() < 12) ::FoBottle(::FoSpot(60) + Vector(0, 0, 420))
		if (::FO.roaches.len() < 3 && !::FO.nukeBusy) ::FoSpawnRoach(::FoSpot(450))
	})

	SigfEvery(16.0, function() { ::FoVats() })
	SigfAfter(24.0, function() { ::FoNuke(null) })
	SigfEvery(48.0, function() { ::FoNuke(null) })
	SigfEvery(40.0, function() { SigfSound("sigf/radio_loop.wav") })
	// rifts: pairs of portals open on the floor; everything that walks or falls in comes out of the other end
	SigfEvery(15.0, function() {
		local a = ::FoSpot(360)
		local b = ::FoSpot(360)
		for (local i = 0; i < 6 && (a - b).Length() < 350; i++) b = ::FoSpot(360)
		::FoRift(a, b, 12.0, { radius = 85, boost = 1.0,
			onEnter = function(ent, from, to) { if (ent.GetClassname() == "player") SigfText("RIFT JUMP!", -1, 0.62, 1.5, "120 255 120", 4) } })
	})
}
