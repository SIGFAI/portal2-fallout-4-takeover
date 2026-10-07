// Mini nuke (part of the Fallout 4 Takeover mod): a bomb drops from the ceiling, falls through portals like any prop,
// and when it lands it blows up: fireballs, fire columns, flash, shake, a shockwave that flings props and kills roaches.

::FoNukeSpot <- function() {
	local host = SigfHost()
	local p = SigfFront(520)
	if (::FoFlat(p, host.GetOrigin()) < 280) p = ::FoSpot(300)
	return p
}

// Drops a bomb above a spot. spot null = in front of the player.
::FoNuke <- function(spot = null) {
	if (::FO.nukeBusy) return
	::FO.nukeBusy = true
	if (spot == null) spot = ::FoNukeSpot()
	::FoSound("nuke_siren")
	SigfIn(16.0, function() { if (::FO.bomb == null) ::FO.nukeBusy = false })
	SigfCaption("MINI NUKE INBOUND", 3.5)
	SigfCmd("host_timescale 0.45")
	SigfIn(9.0, function() { if (!::FO.vats) SigfCmd("host_timescale 1") })
	SigfText("RADIATION WARNING", -1, 0.40, 3.5, "255 200 60", 4)
	SigfProp(::FoMdl("fatman"), spot + Vector(0, 0, 400), 0, function(e) {
		e.SetAngles(180, 0, 0)
		::FO.bomb = { e = e, born = Time(), still = 0, lastZ = 99999.0, fire = null }
		// a fire ball rides with the bomb so it can be followed through the air and through portals
		::FoFire(e.GetOrigin(), 90, 0.0, function(f) { if (::FO.bomb != null) ::FO.bomb.fire = f })
	})
}

::FoNukeExplode <- function(pos) {
	local b = ::FO.bomb
	::FO.bomb = null
	if (b != null && ::FoOk(b.e)) EntFireByHandle(b.e, "Kill", "", 0.0, null, null)
	if (b != null && ::FoOk(b.fire)) EntFireByHandle(b.fire, "Kill", "", 0.0, null, null)
	::FoSound("nuke_boom")
	if (!::FO.vats) SigfCmd("host_timescale 1")
	::FoFlash("255 255 255", 255, 1.6, 0.25)
	::FoShake(30.0, 4.0)
	::FoFire(pos, 260, 9.0)
	::FoFire(pos + Vector(90, 0, 0), 200, 7.0)
	::FoFire(pos + Vector(-80, 70, 0), 180, 6.0)
	::FoFire(pos + Vector(-60, -90, 0), 160, 6.0)
	::FoCloud(pos)
	SigfSpawn("env_explosion", pos + Vector(0, 0, 40), {}, 3, function(e) {
		e.__KeyValueFromInt("iMagnitude", 600)
		e.__KeyValueFromInt("spawnflags", 1)
		EntFireByHandle(e, "Explode", "", 0.0, null, null)
	})
	for (local i = 0; i < 7; i++) {
		local a = i * 51.0 / 57.29578
		local ring = 90.0 + 40.0 * (i % 3)
		local q = pos + Vector(cos(a) * ring, sin(a) * ring, 30 + 55 * (i % 4))
		SigfIn(0.08 * i, function():(q) {
			SigfSpawn("env_explosion", q, {}, 3, function(e) {
				e.__KeyValueFromInt("iMagnitude", 400)
				e.__KeyValueFromInt("spawnflags", 1 + 16)
				EntFireByHandle(e, "Explode", "", 0.0, null, null)
			})
		})
	}
	// shockwave: push props, hurt roaches, bump the player
	local host = SigfHost()
	local R = 760.0
	for (local e = Entities.FindInSphere(null, pos, R); e != null; e = Entities.FindInSphere(e, pos, R)) {
		local cls = e.GetClassname()
		local o = e.GetOrigin()
		local d = ::FoDist(o, pos)
		local away = Vector(o.x - pos.x, o.y - pos.y, 0)
		if (away.Length() < 1) away = Vector(1, 0, 0)
		away.Norm()
		local power = (1.0 - d / R) * 900.0 + 160.0
		if (cls == "prop_weighted_cube" || cls == "npc_portal_turret_floor") {
			SigfPush(e, away * power + Vector(0, 0, power * 0.7))
		} else if (cls == "prop_physics") {
			if (e == (::FO.statue)) continue
			if (::FO.handy != null && e == ::FO.handy.e) continue
			local isRoach = false
			foreach (r in ::FO.roaches) { if (r.e == e) isRoach = true }
			if (isRoach) continue
			if (e.GetModelName().find("cap") != null) continue
			SigfPush(e, away * power + Vector(0, 0, power * 0.8))
		}
	}
	foreach (r in ::FO.roaches) {
		if (r.state != "dead" && ::FoOk(r.e) && ::FoDist(r.e.GetOrigin(), pos) < R) ::FoHit(r, 99, pos)
	}
	local hp = host.GetOrigin()
	local pd = ::FoDist(hp, pos)
	if (pd < R) {
		local away = Vector(hp.x - pos.x, hp.y - pos.y, 0)
		if (away.Length() > 1) away.Norm()
		host.SetVelocity(host.GetVelocity() + away * (260.0 * (1.0 - pd / R) + 80.0) + Vector(0, 0, 330))
	}
	::FoRads(70)
	::FoXp(25)
	SigfText("RADS +70", -1, 0.62, 3, "120 255 80", 4)
	SigfIn(2.5, function() { SigfCaption("WAR NEVER CHANGES", 3) })
	SigfIn(9.0, function() { ::FO.nukeBusy = false })
}

::FoBombTick <- function(now) {
	local b = ::FO.bomb
	if (b == null) return
	if (!::FoOk(b.e)) { ::FO.bomb = null; ::FO.nukeBusy = false; return }
	local o = b.e.GetOrigin()
	if (::FoOk(b.fire)) b.fire.SetOrigin(o + Vector(0, 0, 12))
	if (now - b.born > 1.0) {
		if (fabs(o.z - b.lastZ) < 0.6) b.still++
		else b.still = 0
		b.lastZ = o.z
		if (b.still >= 4 || now - b.born > 14.0) { ::FoNukeExplode(o); return }
	}
}

// Mushroom cloud: one big model made at boot and hidden under the floor; on a blast it rises out of the floor,
// hangs, then sinks back (a prop made in the middle of the action can be refused by the engine, so it is made early).
::FoCloudInit <- function() {
	local host = SigfHost()
	SigfProp(::FoMdl("mushroom"), host.GetOrigin() + SigfForward() * 200 + Vector(0, 0, 60), 0, function(e) {
		EntFireByHandle(e, "DisableMotion", "", 0.0, null, null)
		e.SetOrigin(Vector(0, 0, -9000))
		::FO.cloudEnt = e
	})
}

::FoCloud <- function(pos) {
	local g = ::SigfFloorAt(pos)
	local base = g != null ? g : pos
	if (!::FoOk(::FO.cloudEnt)) return
	::FO.cloud = { e = ::FO.cloudEnt, t0 = Time(), base = base }
}

::FoCloudTick <- function(now) {
	local c = ::FO.cloud
	if (c == null) return
	if (!::FoOk(c.e)) { ::FO.cloud = null; return }
	local age = now - c.t0
	local z = 0.0
	if (age < 3.0) { local k = age / 3.0; z = -430.0 + 430.0 * (1.0 - (1.0 - k) * (1.0 - k)) }
	else if (age < 9.0) z = 0.0
	else if (age < 11.0) z = -215.0 * (age - 9.0)
	else { c.e.SetOrigin(Vector(0, 0, -9000)); ::FO.cloud = null; return }
	c.e.SetOrigin(c.base + Vector(0, 0, z - 4))
	c.e.SetAngles(0, age * 9.0, 0)
}

// ---------- rifts: two ring gates linked by a kit portal pair (two models made at boot, hidden under the floor) ----------

::FoRiftInit <- function() {
	local host = SigfHost()
	for (local i = 0; i < 2; i++) {
		SigfProp(::FoMdl("rift"), host.GetOrigin() + SigfForward() * 220 + Vector(0, 0, 80), 0, function(e) {
			EntFireByHandle(e, "DisableMotion", "", 0.0, null, null)
			e.SetOrigin(Vector(0, 0, -9000))
			::FO.riftEnts.append(e)
		})
	}
}

::FoRift <- function(a, b, life, opts = {}) {
	local o = {}
	foreach (k, v in opts) o[k] <- v
	o.sprite <- false
	SigfPortal(a, b, o, life)
	if (::FO.riftEnts.len() >= 2) ::FO.rift = { a = a, b = b, t0 = Time(), until = Time() + life }
}

::FoRiftTick <- function(now) {
	local r = ::FO.rift
	if (r == null) return
	local host = SigfHost()
	local ents = ::FO.riftEnts
	if (now > r.until || !::FoOk(ents[0]) || !::FoOk(ents[1])) {
		foreach (e in ents) { if (::FoOk(e)) e.SetOrigin(Vector(0, 0, -9000)) }
		::FO.rift = null
		return
	}
	local age = now - r.t0
	local k = age < 0.6 ? age / 0.6 : 1.0
	local pts = [r.a, r.b]
	for (local i = 0; i < 2; i++) {
		local p = pts[i] + Vector(0, 0, 8 + 62 * k + sin(now * 2.0 + i) * 3)
		ents[i].SetOrigin(p)
		ents[i].SetAngles(0, ::FoYaw(p, host.GetOrigin()), now * 70.0)
	}
}
