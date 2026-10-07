// Radroaches, the butler bot's flamethrower and V.A.T.S. (part of the Fallout 4 Takeover mod).
// Roaches are the mod's enemies: 3 hit points, hurt by flying props (cubes, bottles, bombs), flames, V.A.T.S. and nukes.

// Small burst of sparks and fire at a spot (no damage): hit impacts, deaths.
::FoPuff <- function(pos, magnitude) {
	SigfSpawn("env_explosion", pos, {}, 3, function(e):(magnitude) {
		e.__KeyValueFromInt("iMagnitude", magnitude)
		e.__KeyValueFromInt("spawnflags", 1 + 16 + 64)
		EntFireByHandle(e, "Explode", "", 0.05, null, null)
	})
}

// Fire that burns for a while (env_fire): size 0 small .. 2 big
::FoFire <- function(pos, size, life, fn = null) {
	SigfSpawn("env_fire", pos, {}, life, function(e):(size, fn) {
		e.__KeyValueFromInt("firesize", size)
		e.__KeyValueFromInt("fireattack", 1)
		e.__KeyValueFromInt("health", 60)
		e.__KeyValueFromInt("damagescale", 0)
		EntFireByHandle(e, "StartFire", "", 0.05, null, null)
		if (fn != null) fn(e)
	})
}

::FoSpawnRoach <- function(pos, wake = 0.0) {
	if (::FO.roaches.len() >= 4) return
	SigfProp(::FoMdl("roach"), pos + Vector(0, 0, 4), 0, function(e):(wake) {
		EntFireByHandle(e, "DisableMotion", "", 0.3, null, null)
		::FO.roaches.append({ e = e, hp = 3, state = "walk", born = Time(), knock = Vector(0, 0, 0), bite = 0.0,
			yaw = 0.0, fire = null, burnUntil = 0.0, dieAt = 0.0, lastBurn = 0.0, phase = RandomFloat(0, 6.28), wake = (wake > 0.0 ? Time() + wake : 0.0) })
	})
}

// Damage with feedback: red flash, sparks, knock-back, hit sound; death drops caps.
::FoHit <- function(r, dmg, fromPos) {
	if (r.state == "dead") return
	r.hp -= dmg
	r.wake = 0.0
	local o = r.e.GetOrigin()
	::FoSound("roach_hit")
	SigfColor(r.e, "255 60 40")
	SigfIn(0.15, function():(r) { if (::FoOk(r.e)) SigfColor(r.e, "255 255 255") })
	::FoPuff(o + Vector(0, 0, 14), 25)
	local away = Vector(o.x - fromPos.x, o.y - fromPos.y, 0)
	if (away.Length() > 1) { away.Norm(); r.knock = away * 22.0 }
	if (r.hp <= 0) ::FoRoachDie(r)
}

::FoRoachDie <- function(r) {
	r.state = "dead"
	r.dieAt = Time() + 1.4
	::FoSound("roach_die")
	::FO.kills++
	::FoXp(12)
	local o = r.e.GetOrigin()
	::FoPuff(o + Vector(0, 0, 18), 60)
	::FoCap(o + Vector(RandomInt(-30, 30), RandomInt(-30, 30), 0), 6)
	if (RandomInt(0, 2) == 0) ::FoCap(o + Vector(RandomInt(-40, 40), RandomInt(-40, 40), 0), 6)
	SigfText("RADROACH DOWN  +12 XP", -1, 0.66, 1.6, "255 170 60", 4)
}

::FoIgnite <- function(r, sec) {
	if (r.state == "dead") return
	r.state = "burn"
	r.burnUntil = Time() + sec
	r.lastBurn = Time()
	::FoFire(r.e.GetOrigin(), 40, sec + 0.6, function(e):(r) { r.fire = e })
}

// Speed of a prop from its last known positions (VScript cannot read prop velocity).
::FoSpeed <- function(e, now) {
	local id = e.entindex()
	local p = e.GetOrigin()
	local sp = 0.0
	if (id in ::FO.last) {
		local l = ::FO.last[id]
		local dt = now - l.t
		if (dt > 0.01) sp = (p - l.p).Length() / dt
		if (dt > 0.01) ::FO.last[id] = { p = p, t = now }
	} else ::FO.last[id] <- { p = p, t = now }
	return sp
}

::FoMovers <- function(now) {
	local out = []
	for (local c = Entities.FindByClassname(null, "prop_weighted_cube"); c != null; c = Entities.FindByClassname(c, "prop_weighted_cube")) {
		local sp = ::FoSpeed(c, now)
		if (sp > 170) out.append({ e = c, sp = sp })
	}
	foreach (p in ::FO.pickups) {
		if (p.kind != "bottle" || !::FoOk(p.e)) continue
		local sp = ::FoSpeed(p.e, now)
		if (sp > 170) out.append({ e = p.e, sp = sp })
	}
	if (::FO.bomb != null && ::FoOk(::FO.bomb.e)) {
		local sp = ::FoSpeed(::FO.bomb.e, now)
		if (sp > 170) out.append({ e = ::FO.bomb.e, sp = sp })
	}
	return out
}

::FoRoaches <- function(now) {
	local host = SigfHost()
	local hp = host.GetOrigin()
	local movers = ::FoMovers(now)
	local keep = []
	foreach (r in ::FO.roaches) {
		if (!::FoOk(r.e)) continue
		if (r.state == "dead") {
			if (now > r.dieAt) { EntFireByHandle(r.e, "Kill", "", 0.0, null, null); continue }
			local a = r.e.GetAngles()
			r.e.SetAngles(0, a.y + 14, 180)
			keep.append(r)
			continue
		}
		local o = r.e.GetOrigin()
		// hurt by something flying into it
		foreach (m in movers) {
			if (::FoDist(m.e.GetOrigin(), o + Vector(0, 0, 15)) < 85 && now > r.bite - 0.6) {
				::FoHit(r, m.sp > 500 ? 3 : 2, m.e.GetOrigin())
				r.bite = now + 0.5
				break
			}
		}
		if (r.state == "dead") { keep.append(r); continue }
		local dir = Vector(hp.x - o.x, hp.y - o.y, 0)
		local dist = dir.Length()
		if (dist > 0.1) dir.Norm()
		local step = Vector(0, 0, 0)
		if (r.state == "burn") {
			// panic: runs in circles, takes fire damage
			local a = (now * 5.0 + r.phase)
			step = Vector(cos(a), sin(a), 0) * 3.2
			if (now - r.lastBurn > 0.7) { r.lastBurn = now; ::FoHit(r, 1, o + Vector(0, 0, -20)) }
			if (r.state != "dead" && now > r.burnUntil) r.state = "walk"
			if (::FoOk(r.fire)) r.fire.SetOrigin(o)
		} else if (now < r.wake) {
			step = Vector(0, 0, 0)          // dozing until woken (or hit)
		} else if (dist > 150) {
			step = dir * 4.4
		} else if (now > r.bite) {
			// bite the player (health display only: the player cannot die)
			r.bite = now + 1.3
			::FO.hp = ::FO.hp - 7
			if (::FO.hp < 12) ::FO.hp = 12
			SigfText("-7 HP", -1, 0.55, 0.8, "255 80 60", 4)
			::FoFlash("200 20 10", 55, 0.25, 0.0)
		}
		r.knock = r.knock * 0.8
		local n = o + step + r.knock
		local fl = ::SigfFloorAt(Vector(n.x, n.y, o.z))
		local nz = fl != null ? fl.z + 4 : o.z
		r.e.SetOrigin(Vector(n.x, n.y, nz))
		local yaw = dist > 0.1 ? ::FoYaw(o, hp) : r.yaw
		if (r.state == "burn") yaw = now * 400.0
		r.yaw = yaw
		r.e.SetAngles(0, yaw, sin(now * 17 + r.phase) * 5)
		keep.append(r)
	}
	::FO.roaches = keep
}

// ---------- the butler bot torches the nearest roach ----------

::FoHandyAttack <- function(now) {
	local h = ::FO.handy
	if (h == null || !::FoOk(h.e) || now < h.torch) return
	local best = null
	local bd = 950.0
	local ho = h.e.GetOrigin()
	foreach (r in ::FO.roaches) {
		if (r.state != "walk" || !::FoOk(r.e) || now < r.wake) continue
		local d = ::FoDist(r.e.GetOrigin(), ho)
		if (d < bd) { bd = d; best = r }
	}
	if (best == null) return
	h.torch = now + 3.5
	h.say += 1
	if (h.say % 3 == 1) {
		::FoSound("handy_zap")
		::FoSay("\"Oh dear. Terribly sorry, but I shall have to vaporise you.\"", 4.0)
	}
	// a short flame jet from the nozzle to the roach, then the roach burns
	local tgt = best.e.GetOrigin()
	for (local i = 1; i <= 3; i++) {
		local f = i / 3.0
		::FoFire(ho + (tgt - ho) * f, 30, 1.0)
	}
	SigfIn(0.5, function():(best) { if (::FoOk(best.e)) ::FoIgnite(best, 3.2) })
}

// ---------- V.A.T.S.: slow motion, targets, critical hits ----------

::FoVats <- function() {
	if (::FO.vats) return
	local host = SigfHost()
	local hp = host.GetOrigin()
	local targets = []
	foreach (r in ::FO.roaches) {
		if (r.state != "dead" && ::FoOk(r.e) && ::FoDist(r.e.GetOrigin(), hp) < 900) targets.append(r)
	}
	if (targets.len() == 0) return
	::FO.vats = true
	::FoSound("vats")
	SigfCmd("host_timescale 0.25")
	::FoFlash("60 255 90", 70, 0.6, 0.2)
	SigfCaption("V.A.T.S.", 2.5)
	local list = ""
	local i = 0
	foreach (t in targets) {
		if (i >= 3) break
		list += "RADROACH   HEAD " + RandomInt(71, 97) + "%\n"
		i++
	}
	SigfText(list, -1, 0.40, 2.5, "255 255 150", 4)
	// delays are in game seconds: with timescale 0.25 they last four times longer on screen
	SigfIn(0.7, function():(targets) {
		SigfCmd("host_timescale 1")
		::FoSound("crit")
		::FoFlash("255 255 255", 120, 0.3, 0.0)
		SigfCaption("CRITICAL HIT!", 1.6)
		local n = 0
		foreach (t in targets) {
			if (n >= 3) break
			n++
			if (::FoOk(t.e) && t.state != "dead") ::FoHit(t, 99, SigfHost().GetOrigin())
		}
		::FO.vats = false
	})
}

// ---------- tick ----------

::FoEnemyTick <- function(now) {
	::FoRoaches(now)
	::FoHandyAttack(now)
	// hurt feedback decays: the displayed health slowly comes back
	if (::FO.hp < 100 && now - ::FO.hurtAt > 1.0) { ::FO.hurtAt = now; ::FO.hp += 1 }
}
