HTMLWidgets.widget({
	name: "animejs",
	type: "output",

	factory(el, width, height) {
		return {
			renderValue(x) {
				// 1. Inject SVG markup into the widget element.
				el.innerHTML = x.svg || "";

				// A root SVG with a viewBox but no width/height would size
				// itself from its intrinsic ratio and overflow the widget box.
				// Instead, fit it to the element's width and derive its height
				// from the viewBox aspect ratio, letting the element grow to
				// hold the drawing plus any controls bar. The drawing is then
				// shown at full size, not letterboxed inside a fixed box. An SVG
				// that carries explicit dimensions is left alone.
				const root = el.querySelector("svg");
				if (root && !root.hasAttribute("width") && !root.hasAttribute("height")) {
					const vb = (root.getAttribute("viewBox") || "")
						.split(/[\s,]+/)
						.map(Number);
					root.style.display = "block";
					root.style.width = "100%";
					if (vb.length === 4 && vb[2] > 0 && vb[3] > 0) {
						root.style.height = "auto";
						root.style.aspectRatio = vb[2] + " / " + vb[3];
						// Override the fixed height htmlwidgets set on the box so
						// it grows to the drawing's height plus the controls bar.
						el.style.height = "auto";
					} else {
						// No usable viewBox: fall back to filling the box.
						el.style.display = "flex";
						el.style.flexDirection = "column";
						root.style.flex = "1 1 auto";
						root.style.minHeight = "0";
					}
				}

				// 2. Build and play the Anime.js animation or timeline.
				const config = x.config || {};
				const instance =
					config.kind === "animation"
						? buildAnimation(el, config)
						: buildTimeline(el, config);

				if (config.controls) {
					attachControls(el, instance, config.autoplay ?? true);
				}
			},

			resize(width, height) {
				// No-op: SVG viewBox handles scaling.
			},
		};
	},
});

// ---------------------------------------------------------------------------
// resolveEase
// Converts a serialised easing specification to the value Anime.js v4
// expects. Name strings ("outQuad", "outElastic(1,0.3)", etc.) are parsed by
// Anime.js itself and pass through unchanged. Spring, steps, and cubic
// bezier eases have no v4 string form and arrive as tagged objects that are
// reconstructed as the corresponding anime.* function calls.
// ---------------------------------------------------------------------------
function resolveEase(spec) {
	if (spec == null || typeof spec === "string") return spec;

	switch (spec.type) {
		case "spring":
			return anime.spring({
				bounce: spec.bounce,
				duration: spec.duration,
			});
		case "cubicBezier":
			return anime.cubicBezier(...spec.args);
		case "steps":
			return anime.steps(spec.count, spec.fromStart);
	}

	console.warn("[animejs widget] unknown ease specification:", spec);
	return undefined;
}

// Resolve ease fields nested inside serialised props: keyframe arrays and
// per-property {from, to, ease} objects.
function resolvePropEases(props) {
	const out = {};
	for (const [key, value] of Object.entries(props || {})) {
		if (Array.isArray(value)) {
			out[key] = value.map((keyframe) =>
				keyframe &&
				typeof keyframe === "object" &&
				keyframe.ease != null
					? Object.assign({}, keyframe, {
							ease: resolveEase(keyframe.ease),
						})
					: keyframe,
			);
		} else if (value && typeof value === "object" && value.ease != null) {
			out[key] = Object.assign({}, value, {
				ease: resolveEase(value.ease),
			});
		} else {
			out[key] = value;
		}
	}
	return out;
}

// ---------------------------------------------------------------------------
// Shared assembly helpers
// ---------------------------------------------------------------------------

// Tween parameters shared by timeline segments and single animations.
function tweenParams(config) {
	const params = resolvePropEases(config.props);

	if (config.duration != null) params.duration = config.duration;
	if (config.ease != null) params.ease = resolveEase(config.ease);
	if (config.delay != null) params.delay = config.delay;
	if (config.stagger != null) params.delay = resolveStagger(config.stagger);

	return params;
}

// Playback parameters shared by createTimeline() and animate().
function playbackParams(config) {
	const params = {
		loop: config.loop ?? false,
		autoplay: config.autoplay ?? true,
		reversed: config.reversed ?? false,
		alternate: config.alternate ?? false,
	};
	if (config.loopDelay != null) params.loopDelay = config.loopDelay;
	if (config.playbackRate != null) params.playbackRate = config.playbackRate;
	return params;
}

// Event callbacks: values are names of globally scoped JS functions, passed
// to Anime.js as creation parameters (the v4 idiom).
function resolveCallbacks(events) {
	const callbacks = {};
	for (const [event, cbName] of Object.entries(events || {})) {
		if (typeof window[cbName] === "function") {
			callbacks[event] = window[cbName];
		} else {
			console.warn(
				"[animejs widget] event callback not found on window: " +
					cbName,
			);
		}
	}
	return callbacks;
}

// ---------------------------------------------------------------------------
// Discrete text keyframes (anime_text)
// A "text" prop is not a tween: its values are swapped into the target's
// textContent as the segment advances. Split the tween props from the text
// props so Anime.js never tries to interpolate the latter.
// ---------------------------------------------------------------------------
function splitTextProps(props) {
	const tween = {};
	const texts = [];
	for (const [key, value] of Object.entries(props || {})) {
		if (value && typeof value === "object" && value.type === "text") {
			texts.push(value.values || []);
		} else {
			tween[key] = value;
		}
	}
	return { tween, texts };
}

// A timeline timer whose onUpdate maps its own progress onto the value array
// and writes the current value to each target's textContent. Adding it as a
// timer (params with no target) also gives the segment its duration on the
// timeline, so a text-only timeline still has length and scrubs correctly.
function makeTextTimer(targets, values, duration) {
	const n = values.length;
	return {
		duration: duration,
		onUpdate: (self) => {
			if (n === 0) return;
			const p = self.iterationProgress ?? self.progress ?? 0;
			const i = Math.min(Math.max(Math.floor(p * n), 0), n - 1);
			const text = String(values[i]);
			targets.forEach((target) => {
				if (target.textContent !== text) target.textContent = text;
			});
		},
	};
}

// ---------------------------------------------------------------------------
// buildTimeline
// ---------------------------------------------------------------------------
function buildTimeline(el, config) {
	// Shallow copy to avoid mutating the original config object when writing
	// back defaults.ease next and corrupting the state on any subsequent
	// renderValue call (e.g., in Shiny)
	const defaults = Object.assign({}, config.defaults || {});
	if (defaults.ease != null) {
		defaults.ease = resolveEase(defaults.ease);
	}

	const tl = anime.createTimeline(
		Object.assign(
			{ defaults: defaults },
			playbackParams(config),
			resolveCallbacks(config.events),
		),
	);

	const defaultDuration = defaults.duration ?? 0;

	for (const segment of config.segments || []) {
		const { tween, texts } = splitTextProps(segment.props);
		const position = segment.offset ?? "+=0";
		const targets = el.querySelectorAll(segment.selector);

		// Only add a tween when there is at least one tweenable prop; Anime.js
		// rejects an animation with no properties.
		if (Object.keys(tween).length > 0) {
			const tweenSegment = Object.assign({}, segment, { props: tween });
			tl.add(targets, tweenParams(tweenSegment), position);
		}

		if (texts.length > 0) {
			const duration = segment.duration ?? defaultDuration;
			for (const values of texts) {
				tl.add(makeTextTimer(targets, values, duration), position);
			}
		}
	}

	return tl;
}

// ---------------------------------------------------------------------------
// buildAnimation
// Single animation via anime.animate(targets, params).
// ---------------------------------------------------------------------------
function buildAnimation(el, config) {
	const targets = el.querySelectorAll(config.selector);
	const params = Object.assign(
		tweenParams(config),
		playbackParams(config),
		resolveCallbacks(config.events),
	);
	return anime.animate(targets, params);
}

// ---------------------------------------------------------------------------
// resolveStagger
// Converts the serialised anime_stagger R object to an Anime.js stagger call.
// ---------------------------------------------------------------------------
function resolveStagger(s) {
	const opts = {};
	if (s.from != null) opts.from = s.from;
	if (s.start != null) opts.start = s.start;
	if (s.reversed != null) opts.reversed = s.reversed;
	if (s.ease != null) opts.ease = resolveEase(s.ease);
	if (s.grid != null && s.grid.length === 2) {
		opts.grid = s.grid;
		if (s.axis != null) opts.axis = s.axis;
	}
	return anime.stagger(s.value, opts);
}

// ---------------------------------------------------------------------------
// attachControls
// Injects a minimal play / pause / seek bar beneath the SVG. Works on both
// Timeline and JSAnimation instances (shared Timer API). User callbacks
// attached via anime_on() are chained, not overwritten.
// ---------------------------------------------------------------------------
function attachControls(el, instance, autoplay) {
	const bar = document.createElement("div");
	bar.className = "animejs-controls";
	// Keep the bar at its natural height when the widget is a flex column.
	bar.style.flex = "0 0 auto";

	const btn = document.createElement("button");
	btn.setAttribute("aria-label", autoplay ? "Pause" : "Play");

	const ICON_PLAY = `<svg xmlns="http://www.w3.org/2000/svg" width="12" height="12" viewBox="0 0 16 16" aria-hidden="true">
		<polygon points="3,1 14,8 3,15" fill="currentColor"/>
	</svg>`;

	const ICON_PAUSE = `<svg xmlns="http://www.w3.org/2000/svg" width="12" height="12" viewBox="0 0 16 16" aria-hidden="true">
		<rect x="2"  y="1" width="4" height="14" fill="currentColor"/>
		<rect x="10" y="1" width="4" height="14" fill="currentColor"/>
	</svg>`;

	function syncBtn() {
		btn.innerHTML = instance.paused ? ICON_PLAY : ICON_PAUSE;
		btn.setAttribute("aria-label", instance.paused ? "Play" : "Pause");
	}

	btn.innerHTML = autoplay ? ICON_PAUSE : ICON_PLAY;

	const scrubber = document.createElement("input");
	scrubber.type = "range";
	scrubber.min = 0;
	scrubber.max = 1000;
	scrubber.value = 0;

	bar.appendChild(btn);
	bar.appendChild(scrubber);
	el.appendChild(bar);

	// Paint the filled part of the track. The track background is a hard-stop
	// gradient driven by this custom property, so the fill looks the same in
	// every engine instead of relying on browser-specific progress pseudos.
	function updateFill() {
		const pct = (scrubber.value / scrubber.max) * 100;
		scrubber.style.setProperty("--animejs-pct", pct + "%");
	}
	updateFill();

	// Keep the scrubber in sync while playing, chaining any user callback.
	const userOnUpdate = instance.onUpdate;
	instance.onUpdate = function (self) {
		if (typeof userOnUpdate === "function") userOnUpdate(self);
		scrubber.value = Math.round(self.iterationProgress * 1000);
		updateFill();
	};

	const userOnComplete = instance.onComplete;
	instance.onComplete = function (self) {
		if (typeof userOnComplete === "function") userOnComplete(self);
		scrubber.value = 0;
		updateFill();
		syncBtn();
	};

	btn.addEventListener("click", () => {
		if (instance.paused) {
			if (instance.completed) {
				// Rewind before replaying; play() alone does not reset position.
				instance.seek(0);
			}
			instance.play();
		} else {
			instance.pause();
		}
		syncBtn();
	});

	scrubber.addEventListener("input", () => {
		const fraction = scrubber.value / 1000;
		instance.seek(instance.iterationDuration * fraction);
		updateFill();

		// Seeking to a mid-point after completion un-completes the animation.
		if (instance.completed) {
			instance.completed = false;
		}
	});
}
