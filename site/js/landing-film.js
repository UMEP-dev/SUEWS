/*
 * Landing film: "From Code to Climate Policy", at its own pace (about 36 s).
 *
 * A port of the Motion film made for the site (its scenes and score), plus
 * a timeline of the papers SUEWS grew from. The scene code is the film's own
 * (CUT can replay a scene faster for a shorter cut); the small kit below
 * stands in for the Motion player (sequencing, easing, typing, word fades)
 * and a Web Audio synth stands in for its score player. When the end card
 * lands, the hero logo takes over from the film's logo and settles into its
 * place as the page comes up.
 *
 * Never plays on its own: clicking the hero logo plays it, with sound, and
 * a ?film link (the <head> guard sets .logo-intro) opens the page on it,
 * where a browser that will not start audio without a click shows a play
 * button first.
 */
(function () {
    'use strict';
    const root = document.documentElement;
    const heroSvg = document.querySelector('.logo-svg');
    const box = document.querySelector('.logo-container');
    if (!heroSvg || !box) return;

    // ---- Cut: [scene, seconds of the original used, seconds on screen] ----
    const CUT = [
        ['code', 4.0, 4.0],
        ['grow', 6.0, 6.0],
        ['flux', 5.0, 5.0],
        ['history', 6.0, 6.0],
        ['arup', 5.0, 5.0],
        ['ccra', 5.0, 5.0],
        ['end', 3.8, 3.8]
    ];
    let acc = 0;
    const SEQ = CUT.map(([id, src, dur]) => {
        const s = { id, dur, start: acc, speed: src / dur };
        acc += dur;
        return s;
    });
    const T = Object.fromEntries(SEQ.map((s) => [s.id, s.start]));
    const HAND = acc;          // end card is up: the hero takes over
    const END = HAND + 1.2;    // page fully back
    let playing = false;

    // mode: 'on' with sound (started by a click), 'try' with sound if the
    // browser allows it, else a play button
    function play(mode) {
        if (playing) return;
        playing = true;
        root.classList.add('logo-intro', 'film-running');
        window.scrollTo(0, 0);

        // ---- Kit: the Motion player's helpers the scenes use ----
        const clamp01 = (x) => (x < 0 ? 0 : x > 1 ? 1 : x);
        const seg = (t, a, b) => clamp01((t - a) / (b - a));
        const lerp = (a, b, x) => a + (b - a) * x;
        const eo = (x) => 1 - Math.pow(1 - x, 3);
        const eio = (x) => (x < 0.5 ? 4 * x * x * x : 1 - Math.pow(-2 * x + 2, 3) / 2);
        const eob = (x, s = 1.4) => 1 + (s + 1) * Math.pow(x - 1, 3) + s * Math.pow(x - 1, 2);
        const rng = (seed) => () => {
            seed |= 0; seed = (seed + 0x6D2B79F5) | 0;
            let r = Math.imul(seed ^ (seed >>> 15), 1 | seed);
            r = (r + Math.imul(r ^ (r >>> 7), 61 | r)) ^ r;
            return ((r ^ (r >>> 14)) >>> 0) / 4294967296;
        };
        const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
        const h = (html) => {
            const tp = document.createElement('template');
            tp.innerHTML = html.trim();
            return tp.content.firstElementChild;
        };
        const el = (tag, o = {}) => {
            const n = document.createElement(tag);
            if (o.class) n.className = o.class;
            if (o.style) n.style.cssText = o.style;
            if (o.text != null) n.textContent = o.text;
            if (o.html != null) n.innerHTML = o.html;
            for (const k in o.attrs || {}) n.setAttribute(k, o.attrs[k]);
            return n;
        };
        const TextFx = {
            fadeFromRight(node, t, t0, { stagger = 0.11, dur = 0.5, dx = 26 } = {}) {
                if (!node._words) {
                    const parts = node.textContent.split(/(\s+)/);
                    node.textContent = '';
                    node._words = [];
                    for (const w of parts) {
                        if (!w || /^\s+$/.test(w)) { node.append(w); continue; }
                        const s = document.createElement('span');
                        s.textContent = w;
                        s.style.display = 'inline-block';
                        node.append(s);
                        node._words.push(s.style);
                    }
                }
                node._words.forEach((s, i) => {
                    const p = eo(seg(t, t0 + i * stagger, t0 + i * stagger + dur));
                    s.opacity = p;
                    s.transform = 'translateX(' + ((1 - p) * dx).toFixed(1) + 'px)';
                });
            }
        };
        const typingTimes = [];

        // The film's logo, drawn from the hero's own shapes
        const d = (id) => document.getElementById(id).getAttribute('d');
        const LOGO_HTML = '<svg viewBox="-180 -155 2360 2360" width="240" height="240" xmlns="http://www.w3.org/2000/svg">' +
            '<circle cx="1000" cy="1025" r="1068" style="fill:var(--ground);stroke:var(--accent)" stroke-width="20"/>' +
            '<circle cx="620" cy="560" r="320" fill="#F7B538"/><circle cx="620" cy="560" r="200" fill="#E85D04"/>' +
            '<path fill="#5DADE2" d="' + d('buildings-main') + '"/><path fill="#5DADE2" d="' + d('buildings-detail') + '"/>' +
            '<path fill="#09a25c" d="' + d('leaf-ground') + '"/><path fill="#0558a5" d="' + d('wave') + '"/></svg>';

        const SCENES = {};
        const base = {
            seg, lerp, eo, eio, eob, h, el, esc, TextFx,
            module: (name) => (name === 'logo.html' ? LOGO_HTML : ''),
            scene(id, def) { SCENES[id] = def; },
            typist(text, t0, cps, seed = 7) {
                const r = rng(seed), times = [];
                let t = t0;
                for (let i = 0; i < text.length; i++) {
                    times.push(t);
                    t += (1 / cps) * (0.55 + 0.9 * r()) * (text[i] === ' ' ? 1.4 : 1);
                }
                for (const x of times) typingTimes.push(this.start + x / this.speed);
                return {
                    times, end: t,
                    count(tt) { let n = 0; while (n < times.length && times[n] <= tt) n++; return n; },
                    at(tt) { return text.slice(0, this.count(tt)); }
                };
            }
        };

        // ---- Shared modules (the film's shared.js) ----
        const CITY = {
            SRC: 'QSTAR=QH+QE+DQS-QF;CALL OHM(QN,DQS);IF(LAI>0)QE=EVAP(T,RH);REAL::ALB,EMIS;DO IS=1,NSURF;FLUX(IS)=QN*FRAC(IS);END DO;'
        };
        const LOGO = {
            CX: 1035, CY: 1000,
            map(s, x, y, lx, ly) { return [x + (lx - 1035) * s, y + (ly - 1000) * s]; },
            build(kit) {
                const svg = kit.h(kit.module('logo.html') || '<svg></svg>');
                const pa = [...svg.querySelectorAll('path')];
                const pth = (i) => (pa[i] ? "path('" + pa[i].getAttribute('d') + "')" : 'inset(50%)');
                const defs = [
                    ['b1', pth(0), '#5DADE2', 636, 1452],
                    ['b2', pth(1), '#5DADE2', 836, 1246],
                    ['so', 'circle(320px at 620px 560px)', '#F7B538', 236, 884],
                    ['si', 'circle(200px at 620px 560px)', '#E85D04', 356, 764],
                    ['lf', pth(2), '#09a25c', 1120, 1737],
                    ['wv', pth(3), '#0558a5', 1552, 1757]
                ];
                let tex = ''; while (tex.length < 4200) tex += CITY.SRC;
                const box = kit.el('div', { class: 'logo' });
                const parts = defs.map(([k, clip, col, y0, y1]) => {
                    const part = kit.el('div', { class: 'logo-part' }); part.style.clipPath = clip;
                    const solid = kit.el('div', { class: 'logo-solid' }); solid.style.background = col; solid.style.opacity = '0';
                    const rev = kit.el('div', { class: 'logo-rev' });
                    const t = kit.el('div', { class: 'logo-tex', text: tex, attrs: { 'aria-hidden': 'true' } });
                    const edge = kit.el('div', { class: 'logo-edge' });
                    rev.append(t); part.append(solid, rev, edge); box.append(part);
                    return { k, col, y0, y1, solid: solid.style, rev: rev.style, tex: t.style, edge: edge.style };
                });
                return {
                    el: box, parts,
                    place(s, x, y) { box.style.transform = 'translate(' + (x - 1035 * s) + 'px,' + (y - 1000 * s) + 'px) scale(' + s + ')'; },
                    set(i, fill, solid) {
                        const p = parts[i];
                        const yy = p.y1 + (p.y0 - p.y1) * fill;
                        p.rev.clipPath = 'inset(' + yy + 'px 0 0 0)';
                        p.edge.transform = 'translateY(' + (yy - 5) + 'px)';
                        p.edge.opacity = fill > 0.02 && fill < 0.98 ? '0.8' : '0';
                        p.solid.opacity = String(solid);
                        const u = Math.round((1 - solid) * 100);
                        p.tex.color = 'color-mix(in srgb, color-mix(in srgb, ' + p.col + ' 78%, var(--second)) ' + u + '%, color-mix(in srgb, ' + p.col + ' 70%, var(--ground)))';
                    },
                    done() { for (let i = 0; i < parts.length; i++) this.set(i, 1, 1); }
                };
            }
        };

        // ---- Scenes (the film's scene-*.js, unchanged except where noted) ----
        const MODULES = {
            code(kit) {
                const { seg, eo, eio, lerp, TextFx } = kit;
                const CODE_LINES = ['QSTAR + QF = QH + QE + DQS', 'CALL OHM(QN, DQS)    ! heat stored in the city', 'QE = EVAP(T, RH, LAI) ! water it gives back'];
                kit.scene('code', {
                    init(kit) {
                        kit.stage.append(kit.h('<div class="ground"><div class="code-cam" style="position:absolute;inset:0;transform-origin:420px 560px">' +
                            '<div class="words" style="left:200px;top:150px;width:1400px"><p class="kicker code-kick">SUEWS / a scientist\'s notebook</p><h1 class="line code-title">It began as a few lines of code.</h1></div>' +
                            '<div class="code-block" style="position:absolute;left:200px;top:470px">' +
                            '<div class="code cm">! Surface Urban Energy and Water Balance Scheme</div>' +
                            '<div class="code kw code-l0"></div><div class="code code-l1"></div><div class="code code-l2"></div></div>' +
                            '<div class="code-gutter" style="position:absolute;left:150px;top:470px;width:4px;height:204px;border-radius:2px;background:color-mix(in srgb, var(--main) 60%, var(--ground))"></div>' +
                            '</div></div>'));
                        this.cam = kit.stage.querySelector('.code-cam').style;
                        this.kick = kit.stage.querySelector('.code-kick').style;
                        this.title = kit.stage.querySelector('.code-title');
                        this.gut = kit.stage.querySelector('.code-gutter').style;
                        this.lines = [0, 1, 2].map((i) => kit.stage.querySelector('.code-l' + i));
                        this.caret = kit.h('<span class="caret"></span>');
                        let t0 = 0.5;
                        this.ty = CODE_LINES.map((s) => { const y = kit.typist(s, t0, 34, 11); t0 = y.end + 0.18; return y; });
                    },
                    render(t) {
                        TextFx.fadeFromRight(this.title, t, -1.1, { stagger: 0.09, dur: 0.45 });
                        this.kick.opacity = 1;
                        this.cam.transform = 'scale(' + lerp(1, 1.035, eio(seg(t, 0, 4))) + ')';
                        this.gut.transform = 'scaleY(' + lerp(0.25, 1, eo(seg(t, 0.4, 3.2))) + ')';
                        this.gut.transformOrigin = '50% 0';
                        let active = this.lines.length - 1;
                        for (let i = 0; i < this.lines.length; i++) {
                            const s = this.ty[i].at(t);
                            if (this.lines[i].firstChild?.nodeType !== 3 || this.lines[i].firstChild.data !== s) {
                                this.lines[i].textContent = s;
                            }
                            if (t < this.ty[i].end && active === this.lines.length - 1 && t >= this.ty[i].times[0] - 0.2) active = i;
                        }
                        for (let i = 0; i < this.lines.length; i++) if (t < this.ty[i].times[0]) { active = Math.max(0, i - (t < this.ty[0].times[0] ? 0 : 1)); break; }
                        const host = this.lines[t < this.ty[0].times[0] ? 0 : active];
                        if (this.caret.parentNode !== host || host.lastChild !== this.caret) host.append(this.caret);
                        this.caret.style.opacity = t > this.ty[2].end ? (Math.floor(t / 0.53) % 2 ? 0 : 1) : 1;
                    }
                });
            },

            grow(kit) {
                const { seg, eo, eio, lerp, TextFx } = kit;
                const GROW_FILL = [[0.5, 1.9], [0.9, 2.2], [1.5, 2.6], [1.8, 2.8], [2.2, 3.4], [2.7, 3.7]];
                kit.scene('grow', {
                    init(kit) {
                        const root = kit.h('<div class="ground"></div>');
                        kit.stage.append(root);
                        root.append(kit.h('<div class="grow-code" style="position:absolute;left:0;top:0;width:1920px;height:1080px;transform-origin:200px 470px">' +
                            '<div style="position:absolute;left:150px;top:470px;width:4px;height:204px;border-radius:2px;background:color-mix(in srgb, var(--main) 60%, var(--ground))"></div>' +
                            '<div style="position:absolute;left:200px;top:470px">' +
                            '<div class="code cm">! Surface Urban Energy and Water Balance Scheme</div>' +
                            '<div class="code kw">QSTAR + QF = QH + QE + DQS</div>' +
                            '<div class="code">CALL OHM(QN, DQS)    ! heat stored in the city</div>' +
                            '<div class="code">QE = EVAP(T, RH, LAI) ! water it gives back<span class="caret grow-caret"></span></div>' +
                            '</div></div>'));
                        this.logo = LOGO.build(kit);
                        root.append(this.logo.el);
                        root.append(kit.h('<div class="words" style="left:200px;top:600px;width:660px"><h2 class="line grow-title">Line by line, it learned how a city works.</h2></div>'));
                        this.code = root.querySelector('.grow-code').style;
                        this.caret = root.querySelector('.grow-caret').style;
                        this.title = root.querySelector('.grow-title');
                    },
                    render(t) {
                        const m = eio(seg(t, 0.1, 1.3));
                        const s = lerp(1.035, 0.6, m);
                        this.code.transform = 'translate(' + lerp(220 - 220 * 1.035, 0, m) + 'px,' + lerp(90 - 90 * 1.035, -290, m) + 'px) scale(' + s + ')';
                        this.code.opacity = String(lerp(1, 0.5, m));
                        this.caret.opacity = String(1 - eo(seg(t, 0.2, 0.6)));
                        this.logo.place(lerp(0.4, 0.42, eio(seg(t, 0, 6))), 1250, 560);
                        for (let i = 0; i < GROW_FILL.length; i++) {
                            const f = eio(seg(t, GROW_FILL[i][0], GROW_FILL[i][1]));
                            const so = eo(seg(t, 3.7 + i * 0.12, 4.5 + i * 0.12));
                            this.logo.set(i, f, so);
                        }
                        TextFx.fadeFromRight(this.title, t, 3.9, { stagger: 0.1, dur: 0.5 });
                    }
                });
            },

            flux(kit) {
                const { seg, eo, eio, lerp, TextFx } = kit;
                const FLUX_S = 0.42, FLUX_X = 1250, FLUX_Y = 560;
                const FLUX_PTS = [[640, 1080, 'flux'], [900, 880, 'flux'], [1190, 670, 'flux'], [1480, 960, 'flux'], [1690, 1160, 'wflux'], [1430, 1390, 'wflux'], [900, 1600, 'wflux']];
                kit.scene('flux', {
                    init(kit) {
                        const L = LOGO;
                        const root = kit.h('<div class="ground"></div>');
                        kit.stage.append(root);
                        const sun = L.map(FLUX_S, FLUX_X, FLUX_Y, 620, 560);
                        root.append(kit.h('<div class="halo flux-halo" style="left:' + (sun[0] - 300) + 'px;top:' + (sun[1] - 300) + 'px;width:600px;height:600px"></div>'));
                        this.logo = L.build(kit);
                        this.logo.done();
                        this.logo.place(FLUX_S, FLUX_X, FLUX_Y);
                        root.append(this.logo.el);
                        root.append(kit.h('<div class="ring flux-ring" style="left:' + (sun[0] - 134) + 'px;top:' + (sun[1] - 134) + 'px;width:268px;height:268px"></div>'));
                        let svg = '<svg class="art" viewBox="0 0 1920 1080">';
                        FLUX_PTS.forEach((p) => {
                            const q = L.map(FLUX_S, FLUX_X, FLUX_Y, p[0], p[1]);
                            svg += '<path class="' + p[2] + ' flux-a" pathLength="1" d="M' + q[0].toFixed(1) + ' ' + (q[1] - 14).toFixed(1) + ' q 14 -26 0 -52 q -14 -26 0 -52 m -14 16 l 14 -16 l 14 16"/>';
                        });
                        root.append(kit.h(svg + '</svg>'));
                        const qh = L.map(FLUX_S, FLUX_X, FLUX_Y, 1190, 670), qe = L.map(FLUX_S, FLUX_X, FLUX_Y, 1690, 1160);
                        root.append(kit.h('<p class="kicker flux-qh" style="position:absolute;margin:0;left:' + (qh[0] + 26) + 'px;top:' + (qh[1] - 130) + 'px">QH</p>'));
                        root.append(kit.h('<p class="kicker flux-qe" style="position:absolute;margin:0;color:var(--main);left:' + (qe[0] + 26) + 'px;top:' + (qe[1] - 130) + 'px">QE</p>'));
                        root.append(kit.h('<div class="words" style="left:200px;top:330px;width:620px"><p class="kicker flux-kick">Surface Urban Energy and Water Balance Scheme</p><h2 class="line flux-title">Heat in, heat out, water given back.</h2></div>'));
                        this.halo = root.querySelector('.flux-halo').style;
                        this.ring = root.querySelector('.flux-ring').style;
                        this.arrows = [...root.querySelectorAll('.flux-a')].map((a) => a.style);
                        this.qh = root.querySelector('.flux-qh').style;
                        this.qe = root.querySelector('.flux-qe').style;
                        this.kick = root.querySelector('.flux-kick');
                        this.title = root.querySelector('.flux-title');
                    },
                    render(t) {
                        this.halo.opacity = String(lerp(0, 0.9, eio(seg(t, 0, 1.6))) * (1 - eo(seg(t, 4.4, 5))) * (0.9 + 0.1 * Math.sin(t * 2.4)));
                        const rp = ((t - 0.3) % 2.2 + 2.2) % 2.2 / 2.2;
                        const on = t > 0.3 && t < 4.7 ? 1 : 0;
                        this.ring.transform = 'scale(' + lerp(1, 1.6, eo(rp)) + ')';
                        this.ring.opacity = String(on * (1 - rp) * 0.7);
                        this.arrows.forEach((a, i) => {
                            const a0 = 0.8 + i * 0.22;
                            a.strokeDasharray = '1';
                            a.strokeDashoffset = String(1 - eo(seg(t, a0, a0 + 0.6)));
                            a.transform = 'translateY(' + (-Math.max(0, t - a0) * 12 + Math.sin(t * 2 + i) * 3) + 'px)';
                            a.opacity = String(1 - eo(seg(t, 4.3, 4.9)));
                        });
                        const lq = eo(seg(t, 1.9, 2.4)), le = eo(seg(t, 2.4, 2.9)), out = 1 - eo(seg(t, 4.3, 4.9));
                        this.qh.opacity = String(lq * out); this.qe.opacity = String(le * out);
                        TextFx.fadeFromRight(this.kick, t, -0.1, { stagger: 0.08, dur: 0.4 });
                        TextFx.fadeFromRight(this.title, t, 0.4, { stagger: 0.1, dur: 0.5 });
                    }
                });
            },

            // Added for the site: the papers SUEWS grew from (DOIs checked against Crossref)
            history(kit) {
                const { seg, eo, eob, lerp, TextFx } = kit;
                const MILESTONES = [
                    ['1986', 'Urban water balance', 'Grimmond, Oke & Steyn', 'Water Resour. Res.'],        // 10.1029/WR022i010p01397
                    ['1991', 'Heat storage: OHM', 'Grimmond, Cleugh & Oke', 'Atmos. Environ.'],            // 10.1016/0957-1272(91)90003-W
                    ['2002', 'Turbulent fluxes: LUMPS', 'Grimmond & Oke', 'J. Appl. Meteorol.'],           // 10.1175/1520-0450(2002)041<0792:THFIUA>2.0.CO;2
                    ['2011', 'SUEWS', 'Järvi, Grimmond & Christen', 'J. Hydrol.'],                         // 10.1016/j.jhydrol.2011.10.001
                    ['2018', 'UMEP, in QGIS', 'Lindberg et al.', 'Environ. Model. Softw.'],                // 10.1016/j.envsoft.2017.09.020
                    ['2019', 'SuPy, in Python', 'Sun & Grimmond', 'Geosci. Model Dev.']                    // 10.5194/gmd-12-2781-2019
                ];
                const X0 = 160, X1 = 1800, AY = 640, KEY = 3;
                const xs = MILESTONES.map((m, i) => 270 + i * 272);
                // each milestone comes up as the drawn axis reaches it
                const at = (x) => 0.3 + 4 * (x - X0) / (X1 - X0);
                kit.scene('history', {
                    init(kit) {
                        const root = kit.h('<div class="ground"></div>');
                        kit.stage.append(root);
                        root.append(kit.h('<div class="hist-axis" style="position:absolute;left:' + X0 + 'px;top:' + (AY - 2) + 'px;width:' + (X1 - X0) + 'px;height:4px;border-radius:2px;transform-origin:0 50%;background:linear-gradient(90deg, color-mix(in srgb, var(--second) 20%, transparent), color-mix(in srgb, var(--second) 45%, transparent))"></div>'));
                        root.append(kit.h('<svg class="art hist-head" viewBox="0 0 1920 1080"><path d="M' + (X1 - 22) + ' ' + (AY - 16) + ' L' + (X1 + 4) + ' ' + AY + ' L' + (X1 - 22) + ' ' + (AY + 16) + '" fill="none" stroke="color-mix(in srgb, var(--second) 45%, transparent)" stroke-width="4" stroke-linecap="round" stroke-linejoin="round"/></svg>'));
                        this.items = MILESTONES.map(([year, what, who, where], i) => {
                            const key = i === KEY, x = xs[i];
                            const n = kit.h('<div style="position:absolute;left:' + (x - 130) + 'px;top:0;width:260px;height:1080px;text-align:center">' +
                                '<p class="hist-year" style="position:absolute;left:0;right:0;top:' + (AY - 112) + 'px;margin:0;font-family:var(--display);font-weight:700;font-size:' + (key ? 76 : 60) + 'px;line-height:1;color:' + (key ? 'var(--accent)' : 'var(--main)') + '">' + year + '</p>' +
                                '<div class="hist-dot" style="position:absolute;left:' + (130 - (key ? 16 : 11)) + 'px;top:' + (AY - (key ? 16 : 11)) + 'px;width:' + (key ? 32 : 22) + 'px;height:' + (key ? 32 : 22) + 'px;border-radius:50%;background:' + (key ? 'var(--accent)' : 'var(--main)') + ';box-shadow:0 0 0 6px var(--ground)"></div>' +
                                (key ? '<div class="ring hist-ring" style="left:94px;top:' + (AY - 36) + 'px;width:72px;height:72px"></div>' : '') +
                                '<div class="hist-text" style="position:absolute;left:0;right:0;top:' + (AY + 40) + 'px">' +
                                '<p style="margin:0 0 10px;font-size:' + (key ? 40 : 29) + 'px;line-height:1.15;font-weight:600;' + (key ? 'font-family:var(--display);font-weight:700;color:var(--accent)' : '') + '">' + kit.esc(what) + '</p>' +
                                '<p class="cm" style="margin:0;font-size:21px;line-height:1.35">' + kit.esc(who) + '<br><i>' + kit.esc(where) + '</i></p></div></div>');
                            root.append(n);
                            const ring = n.querySelector('.hist-ring');
                            return {
                                t: at(x), year: n.querySelector('.hist-year').style, dot: n.querySelector('.hist-dot').style,
                                text: n.querySelector('.hist-text').style, ring: ring && ring.style
                            };
                        });
                        root.append(kit.h('<div class="words" style="left:160px;top:170px;width:1200px"><p class="kicker hist-kick">1986 to 2019</p><h2 class="line hist-title">Decades in the making</h2></div>'));
                        this.root = root.style;
                        this.axis = root.querySelector('.hist-axis').style;
                        this.head = root.querySelector('.hist-head').style;
                        this.kick = root.querySelector('.hist-kick');
                        this.title = root.querySelector('.hist-title');
                    },
                    render(t) {
                        const out = 1 - eo(seg(t, 5.4, 5.95));
                        this.root.opacity = String(out);
                        this.axis.transform = 'scaleX(' + seg(t, 0.3, 4.3) + ')';
                        this.head.opacity = String(eo(seg(t, 4.2, 4.5)));
                        for (const m of this.items) {
                            const p = seg(t, m.t, m.t + 0.45);
                            m.dot.transform = 'scale(' + (p > 0 ? eob(p, 2) : 0) + ')';
                            m.year.opacity = String(eo(p));
                            m.year.transform = 'translateY(' + lerp(18, 0, eo(p)) + 'px)';
                            const q = eo(seg(t, m.t + 0.15, m.t + 0.65));
                            m.text.opacity = String(q);
                            m.text.transform = 'translateY(' + lerp(-14, 0, q) + 'px)';
                            if (m.ring) {
                                const rp = ((t - m.t) % 1.6 + 1.6) % 1.6 / 1.6;
                                m.ring.transform = 'scale(' + lerp(0.6, 1.5, eo(rp)) + ')';
                                m.ring.opacity = String(t > m.t ? (1 - rp) * 0.7 : 0);
                            }
                        }
                        TextFx.fadeFromRight(this.kick, t, 0, { stagger: 0.08, dur: 0.4 });
                        TextFx.fadeFromRight(this.title, t, 0.3, { stagger: 0.1, dur: 0.5 });
                    }
                });
            },

            arup(kit) {
                const { seg, eo, eio, eob, lerp, TextFx } = kit;
                kit.scene('arup', {
                    init(kit) {
                        const root = kit.h('<div class="ground"></div>');
                        kit.stage.append(root);
                        root.append(kit.h('<div class="panel arup-panel" style="left:920px;top:190px;width:840px;height:660px;transform-origin:1340px 520px">' +
                            '<div style="position:absolute;left:0;right:0;top:0;height:76px;border-bottom:2px solid color-mix(in srgb, var(--second) 16%, transparent)"></div>' +
                            '<p class="code" style="position:absolute;left:36px;top:14px;margin:0;font-size:30px">UHeat</p>' +
                            '<p class="kicker arup-chip" style="position:absolute;right:30px;top:20px;margin:0;padding:6px 16px;border-radius:20px;font-size:22px;border:2px solid var(--accent)">engine: SUEWS</p></div>'));
                        this.logo = LOGO.build(kit);
                        this.logo.done();
                        root.append(this.logo.el);
                        // 2023: Arup's first public UHeat release (21 Aug 2023); the film had July 2021
                        root.append(kit.h('<div class="words arup-words" style="left:160px;top:360px;width:660px"><p class="kicker arup-kick">2023</p><h2 class="line arup-title">Into practice</h2><p class="sub arup-sub">SUEWS goes to work inside Arup\'s UHeat, mapping heat in real cities.</p></div>'));
                        this.panel = root.querySelector('.arup-panel').style;
                        this.chip = root.querySelector('.arup-chip').style;
                        this.kick = root.querySelector('.arup-kick');
                        this.title = root.querySelector('.arup-title');
                        this.sub = root.querySelector('.arup-sub');
                    },
                    render(t) {
                        const p = eio(seg(t, 0.3, 1.6));
                        const br = 1 + 0.008 * Math.sin(Math.max(0, t - 1.6) * 2);
                        this.logo.place(lerp(0.42, 0.28, p) * br, lerp(1250, 1340, p), lerp(560, 590, p));
                        const pp = seg(t, 0.7, 1.5);
                        this.panel.opacity = String(eo(pp));
                        this.panel.transform = 'scale(' + lerp(0.94, 1, eob(pp, 1.6)) + ')';
                        const cp = seg(t, 1.9, 2.4);
                        this.chip.opacity = String(eo(cp));
                        this.chip.transform = 'translateX(' + lerp(20, 0, eo(cp)) + 'px)';
                        TextFx.fadeFromRight(this.kick, t, 0.6, { stagger: 0.08, dur: 0.4 });
                        TextFx.fadeFromRight(this.title, t, 0.9, { stagger: 0.11, dur: 0.5 });
                        TextFx.fadeFromRight(this.sub, t, 1.6, { stagger: 0.06, dur: 0.45 });
                    }
                });
            },

            ccra(kit) {
                const { seg, eo, eio, eob, lerp, TextFx } = kit;
                kit.scene('ccra', {
                    init(kit) {
                        const root = kit.h('<div class="ground"></div>');
                        kit.stage.append(root);
                        let bars = '';
                        const W = [480, 520, 440, 500, 300, 0, 510, 470, 520, 380, 0, 490, 430];
                        W.forEach((w, i) => { if (w) bars += '<div class="ccra-bar' + (i === 7 ? ' ccra-key' : '') + '" style="position:absolute;left:56px;top:' + (300 + i * 36) + 'px;width:' + w + 'px;height:14px;border-radius:7px;background:color-mix(in srgb, var(--ground) 22%, var(--second))"></div>'; });
                        root.append(kit.h('<div class="paper ccra-page" style="left:1040px;top:150px;width:620px;height:820px;transform-origin:1350px 1000px">' +
                            '<p class="kicker" style="position:absolute;left:56px;top:60px;margin:0;font-size:20px;color:color-mix(in srgb, var(--ground) 70%, var(--second))">Climate Change Committee / CCRA4</p>' +
                            '<p class="line" style="position:absolute;left:56px;top:110px;width:520px;margin:0;font-size:64px">A Well-Adapted UK</p>' +
                            '<div class="mark ccra-mark" style="left:46px;top:546px;width:490px;height:30px;transform-origin:0 50%"></div>' + bars + '</div>'));
                        root.append(kit.h('<div class="ccra-tag" style="position:absolute;left:1700px;top:540px;display:flex;align-items:center;gap:0"><div class="ccra-lead" style="width:40px;height:3px;background:var(--accent);transform-origin:0 50%"></div><p class="kicker" style="margin:0;padding:6px 16px;border-radius:20px;border:2px solid var(--accent);font-size:24px">SUEWS</p></div>'));
                        root.append(kit.h('<div class="words" style="left:160px;top:330px;width:760px"><p class="kicker ccra-kick">May 2026</p><h2 class="line ccra-title">Into policy</h2><p class="sub ccra-sub">Arup\'s urban heat work for the UK\'s fourth climate risk assessment used SUEWS.</p></div>'));
                        this.page = root.querySelector('.ccra-page').style;
                        this.mark = root.querySelector('.ccra-mark').style;
                        this.tag = root.querySelector('.ccra-tag').style;
                        this.lead = root.querySelector('.ccra-lead').style;
                        this.kick = root.querySelector('.ccra-kick');
                        this.title = root.querySelector('.ccra-title');
                        this.sub = root.querySelector('.ccra-sub');
                    },
                    render(t) {
                        const pp = seg(t, -0.2, 1.0);
                        this.page.transform = 'translateY(' + lerp(260, 0, eob(pp, 1.4)) + 'px) rotate(' + (lerp(-5, -1.5, eio(pp)) + 0.4 * Math.sin(t * 1.3)) + 'deg)';
                        this.page.opacity = eo(seg(t, -0.2, 0.4));
                        this.mark.transform = 'scaleX(' + eo(seg(t, 2.0, 2.5)) + ')';
                        const tp = seg(t, 2.5, 3.0);
                        this.tag.opacity = eo(tp);
                        this.tag.transform = 'translateX(' + lerp(24, 0, eo(tp)) + 'px)';
                        this.lead.transform = 'scaleX(' + eo(tp) + ')';
                        TextFx.fadeFromRight(this.kick, t, 0.5, { stagger: 0.08, dur: 0.4 });
                        TextFx.fadeFromRight(this.title, t, 0.8, { stagger: 0.11, dur: 0.5 });
                        TextFx.fadeFromRight(this.sub, t, 1.4, { stagger: 0.06, dur: 0.45 });
                    }
                });
            },

            end(kit) {
                const { seg, eo, eio, eob, lerp } = kit;
                kit.scene('end', {
                    init(kit) {
                        const root = kit.el('div', { class: 'ground' });
                        const hold = kit.el('div', { style: 'position:absolute;left:0;top:0;width:1920px;height:1080px;transform-origin:960px 540px' });
                        const logo = kit.el('div', { class: 'end-logo', style: 'position:absolute;left:840px;top:150px;width:240px;height:240px;transform-origin:120px 120px', html: kit.module('logo.html') || '' });
                        const title = kit.el('div', { class: 'big', style: 'position:absolute;left:0;top:410px;width:1920px;text-align:center;font-size:200px', text: 'SUEWS' });
                        const rule = kit.el('div', { style: 'position:absolute;left:810px;top:650px;width:300px;height:8px;border-radius:4px;background:var(--accent);transform-origin:150px 4px' });
                        const tag = kit.el('div', { class: 'sub', style: 'position:absolute;left:0;top:690px;width:1920px;text-align:center;font-size:48px;color:var(--second)', text: 'Built by scientists. Used for decisions.' });
                        const small = kit.el('div', { class: 'kicker', style: 'position:absolute;left:0;top:790px;width:1920px;text-align:center', text: 'Surface Urban Energy and Water Balance Scheme' });
                        hold.append(logo, title, rule, tag, small);
                        root.append(hold);
                        kit.stage.append(root);
                        this.logoEl = logo;
                        this.els = { hold: hold.style, logo: logo.style, title: title.style, rule: rule.style, tag: tag.style, small: small.style };
                    },
                    render(t) {
                        const e = this.els;
                        const lp = seg(t, 0, 0.7);
                        e.logo.opacity = eo(seg(t, 0, 0.25));
                        e.logo.transform = 'scale(' + lerp(0.2, 1, eob(lp, 1.7)) + ') rotate(' + lerp(-12, 0, eo(lp)) + 'deg)';
                        const a = eo(seg(t, 0.4, 0.9));
                        e.title.opacity = a;
                        e.title.transform = 'translateY(' + lerp(24, 0, a) + 'px)';
                        e.rule.transform = 'scaleX(' + eo(seg(t, 0.9, 1.3)) + ')';
                        const b = eo(seg(t, 1.2, 1.8));
                        e.tag.opacity = b;
                        e.tag.transform = 'translateX(' + lerp(26, 0, b) + 'px)';
                        e.small.opacity = eo(seg(t, 1.9, 2.5));
                        e.hold.transform = 'scale(' + lerp(1, 1.02, eio(seg(t, 0.5, 5))) + ')';
                    }
                });
            }
        };

        // ---- Stage ----
        const film = el('div', { class: 'film', attrs: { 'aria-hidden': 'true' } });
        const stage = el('div', { attrs: { id: 'film-stage' } });
        film.append(stage);
        const controls = el('div', { class: 'film-controls' });
        const soundBtn = el('button', { class: 'film-btn', text: '\u{1F50A} Sound on', attrs: { type: 'button', 'aria-pressed': 'false' } });
        const skipBtn = el('button', { class: 'film-btn', text: 'Skip', attrs: { type: 'button' } });
        controls.append(soundBtn, skipBtn);
        document.body.append(film, controls);

        const fit = () => {
            const s = Math.min(window.innerWidth / 1920, window.innerHeight / 1080);
            stage.style.transform = 'translate(' + ((window.innerWidth - 1920 * s) / 2) + 'px,' + ((window.innerHeight - 1080 * s) / 2) + 'px) scale(' + s + ')';
        };
        fit();
        window.addEventListener('resize', fit);

        for (const s of SEQ) {
            s.box = el('div', { class: 'film-scene' });
            stage.append(s.box);
            s.kit = Object.assign(Object.create(base), { stage: s.box, start: s.start, speed: s.speed, dur: s.dur });
            MODULES[s.id](s.kit);
            s.def = SCENES[s.id];
            s.def.init.call(s.def, s.kit);
        }

        // ---- Score: the film's score.js (120 BPM), timed from the cut ----
        const EV = [];
        const beat = (b) => b * 0.5;
        const MIDI = (n) => {
            const m = /^([A-G])(#|b)?(-?\d)$/.exec(n);
            return 12 * (+m[3] + 1) + { C: 0, D: 2, E: 4, F: 5, G: 7, A: 9, B: 11 }[m[1]] + (m[2] === '#' ? 1 : m[2] === 'b' ? -1 : 0);
        };
        const TRACKS = {
            pad: { patch: 'pad', rev: 0.6, gain: -2 },
            keys: { patch: 'ep', rev: 0.3 },
            mallet: { patch: 'marimba', pan: 0.2 },
            bass: { patch: 'bass' },
            bell: { patch: 'glock', rev: 0.5, pan: -0.2 },
            kit: { patch: 'drums', rev: 0.1 },
            fx: { patch: 'fx', rev: 0.3 }
        };
        const S = {
            note(tr, p, { beat: b, beats, t, dur, vel = 64 }) {
                const at = t != null ? t : beat(b), len = dur != null ? dur : beat(beats || 1);
                EV.push({ t: at, tr, kind: 'note', midi: MIDI(p), dur: len, vel });
            },
            chord(tr, ps, o) { ps.forEach((p) => S.note(tr, p, o)); },
            pattern(tr, str, { from, repeat = 1, pitch, vel = 64 }) {
                let k = 0;
                for (let r = 0; r < repeat; r++) {
                    for (let i = 0; i < str.length; i++) {
                        const c = str[i];
                        if (c === '.') continue;
                        S.note(tr, pitch[k++ % pitch.length], { beat: from + (r * str.length + i) * 0.5, beats: 0.5, vel: c === 'X' ? Math.min(127, vel * 1.25) : vel });
                    }
                }
            },
            drum(name, { beat: b, t, vel = 80 }) { EV.push({ t: t != null ? t : beat(b), tr: 'kit', kind: name, vel }); },
            drums(map, { from, repeat = 1, vel = 64 }) {
                for (const name in map) {
                    const str = map[name];
                    for (let r = 0; r < repeat; r++) {
                        for (let i = 0; i < str.length; i++) {
                            const c = str[i];
                            if (c === '.') continue;
                            S.drum(name, { beat: from + (r * str.length + i) * 0.5, vel: c === 'o' ? vel * 0.45 : c === 'X' ? vel * 1.25 : vel });
                        }
                    }
                }
            },
            fx(name, t, { dur = 0.4, vel = 50 } = {}) { EV.push({ t, tr: 'fx', kind: name, dur, vel }); },
            ticks(times, { vel = 24 } = {}) { times.forEach((t) => EV.push({ t, tr: 'fx', kind: 'tick', vel })); },
            run(tr, ps, { t, step, vel, cresc = 0 }) { ps.forEach((p, i) => S.note(tr, p, { t: t + i * step, dur: 0.6, vel: vel + i * cresc })); }
        };
        (function score() {
            const prog = [['D3', 'A3', 'C#4', 'E4'], ['B2', 'F#3', 'A3', 'D4'], ['G2', 'D3', 'F#3', 'B3'], ['A2', 'E3', 'G3', 'C#4']];
            const bass = ['D2', 'B1', 'G1', 'A1'];
            const sp = (id) => SEQ.find((s) => s.id === id).speed;
            // code: quiet pad and keystrokes
            S.chord('pad', ['D3', 'A3', 'E4'], { beat: 0, beats: 8, vel: 34 });
            S.ticks(typingTimes, { vel: 24 });
            // grow: marimba pulse, a pop as each logo part starts to fill, a sparkle as it sets
            S.chord('pad', prog[1], { beat: 8, beats: 12, vel: 30 });
            S.pattern('mallet', 'x.x.X.x.', { from: 8, repeat: 3, pitch: ['F#5', 'A4', 'D5', 'A4'], vel: 50 });
            [0.5, 0.9, 1.5, 1.8, 2.2, 2.7].forEach((d, i) => S.fx('pop', T.grow + d / sp('grow'), { vel: 30 + i * 4 }));
            S.fx('sparkle', T.grow + 3.8 / sp('grow'), { vel: 34 });
            // flux onward: the groove in two-second bars, stopping a second short of the end card
            const from = T.flux / 0.5, bars = Math.round((T.end - 1 - T.flux) / 2);
            for (let bar = 0; bar < bars; bar++) {
                const b = from + bar * 4, c = bar % 4;
                S.chord('keys', prog[c], { beat: b, beats: 3.5, vel: 46 });
                S.note('bass', bass[c], { beat: b, beats: 1.5, vel: 74 });
                S.note('bass', bass[c], { beat: b + 2.5, beats: 1, vel: 60 });
            }
            S.pattern('mallet', 'x.xX.xx.', { from, repeat: bars, pitch: ['D5', 'F#5', 'A5', 'E5'], vel: 54 });
            S.drums({ kick: 'x...x...', sidestick: '..x...x.', hat: 'oxoxoxox' }, { from, repeat: bars, vel: 60 });
            // history: a bell as each milestone lands, the brightest for SUEWS itself
            ['D5', 'E5', 'F#5', 'A5', 'B5', 'D6'].forEach((p, i) => {
                const x = 270 + i * 272;
                S.note('bell', p, { t: T.history + (0.3 + 4 * (x - 160) / 1640) / sp('history'), dur: 0.8, vel: i === 3 ? 58 : 40 });
            });
            // into practice: chip lands; into policy: highlight and tag
            S.fx('pop', T.arup + 1.9 / sp('arup'), { vel: 50 });
            S.fx('whoosh', T.ccra + 2.0 / sp('ccra'), { dur: 0.35, vel: 36 });
            S.fx('ding', T.ccra + 2.5 / sp('ccra'), { vel: 48 });
            // build into the end card, then the hit and let it ring
            S.run('bell', ['D5', 'E5', 'F#5', 'A5', 'B5', 'C#6', 'D6'], { t: T.end - 0.7, step: 0.1, vel: 44, cresc: 8 });
            S.chord('keys', ['D3', 'A3', 'D4', 'F#4', 'E5'], { t: T.end, dur: beat(8), vel: 68 });
            S.chord('pad', ['D3', 'A3', 'F#4'], { t: T.end, dur: beat(10), vel: 44 });
            S.note('bass', 'D2', { t: T.end, dur: beat(4), vel: 84 });
            S.drum('kick', { t: T.end, vel: 90 });
            S.drum('crash', { t: T.end, vel: 50 });
            S.note('bell', 'A5', { t: T.end + beat(2), dur: beat(2), vel: 40 });
        })();

        // ---- Synth: small Web Audio stand-ins for the player's patches ----
        const Sound = (() => {
            let ctx = null, master = null, outs = {}, noise = null, on = false, ended = false;
            const LEVEL = Math.pow(10, -6 / 20);
            const amp = (vel) => Math.pow(Math.min(127, vel) / 127, 1.4);
            const hz = (m) => 440 * Math.pow(2, (m - 69) / 12);
            function decay(g, when, peak, att, dec) {
                g.setValueAtTime(0.0001, when);
                g.exponentialRampToValueAtTime(Math.max(peak, 0.0002), when + att);
                g.exponentialRampToValueAtTime(0.0001, when + att + dec);
            }
            function osc(type, f, when, stop, dest) {
                const o = ctx.createOscillator();
                o.type = type; o.frequency.setValueAtTime(f, when);
                o.connect(dest); o.start(when); o.stop(stop);
                return o;
            }
            function gain(dest) { const g = ctx.createGain(); g.connect(dest); return g; }
            function hiss(when, dur, filter, f, q, dest) {
                const src = ctx.createBufferSource();
                src.buffer = noise;
                const bq = ctx.createBiquadFilter();
                bq.type = filter; bq.frequency.setValueAtTime(f, when); bq.Q.value = q || 0.7;
                src.connect(bq); bq.connect(dest);
                src.start(when, Math.random()); src.stop(when + dur);
                return bq;
            }
            function note(e, when, out) {
                const f = hz(e.midi), a = amp(e.vel), p = TRACKS[e.tr].patch;
                if (p === 'pad') {
                    const g = gain(out), lp = ctx.createBiquadFilter();
                    lp.type = 'lowpass'; lp.frequency.value = 1400; lp.connect(g);
                    g.gain.setValueAtTime(0, when);
                    g.gain.linearRampToValueAtTime(a * 0.07, when + 0.3);
                    g.gain.setValueAtTime(a * 0.07, when + e.dur);
                    g.gain.linearRampToValueAtTime(0, when + e.dur + 0.8);
                    for (const det of [-7, 7]) osc('sawtooth', f, when, when + e.dur + 0.9, lp).detune.value = det;
                } else if (p === 'ep') {
                    const g = gain(out), mg = ctx.createGain();
                    const len = Math.min(e.dur + 0.6, 2.6);
                    decay(g.gain, when, a * 0.16, 0.005, len);
                    const car = osc('sine', f, when, when + len + 0.1, g);
                    mg.gain.setValueAtTime(f * 1.2, when);
                    mg.gain.exponentialRampToValueAtTime(f * 0.1, when + 0.5);
                    mg.connect(car.frequency);
                    osc('sine', f, when, when + len + 0.1, mg);
                } else if (p === 'marimba') {
                    const g = gain(out), g2 = gain(out);
                    decay(g.gain, when, a * 0.3, 0.003, 0.45);
                    decay(g2.gain, when, a * 0.07, 0.002, 0.06);
                    osc('sine', f, when, when + 0.6, g);
                    osc('sine', f * 4, when, when + 0.1, g2);
                } else if (p === 'bass') {
                    const g = gain(out), lp = ctx.createBiquadFilter();
                    lp.type = 'lowpass'; lp.frequency.value = 700; lp.connect(g);
                    decay(g.gain, when, a * 0.32, 0.01, e.dur + 0.15);
                    osc('triangle', f, when, when + e.dur + 0.3, lp);
                    osc('sine', f, when, when + e.dur + 0.3, lp);
                } else {
                    const g = gain(out), g2 = gain(out);
                    decay(g.gain, when, a * 0.16, 0.002, 1.4);
                    decay(g2.gain, when, a * 0.05, 0.002, 0.35);
                    osc('sine', f, when, when + 1.5, g);
                    osc('sine', f * 2.76, when, when + 0.4, g2);
                }
            }
            function hit(e, when, out) {
                const a = amp(e.vel);
                const g = gain(out);
                switch (e.kind) {
                    case 'kick': {
                        decay(g.gain, when, a * 0.8, 0.002, 0.35);
                        const o = osc('sine', 150, when, when + 0.4, g);
                        o.frequency.exponentialRampToValueAtTime(45, when + 0.12);
                        break;
                    }
                    case 'sidestick': decay(g.gain, when, a * 0.4, 0.001, 0.05); hiss(when, 0.08, 'bandpass', 1900, 4, g); break;
                    case 'hat': decay(g.gain, when, a * 0.16, 0.001, 0.045); hiss(when, 0.06, 'highpass', 7000, 0.7, g); break;
                    case 'crash': decay(g.gain, when, a * 0.22, 0.002, 1.6); hiss(when, 1.7, 'highpass', 4500, 0.7, g); break;
                    case 'tick': decay(g.gain, when, a * 0.35, 0.001, 0.018); hiss(when, 0.03, 'bandpass', 2500 + Math.random() * 1500, 6, g); break;
                    case 'pop': {
                        decay(g.gain, when, a * 0.35, 0.002, 0.09);
                        const o = osc('sine', 520, when, when + 0.12, g);
                        o.frequency.exponentialRampToValueAtTime(1040, when + 0.06);
                        break;
                    }
                    case 'whoosh': {
                        g.gain.setValueAtTime(0, when);
                        g.gain.linearRampToValueAtTime(a * 0.3, when + e.dur * 0.7);
                        g.gain.linearRampToValueAtTime(0, when + e.dur);
                        const bq = hiss(when, e.dur + 0.05, 'bandpass', 400, 1.2, g);
                        bq.frequency.exponentialRampToValueAtTime(2600, when + e.dur);
                        break;
                    }
                    case 'sparkle':
                        ['A6', 'D7', 'F#6', 'B6', 'E7', 'A7'].forEach((n, i) => note({ midi: MIDI(n), vel: e.vel, tr: 'bell', dur: 0.3 }, when + i * 0.06, out));
                        break;
                    case 'ding':
                        note({ midi: MIDI('E6'), vel: e.vel, tr: 'bell', dur: 1 }, when, out);
                        note({ midi: MIDI('B6'), vel: e.vel * 0.6, tr: 'bell', dur: 1 }, when, out);
                        break;
                }
            }
            function build() {
                ctx = new (window.AudioContext || window.webkitAudioContext)();
                const comp = ctx.createDynamicsCompressor();
                comp.connect(ctx.destination);
                master = ctx.createGain();
                master.gain.value = 0;
                master.connect(comp);
                const verb = ctx.createConvolver();
                const len = Math.floor(ctx.sampleRate * 2.2), ir = ctx.createBuffer(2, len, ctx.sampleRate);
                for (let c = 0; c < 2; c++) {
                    const ch = ir.getChannelData(c);
                    for (let i = 0; i < len; i++) ch[i] = (Math.random() * 2 - 1) * Math.pow(1 - i / len, 3);
                }
                verb.buffer = ir; verb.connect(master);
                noise = ctx.createBuffer(1, ctx.sampleRate * 2, ctx.sampleRate);
                const nd = noise.getChannelData(0);
                for (let i = 0; i < nd.length; i++) nd[i] = Math.random() * 2 - 1;
                for (const k in TRACKS) {
                    const tr = TRACKS[k], g = ctx.createGain(), pan = ctx.createStereoPanner();
                    g.gain.value = Math.pow(10, (tr.gain || 0) / 20);
                    pan.pan.value = tr.pan || 0;
                    g.connect(pan); pan.connect(master);
                    if (tr.rev) { const s = ctx.createGain(); s.gain.value = tr.rev * 0.6; g.connect(s); s.connect(verb); }
                    outs[k] = g;
                }
            }
            function schedule(filmT) {
                const now = ctx.currentTime + 0.06;
                for (const e of EV) {
                    if (e.t < filmT - 0.02) continue;
                    const when = now + (e.t - filmT);
                    (e.kind === 'note' ? note : hit)(e, when, outs[e.tr]);
                }
                master.gain.setValueAtTime(LEVEL, now);
                const fadeAt = now + Math.max(0, END - 1.6 - filmT);
                master.gain.setValueAtTime(LEVEL, fadeAt);
                master.gain.linearRampToValueAtTime(0, fadeAt + 1.6 + 0.8);
                on = true;
            }
            return {
                get on() { return on; },
                // Without a click: start from the top only if the browser lets audio run
                autoplay() {
                    build();
                    if (ctx.state !== 'running') { ctx.close(); ctx = null; outs = {}; return false; }
                    schedule(0);
                    return true;
                },
                toggle(filmT) {
                    if (ended) return;
                    if (!ctx) {
                        build();
                        schedule(filmT);
                    } else {
                        on = !on;
                        master.gain.cancelScheduledValues(ctx.currentTime);
                        master.gain.setTargetAtTime(on ? LEVEL : 0, ctx.currentTime, 0.05);
                    }
                    if (ctx.state === 'suspended') ctx.resume();
                },
                stop(fade) {
                    ended = true;
                    if (!ctx) return;
                    master.gain.cancelScheduledValues(ctx.currentTime);
                    master.gain.setTargetAtTime(0, ctx.currentTime, fade);
                    setTimeout(() => ctx.close(), fade * 6000 + 200);
                }
            };
        })();

        // ---- Playback and the hand-off to the hero ----
        let t0 = performance.now(), filmT = 0, swap = null, done = false;
        const endScene = SEQ[SEQ.length - 1];

        function render(t) {
            if (t < HAND) {
                let s = SEQ[0];
                for (const x of SEQ) if (t >= x.start) s = x;
                for (const x of SEQ) x.box.style.display = x === s ? '' : 'none';
                s.def.render.call(s.def, (t - s.start) * s.speed, s.kit);
                return;
            }
            if (!swap) {
                // Freeze the end card and lay the hero logo exactly over the film's
                endScene.def.render.call(endScene.def, endScene.dur * endScene.speed, endScene.kit);
                for (const x of SEQ) x.box.style.display = x === endScene ? '' : 'none';
                const fr = endScene.def.logoEl.firstElementChild.getBoundingClientRect();
                const u = fr.width / 2360;
                const hr = heroSvg.getBoundingClientRect();
                const size = 2048 * u;
                swap = {
                    dx: fr.left + 180 * u + size / 2 - (hr.left + hr.width / 2),
                    dy: fr.top + 155 * u + size / 2 - (hr.top + hr.height / 2),
                    k: size / hr.width
                };
                root.classList.add('film-swap');
            }
            const p = eio(seg(t, HAND, END));
            box.style.transform = 'translate(' + (swap.dx * (1 - p)).toFixed(1) + 'px,' + (swap.dy * (1 - p)).toFixed(1) + 'px) scale(' + (swap.k + (1 - swap.k) * p).toFixed(4) + ')';
            endScene.def.logoEl.style.opacity = String(1 - seg(t, HAND, HAND + 0.25));
            film.style.opacity = String(1 - eo(seg(t, HAND + 0.15, END - 0.1)));
            controls.style.opacity = String(1 - seg(t, HAND - 0.3, HAND));
            if (t >= HAND + 0.3) root.classList.add('film-reveal');
        }

        function finish() {
            if (done) return;
            done = true;
            window.removeEventListener('resize', fit);
            document.removeEventListener('keydown', onKey);
            film.remove();
            controls.remove();
            if (poster) poster.remove();
            box.style.transform = '';
            root.classList.remove('logo-intro', 'film-running', 'film-reveal', 'film-swap');
            playing = false;
        }

        // The clock starts here, straight away or from the play button
        let started = false, poster = null;
        function begin(at) {
            if (started) return;
            started = true;
            if (poster) { poster.remove(); poster = null; }
            controls.style.visibility = stage.style.visibility = '';
            requestAnimationFrame((now) => { t0 = now - at * 1000; frame(now); });
        }
        function jump() {
            if (!started) { Sound.stop(0); begin(HAND); return; }
            const t = (performance.now() - t0) / 1000;
            if (t < HAND) {
                t0 -= (HAND - t) * 1000;
                if (Sound.on) Sound.stop(0.15);
            }
        }
        function syncSoundBtn() {
            soundBtn.setAttribute('aria-pressed', String(Sound.on));
            soundBtn.textContent = Sound.on ? '\u{1F507} Mute' : '\u{1F50A} Sound on';
        }
        function onKey(e) { if (e.key === 'Escape') jump(); }
        skipBtn.addEventListener('click', jump);
        soundBtn.addEventListener('click', () => {
            Sound.toggle(started ? filmT : 0);
            syncSoundBtn();
            begin(0);
        });
        document.addEventListener('keydown', onKey);

        function frame(now) {
            filmT = (now - t0) / 1000;
            try {
                render(filmT);
            } catch (err) {
                Sound.stop(0.1);
                finish();
                throw err;
            }
            if (filmT < END) requestAnimationFrame(frame);
            else finish();
        }
        render(0);
        if (mode === 'on') Sound.toggle(0);
        if (mode === 'try' && !Sound.autoplay()) {
            // The browser wants a click before any sound: hold the first frame
            poster = el('button', { class: 'film-play', attrs: { type: 'button' }, html: '<strong>From code to climate policy</strong><span class="film-play-icon" aria-hidden="true"></span><span>Play with sound</span><small>' + Math.round(END) + ' s</small>' });
            poster.addEventListener('click', () => { Sound.toggle(0); syncSoundBtn(); begin(0); });
            controls.style.visibility = stage.style.visibility = 'hidden';
            controls.after(poster);
            poster.focus();
            return;
        }
        syncSoundBtn();
        begin(0);
    }

    // The hero logo plays it, with sound, since that is a click; a hint
    // shows on hover
    const logoBtn = document.querySelector('.logo-link');
    const hint = document.querySelector('.film-hint');
    if (logoBtn && hint) {
        hint.querySelector('.film-hint-len').textContent = '\u00b7 ' + Math.round(END) + ' s';
        hint.hidden = false;
        logoBtn.setAttribute('aria-label', 'Watch the SUEWS story (' + Math.round(END) + ' seconds, with sound)');
        logoBtn.addEventListener('click', () => play('on'));
    }
    if (root.classList.contains('logo-intro')) play('try');
})();
