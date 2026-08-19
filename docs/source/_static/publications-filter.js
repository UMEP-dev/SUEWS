/*
 * publications-filter.js — chip-based topic filter for the SUEWS
 * publication pages (`related_publications`, `community_publications`).
 *
 * Collaborates with the Sphinx extension
 * `_ext/publications_topic_annotate.py`, which stamps each bibliography
 * <li> with a `pub-entry` class plus one `pub-kw-<slug>` per keyword
 * from the BibTeX entry. This script reads those classes, renders a
 * row of chip buttons inside `#pub-filter-mount`, and shows/hides
 * entries as chips toggle.
 *
 * Also synchronises state with `?topic=<slug>[,<slug>...]` in the URL
 * so the SUEWS landing page can deep-link a chip-active view.
 */
(function () {
  'use strict';

  const SLUG_LABELS = {
    'energy-balance': 'Energy balance',
    'water-balance': 'Water balance',
    'radiation': 'Radiation',
    'storage-heat': 'Storage heat',
    'anthropogenic-heat': 'Anthropogenic heat',
    'building-energy': 'Building energy',
    'carbon-flux': 'Carbon flux',
    'model-infrastructure': 'Model infrastructure',
  };

  let entries = [];

  function readTopicsFromUrl() {
    const params = new URLSearchParams(window.location.search);
    const raw = params.get('topic');
    if (!raw) return new Set();
    return new Set(
      raw.split(',').map(s => s.trim().toLowerCase()).filter(Boolean)
    );
  }

  function writeTopicsToUrl(active) {
    const params = new URLSearchParams(window.location.search);
    if (active.size === 0) {
      params.delete('topic');
    } else {
      params.set('topic', Array.from(active).sort().join(','));
    }
    const qs = params.toString();
    const newUrl = window.location.pathname + (qs ? '?' + qs : '') + window.location.hash;
    window.history.replaceState(null, '', newUrl);
  }

  function collectEntries() {
    const out = [];
    document.querySelectorAll('li.pub-entry').forEach(li => {
      const slugs = [];
      li.classList.forEach(cls => {
        if (cls.startsWith('pub-kw-')) {
          slugs.push(cls.slice('pub-kw-'.length));
        }
      });
      out.push({ li, slugs });
    });
    return out;
  }

  function countSlugs(items) {
    const counts = new Map();
    items.forEach(({ slugs }) => {
      slugs.forEach(s => counts.set(s, (counts.get(s) || 0) + 1));
    });
    return counts;
  }

  function applyFilter(items, active) {
    let visible = 0;
    items.forEach(({ li, slugs }) => {
      let show = true;
      if (active.size > 0) {
        show = slugs.some(s => active.has(s));
      }
      li.hidden = !show;
      if (show) visible++;
    });
    toggleEmptyState(visible === 0 && active.size > 0);
  }

  function toggleEmptyState(isEmpty) {
    let empty = document.getElementById('pub-filter-empty');
    if (isEmpty) {
      if (!empty) {
        const mount = document.getElementById('pub-filter-mount');
        if (!mount || !mount.parentNode) return;
        empty = document.createElement('p');
        empty.id = 'pub-filter-empty';
        empty.className = 'pub-filter__empty';
        empty.textContent = 'No publications match the selected topics.';
        mount.parentNode.insertBefore(empty, mount.nextSibling);
      }
    } else if (empty) {
      empty.remove();
    }
  }

  function renderChips(mount, counts, active) {
    mount.innerHTML = '';

    const allBtn = document.createElement('button');
    allBtn.type = 'button';
    allBtn.className = 'pub-filter__chip pub-filter__chip--all';
    allBtn.textContent = 'All';
    allBtn.setAttribute('aria-pressed', active.size === 0 ? 'true' : 'false');
    if (active.size === 0) allBtn.classList.add('is-active');
    allBtn.addEventListener('click', () => {
      active.clear();
      applyFilter(entries, active);
      writeTopicsToUrl(active);
      renderChips(mount, counts, active);
    });
    mount.appendChild(allBtn);

    const slugs = Array.from(counts.keys()).sort((a, b) => {
      const diff = (counts.get(b) || 0) - (counts.get(a) || 0);
      return diff !== 0 ? diff : a.localeCompare(b);
    });

    slugs.forEach(slug => {
      const count = counts.get(slug) || 0;
      const btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'pub-filter__chip';
      btn.dataset.slug = slug;
      const label = SLUG_LABELS[slug] || slug;
      btn.innerHTML = '';
      btn.appendChild(document.createTextNode(label));
      const countSpan = document.createElement('span');
      countSpan.className = 'pub-filter__count';
      countSpan.textContent = String(count);
      btn.appendChild(countSpan);

      const isActive = active.has(slug);
      btn.setAttribute('aria-pressed', isActive ? 'true' : 'false');
      if (isActive) btn.classList.add('is-active');
      if (count === 0) {
        btn.classList.add('is-empty');
        btn.disabled = true;
      }
      btn.addEventListener('click', () => {
        if (active.has(slug)) active.delete(slug);
        else active.add(slug);
        applyFilter(entries, active);
        writeTopicsToUrl(active);
        renderChips(mount, counts, active);
      });
      mount.appendChild(btn);
    });
  }

  function init() {
    const mount = document.getElementById('pub-filter-mount');
    if (!mount) return;
    entries = collectEntries();
    if (entries.length === 0) {
      mount.remove();
      return;
    }
    const counts = countSlugs(entries);
    const urlActive = readTopicsFromUrl();
    const active = new Set();
    urlActive.forEach(s => { if (counts.has(s)) active.add(s); });
    renderChips(mount, counts, active);
    applyFilter(entries, active);
    if (urlActive.size !== active.size) writeTopicsToUrl(active);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
