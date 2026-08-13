(function () {
  'use strict';

  var state = {
    demoId: 'tbi-private-demo',
    expiresAtClient: 0,
    timer: null
  };

  function storageKey() {
    return 'tbi_demo_vault::' + state.demoId;
  }

  function safeParse(value, fallback) {
    try {
      return JSON.parse(value);
    } catch (error) {
      return fallback;
    }
  }

  function readVault() {
    var raw = window.localStorage.getItem(storageKey());
    var parsed = safeParse(raw, []);
    return Array.isArray(parsed) ? parsed : [];
  }

  function writeVault(items) {
    window.localStorage.setItem(
      storageKey(),
      JSON.stringify(items)
    );
  }

  function clearVault() {
    window.localStorage.removeItem(storageKey());
    renderVault();
  }

  function boundInputValue(element) {
    try {
      var binding = window.jQuery(element).data('shiny-input-binding');

      if (binding && typeof binding.getValue === 'function') {
        return binding.getValue(element);
      }
    } catch (error) {
      return null;
    }

    return null;
  }

  function isWorkspaceInput(id) {
    if (!id) return false;

    return /team|season|partner|outgoing|incoming|draft|pick/i.test(id);
  }

  function collectWorkspaceInputs() {
    var result = {};

    document
      .querySelectorAll('.shiny-bound-input')
      .forEach(function (element) {
        var id = element.id || '';

        if (!isWorkspaceInput(id)) return;

        var value = boundInputValue(element);

        if (typeof value !== 'undefined') {
          result[id] = value;
        }
      });

    return result;
  }

  function scenarioName() {
    var candidates = [
      "[id$='scenario_label']",
      "[id$='team_a_name']",
      "[id$='team_a_short_name']"
    ];

    for (var i = 0; i < candidates.length; i += 1) {
      var node = document.querySelector(candidates[i]);

      if (node && node.textContent.trim()) {
        return node.textContent.trim();
      }
    }

    return 'TBI Trade Scenario';
  }

  function formatSavedTime(value) {
    try {
      return new Date(value).toLocaleString();
    } catch (error) {
      return '';
    }
  }

  function saveCurrentScenario() {
    var inputs = collectWorkspaceInputs();
    var keys = Object.keys(inputs);

    if (!keys.length) {
      window.alert('Build a scenario before saving it.');
      return;
    }

    var items = readVault();

    items.unshift({
      id: 'scenario-' + Date.now() + '-' + Math.floor(Math.random() * 100000),
      name: scenarioName(),
      savedAt: new Date().toISOString(),
      inputs: inputs
    });

    if (items.length > 20) {
      items = items.slice(0, 20);
    }

    writeVault(items);
    renderVault();
  }

  function inputPriority(id) {
    var value = String(id || '').toLowerCase();

    if (value.indexOf('season') >= 0) return 1;

    if (
      value.indexOf('team') >= 0 &&
      value.indexOf('partner') < 0
    ) return 2;

    if (value.indexOf('partner') >= 0) return 3;

    if (
      value.indexOf('outgoing') >= 0 ||
      value.indexOf('incoming') >= 0
    ) return 4;

    if (
      value.indexOf('draft') >= 0 ||
      value.indexOf('pick') >= 0
    ) return 5;

    return 9;
  }

  function applyInputValue(id, value) {
    var element = document.getElementById(id);

    if (!element) return false;

    try {
      var jq = window.jQuery(element);
      var binding = jq.data('shiny-input-binding');

      if (
        binding &&
        typeof binding.receiveMessage === 'function'
      ) {
        binding.receiveMessage(
          element,
          { value: value }
        );

        jq.trigger('change');
        return true;
      }
    } catch (error) {
      return false;
    }

    return false;
  }

  function restoreScenario(item) {
    if (!item || !item.inputs) return;

    var entries = Object.keys(item.inputs).map(function (id) {
      return {
        id: id,
        value: item.inputs[id],
        priority: inputPriority(id)
      };
    });

    entries.sort(function (a, b) {
      return a.priority - b.priority;
    });

    var groups = [1, 2, 3, 4, 5, 9];
    var delay = 0;

    groups.forEach(function (group) {
      var selected = entries.filter(function (entry) {
        return entry.priority === group;
      });

      if (!selected.length) return;

      window.setTimeout(function () {
        selected.forEach(function (entry) {
          applyInputValue(entry.id, entry.value);
        });
      }, delay);

      delay += 700;
    });

    closeVault();
  }

  function deleteScenario(id) {
    var items = readVault().filter(function (item) {
      return item.id !== id;
    });

    writeVault(items);
    renderVault();
  }

  function escapeHtml(value) {
    return String(value || '')
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  function renderVault() {
    var container = document.getElementById('tbi-demo-scenario-list');

    if (!container) return;

    var items = readVault();

    if (!items.length) {
      container.innerHTML =
        '<div class="tbi-demo-empty">No saved scenarios yet.</div>';
      return;
    }

    container.innerHTML = '';

    items.forEach(function (item) {
      var row = document.createElement('div');
      row.className = 'tbi-demo-scenario-card';

      row.innerHTML =
        '<div class="tbi-demo-scenario-copy">' +
          '<strong>' + escapeHtml(item.name) + '</strong>' +
          '<span>' + escapeHtml(formatSavedTime(item.savedAt)) + '</span>' +
        '</div>' +
        '<div class="tbi-demo-scenario-actions">' +
          '<button type="button" class="tbi-demo-load">Load</button>' +
          '<button type="button" class="tbi-demo-delete">Delete</button>' +
        '</div>';

      row.querySelector('.tbi-demo-load').addEventListener(
        'click',
        function () {
          restoreScenario(item);
        }
      );

      row.querySelector('.tbi-demo-delete').addEventListener(
        'click',
        function () {
          deleteScenario(item.id);
        }
      );

      container.appendChild(row);
    });
  }

  function openVault() {
    var panel = document.getElementById('tbi-demo-vault');
    if (panel) panel.classList.add('open');
    renderVault();
  }

  function closeVault() {
    var panel = document.getElementById('tbi-demo-vault');
    if (panel) panel.classList.remove('open');
  }

  function formatRemaining(ms) {
    if (ms <= 0) return 'Expired';

    var total = Math.floor(ms / 1000);
    var hours = Math.floor(total / 3600);
    var minutes = Math.floor((total % 3600) / 60);
    var seconds = total % 60;

    return (
      String(hours).padStart(2, '0') + ':' +
      String(minutes).padStart(2, '0') + ':' +
      String(seconds).padStart(2, '0') +
      ' remaining'
    );
  }

  function showExpired() {
    clearVault();

    var overlay = document.getElementById(
      'tbi-demo-expiration-overlay'
    );

    if (overlay) {
      overlay.classList.add('visible');
    }

    var countdown = document.getElementById(
      'tbi-demo-countdown'
    );

    if (countdown) {
      countdown.textContent = 'Expired';
    }
  }

  function startCountdown(remainingMs) {
    if (state.timer) {
      window.clearInterval(state.timer);
    }

    state.expiresAtClient = Date.now() + Math.max(0, remainingMs);

    function tick() {
      var left = state.expiresAtClient - Date.now();
      var node = document.getElementById('tbi-demo-countdown');

      if (node) {
        node.textContent = formatRemaining(left);
      }

      if (left <= 0) {
        window.clearInterval(state.timer);
        state.timer = null;
        showExpired();
      }
    }

    tick();
    state.timer = window.setInterval(tick, 1000);
  }

  function bindControls() {
    var toggle = document.getElementById('tbi-demo-vault-toggle');
    var close = document.getElementById('tbi-demo-vault-close');
    var save = document.getElementById('tbi-demo-save');
    var clear = document.getElementById('tbi-demo-clear');

    if (toggle) toggle.addEventListener('click', openVault);
    if (close) close.addEventListener('click', closeVault);
    if (save) save.addEventListener('click', saveCurrentScenario);

    if (clear) {
      clear.addEventListener('click', function () {
        if (window.confirm('Delete all saved demo scenarios?')) {
          clearVault();
        }
      });
    }

    renderVault();
  }

  function registerShinyHandlers() {

    if (
      !window.Shiny ||
      typeof window.Shiny.addCustomMessageHandler !== 'function'
    ) {
      window.setTimeout(registerShinyHandlers, 50);
      return;
    }

    Shiny.addCustomMessageHandler(
      'tbi-demo-config',
      function (message) {
        state.demoId = message.demo_id || 'tbi-private-demo';

        startCountdown(
          Number(message.remaining_ms || 0)
        );

        renderVault();
      }
    );

    Shiny.addCustomMessageHandler(
      'tbi-demo-expired',
      function () {
        showExpired();
      }
    );
  }

  if (document.readyState === 'loading') {

    document.addEventListener(
      'DOMContentLoaded',
      bindControls
    );

  } else {

    bindControls();
  }

  registerShinyHandlers();
}());
