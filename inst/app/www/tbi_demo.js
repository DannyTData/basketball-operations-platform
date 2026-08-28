(function () {
  'use strict';

  var state = {
    demoId: 'tbi-private-demo',
    expiresAtClient: 0,
    timer: null,
    expirationDisabled: window.TBI_DEMO_EXPIRATION_DISABLED === true,
    feedbackMode: window.TBI_FEEDBACK_MODE === true,
    vaultPolicyKnown: window.TBI_FEEDBACK_MODE !== true,
    vaultSupported: window.TBI_FEEDBACK_MODE !== true,
    scenarioScope: null,
    scenarioType: null,
    vaultMessage: '',
    restoreGeneration: 0,
    restoreTimers: []
  };

  var tradeLocalVaultMessage =
    'Multi-team scenario save/restore is not supported in V2 feedback.';

  function isTradeLocalVaultItem(item) {
    if (!item) return false;

    return (
      item.scenarioScope === 'TRADE_LOCAL' ||
      item.scenario_scope === 'TRADE_LOCAL' ||
      item.scenarioType === 'v2_multiteam_trade' ||
      item.scenario_type === 'v2_multiteam_trade'
    );
  }

  function vaultActionBlocked(item) {
    if (!state.feedbackMode) return false;
    if (!state.vaultPolicyKnown || !state.vaultSupported) return true;
    return isTradeLocalVaultItem(item);
  }

  function vaultBlockMessage(item) {
    if (
      state.scenarioScope === 'TRADE_LOCAL' ||
      isTradeLocalVaultItem(item)
    ) {
      return tradeLocalVaultMessage;
    }

    return (
      state.vaultMessage ||
      'Scenario Vault is unavailable until feedback scenario state is verified.'
    );
  }

  function reportVaultBlocked(item) {
    window.alert(vaultBlockMessage(item));
  }

  function cancelPendingRestores() {
    state.restoreGeneration += 1;
    state.restoreTimers.forEach(function (timer) {
      window.clearTimeout(timer);
    });
    state.restoreTimers = [];
  }

  function updateVaultPolicyUi() {
    var save = document.getElementById('tbi-demo-save');
    var notice = document.getElementById('tbi-demo-vault-notice');
    var blocked = vaultActionBlocked();

    if (save) {
      save.disabled = blocked;
      save.setAttribute('aria-disabled', blocked ? 'true' : 'false');
      save.title = blocked ? vaultBlockMessage() : '';
    }

    if (notice) {
      var showNotice = (
        state.feedbackMode &&
        state.vaultPolicyKnown &&
        !state.vaultSupported
      );
      notice.hidden = !showNotice;
      notice.textContent = showNotice ? vaultBlockMessage() : '';
    }
  }

  function applyVaultPolicy(message) {
    message = message || {};
    state.vaultPolicyKnown = true;
    state.vaultSupported = message.supported === true;
    state.scenarioScope = message.scenario_scope || null;
    state.scenarioType = message.scenario_type || null;
    state.vaultMessage = message.message || '';
    if (vaultActionBlocked()) cancelPendingRestores();
    renderVault();
  }

  function storageKey() {
    return 'tbi_demo_vault::' + state.demoId;
  }

  function vaultStorage() {
    return state.feedbackMode
      ? window.sessionStorage
      : window.localStorage;
  }

  function safeParse(value, fallback) {
    try {
      return JSON.parse(value);
    } catch (error) {
      return fallback;
    }
  }

  function readVault() {
    var raw = vaultStorage().getItem(storageKey());
    var parsed = safeParse(raw, []);
    return Array.isArray(parsed) ? parsed : [];
  }

  function writeVault(items) {
    vaultStorage().setItem(
      storageKey(),
      JSON.stringify(items)
    );
  }

  function clearVault() {
    var key = storageKey();
    vaultStorage().removeItem(key);
    if (state.feedbackMode) {
      window.localStorage.removeItem(key);
    }
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
    if (vaultActionBlocked()) {
      reportVaultBlocked();
      return;
    }

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
      scenarioScope: state.scenarioScope,
      scenarioType: state.scenarioType,
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

    if (vaultActionBlocked(item)) {
      reportVaultBlocked(item);
      return;
    }

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

    cancelPendingRestores();
    var restoreGeneration = state.restoreGeneration;
    var groups = [1, 2, 3, 4, 5, 9];
    var delay = 0;

    groups.forEach(function (group) {
      var selected = entries.filter(function (entry) {
        return entry.priority === group;
      });

      if (!selected.length) return;

      var timer = window.setTimeout(function () {
        state.restoreTimers = state.restoreTimers.filter(function (candidate) {
          return candidate !== timer;
        });
        if (
          restoreGeneration !== state.restoreGeneration ||
          vaultActionBlocked(item)
        ) return;

        selected.forEach(function (entry) {
          applyInputValue(entry.id, entry.value);
        });
      }, delay);
      state.restoreTimers.push(timer);

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

    updateVaultPolicyUi();

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

      var loadButton = row.querySelector('.tbi-demo-load');
      var loadBlocked = vaultActionBlocked(item);

      loadButton.disabled = loadBlocked;
      loadButton.setAttribute(
        'aria-disabled',
        loadBlocked ? 'true' : 'false'
      );
      loadButton.title = loadBlocked ? vaultBlockMessage(item) : '';

      loadButton.addEventListener(
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
    if (state.expirationDisabled) return;

    cancelPendingRestores();
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
    if (state.expirationDisabled) return;

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
        state.feedbackMode =
          state.feedbackMode || message.feedback_mode === true;
        if (!state.feedbackMode) {
          state.vaultPolicyKnown = true;
          state.vaultSupported = true;
        }
        if (message.scenario_vault) {
          applyVaultPolicy(message.scenario_vault);
        }
        state.expirationDisabled =
          state.expirationDisabled || message.expiration_disabled === true;

        if (state.expirationDisabled) {
          if (state.timer) {
            window.clearInterval(state.timer);
            state.timer = null;
          }
          state.expiresAtClient = 0;

          var countdown = document.getElementById('tbi-demo-countdown');
          if (countdown) {
            countdown.textContent = 'Expiration disabled for local development';
          }

          renderVault();
          return;
        }

        startCountdown(
          Number(message.remaining_ms || 0)
        );

        renderVault();
      }
    );

    Shiny.addCustomMessageHandler(
      'tbi-demo-scenario-vault-policy',
      function (message) {
        applyVaultPolicy(message);
      }
    );

    Shiny.addCustomMessageHandler(
      'tbi-demo-expired',
      function (message) {
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

  document.addEventListener('shiny:disconnected', function () {
    if (!state.feedbackMode) return;
    cancelPendingRestores();
    clearVault();
  });

  registerShinyHandlers();
}());
