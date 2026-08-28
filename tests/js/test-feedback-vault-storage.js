'use strict';

const fs = require('fs');
const path = require('path');
const vm = require('vm');

const root = path.resolve(__dirname, '..', '..');
const source = fs.readFileSync(
  path.join(root, 'inst', 'app', 'www', 'tbi_demo.js'),
  'utf8'
);

function storage() {
  const values = new Map();
  return {
    values: values,
    getItem: function (key) {
      return values.has(key) ? values.get(key) : null;
    },
    setItem: function (key, value) {
      values.set(key, value);
    },
    removeItem: function (key) {
      values.delete(key);
    }
  };
}

function run(feedbackMode) {
  const handlers = {};
  const listeners = {};
  const local = storage();
  const session = storage();
  const document = {
    readyState: 'loading',
    addEventListener: function (name, handler) {
      listeners[name] = handler;
    },
    getElementById: function () { return null; }
  };
  const Shiny = {
    addCustomMessageHandler: function (name, handler) {
      handlers[name] = handler;
    }
  };
  const window = {
    Shiny: Shiny,
    TBI_DEMO_EXPIRATION_DISABLED: true,
    TBI_FEEDBACK_MODE: feedbackMode,
    localStorage: local,
    sessionStorage: session,
    setTimeout: function () { return 1; },
    clearTimeout: function () {},
    setInterval: function () { return 1; },
    clearInterval: function () {}
  };

  vm.runInNewContext(source, {
    window: window,
    document: document,
    Shiny: Shiny
  });

  handlers['tbi-demo-config']({
    demo_id: feedbackMode ? 'tbi-feedback-session-token' : 'tbi-private-demo',
    feedback_mode: feedbackMode,
    expiration_disabled: true
  });

  return {
    listeners: listeners,
    key: feedbackMode
      ? 'tbi_demo_vault::tbi-feedback-session-token'
      : 'tbi_demo_vault::tbi-private-demo',
    local: local,
    session: session
  };
}

const feedback = run(true);
feedback.local.setItem(feedback.key, 'legacy-feedback-state');
feedback.session.setItem(feedback.key, 'current-feedback-state');
if (typeof feedback.listeners['shiny:disconnected'] !== 'function') {
  throw new Error('Feedback session cleanup listener was not registered.');
}
feedback.listeners['shiny:disconnected']();
if (
  feedback.session.getItem(feedback.key) !== null ||
  feedback.local.getItem(feedback.key) !== null
) {
  throw new Error('Feedback disconnect did not clear current and legacy vault state.');
}

const local = run(false);
local.local.setItem(local.key, 'persistent-local-state');
local.listeners['shiny:disconnected']();
if (local.local.getItem(local.key) !== 'persistent-local-state') {
  throw new Error('Normal/local vault persistence was cleared on disconnect.');
}

const storageStart = source.indexOf('function storageKey()');
const storageEnd = source.indexOf('function boundInputValue', storageStart);
const storageHelpers = source.slice(storageStart, storageEnd);
if (
  storageStart < 0 ||
  storageEnd < 0 ||
  !storageHelpers.includes('window.sessionStorage') ||
  !storageHelpers.includes('state.feedbackMode')
) {
  throw new Error('Feedback vault operations are not routed through sessionStorage.');
}

process.stdout.write('FEEDBACK_VAULT_STORAGE_OK\n');
