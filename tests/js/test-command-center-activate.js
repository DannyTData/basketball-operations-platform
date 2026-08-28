'use strict';

const fs = require('fs');
const path = require('path');
const vm = require('vm');

const root = path.resolve(__dirname, '..', '..');
const source = fs.readFileSync(
  path.join(root, 'inst', 'app', 'www', 'tbi_ux_foundation.js'),
  'utf8'
);
const commandStart = source.indexOf('TBI_COMMAND_CENTER_TABS_START >>>');
const commandEnd = source.indexOf('<<< TBI_COMMAND_CENTER_TABS_END <<<');
if (commandStart < 0 || commandEnd < 0) {
  throw new Error('Command Center builder markers are missing.');
}
const command = source.slice(commandStart, commandEnd);
const activateStart = command.indexOf('function activate(page, tabName)');
const activateEnd = command.indexOf('function buildNav(page)', activateStart);
if (activateStart < 0 || activateEnd < 0) {
  throw new Error('Command Center activate() could not be isolated.');
}
const activateSource = command.slice(activateStart, activateEnd);

function classList() {
  const values = new Set();
  return {
    toggle: function (name, enabled) {
      if (enabled) values.add(name); else values.delete(name);
    },
    has: function (name) { return values.has(name); }
  };
}

function element(attributes) {
  const attrs = Object.assign({}, attributes);
  return {
    classList: classList(),
    getAttribute: function (name) { return attrs[name] || null; },
    setAttribute: function (name, value) { attrs[name] = String(value); }
  };
}

const buttons = [element({ 'data-tab': 'decision' }), element({ 'data-tab': 'context' })];
const targets = [
  element({ 'data-tbi-command-tab': 'decision' }),
  element({ 'data-tbi-command-tab': 'context' })
];
const page = element({ 'data-tbi-command-active-tab': 'decision' });
page.querySelectorAll = function (selector) {
  if (selector === '.tbi-command-subtab') return buttons;
  if (selector === '.tbi-command-tab-target') return targets;
  return [];
};

const notifications = [];
const stored = [];
const sandbox = {
  page: page,
  window: {
    TBIUX: {
      notifySubtab: function (_page, tabName) { notifications.push(tabName); }
    }
  },
  sessionStorage: {
    setItem: function (key, value) { stored.push([key, value]); }
  }
};
vm.runInNewContext(activateSource + '\nactivate(page, "context");', sandbox);
if (page.getAttribute('data-tbi-command-active-tab') !== 'context') {
  throw new Error('activate() did not update the active Command Center tab.');
}
if (notifications.length !== 1 || notifications[0] !== 'context') {
  throw new Error('activate() did not notify Shiny of the changed subtab.');
}
if (stored.length !== 1 || stored[0][1] !== 'context') {
  throw new Error('activate() did not persist the changed subtab.');
}
vm.runInNewContext(activateSource + '\nactivate(page, "context");', sandbox);
if (notifications.length !== 1 || stored.length !== 1) {
  throw new Error('activate() emitted duplicate state for an unchanged subtab.');
}

process.stdout.write('COMMAND_ACTIVATE_OK\n');
