'use strict';

const fs = require('fs');
const path = require('path');
const vm = require('vm');

const root = path.resolve(__dirname, '..', '..');
const source = fs.readFileSync(
  path.join(root, 'inst', 'app', 'www', 'tbi_ux_foundation.js'),
  'utf8'
);

const teamKey = source.indexOf("var TEAM_KEY = 'tbi-v2-selected-team-v1';");
const restoreStart = source.lastIndexOf('(function (window, document) {', teamKey);
const restoreEnd = source.indexOf('})(window, document);', teamKey);
if (teamKey < 0 || restoreStart < 0 || restoreEnd < 0) {
  throw new Error('Selected-team restore module could not be isolated.');
}
const restoreModule = source.slice(
  restoreStart,
  restoreEnd + '})(window, document);'.length
);

function runRestore(savedValue, withSelectize) {
  const listeners = {};
  const storage = {
    value: savedValue,
    setCalls: [],
    removedKeys: []
  };
  const selector = {
    value: '',
    options: [
      { value: '' },
      { value: 'Boston Celtics' },
      { value: 'Portland Trail Blazers' }
    ],
    dispatched: [],
    dispatchEvent: function (event) {
      this.dispatched.push(event);
    }
  };
  if (withSelectize) {
    selector.selectize = {
      values: [],
      setValue: function (value) {
        this.values.push(value);
        selector.value = value;
      }
    };
  }
  const document = {
    addEventListener: function (name, handler) {
      listeners[name] = handler;
    },
    getElementById: function (id) {
      return id === 'selected_team' ? selector : null;
    }
  };
  const window = {
    sessionStorage: {
      getItem: function () { return storage.value; },
      setItem: function (key, value) {
        storage.value = value;
        storage.setCalls.push([key, value]);
      },
      removeItem: function (key) {
        storage.value = null;
        storage.removedKeys.push(key);
      }
    }
  };
  function MockEvent(type, options) {
    this.type = type;
    this.bubbles = Boolean(options && options.bubbles);
  }

  vm.runInNewContext(restoreModule, {
    window: window,
    document: document,
    Event: MockEvent
  });
  listeners['shiny:connected']();
  return { selector: selector, listeners: listeners, storage: storage };
}

const restoredRun = runRestore('Boston Celtics');
const restored = restoredRun.selector;
if (restored.value !== 'Boston Celtics') {
  throw new Error('Saved team did not update the visible selector.');
}
if (
  restored.dispatched.length !== 1 ||
  restored.dispatched[0].type !== 'change' ||
  restored.dispatched[0].bubbles !== true
) {
  throw new Error('Saved team did not dispatch one bubbling change event.');
}

const invalid = runRestore('Not a selectable team').selector;
if (invalid.value !== '' || invalid.dispatched.length !== 0) {
  throw new Error('Invalid saved team was not ignored safely.');
}

const selectized = runRestore('Portland Trail Blazers', true).selector;
if (
  selectized.value !== 'Portland Trail Blazers' ||
  selectized.selectize.values.length !== 1 ||
  selectized.selectize.values[0] !== 'Portland Trail Blazers' ||
  selectized.dispatched.length !== 0
) {
  throw new Error('Saved team did not restore through the mounted Selectize control.');
}

restored.value = '';
restored.dispatched = [];
restoredRun.listeners['shiny:inputchanged']({
  name: 'selected_team',
  value: ''
});
if (
  restoredRun.storage.removedKeys.length !== 1 ||
  restoredRun.storage.removedKeys[0] !== 'tbi-v2-selected-team-v1'
) {
  throw new Error('Clearing selected_team did not remove the persisted team.');
}
restoredRun.listeners['shiny:connected']();
if (restored.value !== '' || restored.dispatched.length !== 0) {
  throw new Error('A cleared team was resurrected on reconnect.');
}

const persistedRun = runRestore(null);
persistedRun.listeners['shiny:inputchanged']({
  name: 'selected_team',
  value: 'Boston Celtics'
});
if (
  persistedRun.storage.setCalls.length !== 1 ||
  persistedRun.storage.setCalls[0][0] !== 'tbi-v2-selected-team-v1' ||
  persistedRun.storage.setCalls[0][1] !== 'Boston Celtics'
) {
  throw new Error('Selecting a team did not persist the current value.');
}
persistedRun.listeners['shiny:inputchanged']({ name: 'other_input', value: '' });
if (persistedRun.storage.removedKeys.length !== 0) {
  throw new Error('An unrelated empty input cleared selected-team state.');
}

const teamTabsStart = source.indexOf("var VERSION = '2.0.0';", teamKey);
const teamTabsEnd = source.indexOf("var VERSION = '2.0.0';", teamTabsStart + 1);
const teamTabs = source.slice(teamTabsStart, teamTabsEnd);
if (
  teamTabs.includes('VALID.indexOf(stored)') ||
  !teamTabs.includes("tabs.some(function(item) { return item[0] === stored; })")
) {
  throw new Error('Team Overview stored-tab validation is not derived from its tab list.');
}

process.stdout.write('UX_FOUNDATION_STATE_OK\n');
