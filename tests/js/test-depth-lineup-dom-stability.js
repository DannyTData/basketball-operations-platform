'use strict';

const fs = require('fs');
const path = require('path');
const vm = require('vm');

const root = path.resolve(__dirname, '..', '..');
const source = fs.readFileSync(
  path.join(root, 'inst', 'app', 'www', 'tbi_ux_foundation.js'),
  'utf8'
);
const depthStart = source.indexOf('TBI_DEPTH_SUBTABS_START >>>');
const depthEnd = source.indexOf('<<< TBI_DEPTH_SUBTABS_END <<<', depthStart);
const depth = source.slice(depthStart, depthEnd);
const bindStart = depth.indexOf('function bindSituationalLineups(scope)');
const activateEnd = depth.indexOf('function placeDepthTabs(shell, nav)');
if (depthStart < 0 || depthEnd < 0 || bindStart < 0 || activateEnd < 0) {
  throw new Error('Depth activation helpers could not be isolated.');
}
const helpers = depth.slice(bindStart, activateEnd);

function makeFixture(initiallyAdjacent) {
  let moveCount = 0;
  const workspace = {
    marker: 'preserved-user-state',
    parentNode: null,
    nextElementSibling: null
  };
  const shell = {
    parentNode: null,
    nextElementSibling: null
  };
  const parent = {
    insertBefore: function (node, reference) {
      if (node !== workspace || reference !== shell) {
        throw new Error('Unexpected Depth DOM move.');
      }
      moveCount += 1;
      workspace.parentNode = parent;
      workspace.nextElementSibling = shell;
    }
  };
  shell.parentNode = parent;
  workspace.parentNode = parent;
  workspace.nextElementSibling = initiallyAdjacent ? shell : null;

  const depthPage = {
    active: null,
    getAttribute: function () { return this.active; },
    setAttribute: function (_name, value) { this.active = value; },
    querySelectorAll: function () { return []; },
    querySelector: function (selector) {
      if (selector === '.depth-v21-shell') return shell;
      if (selector === '.tbi-depth-v2-workspace') return workspace;
      return null;
    }
  };
  const page = {
    closest: function () { return depthPage; },
    parentElement: depthPage
  };

  return {
    page: page,
    workspace: workspace,
    moveCount: function () { return moveCount; }
  };
}

function activateTwice(fixture) {
  vm.runInNewContext(
    helpers + '\nactivate(page, "lineup");\nactivate(page, "lineup");',
    {
      page: fixture.page,
      sessionStorage: { setItem: function () {} }
    }
  );
}

const mounted = makeFixture(true);
activateTwice(mounted);
if (mounted.moveCount() !== 0) {
  throw new Error('Already-mounted Lineups content was repeatedly reinserted.');
}
if (mounted.workspace.marker !== 'preserved-user-state') {
  throw new Error('Lineups workspace identity/state was not preserved.');
}

const misplaced = makeFixture(false);
activateTwice(misplaced);
if (misplaced.moveCount() !== 1) {
  throw new Error('Misplaced Lineups content was not stabilized with one DOM move.');
}

process.stdout.write('DEPTH_LINEUP_DOM_STABILITY_OK\n');
