'use strict';

const fs = require('fs');
const path = require('path');
const vm = require('vm');

const root = path.resolve(__dirname, '..', '..');
const source = fs.readFileSync(
  path.join(root, 'inst', 'app', 'www', 'tbi_demo.js'),
  'utf8'
);
const helperStart = source.indexOf('var tradeLocalVaultMessage');
const helperEnd = source.indexOf('function updateVaultPolicyUi()', helperStart);
if (helperStart < 0 || helperEnd < 0) {
  throw new Error('Scenario Vault policy helpers could not be isolated.');
}
const helpers = source.slice(helperStart, helperEnd);

function policyState(overrides) {
  return Object.assign({
    feedbackMode: true,
    vaultPolicyKnown: true,
    vaultSupported: true,
    scenarioScope: 'SHARED_SUPPORTED',
    vaultMessage: ''
  }, overrides || {});
}

function evaluate(state, expression) {
  const alerts = [];
  const sandbox = {
    state: state,
    window: {
      alert: function (message) { alerts.push(message); }
    }
  };
  vm.runInNewContext(helpers + '\nresult = ' + expression + ';', sandbox);
  return { result: sandbox.result, alerts: alerts };
}

if (!evaluate(
  policyState({ vaultSupported: false, scenarioScope: 'TRADE_LOCAL' }),
  'vaultActionBlocked()'
).result) {
  throw new Error('Active Trade-local feedback state did not block save.');
}

if (!evaluate(
  policyState(),
  "vaultActionBlocked({ scenarioScope: 'TRADE_LOCAL' })"
).result) {
  throw new Error('A saved Trade-local item did not block restore.');
}

if (evaluate(policyState(), 'vaultActionBlocked()').result) {
  throw new Error('Supported shared feedback scenario was incorrectly blocked.');
}

if (evaluate(
  policyState({ feedbackMode: false, vaultSupported: false }),
  'vaultActionBlocked()'
).result) {
  throw new Error('Local development behavior was incorrectly blocked.');
}

if (!evaluate(
  policyState({ vaultPolicyKnown: false, vaultSupported: false }),
  'vaultActionBlocked()'
).result) {
  throw new Error('Feedback vault did not fail closed before policy arrival.');
}

const alertResult = evaluate(
  policyState({ vaultSupported: false, scenarioScope: 'TRADE_LOCAL' }),
  'reportVaultBlocked()'
);
if (
  alertResult.alerts.length !== 1 ||
  alertResult.alerts[0] !==
    'Multi-team scenario save/restore is not supported in V2 feedback.'
) {
  throw new Error('Trade-local block explanation was not surfaced exactly.');
}

const saveStart = source.indexOf('function saveCurrentScenario()');
const saveEnd = source.indexOf('function inputPriority', saveStart);
const restoreStart = source.indexOf('function restoreScenario(item)');
const restoreEnd = source.indexOf('function deleteScenario', restoreStart);
if (
  saveStart < 0 || saveEnd < 0 ||
  restoreStart < 0 || restoreEnd < 0 ||
  !source.slice(saveStart, saveEnd).includes('vaultActionBlocked()') ||
  !source.slice(restoreStart, restoreEnd).includes('vaultActionBlocked(item)') ||
  !source.slice(restoreStart, restoreEnd).includes('restoreGeneration !== state.restoreGeneration')
) {
  throw new Error('Save and restore are not both guarded by the vault policy.');
}

const policyStart = source.indexOf('function applyVaultPolicy(message)');
const policyEnd = source.indexOf('function storageKey()', policyStart);
if (
  policyStart < 0 || policyEnd < 0 ||
  !source.slice(policyStart, policyEnd).includes('cancelPendingRestores()')
) {
  throw new Error('A blocking policy update does not cancel pending restores.');
}

process.stdout.write('SCENARIO_VAULT_OK\n');
