'use strict';

const fs = require('fs');
const path = require('path');
const vm = require('vm');

const root = path.resolve(__dirname, '..', '..');
const source = fs.readFileSync(
  path.join(root, 'inst', 'app', 'www', 'tbi_demo.js'),
  'utf8'
);

function runExpirationHandler(expirationDisabled, feedbackMode) {
  const handlers = {};
  const localRemovedKeys = [];
  const sessionRemovedKeys = [];
  const visibleClasses = new Set();
  const overlay = {
    classList: {
      add: function (name) { visibleClasses.add(name); }
    }
  };
  const countdown = { textContent: '' };
  const document = {
    readyState: 'loading',
    addEventListener: function () {},
    getElementById: function (id) {
      if (id === 'tbi-demo-expiration-overlay') return overlay;
      if (id === 'tbi-demo-countdown') return countdown;
      return null;
    }
  };
  const Shiny = {
    addCustomMessageHandler: function (name, handler) {
      handlers[name] = handler;
    }
  };
  const window = {
    Shiny: Shiny,
    TBI_DEMO_EXPIRATION_DISABLED: expirationDisabled,
    TBI_FEEDBACK_MODE: feedbackMode === true,
    localStorage: {
      getItem: function () { return null; },
      setItem: function () {},
      removeItem: function (key) { localRemovedKeys.push(key); }
    },
    sessionStorage: {
      getItem: function () { return null; },
      setItem: function () {},
      removeItem: function (key) { sessionRemovedKeys.push(key); }
    }
  };

  vm.runInNewContext(source, {
    window: window,
    document: document,
    Shiny: Shiny
  });

  const handler = handlers['tbi-demo-expired'];
  if (typeof handler !== 'function') {
    throw new Error('The tbi-demo-expired handler was not registered.');
  }
  if (handler.length !== 1) {
    throw new Error('The tbi-demo-expired handler must accept one Shiny payload.');
  }

  handler({ expired: true });
  return {
    visible: visibleClasses.has('visible'),
    countdown: countdown.textContent,
    localRemovedKeys: localRemovedKeys,
    sessionRemovedKeys: sessionRemovedKeys
  };
}

const enabled = runExpirationHandler(false, false);
if (!enabled.visible || enabled.countdown !== 'Expired') {
  throw new Error('The expiration handler did not activate the expired state.');
}
if (
  enabled.localRemovedKeys.length !== 1 ||
  enabled.localRemovedKeys[0] !== 'tbi_demo_vault::tbi-private-demo' ||
  enabled.sessionRemovedKeys.length !== 0
) {
  throw new Error('The expiration handler did not clear the current demo vault.');
}

const feedback = runExpirationHandler(false, true);
if (
  !feedback.visible ||
  feedback.sessionRemovedKeys.length !== 1 ||
  feedback.sessionRemovedKeys[0] !== 'tbi_demo_vault::tbi-private-demo' ||
  feedback.localRemovedKeys.length !== 1
) {
  throw new Error('Feedback expiration did not clear session and legacy vault state.');
}

const disabled = runExpirationHandler(true, false);
if (
  disabled.visible ||
  disabled.countdown !== '' ||
  disabled.localRemovedKeys.length ||
  disabled.sessionRemovedKeys.length
) {
  throw new Error('Expiration bypass did not leave the demo session untouched.');
}

process.stdout.write('DEMO_EXPIRATION_HANDLER_OK\n');
