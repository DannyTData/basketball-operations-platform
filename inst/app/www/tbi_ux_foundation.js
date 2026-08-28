(function (window, document) {
  'use strict';

  var builders = [];
  var framePending = false;

  function runBuilders() {
    framePending = false;
    builders.forEach(function (builder) {
      builder();
    });
  }

  function schedule() {
    if (framePending) return;
    framePending = true;
    window.requestAnimationFrame(runBuilders);
  }

  function notifySubtab(page, tabName) {
    var inputId = page && page.getAttribute('data-tbi-subtab-input');

    if (
      !inputId ||
      !window.Shiny ||
      typeof window.Shiny.setInputValue !== 'function'
    ) return;

    window.Shiny.setInputValue(inputId, tabName);
  }

  function sendCbaContextEvent(inputId, field, value) {
    if (
      !value ||
      !window.Shiny ||
      typeof window.Shiny.setInputValue !== 'function'
    ) return;

    var request = { nonce: Date.now() };
    request[field] = value;
    window.Shiny.setInputValue(inputId, request, { priority: 'event' });
  }

  function handleCbaContextClick(event) {
    if (!event.target || typeof event.target.closest !== 'function') return;

    var termLink = event.target.closest('[data-cba-term-link]');
    if (termLink) {
      event.preventDefault();
      sendCbaContextEvent(
        'cba_open_term',
        'term',
        termLink.getAttribute('data-cba-term-link')
      );
      return;
    }

    var moduleLink = event.target.closest('[data-cba-module-link]');
    if (!moduleLink) return;

    event.preventDefault();
    sendCbaContextEvent(
      'cba_open_module',
      'module',
      moduleLink.getAttribute('data-cba-module-link')
    );
  }

  window.TBIUX = {
    register: function (builder) {
      builders.push(builder);
      schedule();
    },
    notifySubtab: notifySubtab
  };

  document.addEventListener('DOMContentLoaded', schedule);
  document.addEventListener('shiny:connected', schedule);
  document.addEventListener('shiny:value', schedule);
  document.addEventListener('click', handleCbaContextClick);

  new MutationObserver(schedule).observe(document.documentElement, {
    childList: true,
    subtree: true
  });

})(window, document);

(function (window, document) {
  'use strict';

  var TEAM_KEY = 'tbi-v2-selected-team-v1';

  document.addEventListener('shiny:connected', function () {
    var selector = document.getElementById('selected_team');
    if (!selector || selector.value) return;
    try {
      var saved = window.sessionStorage.getItem(TEAM_KEY);
      var savedOption = saved && Array.prototype.some.call(
        selector.options,
        function (option) { return option.value === saved; }
      );
      if (savedOption) {
        if (selector.selectize && typeof selector.selectize.setValue === 'function') {
          selector.selectize.setValue(saved);
        } else {
          selector.value = saved;
          selector.dispatchEvent(new Event('change', { bubbles: true }));
        }
      }
    } catch (error) {
      // Session storage is optional; neutral selection remains the fallback.
    }
  });

  document.addEventListener('shiny:inputchanged', function (event) {
    if (!event || event.name !== 'selected_team') return;
    try {
      if (event.value) {
        window.sessionStorage.setItem(TEAM_KEY, event.value);
      } else {
        window.sessionStorage.removeItem(TEAM_KEY);
      }
    } catch (error) {
      // Selection remains mounted even when storage is unavailable.
    }
  });
})(window, document);

(function () {
  'use strict';

  var TBI_PM_VERSION = '2.0.0';
  var TBI_PM_TABS = ['overview', 'value', 'development', 'contract', 'recommendation'];

  function classifyPanel(panel) {
    var explicitTab = panel.getAttribute('data-tbi-pm-tab');
    if (TBI_PM_TABS.indexOf(explicitTab) >= 0) return explicitTab;
    return 'overview';
  }

  function setTab(page, tabName) {
    if (TBI_PM_TABS.indexOf(tabName) < 0) tabName = 'overview';

    page.setAttribute('data-tbi-pm-active-tab', tabName);

    page.querySelectorAll('.tbi-pm-subtab').forEach(function (button) {
      var isActive = button.getAttribute('data-tab') === tabName;
      button.classList.toggle('active', isActive);
      button.setAttribute('aria-selected', isActive ? 'true' : 'false');
      button.setAttribute('tabindex', isActive ? '0' : '-1');
    });

    page.querySelectorAll('.tbi-pm-tab-target').forEach(function (target) {
      var isActive = target.getAttribute('data-tbi-pm-tab') === tabName;
      target.classList.toggle('tbi-pm-hidden', !isActive);
      target.setAttribute('aria-hidden', isActive ? 'false' : 'true');
    });

    try {
      window.sessionStorage.setItem('tbi-player-management-tab', tabName);
    } catch (e) {
      // Browser storage is optional.
    }

    window.TBIUX.notifySubtab(page, tabName);

    var workspace = document.querySelector('.tbi-product-workspace');
    if (workspace) workspace.scrollTo({ top: 0, behavior: 'smooth' });
  }

  function buildPlayerTabs() {
    var page = document.querySelector('.pi-page');
    if (!page || page.getAttribute('data-tbi-subtabs-ready') === TBI_PM_VERSION) return;

    var mainGrid = page.querySelector('.pi-main-grid');
    if (!mainGrid) return;

    mainGrid.querySelectorAll('.pi-panel').forEach(function (panel) {
      panel.classList.add('tbi-pm-tab-target');
      panel.setAttribute('data-tbi-pm-tab', classifyPanel(panel));
    });

    var subnav = document.createElement('div');
    subnav.className = 'tbi-pm-subnav';
    subnav.setAttribute('role', 'tablist');

    var labels = {
      overview: 'Overview',
      value: 'Value & Fit',
      development: 'Development',
      contract: 'Contract & CBA',
      recommendation: 'Recommendation'
    };

    TBI_PM_TABS.forEach(function (tabName) {
      var button = document.createElement('button');
      button.type = 'button';
      button.className = 'tbi-pm-subtab';
      button.textContent = labels[tabName];
      button.setAttribute('data-tab', tabName);
      button.setAttribute('role', 'tab');
      button.addEventListener('click', function () { setTab(page, tabName); });
      subnav.appendChild(button);
    });

    mainGrid.parentNode.insertBefore(subnav, mainGrid);

    var savedTab = 'overview';
    try {
      var stored = window.sessionStorage.getItem('tbi-player-management-tab');
      if (TBI_PM_TABS.indexOf(stored) >= 0) savedTab = stored;
    } catch (e) {
      savedTab = 'overview';
    }

    page.setAttribute('data-tbi-subtabs-ready', TBI_PM_VERSION);
    setTab(page, savedTab);
  }

  var scheduled = false;

  function scheduleBuild() {
    if (scheduled) return;
    scheduled = true;
    window.requestAnimationFrame(function () {
      scheduled = false;
      buildPlayerTabs();
    });
  }

  window.TBIUX.register(scheduleBuild);

})();

// >>> TBI_ROSTER_SUBTABS_START >>>

(function () {

  'use strict';

  var VERSION = '2.1.0';

  function text(el) {
    return (el && el.textContent ? el.textContent : '')
      .replace(/\s+/g, ' ')
      .trim()
      .toUpperCase();
  }

  function classify(el) {

    var explicit = el.getAttribute('data-tbi-roster-tab');

    if (explicit) return explicit;

    var value = text(el);

    if (value.indexOf('ROSTER NEEDS + GAP ANALYSIS') >= 0) {
      return 'risk';
    }

    if (value.indexOf('ROSTER DECISION INTELLIGENCE') >= 0) {
      return 'assessment';
    }

    if (value.indexOf('COMPLETE ROSTER') >= 0) {
      return 'roster';
    }

    return 'overview';
  }

  function activate(page, tab) {

    page.querySelectorAll('.tbi-roster-subtab')
      .forEach(function(button) {
        var active = button.getAttribute('data-tab') === tab;
        button.classList.toggle('active', active);
        button.setAttribute('aria-selected', active ? 'true' : 'false');
      });

    page.querySelectorAll('.tbi-roster-tab-target')
      .forEach(function(target) {

        var targetTabs = (target.getAttribute('data-tbi-roster-tab') || '')
          .split(/\s+/)
          .filter(Boolean);

        target.classList.toggle(
          'tbi-roster-hidden',
          targetTabs.indexOf(tab) < 0
        );

      });

    try {
      sessionStorage.setItem(
        'tbi-roster-tab',
        tab
      );
    } catch(e) {}

    page.setAttribute('data-tbi-roster-active-tab', tab);

    window.TBIUX.notifySubtab(page, tab);

  }

  function build() {

    var page =
      document.querySelector('.tbi-v2-roster-page');

    if (!page) return;

    if (
      page.getAttribute('data-tbi-roster-tabs-ready') === VERSION
    ) return;

    var intro =
      page.querySelector('.tbi-v2-module-intro');

    if (!intro) return;

    var candidates = [];

    Array.prototype.forEach.call(
      page.children,
      function(child) {

        if (child === intro) return;

        if (
          child.classList.contains('tbi-roster-subnav')
        ) return;

        candidates.push(child);

      }
    );

    candidates.forEach(function(el) {

      el.classList.add('tbi-roster-tab-target');

      if (!el.getAttribute('data-tbi-roster-tab')) {
        el.setAttribute(
          'data-tbi-roster-tab',
          classify(el)
        );
      }

    });

    var nav = document.createElement('div');
    nav.className = 'tbi-roster-subnav';
    nav.setAttribute('role', 'tablist');

    var tabs = [
      ['overview', 'Overview'],
      ['construction', 'Roster Construction'],
      ['assessment', 'Roster Assessment'],
      ['risk', 'Risks & Opportunities'],
      ['roster', 'Complete Roster']
    ];

    tabs.forEach(function(item) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-roster-subtab';
      button.textContent = item[1];
      button.setAttribute('data-tab', item[0]);
      button.setAttribute('role', 'tab');
      button.setAttribute('aria-selected', 'false');

      button.addEventListener(
        'click',
        function() {
          activate(page, item[0]);
        }
      );

      nav.appendChild(button);

    });

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

    var selected = 'overview';

    try {
      var stored = sessionStorage.getItem('tbi-roster-tab');
      var validTabs = tabs.map(function(item) { return item[0]; });
      if (validTabs.indexOf(stored) >= 0) selected = stored;
    } catch(e) {}

    page.setAttribute(
      'data-tbi-roster-tabs-ready',
      VERSION
    );

    activate(page, selected);

  }

  var queued = false;

  function schedule() {

    if (queued) return;
    queued = true;

    requestAnimationFrame(function() {
      queued = false;
      build();
    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_ROSTER_SUBTABS_END <<<

// >>> TBI_ROSTER_TAB_CORRECTION_START >>>

(function () {

  'use strict';

  var VERSION = '1.0.0';

  function cleanText(el) {
    return (el && el.textContent ? el.textContent : '')
      .replace(/\s+/g, ' ')
      .trim()
      .toUpperCase();
  }

  function correctRosterTabs() {

    var page = document.querySelector('.tbi-v2-roster-page');

    if (!page) return;

    var nav = page.querySelector('.tbi-roster-subnav');

    if (!nav) return;

    if (page.getAttribute('data-tbi-roster-tabs-ready') === '2.1.0') {
      page.setAttribute('data-tbi-roster-correction', VERSION);
      return;
    }

    if (
      page.getAttribute('data-tbi-roster-correction') === VERSION
    ) return;

    /* -----------------------------------------------------
       Rename buttons
       ----------------------------------------------------- */

    var buttons = nav.querySelectorAll('.tbi-roster-subtab');

    buttons.forEach(function(button) {

      var tab = button.getAttribute('data-tab');

      if (tab === 'decision') {
        button.textContent = 'Roster Assessment';
      }

      if (tab === 'needs') {
        button.textContent = 'Risks & Opportunities';
      }

    });

    /* -----------------------------------------------------
       Re-map page blocks using intelligence that actually
       exists in V1.
       ----------------------------------------------------- */

    var targets = page.querySelectorAll('.tbi-roster-tab-target');

    targets.forEach(function(target) {

      var txt = cleanText(target);

      /* Dead legacy BIE roster blocks */

      if (
        txt.indexOf('ROSTER DECISION INTELLIGENCE') >= 0 &&
        txt.indexOf('BIE ROSTER') >= 0
      ) {

        target.setAttribute(
          'data-tbi-roster-tab',
          'legacy-hidden'
        );

        target.style.display = 'none';

        return;
      }

      if (
        txt.indexOf('ROSTER NEEDS + GAP ANALYSIS') >= 0 &&
        txt.indexOf('BIE NEEDS') >= 0
      ) {

        target.setAttribute(
          'data-tbi-roster-tab',
          'legacy-hidden'
        );

        target.style.display = 'none';

        return;
      }

      /* Existing working roster assessment/composition */

      if (
        txt.indexOf('ROSTER ASSESSMENT') >= 0 ||
        txt.indexOf('CONTRACT AND ROSTER CATEGORIES') >= 0
      ) {

        target.style.display = '';

        target.setAttribute(
          'data-tbi-roster-tab',
          'decision'
        );

        return;
      }

      /* Existing working risks/opportunities */

      if (
        txt.indexOf('KEY RISKS') >= 0 ||
        txt.indexOf('KEY OPPORTUNITIES') >= 0
      ) {

        target.style.display = '';

        target.setAttribute(
          'data-tbi-roster-tab',
          'needs'
        );

        return;
      }

      /* Complete roster stays complete roster */

      if (txt.indexOf('COMPLETE ROSTER') >= 0) {

        target.style.display = '';

        target.setAttribute(
          'data-tbi-roster-tab',
          'roster'
        );

        return;
      }

      /* Everything else belongs to Overview */

      target.style.display = '';

      target.setAttribute(
        'data-tbi-roster-tab',
        'overview'
      );

    });

    /* -----------------------------------------------------
       Reset remembered dead tabs if necessary
       ----------------------------------------------------- */

    try {

      var stored = sessionStorage.getItem('tbi-roster-tab');

      if (
        stored !== 'overview' &&
        stored !== 'decision' &&
        stored !== 'needs' &&
        stored !== 'roster'
      ) {
        sessionStorage.setItem('tbi-roster-tab', 'overview');
      }

    } catch(e) {}

    page.setAttribute(
      'data-tbi-roster-correction',
      VERSION
    );

    /* Re-click current tab so visibility refreshes */

    var active = nav.querySelector('.tbi-roster-subtab.active');

    if (active) {
      active.click();
    } else {

      var overview =
        nav.querySelector('.tbi-roster-subtab[data-tab="overview"]');

      if (overview) overview.click();

    }

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function() {

      queued = false;
      correctRosterTabs();

    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_ROSTER_TAB_CORRECTION_END <<<

// >>> TBI_DEPTH_SUBTABS_START >>>

(function () {

  'use strict';

  var VERSION = '1.1.0';

  function bindSituationalLineups(scope) {

    scope
      .querySelectorAll('.tbi-p3-situational-lineups')
      .forEach(function(container) {

        if (container.getAttribute('data-tbi-situational-ready') === VERSION) return;

        container
          .querySelectorAll('[data-situational-lineup]')
          .forEach(function(button) {

            button.addEventListener('click', function() {
              var type = button.getAttribute('data-situational-lineup');
              container.setAttribute('data-tbi-situational-active', type);
              container
                .querySelectorAll('[data-situational-lineup]')
                .forEach(function(peer) {
                  peer.setAttribute(
                    'aria-pressed',
                    peer.getAttribute('data-situational-lineup') === type ? 'true' : 'false'
                  );
                });
            });

          });

        container.setAttribute('data-tbi-situational-ready', VERSION);

      });

  }

  function activate(page, tabName) {

    var depthPage = page.closest('.tbi-depth-page') || page.parentElement;
    var previousTab = depthPage && depthPage.getAttribute('data-tbi-depth-active-tab');
    if (depthPage) depthPage.setAttribute('data-tbi-depth-active-tab', tabName);
    var scope = depthPage || page;
    bindSituationalLineups(scope);

    scope
      .querySelectorAll('.tbi-depth-subtab')
      .forEach(function(button) {

        var active = button.getAttribute('data-tab') === tabName;

        button.classList.toggle('active', active);
        button.setAttribute('aria-selected', active ? 'true' : 'false');
        button.setAttribute('tabindex', active ? '0' : '-1');

      });

    scope
      .querySelectorAll('.tbi-depth-tab-target')
      .forEach(function(target) {

        var targetTabs =
          (target.getAttribute('data-tbi-depth-tab') || '').split(' ');

        target.classList.toggle(
          'tbi-depth-hidden',
          targetTabs.indexOf(tabName) < 0
        );

      });

    var shell = scope.querySelector('.depth-v21-shell');
    var workspace = scope.querySelector('.tbi-depth-v2-workspace');
    var editorDisclosure = scope.querySelector('.depth-lineup-editor-disclosure');

    if (shell && workspace) {
      if (tabName === 'lineup') {
        if (
          workspace.parentNode !== shell.parentNode ||
          workspace.nextElementSibling !== shell
        ) {
          shell.parentNode.insertBefore(workspace, shell);
        }
      } else if (shell.nextElementSibling !== workspace) {
        shell.insertAdjacentElement('afterend', workspace);
      }
    }

    if (editorDisclosure) {
      if (tabName === 'depth') {
        editorDisclosure.setAttribute('open', '');
      } else if (tabName === 'lineup' && previousTab !== 'lineup') {
        editorDisclosure.removeAttribute('open');
      }
    }

    try {

      sessionStorage.setItem(
        'tbi-depth-tab',
        tabName
      );

    } catch(e) {}

  }

  function placeDepthTabs(shell, nav) {

    var depthPage = shell.closest('.tbi-depth-page') || shell.parentElement;
    var intro = depthPage && depthPage.querySelector('.tbi-depth-module-intro');

    if (intro) {
      if (intro.nextElementSibling !== nav) {
        intro.insertAdjacentElement('afterend', nav);
      }
      return;
    }

    if (nav.parentElement !== shell || nav !== shell.firstElementChild) {
      shell.insertBefore(nav, shell.firstChild);
    }

  }

  function buildDepthTabs() {

    var shell =
      document.querySelector('.depth-v21-shell');

    if (!shell) return;

    if (
      shell.getAttribute('data-tbi-depth-tabs-ready') === VERSION
    ) {
      var existingPage = shell.closest('.tbi-depth-page') || shell.parentElement;
      var existingNav = existingPage && existingPage.querySelector('.tbi-depth-subnav');
      if (existingNav) placeDepthTabs(shell, existingNav);
      var existingWorkspace = existingPage && existingPage.querySelector('.tbi-depth-v2-workspace');
      if (existingWorkspace) {
        if (!existingWorkspace.classList.contains('tbi-depth-tab-target')) {
          existingWorkspace.classList.add('tbi-depth-tab-target');
        }
        existingWorkspace.setAttribute('data-tbi-depth-tab', 'rotation lineup staggering gameplan');
      }
      var activeButton = existingPage.querySelector('.tbi-depth-subtab.active');
      activate(shell, activeButton ? activeButton.getAttribute('data-tab') : 'depth');
      return;
    }

    var board =
      shell.querySelector('.depth-v21-board');

    var court =
      shell.querySelector('.depth-v23-court-panel');

    if (!court) {
      court = shell.querySelector('.depth-v21-court-panel');
    }

    var playerRail =
      shell.querySelector('.depth-v21-rail');

    if (!board || !court || !playerRail) {
      return;
    }

    board.classList.add(
      'tbi-depth-tab-target'
    );

    board.setAttribute(
      'data-tbi-depth-tab',
      'depth'
    );

    court.classList.add(
      'tbi-depth-tab-target'
    );

    court.setAttribute(
      'data-tbi-depth-tab',
      'depth lineup'
    );

    playerRail.classList.add(
      'tbi-depth-tab-target'
    );

    playerRail.setAttribute('data-tbi-depth-tab', 'depth');

    var depthPage = shell.closest('.tbi-depth-page') || shell.parentElement;
    var v2Workspace = depthPage && depthPage.querySelector('.tbi-depth-v2-workspace');
    if (v2Workspace) {
      v2Workspace.classList.add('tbi-depth-tab-target');
      v2Workspace.setAttribute('data-tbi-depth-tab', 'rotation lineup staggering gameplan');
    }

    var nav =
      document.createElement('div');

    nav.className =
      'tbi-depth-subnav';

    nav.setAttribute(
      'role',
      'tablist'
    );

    var tabs = [
      ['depth', 'Depth Chart'],
      ['rotation', 'Rotation'],
      ['lineup', 'Lineups'],
      ['staggering', 'Staggering'],
      ['gameplan', 'Game Plan']
    ];

    tabs.forEach(function(item) {

      var button =
        document.createElement('button');

      button.type = 'button';

      button.className =
        'tbi-depth-subtab';

      button.textContent = item[1];

      button.setAttribute(
        'data-tab',
        item[0]
      );

      button.setAttribute(
        'role',
        'tab'
      );

      button.addEventListener(
        'click',
        function() {

          activate(
            shell,
            item[0]
          );

        }
      );

      nav.appendChild(button);

    });

    placeDepthTabs(shell, nav);

    var selected = 'depth';

    try {

      var stored =
        sessionStorage.getItem(
          'tbi-depth-tab'
        );

      if (
        stored === 'depth' ||
        stored === 'rotation' ||
        stored === 'lineup' ||
        stored === 'staggering' ||
        stored === 'gameplan'
      ) {
        selected = stored;
      }

    } catch(e) {}

    shell.setAttribute(
      'data-tbi-depth-tabs-ready',
      VERSION
    );

    activate(
      shell,
      selected
    );

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function() {

      queued = false;
      buildDepthTabs();

    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_DEPTH_SUBTABS_END <<<

// >>> TBI_CAP_SUBTABS_START >>>

(function () {

  'use strict';

  var VERSION = '2.1.0';

  function cleanText(el) {

    return (el && el.textContent ? el.textContent : '')
      .replace(/\s+/g, ' ')
      .trim()
      .toUpperCase();

  }

  function classify(target) {

    var txt = cleanText(target);

    if (
      txt.indexOf('FINANCIAL OPERATING DECISION') >= 0 ||
      txt.indexOf('CAP THRESHOLD POSITION') >= 0
    ) {
      return 'decision';
    }

    if (
      txt.indexOf('CBA ALERTS') >= 0 ||
      txt.indexOf('FINANCIAL RISKS') >= 0 ||
      txt.indexOf('FLEXIBILITY OPPORTUNITIES') >= 0
    ) {
      return 'risk';
    }

    if (
      txt.indexOf('CONTRACT LEDGER') >= 0 ||
      txt.indexOf('CURRENT-SEASON COMMITMENTS') >= 0 ||
      txt.indexOf('FRONT-OFFICE READOUT') >= 0 ||
      txt.indexOf('CAP ENGINE') >= 0
    ) {
      return 'contracts';
    }

    return 'overview';

  }

  function activate(page, tabName) {

    page
      .querySelectorAll('.tbi-cap-subtab')
      .forEach(function(button) {

        var selected = button.getAttribute('data-tab') === tabName;

        button.classList.toggle('active', selected);
        button.setAttribute('aria-selected', selected ? 'true' : 'false');
        button.setAttribute('tabindex', selected ? '0' : '-1');

      });

    page
      .querySelectorAll('.tbi-cap-tab-target')
      .forEach(function(target) {

        var targetTab =
          target.getAttribute('data-tbi-cap-tab');

        target.classList.toggle(
          'tbi-cap-hidden',
          targetTab !== tabName
        );

        target.setAttribute(
          'aria-hidden',
          targetTab === tabName ? 'false' : 'true'
        );

      });

    try {

      sessionStorage.setItem(
        'tbi-cap-tab',
        tabName
      );

    } catch(e) {}

    page.setAttribute('data-tbi-cap-active-tab', tabName);

    window.TBIUX.notifySubtab(page, tabName);

  }

  function buildCapTabs() {

    var page =
      document.querySelector('.tbi-v2-cap-page');

    if (!page) return;

    if (
      page.getAttribute('data-tbi-cap-tabs-ready') === VERSION
    ) {
      return;
    }

    var intro =
      page.querySelector('.tbi-v2-module-intro');

    if (!intro) return;

    /* -----------------------------------------------------
       Classify each major page section.
       Intro always stays visible.
       ----------------------------------------------------- */

    Array.prototype.forEach.call(
      page.children,
      function(child) {

        if (child === intro) return;

        if (
          child.classList &&
          child.classList.contains('tbi-cap-subnav')
        ) return;

        if (!child.getAttribute('data-tbi-cap-tab')) {
          child.classList.add('tbi-cap-tab-target');
          child.setAttribute('data-tbi-cap-tab', classify(child));
        }

        if (child.classList.contains('tbi-cap-tab-target')) {
          child.setAttribute('role', 'tabpanel');
        }

      }
    );

    /* -----------------------------------------------------
       Navigation
       ----------------------------------------------------- */

    var nav =
      document.createElement('div');

    nav.className =
      'tbi-cap-subnav';

    nav.setAttribute(
      'role',
      'tablist'
    );

    var tabs = [
      ['overview', 'Overview'],
      ['decision', 'Decision & Thresholds'],
      ['contracts', 'Contracts & Commitments'],
      ['market', 'Free Agent Market'],
      ['risk', 'Risks & Flexibility'],
      ['recommendation', 'Recommendation']
    ];

    tabs.forEach(function(item) {

      var button =
        document.createElement('button');

      button.type = 'button';

      button.className =
        'tbi-cap-subtab';

      button.textContent =
        item[1];

      button.setAttribute(
        'data-tab',
        item[0]
      );

      button.setAttribute(
        'role',
        'tab'
      );

      button.id = 'tbi-cap-tab-' + item[0];

      button.addEventListener(
        'click',
        function() {

          activate(
            page,
            item[0]
          );

        }
      );

      button.addEventListener('keydown', function(event) {
        var keys = ['ArrowLeft', 'ArrowRight', 'Home', 'End'];
        if (keys.indexOf(event.key) < 0) return;
        event.preventDefault();
        var current = tabs.findIndex(function(tab) { return tab[0] === item[0]; });
        var next = event.key === 'Home' ? 0 :
          event.key === 'End' ? tabs.length - 1 :
          (current + (event.key === 'ArrowRight' ? 1 : -1) + tabs.length) % tabs.length;
        activate(page, tabs[next][0]);
        var nextButton = nav.querySelector('[data-tab="' + tabs[next][0] + '"]');
        if (nextButton) nextButton.focus();
      });

      nav.appendChild(button);

    });

    /* Put tabs directly below the Cap Intelligence intro */

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

    tabs.forEach(function(item) {
      var button = nav.querySelector('[data-tab="' + item[0] + '"]');
      var controlled = [];
      page.querySelectorAll('.tbi-cap-tab-target[data-tbi-cap-tab="' + item[0] + '"]').forEach(function(target, index) {
        target.id = 'tbi-cap-panel-' + item[0] + '-' + (index + 1);
        target.setAttribute('aria-labelledby', button.id);
        controlled.push(target.id);
      });
      button.setAttribute('aria-controls', controlled.join(' '));
    });

    var selected = 'overview';

    try {

      var stored =
        sessionStorage.getItem(
          'tbi-cap-tab'
        );

      if (tabs.some(function(item) { return item[0] === stored; })) {
        selected = stored;
      }

    } catch(e) {}

    page.setAttribute(
      'data-tbi-cap-tabs-ready',
      VERSION
    );

    activate(
      page,
      selected
    );

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function() {

      queued = false;
      buildCapTabs();

    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_CAP_SUBTABS_END <<<

// >>> TBI_TRADE_SUBTABS_START >>>

(function () {

  'use strict';

  var VERSION = '1.0.0';

  function text(el) {

    return (el && el.textContent ? el.textContent : '')
      .replace(/\s+/g, ' ')
      .trim()
      .toUpperCase();

  }

  function classify(target) {

    var explicit = target.getAttribute('data-tbi-team-tab');

    if (explicit) return explicit;

    var txt = text(target);

    /* Builder is identified structurally first */

    if (
      target.classList &&
      target.classList.contains('tbi-trade-workspace-grid')
    ) {
      return 'builder';
    }

    /* Evaluation workspace */

    if (
      txt.indexOf('TRANSACTION DECISION') >= 0 ||
      txt.indexOf('CBA RULE SCORECARD') >= 0 ||
      txt.indexOf('BIE TRADE BASKETBALL IMPACT') >= 0
    ) {
      return 'evaluation';
    }

    /* Recommendation workspace */

    if (
      txt.indexOf('EXECUTIVE RECOMMENDATION') >= 0 ||
      txt.indexOf('TRANSACTION RISKS') >= 0 ||
      txt.indexOf('TRANSACTION OPPORTUNITIES') >= 0 ||
      txt.indexOf('CBA SCREEN RESULT') >= 0
    ) {
      return 'recommendation';
    }

    /* Everything else = overview, primarily snapshot */

    return 'overview';

  }

  function activate(page, tabName) {

    page
      .querySelectorAll('.tbi-trade-subtab')
      .forEach(function(button) {

        var isActive = button.getAttribute('data-tab') === tabName;

        button.classList.toggle(
          'active',
          isActive
        );

        button.setAttribute('aria-selected', isActive ? 'true' : 'false');
        button.setAttribute('tabindex', isActive ? '0' : '-1');

      });

    page
      .querySelectorAll('.tbi-trade-tab-target')
      .forEach(function(target) {

        var targetTab =
          target.getAttribute('data-tbi-trade-tab');

        target.classList.toggle(
          'tbi-trade-hidden',
          targetTab !== tabName
        );

      });

    try {

      sessionStorage.setItem(
        'tbi-trade-tab',
        tabName
      );

    } catch(e) {}

  }

  function buildTradeTabs() {

    var page =
      document.querySelector('.tbi-v2-trade-page');

    if (!page) return;

    if (
      page.getAttribute('data-tbi-trade-tabs-ready') === VERSION
    ) {
      return;
    }

    var intro =
      page.querySelector('.tbi-v2-module-intro');

    if (!intro) return;

    /* -----------------------------------------------------
       Tag each existing major UI block.
       Intro remains permanently visible.
       ----------------------------------------------------- */

    Array.prototype.forEach.call(
      page.children,
      function(child) {

        if (child === intro) return;

        if (
          child.classList &&
          child.classList.contains('tbi-trade-subnav')
        ) {
          return;
        }

        child.classList.add(
          'tbi-trade-tab-target'
        );

        child.setAttribute(
          'data-tbi-trade-tab',
          classify(child)
        );

      }
    );

    /* -----------------------------------------------------
       Build navigation
       ----------------------------------------------------- */

    var nav =
      document.createElement('div');

    nav.className =
      'tbi-trade-subnav';

    nav.setAttribute(
      'role',
      'tablist'
    );

    var tabs = [
      ['overview', 'Overview'],
      ['builder', 'Trade Builder'],
      ['evaluation', 'Evaluation'],
      ['recommendation', 'Recommendation']
    ];

    tabs.forEach(function(item) {

      var button =
        document.createElement('button');

      button.type = 'button';

      button.className =
        'tbi-trade-subtab';

      button.textContent = item[1];

      button.setAttribute(
        'data-tab',
        item[0]
      );

      button.setAttribute(
        'role',
        'tab'
      );

      button.addEventListener(
        'click',
        function() {

          activate(
            page,
            item[0]
          );

        }
      );

      nav.appendChild(button);

    });

    /* Tabs immediately under page identity */

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

    var selected = 'overview';

    try {

      var stored =
        sessionStorage.getItem(
          'tbi-trade-tab'
        );

      if (
        stored === 'overview' ||
        stored === 'builder' ||
        stored === 'evaluation' ||
        stored === 'recommendation'
      ) {

        selected = stored;

      }

    } catch(e) {}

    page.setAttribute(
      'data-tbi-trade-tabs-ready',
      VERSION
    );

    activate(
      page,
      selected
    );

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function() {

      queued = false;
      buildTradeTabs();

    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_TRADE_SUBTABS_END <<<


// >>> TBI_DRAFT_SUBTABS_START >>>

(function () {

  'use strict';

  var VERSION = '1.0.0';

  function text(el) {

    return (el && el.textContent ? el.textContent : '')
      .replace(/\s+/g, ' ')
      .trim()
      .toUpperCase();

  }

  function classify(target) {

    var txt = text(target);

    /* Portfolio value + timeline */

    if (
      target.classList &&
      target.classList.contains('draft-v2-portfolio-grid')
    ) {
      return 'portfolio';
    }

    /* Risk / opportunity workspace */

    if (
      txt.indexOf('DRAFT HEADLINES') >= 0 ||
      txt.indexOf('ASSET RISKS') >= 0 ||
      txt.indexOf('ASSET OPPORTUNITIES') >= 0
    ) {
      return 'risk';
    }

    /* Ledger */

    if (
      txt.indexOf('DRAFT ASSET LEDGER') >= 0 ||
      txt.indexOf('CONTROLLED PICKS, SWAPS, AND OBLIGATIONS') >= 0
    ) {
      return 'ledger';
    }

    /* Recommendation */

    if (
      txt.indexOf('EXECUTIVE RECOMMENDATION') >= 0 ||
      txt.indexOf('RECOMMENDED DRAFT-CAPITAL POSTURE') >= 0
    ) {
      return 'recommendation';
    }

    /* Snapshot + draft decision/portfolio scorecard */

    return 'overview';

  }

  function activate(page, tabName) {

    page
      .querySelectorAll('.tbi-draft-subtab')
      .forEach(function(button) {

        button.classList.toggle(
          'active',
          button.getAttribute('data-tab') === tabName
        );

      });

    page
      .querySelectorAll('.tbi-draft-tab-target')
      .forEach(function(target) {

        var targetTab =
          target.getAttribute('data-tbi-draft-tab');

        target.classList.toggle(
          'tbi-draft-hidden',
          targetTab !== tabName
        );

      });

    try {

      sessionStorage.setItem(
        'tbi-draft-tab',
        tabName
      );

    } catch(e) {}

  }

  function buildDraftTabs() {

    var page =
      document.querySelector('.tbi-v2-draft-page');

    if (!page) return;

    if (
      page.getAttribute('data-tbi-draft-tabs-ready') === VERSION
    ) {
      return;
    }

    var intro =
      page.querySelector('.tbi-v2-module-intro');

    if (!intro) return;

    Array.prototype.forEach.call(
      page.children,
      function(child) {

        if (child === intro) return;

        if (
          child.classList &&
          child.classList.contains('tbi-draft-subnav')
        ) {
          return;
        }

        child.classList.add(
          'tbi-draft-tab-target'
        );

        child.setAttribute(
          'data-tbi-draft-tab',
          classify(child)
        );

      }
    );

    var nav =
      document.createElement('div');

    nav.className =
      'tbi-draft-subnav';

    nav.setAttribute(
      'role',
      'tablist'
    );

    var tabs = [
      ['overview', 'Overview'],
      ['portfolio', 'Portfolio & Timeline'],
      ['risk', 'Risks & Opportunities'],
      ['ledger', 'Asset Ledger'],
      ['recommendation', 'Recommendation']
    ];

    tabs.forEach(function(item) {

      var button =
        document.createElement('button');

      button.type = 'button';

      button.className =
        'tbi-draft-subtab';

      button.textContent = item[1];

      button.setAttribute(
        'data-tab',
        item[0]
      );

      button.setAttribute(
        'role',
        'tab'
      );

      button.addEventListener(
        'click',
        function() {

          activate(
            page,
            item[0]
          );

        }
      );

      nav.appendChild(button);

    });

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

    var selected = 'overview';

    try {

      var stored =
        sessionStorage.getItem(
          'tbi-draft-tab'
        );

      if (
        stored === 'overview' ||
        stored === 'portfolio' ||
        stored === 'risk' ||
        stored === 'ledger' ||
        stored === 'recommendation'
      ) {

        selected = stored;

      }

    } catch(e) {}

    page.setAttribute(
      'data-tbi-draft-tabs-ready',
      VERSION
    );

    activate(
      page,
      selected
    );

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function() {

      queued = false;
      buildDraftTabs();

    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_DRAFT_SUBTABS_END <<<


// >>> TBI_DEPTH_MODULE_HEADER_START >>>

(function () {

  'use strict';

  function buildDepthHeader() {

    var page =
      document.querySelector('.depth-v21-page');

    if (!page) return;

    if (
      page.querySelector('.tbi-depth-module-intro')
    ) {
      return;
    }

    var shell =
      page.querySelector('.depth-v21-shell');

    if (!shell) return;

    var header =
      document.createElement('div');

    header.className =
      'tbi-depth-module-intro';

    var copy =
      document.createElement('div');

    copy.className =
      'tbi-depth-module-copy';

    var eyebrow =
      document.createElement('div');

    eyebrow.className =
      'tbi-depth-module-eyebrow';

    eyebrow.textContent =
      'ROTATION MANAGEMENT';

    var title =
      document.createElement('h2');

    title.className =
      'tbi-depth-module-title';

    title.textContent =
      'Depth & Rotation Intelligence';

    var subtitle =
      document.createElement('p');

    subtitle.className =
      'tbi-depth-module-subtitle';

    subtitle.textContent =
      'Manage positional depth, starting groups, rotation structure, and player assignments.';

    copy.appendChild(eyebrow);
    copy.appendChild(title);
    copy.appendChild(subtitle);

    var model =
      document.createElement('div');

    model.className =
      'tbi-depth-module-model';

    var modelLabel =
      document.createElement('span');

    modelLabel.textContent =
      'DEPTH MODEL';

    var modelValue =
      document.createElement('strong');

    modelValue.textContent =
      'V2.1';

    model.appendChild(modelLabel);
    model.appendChild(modelValue);

    header.appendChild(copy);
    header.appendChild(model);

    page.insertBefore(
      header,
      shell
    );

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function () {
      queued = false;
      buildDepthHeader();
    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_DEPTH_MODULE_HEADER_END <<<

// >>> TBI_COMMAND_CENTER_TABS_START >>>

(function () {

  'use strict';

  var TABS = [
    ['decision', 'Executive Home'],
    ['scorecard', 'Decision Scorecard'],
    ['risks', 'Executive Priorities'],
    ['context', 'Team Context'],
    ['confidence', 'Decision Evidence']
  ];

  function selectedTab() {

    var selected = 'decision';

    try {
      var stored = sessionStorage.getItem('tbi-command-tab');
      if (TABS.some(function(item) { return item[0] === stored; })) selected = stored;
    } catch(e) {}

    return selected;
  }

  function tag(target, tabName) {

    if (!target) return;

    target.classList.add('tbi-command-tab-target');

    target.setAttribute(
      'data-tbi-command-tab',
      tabName
    );

    target.setAttribute('role', 'tabpanel');

  }

  function activate(page, tabName) {

    var changed =
      page.getAttribute('data-tbi-command-active-tab') !== tabName;

    page
      .querySelectorAll('.tbi-command-subtab')
      .forEach(function(button) {

        var isActive = button.getAttribute('data-tab') === tabName;

        button.classList.toggle(
          'active',
          isActive
        );

        button.setAttribute('aria-selected', isActive ? 'true' : 'false');
        button.setAttribute('tabindex', isActive ? '0' : '-1');

      });

    page
      .querySelectorAll('.tbi-command-tab-target')
      .forEach(function(target) {

        var targetTab =
          target.getAttribute('data-tbi-command-tab');

        target.classList.toggle(
          'tbi-command-hidden',
          targetTab !== tabName
        );

        target.setAttribute('aria-hidden', targetTab === tabName ? 'false' : 'true');

      });

    page.setAttribute('data-tbi-command-active-tab', tabName);

    if (changed) {
      try {
        sessionStorage.setItem('tbi-command-tab', tabName);
      } catch(e) {}

      window.TBIUX.notifySubtab(page, tabName);
    }

  }

  function buildNav(page) {

    var existing =
      page.querySelector('.tbi-command-subnav');

    if (existing) return existing;

    var header =
      page.querySelector('.executive-header-row');

    if (!header) return null;

    var nav = document.createElement('div');

    nav.className = 'tbi-command-subnav';
    nav.setAttribute('role', 'tablist');
    nav.setAttribute('aria-label', 'Command Center sections');

    TABS.forEach(function(item, tabIndex) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-command-subtab';
      button.textContent = item[1];

      button.setAttribute('data-tab', item[0]);
      button.setAttribute('role', 'tab');
      button.id = 'tbi-command-tab-' + item[0];

      button.addEventListener('click', function() {
        activate(page, item[0]);
      });

      button.addEventListener('keydown', function(event) {
        var nextIndex = tabIndex;
        if (event.key === 'ArrowRight') nextIndex = (tabIndex + 1) % TABS.length;
        else if (event.key === 'ArrowLeft') nextIndex = (tabIndex + TABS.length - 1) % TABS.length;
        else if (event.key === 'Home') nextIndex = 0;
        else if (event.key === 'End') nextIndex = TABS.length - 1;
        else return;
        event.preventDefault();
        var nextButton = nav.querySelectorAll('.tbi-command-subtab')[nextIndex];
        activate(page, nextButton.getAttribute('data-tab'));
        nextButton.focus();
      });

      nav.appendChild(button);

    });

    header.parentNode.insertBefore(
      nav,
      header.nextSibling
    );

    return nav;

  }

  function tagDecisionSections(page) {

    var shell =
      page.querySelector('.executive-intelligence-shell');

    if (!shell) return;

    tag(
      shell.querySelector('.tbi-exec-recommendation'),
      'decision'
    );

    tag(
      shell.querySelector('.tbi-exec-scorecard'),
      'scorecard'
    );

    var twoColumn =
      shell.querySelector('.tbi-executive-decision-view__two-column');

    tag(twoColumn, 'risks');

    tag(
      shell.querySelector('.tbi-exec-data-quality'),
      'confidence'
    );

    tag(
      shell.querySelector('.tbi-executive-decision-view__scope-note'),
      'confidence'
    );

  }

  function tagContextSections(page) {

    var status =
      page.querySelector('.executive-status-strip');

    tag(status, 'decision');

    tag(page.querySelector('.command-team-context'), 'context');
    tag(page.querySelector('.command-v2-basketball'), 'decision');
    tag(page.querySelector('.command-bie-priorities'), 'risks');
    tag(page.querySelector('.command-scenario-delta'), 'decision');
    tag(page.querySelector('.command-cba-reference'), 'decision');

  }

  function build() {

    var page =
      document.querySelector('.tbi-exec-dashboard-v2');

    if (!page) return;

    var nav = buildNav(page);

    if (!nav) return;

    tagDecisionSections(page);
    tagContextSections(page);
    TABS.forEach(function(item) {
      var button = nav.querySelector('[data-tab="' + item[0] + '"]');
      var controlled = [];
      page.querySelectorAll('.tbi-command-tab-target[data-tbi-command-tab="' + item[0] + '"]').forEach(function(target, index) {
        if (!target.id) target.id = 'tbi-command-panel-' + item[0] + '-' + (index + 1);
        target.setAttribute('aria-labelledby', button.id);
        controlled.push(target.id);
      });
      button.setAttribute('aria-controls', controlled.join(' '));
    });

    activate(
      page,
      selectedTab()
    );

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function() {
      queued = false;
      build();
    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_COMMAND_CENTER_TABS_END <<<

// >>> TBI_TEAM_OVERVIEW_TABS_START >>>

(function () {

  'use strict';

  var VERSION = '2.0.0';

  function text(el) {

    return (el && el.textContent ? el.textContent : '')
      .replace(/\s+/g, ' ')
      .trim()
      .toUpperCase();

  }

  function classify(target) {

    var txt = text(target);

    if (
      txt.indexOf('ORGANIZATIONAL DECISION') >= 0 ||
      txt.indexOf('ORGANIZATIONAL SCORECARD') >= 0
    ) {
      return 'decision';
    }

    if (
      txt.indexOf('TEAM PROFILE') >= 0 &&
      txt.indexOf('COMPETITIVE AND ROSTER INDICATORS') >= 0
    ) {
      return 'profile';
    }

    if (
      txt.indexOf('TEAM HEADLINES') >= 0 ||
      txt.indexOf('KEY RISKS') >= 0 ||
      txt.indexOf('KEY OPPORTUNITIES') >= 0
    ) {
      return 'risk';
    }

    if (
      txt.indexOf('CORE PERSONNEL') >= 0 ||
      txt.indexOf('LEAGUE CONTEXT') >= 0
    ) {
      return 'personnel';
    }

    if (
      txt.indexOf('EXECUTIVE RECOMMENDATION') >= 0 ||
      txt.indexOf('RECOMMENDED ORGANIZATIONAL POSTURE') >= 0
    ) {
      return 'recommendation';
    }

    return 'overview';

  }

  function activate(page, tabName) {

    page
      .querySelectorAll('.tbi-team-subtab')
      .forEach(function(button) {

        var isActive = button.getAttribute('data-tab') === tabName;

        button.classList.toggle(
          'active',
          isActive
        );

        button.setAttribute('aria-selected', isActive ? 'true' : 'false');
        button.setAttribute('tabindex', isActive ? '0' : '-1');

      });

    page
      .querySelectorAll('.tbi-team-tab-target')
      .forEach(function(target) {

        var targetTab =
          target.getAttribute('data-tbi-team-tab');

        target.classList.toggle(
          'tbi-team-hidden',
          targetTab !== tabName
        );

        target.setAttribute('aria-hidden', targetTab === tabName ? 'false' : 'true');

      });

    try {
      sessionStorage.setItem('tbi-team-tab', tabName);
    } catch(e) {}

    page.setAttribute('data-tbi-team-active-tab', tabName);

    window.TBIUX.notifySubtab(page, tabName);

  }

  function build() {

    var page =
      document.querySelector('.tbi-v2-team-page');

    if (!page) return;

    if (page.getAttribute('data-tbi-team-tabs-ready') === VERSION) {
      return;
    }

    var intro =
      page.querySelector('.tbi-v2-module-intro');

    if (!intro) return;

    Array.prototype.forEach.call(
      page.children,
      function(child) {

        if (child === intro) return;

        if (
          child.classList &&
          child.classList.contains('tbi-team-subnav')
        ) {
          return;
        }

        /* Trade scenario banner remains visible on every tab */

        if (
          child.id &&
          child.id.indexOf('team_trade_scenario_banner') >= 0
        ) {
          return;
        }

        child.classList.add('tbi-team-tab-target');

        if (!child.getAttribute('data-tbi-team-tab')) {
          child.setAttribute('data-tbi-team-tab', classify(child));
        }

        child.setAttribute('role', 'tabpanel');

      }
    );

    var nav = document.createElement('div');

    nav.className = 'tbi-team-subnav';
    nav.setAttribute('role', 'tablist');

    var tabs = [
      ['overview', 'Overview'],
      ['decision', 'Decision & Scorecard'],
      ['profile', 'Team Profile'],
      ['risk', 'Risks & Opportunities'],
      ['personnel', 'Personnel & Context'],
      ['recommendation', 'Recommendation']
    ];

    tabs.forEach(function(item, tabIndex) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-team-subtab';
      button.textContent = item[1];

      button.setAttribute('data-tab', item[0]);
      button.setAttribute('role', 'tab');
      button.id = 'tbi-team-tab-' + item[0];

      button.addEventListener('click', function() {
        activate(page, item[0]);
      });

      button.addEventListener('keydown', function(event) {
        var nextIndex = tabIndex;
        if (event.key === 'ArrowRight') nextIndex = (tabIndex + 1) % tabs.length;
        else if (event.key === 'ArrowLeft') nextIndex = (tabIndex + tabs.length - 1) % tabs.length;
        else if (event.key === 'Home') nextIndex = 0;
        else if (event.key === 'End') nextIndex = tabs.length - 1;
        else return;
        event.preventDefault();
        var nextButton = nav.querySelectorAll('.tbi-team-subtab')[nextIndex];
        activate(page, nextButton.getAttribute('data-tab'));
        nextButton.focus();
      });

      nav.appendChild(button);

    });

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

    var selected = 'overview';

    tabs.forEach(function(item) {
      var button = nav.querySelector('[data-tab="' + item[0] + '"]');
      var controlled = [];
      page.querySelectorAll('.tbi-team-tab-target[data-tbi-team-tab="' + item[0] + '"]').forEach(function(target, index) {
        target.id = 'tbi-team-panel-' + item[0] + '-' + (index + 1);
        target.setAttribute('aria-labelledby', button.id);
        controlled.push(target.id);
      });
      button.setAttribute('aria-controls', controlled.join(' '));
    });

    try {

      var stored = sessionStorage.getItem('tbi-team-tab');

      if (tabs.some(function(item) { return item[0] === stored; })) {
        selected = stored;
      }

    } catch(e) {}

    page.setAttribute('data-tbi-team-tabs-ready', VERSION);

    activate(page, selected);

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function() {
      queued = false;
      build();
    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_TEAM_OVERVIEW_TABS_END <<<

// >>> TBI_FIVE_YEAR_OUTLOOK_TABS_START >>>

(function () {

  'use strict';

  var VERSION = '2.0.0';

  var VALID = [
    'overview',
    'flexibility',
    'timeline',
    'contracts-free-agency',
    'draft-optionality',
    'recommendation'
  ];

  var REQUIRED_SECTIONS = [
    'long-range-snapshot',
    'overview-story',
    'strategic-flexibility',
    'organizational-timeline',
    'contract-runway',
    'draft-control-and-optionality',
    'executive-recommendation'
  ];

  function activate(page, tabName) {

    if (page.getAttribute('data-tbi-team-active-tab') === tabName) return;

    if (page.getAttribute('data-tbi-cap-active-tab') === tabName) return;

    page
      .querySelectorAll('.tbi-outlook-subtab')
      .forEach(function (button) {

        var isActive = button.getAttribute('data-tab') === tabName;

        button.classList.toggle('active', isActive);
        button.setAttribute('aria-selected', isActive ? 'true' : 'false');
        button.setAttribute('tabindex', isActive ? '0' : '-1');

      });

    page
      .querySelectorAll('[data-tbi-outlook-tab]')
      .forEach(function (target) {

        var isActive =
          target.getAttribute('data-tbi-outlook-tab') === tabName;

        target.classList.toggle('tbi-outlook-hidden', !isActive);
        target.setAttribute('aria-hidden', isActive ? 'false' : 'true');

      });

    try {
      sessionStorage.setItem('tbi-five-year-outlook-tab', tabName);
    } catch(e) {}

    window.TBIUX.notifySubtab(page, tabName);

  }

  function build() {

    var page =
      document.querySelector('.tbi-v2-outlook-page');

    if (!page) return;

    if (
      page.getAttribute('data-tbi-outlook-tabs-ready') === VERSION
    ) {
      return;
    }

    var intro =
      page.querySelector('.tbi-v2-module-intro');

    var targets =
      page.querySelectorAll('[data-tbi-outlook-tab]');

    var availableSections = Array.prototype.map.call(
      targets,
      function (target) {
        return target.getAttribute('data-tbi-outlook-section');
      }
    );

    if (
      !intro ||
      !REQUIRED_SECTIONS.every(function (section) {
        return availableSections.indexOf(section) >= 0;
      })
    ) return;

    targets.forEach(function (target) {
      target.classList.add('tbi-outlook-tab-target');
      target.setAttribute('role', 'tabpanel');
    });

    var previousNav = page.querySelector('.tbi-outlook-subnav');
    if (previousNav) previousNav.remove();

    var nav = document.createElement('div');

    nav.className = 'tbi-outlook-subnav';
    nav.setAttribute('role', 'tablist');
    nav.setAttribute('aria-label', 'Five-Year Outlook sections');

    var tabs = [
      ['overview', 'Overview'],
      ['flexibility', 'Flexibility'],
      ['timeline', 'Timeline'],
      ['contracts-free-agency', 'Contracts & Free Agency'],
      ['draft-optionality', 'Draft & Optionality'],
      ['recommendation', 'Recommendation']
    ];

    tabs.forEach(function (item, tabIndex) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-outlook-subtab';
      button.textContent = item[1];

      button.setAttribute('data-tab', item[0]);
      button.id = 'tbi-outlook-tab-' + item[0];
      button.setAttribute('role', 'tab');
      button.setAttribute('aria-selected', 'false');
      button.setAttribute('tabindex', '-1');

      var controlledTargets = Array.prototype.filter.call(
        targets,
        function (target) {
          return target.getAttribute('data-tbi-outlook-tab') === item[0];
        }
      );

      controlledTargets.forEach(function (target, targetIndex) {
        target.id = 'tbi-outlook-panel-' + item[0] + '-' + (targetIndex + 1);
        target.setAttribute('aria-labelledby', button.id);
      });

      button.setAttribute(
        'aria-controls',
        controlledTargets.map(function (target) { return target.id; }).join(' ')
      );

      button.addEventListener('click', function () {
        activate(page, item[0]);
      });

      button.addEventListener('keydown', function (event) {
        var nextIndex = tabIndex;

        if (event.key === 'ArrowRight') nextIndex = (tabIndex + 1) % tabs.length;
        else if (event.key === 'ArrowLeft') nextIndex = (tabIndex + tabs.length - 1) % tabs.length;
        else if (event.key === 'Home') nextIndex = 0;
        else if (event.key === 'End') nextIndex = tabs.length - 1;
        else return;

        event.preventDefault();
        var nextTab = nav.querySelectorAll('.tbi-outlook-subtab')[nextIndex];
        activate(page, nextTab.getAttribute('data-tab'));
        nextTab.focus();
      });

      nav.appendChild(button);

    });

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

    var selected = 'overview';

    try {

      var stored =
        sessionStorage.getItem('tbi-five-year-outlook-tab');

      if (tabs.some(function(item) { return item[0] === stored; })) {
        selected = stored;
      }

    } catch(e) {}

    page.setAttribute(
      'data-tbi-outlook-tabs-ready',
      VERSION
    );

    activate(page, selected);

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function () {
      queued = false;
      build();
    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_FIVE_YEAR_OUTLOOK_TABS_END <<<

// >>> TBI_EXTENSION_SIMULATOR_TABS_START >>>

(function () {

  'use strict';

  var VERSION = '2.1.0';

  var TABS = [
    ['proposal', 'Proposal'],
    ['cba-screen', 'CBA Screen'],
    ['financial-impact', 'Financial Impact'],
    ['recommendation', 'Recommendation']
  ];

  var resizeQueued = false;

  function syncLayouts(page, tabName) {

    page
      .querySelectorAll('.tbi-extension-tab-layout')
      .forEach(function (layout) {

        var hasVisibleSection = Array.prototype.some.call(
          layout.children,
          function (child) {
            return child.getAttribute('data-tbi-extension-tab') === tabName;
          }
        );

        layout.classList.toggle(
          'tbi-extension-layout-hidden',
          !hasVisibleSection
        );

        layout.setAttribute(
          'data-tbi-extension-active-tab',
          tabName
        );

      });

  }

  function resizeVisibleWidgets() {

    if (resizeQueued) return;

    resizeQueued = true;

    requestAnimationFrame(function () {
      resizeQueued = false;
      window.dispatchEvent(new Event('resize'));
    });

  }

  function activate(page, tabName) {

    page
      .querySelectorAll('.tbi-extension-subtab')
      .forEach(function (button) {

        var isActive = button.getAttribute('data-tab') === tabName;

        button.classList.toggle('active', isActive);
        button.setAttribute('aria-selected', isActive ? 'true' : 'false');
        button.setAttribute('tabindex', isActive ? '0' : '-1');

      });

    page
      .querySelectorAll('[data-tbi-extension-tab]')
      .forEach(function (target) {

        var isActive =
          target.getAttribute('data-tbi-extension-tab') === tabName;

        target.classList.toggle('tbi-extension-hidden', !isActive);
        target.setAttribute('aria-hidden', isActive ? 'false' : 'true');

      });

    syncLayouts(page, tabName);
    page.setAttribute('data-tbi-extension-active-tab', tabName);

    try {
      sessionStorage.setItem('tbi-extension-simulator-tab', tabName);
    } catch(e) {}

    window.TBIUX.notifySubtab(page, tabName);
    resizeVisibleWidgets();

  }

  function build() {

    var page =
      document.querySelector('.tbi-v2-extension-page');

    if (!page) return;

    if (
      page.getAttribute('data-tbi-extension-tabs-ready') === VERSION
    ) {
      return;
    }

    var intro =
      page.querySelector('.tbi-v2-module-intro');

    var targets =
      page.querySelectorAll('[data-tbi-extension-tab]');

    if (!intro || !targets.length) return;

    targets.forEach(function (target) {
      target.classList.add('tbi-extension-tab-target');
    });

    var nav = document.createElement('div');

    nav.className = 'tbi-extension-subnav';
    nav.setAttribute('role', 'tablist');
    nav.setAttribute('aria-label', 'Extension Simulator sections');

    TABS.forEach(function (item, tabIndex) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-extension-subtab';
      button.textContent = item[1];

      button.setAttribute('data-tab', item[0]);
      button.setAttribute('role', 'tab');
      button.id = 'tbi-extension-tab-' + item[0];

      button.addEventListener('click', function () {
        activate(page, item[0]);
      });

      button.addEventListener('keydown', function (event) {
        var nextIndex = tabIndex;
        if (event.key === 'ArrowRight') nextIndex = (tabIndex + 1) % TABS.length;
        else if (event.key === 'ArrowLeft') nextIndex = (tabIndex + TABS.length - 1) % TABS.length;
        else if (event.key === 'Home') nextIndex = 0;
        else if (event.key === 'End') nextIndex = TABS.length - 1;
        else return;
        event.preventDefault();
        var nextButton = nav.querySelectorAll('.tbi-extension-subtab')[nextIndex];
        activate(page, nextButton.getAttribute('data-tab'));
        nextButton.focus();
      });

      nav.appendChild(button);

    });

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

    TABS.forEach(function (item) {
      var button = nav.querySelector('[data-tab="' + item[0] + '"]');
      var controlled = [];
      page.querySelectorAll('[data-tbi-extension-tab="' + item[0] + '"]').forEach(function (target, index) {
        target.id = 'tbi-extension-panel-' + item[0] + '-' + (index + 1);
        target.setAttribute('role', 'tabpanel');
        target.setAttribute('aria-labelledby', button.id);
        controlled.push(target.id);
      });
      button.setAttribute('aria-controls', controlled.join(' '));
    });

    var selected = 'proposal';

    try {

      var stored =
        sessionStorage.getItem('tbi-extension-simulator-tab');

      var storedIsValid = TABS.some(function (item) {
        return item[0] === stored;
      });

      if (storedIsValid) {
        selected = stored;
      }

    } catch(e) {}

    page.setAttribute(
      'data-tbi-extension-tabs-ready',
      VERSION
    );

    activate(page, selected);

  }

  var queued = false;

  function schedule() {

    if (queued) return;

    queued = true;

    requestAnimationFrame(function () {
      queued = false;
      build();
    });

  }

  window.TBIUX.register(schedule);

})();

// <<< TBI_EXTENSION_SIMULATOR_TABS_END <<<
