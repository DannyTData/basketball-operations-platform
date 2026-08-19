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

(function () {
  'use strict';

  var TBI_PM_VERSION = '1.0.0';

  function normalizeText(value) {
    return (value || '').replace(/\s+/g, ' ').trim().toUpperCase();
  }

  function panelTitle(panel) {
    var title = panel.querySelector('.pi-panel-title');
    return title ? normalizeText(title.textContent) : '';
  }

  function classifyPanel(panel) {
    var title = panelTitle(panel);

    if (title.indexOf('PLAYER VALUE') >= 0 || title.indexOf('VALUE & FIT') >= 0 || title.indexOf('BIE') >= 0) {
      return 'value';
    }
    if (title.indexOf('FUTURE PROJECTION') >= 0 || title.indexOf('DEVELOPMENT') >= 0 || title.indexOf('ADVANCED IMPACT') >= 0) {
      return 'development';
    }
    if (title.indexOf('CONTRACT INTELLIGENCE') >= 0 || title.indexOf('CBA FLAGS') >= 0 || title.indexOf('ELIGIBILITY') >= 0) {
      return 'contract';
    }
    if (title.indexOf('EXECUTIVE RECOMMENDATION') >= 0 || title.indexOf('RECOMMENDATION') >= 0 || title.indexOf('RISK & OPPORTUNITY') >= 0) {
      return 'recommendation';
    }
    return 'overview';
  }

  function setTab(page, tabName) {
    page.querySelectorAll('.tbi-pm-subtab').forEach(function (button) {
      button.classList.toggle('active', button.getAttribute('data-tab') === tabName);
    });

    page.querySelectorAll('.tbi-pm-tab-target').forEach(function (target) {
      target.classList.toggle('tbi-pm-hidden', target.getAttribute('data-tbi-pm-tab') !== tabName);
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

    [
      ['overview', 'Overview'],
      ['value', 'Value & Fit'],
      ['development', 'Development'],
      ['contract', 'Contract & CBA'],
      ['recommendation', 'Recommendation']
    ].forEach(function (tab) {
      var button = document.createElement('button');
      button.type = 'button';
      button.className = 'tbi-pm-subtab';
      button.textContent = tab[1];
      button.setAttribute('data-tab', tab[0]);
      button.setAttribute('role', 'tab');
      button.addEventListener('click', function () { setTab(page, tab[0]); });
      subnav.appendChild(button);
    });

    mainGrid.parentNode.insertBefore(subnav, mainGrid);

    var savedTab = 'overview';
    try {
      var stored = window.sessionStorage.getItem('tbi-player-management-tab');
      if (stored) savedTab = stored;
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

  var VERSION = '1.0.0';

  function text(el) {
    return (el && el.textContent ? el.textContent : '')
      .replace(/\s+/g, ' ')
      .trim()
      .toUpperCase();
  }

  function classify(el) {

    var value = text(el);

    if (value.indexOf('ROSTER NEEDS + GAP ANALYSIS') >= 0) {
      return 'needs';
    }

    if (value.indexOf('ROSTER DECISION INTELLIGENCE') >= 0) {
      return 'decision';
    }

    if (value.indexOf('COMPLETE ROSTER') >= 0) {
      return 'roster';
    }

    return 'overview';
  }

  function activate(page, tab) {

    page.querySelectorAll('.tbi-roster-subtab')
      .forEach(function(button) {
        button.classList.toggle(
          'active',
          button.getAttribute('data-tab') === tab
        );
      });

    page.querySelectorAll('.tbi-roster-tab-target')
      .forEach(function(target) {

        var targetTab =
          target.getAttribute('data-tbi-roster-tab');

        target.classList.toggle(
          'tbi-roster-hidden',
          targetTab !== tab
        );

      });

    try {
      sessionStorage.setItem(
        'tbi-roster-tab',
        tab
      );
    } catch(e) {}

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

      el.setAttribute(
        'data-tbi-roster-tab',
        classify(el)
      );

    });

    var nav = document.createElement('div');
    nav.className = 'tbi-roster-subnav';
    nav.setAttribute('role', 'tablist');

    var tabs = [
      ['overview', 'Overview'],
      ['decision', 'Decision Intelligence'],
      ['needs', 'Needs & Gaps'],
      ['roster', 'Complete Roster']
    ];

    tabs.forEach(function(item) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-roster-subtab';
      button.textContent = item[1];
      button.setAttribute('data-tab', item[0]);
      button.setAttribute('role', 'tab');

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
      if (stored) selected = stored;
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

  var VERSION = '2.0.0';

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

  var VERSION = '1.0.0';

  function activate(page, tabName) {

    page
      .querySelectorAll('.tbi-depth-subtab')
      .forEach(function(button) {

        button.classList.toggle(
          'active',
          button.getAttribute('data-tab') === tabName
        );

      });

    page
      .querySelectorAll('.tbi-depth-tab-target')
      .forEach(function(target) {

        var targetTab =
          target.getAttribute('data-tbi-depth-tab');

        target.classList.toggle(
          'tbi-depth-hidden',
          targetTab !== tabName
        );

      });

    try {

      sessionStorage.setItem(
        'tbi-depth-tab',
        tabName
      );

    } catch(e) {}

  }

  function buildDepthTabs() {

    var shell =
      document.querySelector('.depth-v21-shell');

    if (!shell) return;

    if (
      shell.getAttribute('data-tbi-depth-tabs-ready') === VERSION
    ) {
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
      'lineup'
    );

    playerRail.classList.add(
      'tbi-depth-tab-target'
    );

    playerRail.setAttribute(
      'data-tbi-depth-tab',
      'player'
    );

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
      ['lineup', 'Lineup & Rotation'],
      ['player', 'Player Intelligence']
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

    shell.insertBefore(
      nav,
      shell.firstChild
    );

    var selected = 'depth';

    try {

      var stored =
        sessionStorage.getItem(
          'tbi-depth-tab'
        );

      if (
        stored === 'depth' ||
        stored === 'lineup' ||
        stored === 'player'
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

  var VERSION = '1.0.0';

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

        button.classList.toggle(
          'active',
          button.getAttribute('data-tab') === tabName
        );

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

      });

    try {

      sessionStorage.setItem(
        'tbi-cap-tab',
        tabName
      );

    } catch(e) {}

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

        child.classList.add(
          'tbi-cap-tab-target'
        );

        child.setAttribute(
          'data-tbi-cap-tab',
          classify(child)
        );

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
      ['risk', 'Risks & Flexibility'],
      ['contracts', 'Contracts & Readout']
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

    /* Put tabs directly below the Cap Intelligence intro */

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

    var selected = 'overview';

    try {

      var stored =
        sessionStorage.getItem(
          'tbi-cap-tab'
        );

      if (
        stored === 'overview' ||
        stored === 'decision' ||
        stored === 'risk' ||
        stored === 'contracts'
      ) {
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

        button.classList.toggle(
          'active',
          button.getAttribute('data-tab') === tabName
        );

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

  var VALID = [
    'decision',
    'scorecard',
    'risks',
    'context',
    'confidence'
  ];

  function selectedTab() {

    var selected = 'decision';

    try {
      var stored = sessionStorage.getItem('tbi-command-tab');
      if (VALID.indexOf(stored) >= 0) selected = stored;
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

  }

  function activate(page, tabName) {

    page
      .querySelectorAll('.tbi-command-subtab')
      .forEach(function(button) {

        button.classList.toggle(
          'active',
          button.getAttribute('data-tab') === tabName
        );

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

      });

    try {
      sessionStorage.setItem('tbi-command-tab', tabName);
    } catch(e) {}

    window.TBIUX.notifySubtab(page, tabName);

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

    var tabs = [
      ['decision', 'Decision'],
      ['scorecard', 'Scorecard'],
      ['risks', 'Risks & Opportunities'],
      ['context', 'Team Context'],
      ['confidence', 'Data Confidence']
    ];

    tabs.forEach(function(item) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-command-subtab';
      button.textContent = item[1];

      button.setAttribute('data-tab', item[0]);
      button.setAttribute('role', 'tab');

      button.addEventListener('click', function() {
        activate(page, item[0]);
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

    /* -----------------------------------------------------
       Elements returned dynamically by executive_decision
       are re-tagged after every Shiny render.
       ----------------------------------------------------- */

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

    /* Scope note belongs with Data Confidence */

    tag(
      shell.querySelector('.tbi-executive-decision-view__scope-note'),
      'confidence'
    );

  }

  function tagContextSections(page) {

    /* Executive status remains with the primary decision */

    var status =
      page.querySelector('.executive-status-strip');

    tag(status, 'decision');

    /* All broader team information becomes Team Context */

    var kpis =
      page.querySelector('.terminal-kpi-grid');

    tag(kpis, 'context');

    var mainGrid =
      page.querySelector('.terminal-main-grid');

    tag(mainGrid, 'context');

    var standings =
      page.querySelector('.standings-panel');

    tag(standings, 'context');

  }

  function build() {

    var page =
      document.querySelector('.tbi-exec-dashboard-v2');

    if (!page) return;

    var nav = buildNav(page);

    if (!nav) return;

    tagDecisionSections(page);
    tagContextSections(page);

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

  var VALID = [
    'overview',
    'decision',
    'profile',
    'risk',
    'personnel',
    'recommendation'
  ];

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

        button.classList.toggle(
          'active',
          button.getAttribute('data-tab') === tabName
        );

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

      });

    try {
      sessionStorage.setItem('tbi-team-tab', tabName);
    } catch(e) {}

    window.TBIUX.notifySubtab(page, tabName);

  }

  function build() {

    var page =
      document.querySelector('.tbi-v2-team-page');

    if (!page) return;

    if (page.getAttribute('data-tbi-team-tabs-ready') === '1') {
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

        child.setAttribute(
          'data-tbi-team-tab',
          classify(child)
        );

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

    tabs.forEach(function(item) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-team-subtab';
      button.textContent = item[1];

      button.setAttribute('data-tab', item[0]);
      button.setAttribute('role', 'tab');

      button.addEventListener('click', function() {
        activate(page, item[0]);
      });

      nav.appendChild(button);

    });

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

    var selected = 'overview';

    try {

      var stored = sessionStorage.getItem('tbi-team-tab');

      if (VALID.indexOf(stored) >= 0) {
        selected = stored;
      }

    } catch(e) {}

    page.setAttribute('data-tbi-team-tabs-ready', '1');

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

  var VERSION = '1.0.0';

  var VALID = [
    'overview',
    'flexibility',
    'timeline',
    'contracts-free-agency',
    'draft-optionality',
    'recommendation'
  ];

  function syncLayouts(page, tabName) {

    page
      .querySelectorAll('.tbi-outlook-tab-layout')
      .forEach(function (layout) {

        var hasVisibleSection = Array.prototype.some.call(
          layout.children,
          function (child) {
            return child.getAttribute('data-tbi-outlook-tab') === tabName;
          }
        );

        layout.classList.toggle(
          'tbi-outlook-layout-hidden',
          !hasVisibleSection
        );

        layout.setAttribute(
          'data-tbi-outlook-active-tab',
          tabName
        );

      });

  }

  function activate(page, tabName) {

    page
      .querySelectorAll('.tbi-outlook-subtab')
      .forEach(function (button) {

        var isActive = button.getAttribute('data-tab') === tabName;

        button.classList.toggle('active', isActive);
        button.setAttribute('aria-selected', isActive ? 'true' : 'false');

      });

    page
      .querySelectorAll('[data-tbi-outlook-tab]')
      .forEach(function (target) {

        var isActive =
          target.getAttribute('data-tbi-outlook-tab') === tabName;

        target.classList.toggle('tbi-outlook-hidden', !isActive);
        target.setAttribute('aria-hidden', isActive ? 'false' : 'true');

      });

    syncLayouts(page, tabName);

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

    if (!intro || targets.length !== 9) return;

    targets.forEach(function (target) {
      target.classList.add('tbi-outlook-tab-target');
    });

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

    tabs.forEach(function (item) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-outlook-subtab';
      button.textContent = item[1];

      button.setAttribute('data-tab', item[0]);
      button.setAttribute('role', 'tab');

      button.addEventListener('click', function () {
        activate(page, item[0]);
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

      if (VALID.indexOf(stored) >= 0) {
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

  var VERSION = '1.0.0';

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

    if (!intro || targets.length !== 11) return;

    targets.forEach(function (target) {
      target.classList.add('tbi-extension-tab-target');
    });

    var nav = document.createElement('div');

    nav.className = 'tbi-extension-subnav';
    nav.setAttribute('role', 'tablist');
    nav.setAttribute('aria-label', 'Extension Simulator sections');

    TABS.forEach(function (item) {

      var button = document.createElement('button');

      button.type = 'button';
      button.className = 'tbi-extension-subtab';
      button.textContent = item[1];

      button.setAttribute('data-tab', item[0]);
      button.setAttribute('role', 'tab');

      button.addEventListener('click', function () {
        activate(page, item[0]);
      });

      nav.appendChild(button);

    });

    intro.parentNode.insertBefore(
      nav,
      intro.nextSibling
    );

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
