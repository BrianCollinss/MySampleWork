const summaryCardBinding = new Shiny.InputBinding();

$.extend(summaryCardBinding, {
  initialize: function(el) {
    if (!el.classList.contains('irat-summary-card__value--animated')) {
      return;
    }
    if (el.classList.contains('irat-summary-card__value--animated-rendered')) {
      return;
    }
    function decodeEntities(encodedString) {
      const translate_re = /&(nbsp|amp|quot|lt|gt);/g;
      const translate = {
          "nbsp":" ",
          "amp" : "&",
          "quot": "\"",
          "lt"  : "<",
          "gt"  : ">"
      };
      return encodedString.replace(translate_re, function(match, entity) {
          return translate[entity];
      }).replace(/&#(\d+);/gi, function(match, numStr) {
          const num = parseInt(numStr, 10);
          return String.fromCharCode(num);
      });
  }
    const animation = bounty.default({
      el: '#' + el.id,
      value: decodeEntities(el.dataset.value),
      lineHeight: 1.4,
      duration: 1000
    });
    const observer = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        const visible = entry.intersectionRatio > 0;
        visible ? animation.resume() : animation.pause();
      });
    });
    observer.observe(el);
    el.classList.add('irat-summary-card__value--animated-rendered');
  },
  find: function(scope) {
    return $(scope).find('.irat-summary-card__value');
  },
  getValue: function() {},
  receiveMessage: function() {},
});

Shiny.inputBindings.register(summaryCardBinding);
